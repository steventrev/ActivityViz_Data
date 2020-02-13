
# trips by zone, period, mode weighted by HH weight for 3D animated map
# Ben.Stabler@rsginc.com, 01/12/20

# -9998	Missing: Non-response
# 1	Walk
# 2	Bik
# 3	Car
# 4	Taxi (non-TNC)
# 5	Transit
# 6	School bus
# 7	Other
# 8	Shuttle/vanpool
# 9	TNC
# 10	Car share
# 11	Bike share
# 13	Long distance passenger mode
library(data.table)

#The PERIOD entries for TimeUse and 3DAnimatedMap are 1 to 48 and represent 30 minute periods from 3am to 3am the next day.

if(!fil.exists("trips.rds")){
  trips = "Q:/Projects/FL/FDOT/R1705_Tampa Bay Surveys/7.Documentation/2.Main/deliverable_20191218/HTS Data files (CSV)/TBRTS-HTS_Trip_Table_20191218.csv"
  trips = fread(trips)
  trips = trips[,c("mode_type","arrival_time","d_taz_2020","trip_weight_household"), with = FALSE]
  saveRDS(trips, file = "trips.rds")
} else {
  trips = readRDS("trips.rds")
}


#recode mode

library(stringr)

modes = c("Missing"         = -9998, 
          "Walk"            = 1, 
          "Bike"            = 2, 
          "Car"             = 3, 
          "Taxi_Not_TNC"    = 4, 
          "Transit"         = 5, 
          "School_Bus"      = 6, 
          "Other"           = 7, 
          "Shuttle_VanPool" = 8, 
          "TNC"             = 9, 
          "Car_Share"       = 10, 
          "Bike_Share"      = 11, 
          "Long_Dist_Pass"  = 13
          )
trips[, mode := names(modes)[match(mode_type,modes)]]
trips[, mode := str_to_upper(mode)]

#recode arrival time

trips[, time := sapply(as.character(arrival_time), function(x) strsplit(x, " ")[[1]][2])]
trips[, hour := sapply(as.character(time), function(x) strsplit(x, ":")[[1]][1])]
trips[, min  := sapply(as.character(time), function(x) strsplit(x, ":")[[1]][2])]
trips[, mpm  := as.integer(hour) * 60 + as.integer(min)]
trips[, mpm_period := as.integer(mpm / 30)]
trips[, mpm_period_shift := mpm_period - 5]
trips[mpm_period_shift < 1, mpm_period_shift := mpm_period_shift + 48]
trips[, mpm_period_shift := paste0("PER", ifelse(mpm_period_shift<10, "0", ""), mpm_period_shift)]

table(trips$mpm_period_shift)

# create summary of all combinations
setkey(trips, d_taz_2020, mpm_period_shift, mode)
trips_period_mode = trips[
  !(is.na(d_taz_2020) | is.na(mpm_period_shift) | is.na(mode))
  ][CJ(d_taz_2020,
       mpm_period_shift,
       mode,
       unique = TRUE)][, .(TRIPS = sum(trip_weight_household)),
                       by = .(ZONE = d_taz_2020,
                              PER  = mpm_period_shift,
                              MODE = mode)][order(ZONE, PER, MODE)]

# all day
setkey(trips, d_taz_2020, mode)
trips_mode = trips[
  !(is.na(d_taz_2020) | is.na(mode))
  ][CJ(d_taz_2020,
       mode,
       unique = TRUE)][, .(TRIPS = sum(trip_weight_household)),
                       by = .(ZONE = d_taz_2020,
                              MODE = mode)][order(ZONE, MODE)]

# Output trips by mode
trips_mode_fname = file.path(getwd(), c("Executive_Summary",
                                        "Passive_Data",
                                        "Travel_Survey"),
                             "trips_mode_zone.csv")
fwrite(trips_mode, file = trips_mode_fname, row.names=F, quote=F)
