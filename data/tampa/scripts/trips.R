
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

#The PERIOD entries for TimeUse and 3DAnimatedMap are 1 to 48 and represent 30 minute periods from 3am to 3am the next day.  

trips = "C:/projects/tampa/tampa_survey_deliverable_20191218/HTS Data files (CSV)/TBRTS-HTS_Trip_Table_20191218.csv"
trips = read.csv(trips)
trips = trips[,c("mode_type","arrival_time","d_taz_2020","trip_weight_household")]

#recode mode

library(stringr)

modes = c("Missing"=-9998, "Walk"=1, "Bike"=2, "Car"=3, "Taxi_Not_TNC"=4, "Transit"=5, "School_Bus"=6, "Other"=7, "Shuttle_VanPool"=8, "TNC"=9, "Car_Share"=10, "Bike_Share"=11, "Long_Dist_Pass"=13)
trips$mode = names(modes)[match(trips$mode_type,modes)]
trips$mode = str_to_upper(trips$mode)

#recode arrival time

trips$time = sapply(as.character(trips$arrival_time), function(x) strsplit(x, " ")[[1]][2])
trips$hour = sapply(as.character(trips$time), function(x) strsplit(x, ":")[[1]][1])
trips$min = sapply(as.character(trips$time), function(x) strsplit(x, ":")[[1]][2])
trips$mpm = as.integer(trips$hour) * 60 + as.integer(trips$min)
trips$mpm_period = as.integer(trips$mpm / 30)
trips$mpm_period_shift = trips$mpm_period - 5
trips$mpm_period_shift[trips$mpm_period_shift < 1] = trips$mpm_period_shift[trips$mpm_period_shift < 1] + 48
trips$mpm_period_shift = paste0("PER", ifelse(trips$mpm_period_shift<10, "0", ""), trips$mpm_period_shift)

table(trips$mpm_period_shift)

# create summary of all combinations
trips_period_mode = aggregate(trips$trip_weight_household, list(trips$d_taz_2020,trips$mpm_period_shift,trips$mode), sum, drop=FALSE)
colnames(trips_period_mode) = c("ZONE","PER","MODE","TRIPS")

#for(mname in unique(trips_period_mode$MODE)) {
#  out_table = trips_period_mode[trips_period_mode$MODE=="MISSING",c("ZONE","PER")]
#  out_table[paste0(mname,"_TRIPS")] = as.integer(trips_period_mode$TRIPS[trips_period_mode$MODE==mname])
#  out_file_name = paste0("C:/projects/tampa/survey-viz/trips_zone_period_", mname, ".csv")
#  out_table = out_table[order(out_table$ZONE,out_table$PER),]
#  write.csv(out_table, out_file_name, row.names=F, quote=F)
#}

# all day
trips_mode = aggregate(trips$trip_weight_household, list(trips$d_taz_2020,trips$mode), sum, drop=FALSE)
colnames(trips_mode) = c("ZONE","PER","TRIPS")
trips_mode_fname = "C:/projects/tampa/survey-viz/trips_zone.csv"
write.csv(trips_mode, trips_mode_fname, row.names=F, quote=F)
