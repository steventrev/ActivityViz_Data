##################################################################################
# Create data for Executive Summary
# Author: Aditya Gore
##################################################################################


### Load/Install required packages ###############################################
##################################################################################
library(data.table)
library(jsonlite)
library(stringr)


### Define Constants #############################################################
##################################################################################

# Input files
data_dir   = normalizePath(paste0("Q:/Projects/FL/FDOT/R1705_Tampa Bay Surveys/",
                                  "7.Documentation/2.Main/deliverable_20191218/",
                                  "HTS Data files (CSV)"),
                           winslash = "/")

household_file = file.path(data_dir, "TBRTS-HTS_Household_Table_20191218.csv")
person_file    = file.path(data_dir, "TBRTS-HTS_Person_Table_20191218.csv") 
vehicle_file   = file.path(data_dir, "TBRTS-HTS_Vehicle_Table_20191218.csv")
day_file       = file.path(data_dir, "TBRTS-HTS_Day_Table_20191218.csv") 
trip_file      = file.path(data_dir, "TBRTS-HTS_Trip_Table_20191218.csv")

# Geography input files
taz_file       = file.path(getwd(), "tampa2020.json")
county_file    = file.path(getwd(), "counties.json")

# Output files
es_output_dir = file.path(getwd(), "Executive_Summary")
pd_output_dir = file.path(getwd(), "Passive_Data")
ts_output_dir = file.path(getwd(), "Travel_Survey")

# Executive_Summary
snapshot_file = file.path(es_output_dir, "snapshot.csv")
trip_zone_file1  = file.path(es_output_dir, "trips_zone.csv")
trips_period_file = file.path(es_output_dir, "trips_period.csv")
destination_file = file.path(es_output_dir, "trips_destination.csv")
work_mode_file   = file.path(es_output_dir, "work_mode.csv")
telecommute_file = file.path(es_output_dir, "telecommute_freq_age.csv")
bike_file        = file.path(es_output_dir, "bike_freq_age.csv")
commute_freq_file= file.path(es_output_dir, "commute_freq_age.csv")
age_file         = file.path(es_output_dir, "age_distribution.csv")
gender_file      = file.path(es_output_dir, "gender_distribution.csv")

# Passive_Data
trip_zone_file2  = file.path(pd_output_dir, "trips_zone.csv")
trip_od_file1    = file.path(pd_output_dir, "trip_od.csv")

# Travel_Survey
trip_zone_file3           = file.path(ts_output_dir, "trips_zone.csv")
trip_mode_file            = file.path(ts_output_dir, "trip_mode_zone.csv")
seasonal_trip_mode_file   = file.path(ts_output_dir, "seasonal_trip_mode_zone.csv")
uni_trip_mode_file        = file.path(ts_output_dir, "uni_trip_mode_zone.csv")
day_pattern_file          = file.path(ts_output_dir, "day_pattern.csv")
time_use_file             = file.path(ts_output_dir, "time_use.csv")
trip_od_file2             = file.path(ts_output_dir, "trip_od.csv")

### Load required datasets #######################################################
##################################################################################

if(!file.exists("../hts_data.RData")){
  household_dt = fread(household_file)
  person_dt    = fread(person_file)
  day_dt       = fread(day_file)
  trip_dt      = fread(trip_file)
  save(household_dt, person_dt, day_dt, trip_dt,
       file = "hts_data.RData")
} else {
  load("../hts_data.RData")
}

taz_ls = fromJSON(taz_file)
county_ls = fromJSON(county_file)


### Create output data ###########################################################
##################################################################################

# Snapshot Data
snapshot_dt = data.table(Value = c(household_dt[,.N],
                                   person_dt[,.N],
                                   trip_dt[,.N]),
                         Description = c("households participated in the survey",
                                         "total people took part",
                                         "trips recorded as part of the survey"))

# Trip mode by zone

# Trips start by period
# 3 a.m. - 3 a.m <--> 1 - 48 on the chart
trip_dt[, dep_time:=as.POSIXct(departure_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")]
trip_dt[, hour := hour(dep_time)]
# trip_dt[, min  := minute(dep_time)]
# trip_dt[, mpm  := hour * 60]# + min]
# trip_dt[, mpm_period := as.integer(mpm / 60)]
# trip_dt[, mpm_period_shift := mpm_period - 5]
# trip_dt[mpm_period_shift < 1, mpm_period_shift := mpm_period_shift + 48]
# trip_dt[, mpm_period_shift := paste0("PER", ifelse(mpm_period_shift<10, "0", ""), mpm_period_shift)]
# 
trip_start_dt = trip_dt[,.(TYPE = "TRIP START",
                           TRIPS = round(sum(trip_weight_household), 2)),
                        by = .(HOUR = hour)]
# # Trips end by period
trip_dt[, arr_time:=as.POSIXct(arrival_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")]
trip_dt[, hour := hour(arr_time)]
# trip_dt[, min  := minute(arr_time)]
# trip_dt[, mpm  := hour * 60 + min]
# trip_dt[, mpm_period := as.integer(mpm / 30)]
# trip_dt[, mpm_period_shift := mpm_period - 5]
# trip_dt[mpm_period_shift < 1, mpm_period_shift := mpm_period_shift + 48]
# trip_dt[, mpm_period_shift := paste0("PER", ifelse(mpm_period_shift<10, "0", ""), mpm_period_shift)]
# 
trip_end_dt = trip_dt[,.(TYPE = "TRIP END",
                         TRIPS = round(sum(trip_weight_household), 2)),
                      by = .(HOUR = hour)]

trip_period_dt = rbindlist(list(trip_start_dt, trip_end_dt), use.names = TRUE)
# trip_period_dt[,PER:=as.integer(gsub("\\D","",PER))]
# trip_period_dt[,GROUP:="ALL"]
rm(trip_start_dt, trip_end_dt)
trip_period_dt[,PER:=ifelse(HOUR > 12, paste0(HOUR - 12," PM"),
                            ifelse(HOUR == 12, "12 PM",
                                   ifelse(HOUR == 0, "12 AM", paste0(HOUR, " AM"))))]
setorder(trip_period_dt, TYPE, HOUR)
trip_period_dt[,HOUR:=NULL]
setcolorder(trip_period_dt, c("TYPE", "PER"))
trip_period_dt[,CHART:="TYPICAL TRAVEL"]

# setkey(trip_dt, d_taz_2020, mpm_period_shift)
# trip_start_dt = trip_dt[
#   !(is.na(d_taz_2020) | is.na(mpm_period_shift))
#   ][CJ(d_taz_2020,
#        mpm_period_shift,
#        unique = TRUE)][, .(TRIPS = sum(trip_weight_household)),
#                        by = .(ZONE = d_taz_2020,
#                               PER = mpm_period_shift)][order(ZONE, PER)]


# Daily trip destination data
destination_labels_ar = c(
  "Missing: Non-response"                      = -9998,
  "Activity at Home"                           = 1,
  "Work/Work-related"                          = 2,
  "Work/Work-related"                          = 3,
  "Attending schoool/class"                    = 4,
  "Drop-off/pick-up/accompany another person"  = 5,
  "Shopping/errands/appointments"              = 6,
  "Dine out/get coffee or take-out"            = 7,
  "Social/Recreation"                          = 8,
  "Shopping/errands/appointments"              = 9,
  "Change travel mode"                         = 10,
  "Other (Unknown)"                            = 11,
  "Other/Missing"                              = 12
)

# destination_label_order = c(
#   "Activity at Home",
#   "Shopping/errands/appointments",
#   "Work/Work-related",
#   "Social/Recreation/Entertainment",
#   "Drop-off/pick-up/accompany another person",
#   "Dine out/get coffee or take-out",
#   "Attending schoool/class",
#   "Change travel mode",
#   "Missing: Non-response",
#   "Shopping/errands/appointments",
#   "Other (Unknown)",
#   "Other/Missing"
# )


trip_dt[,destination_labels:=names(destination_labels_ar)[match(d_purpose_category_imputed,
                                                                destination_labels_ar)]]

trip_destination_dt = trip_dt[!d_purpose_category_imputed %in% c(-9998, 11, 12),
                              .(TRIPS = round(sum(trip_weight_household), 2)),
                              by = .(DESTINATION = destination_labels)]
setorder(trip_destination_dt, -TRIPS)
trip_destination_dt[,CHART:="TRIP DESTINATION"]
trip_destination_dt[,GROUP:="ALL"]
setcolorder(trip_destination_dt, c("GROUP","DESTINATION", "TRIPS", "CHART"))

# Usual ways to commutes to work
work_mode_labels_ar = c(
  "Missing: Non-response" = -9998,
  "WALK_WHEELCHAIR_BICYCLE" = 1,
  "WALK_WHEELCHAIR_BICYCLE" = 2,
  "DRIVEALONE" = 3,
  "CARPOOL" = 4,
  "CARPOOL" = 5,
  "OTHER" = 6,
  "PUBLICTRANSIT" = 7,
  "OTHER" = 8,
  "PUBLICTRANSIT" = 9,
  "OTHER" = 10,
  "OTHER" = 11,
  "OTHER" = 12,
  "PUBLICTRANSIT" = 13,
  "Missing: Skip logic" = 995,
  "OTHER" = 997
)

person_dt[,work_mode_labels:=names(work_mode_labels_ar)[match(work_mode,
                                                              work_mode_labels_ar)]]

work_mode_dt = person_dt[!work_mode %in% c(-9998, 995),.(PERSON = round(sum(weight_household), 2)),
                         by = .(MODE = work_mode_labels)]

setorder(work_mode_dt, -PERSON)
# work_mode_dt[,ACTIVITY:="WORK"]
# setcolorder(work_mode_dt, c("ACTIVITY"))
# work_mode_dt[,.(MODE, PERSON, PERCENT = scales::percent(prop.table(PERSON)))]

# Telecommute frequency by age
telecommute_labels_ar = c(
  "4+ days per week" = 1,
  "4+ days per week" = 2,
  "4+ days per week" = 3,
  "1-3 days per week" = 4,
  "1-3 days per week" = 5,
  "4+ days per week" = 6,
  "1-3 days per month" = 7,
  "A few times per year" = 8,
  "Never" = 9,
  "Missing: Skip logic" = 995
)

age_labels_ar = c(
  "16-17" = 1,
  "Under 5" = 3,
  "5 to 15" = 4,
  "18-34" = 5,
  "18-34" = 6,
  "35-54" = 7,
  "35-54" = 8,
  "55+" = 9,
  "55+" = 10,
  "55+" = 11
)

person_dt[,telework_labels:=names(telecommute_labels_ar)[match(telework_freq,
                                                               telecommute_labels_ar)]]
person_dt[,age_group:=names(age_labels_ar)[match(age,
                                          age_labels_ar)]]

telecommute_freq_dt = person_dt[!telework_freq %in% c(995) &
                                  age > 4,.(PERSON = sum(weight_household)),
                                by = .(AGE_GROUP = age_group,
                                       FREQ = telework_labels)]

telecommute_total = person_dt[!telework_freq %in% c(995) &
                                age > 4,.(AGE_GROUP = "Total",
                                          PERSON = sum(weight_household)),
                              by = .(FREQ = telework_labels)]
telecommute_freq_dt = rbindlist(list(telecommute_freq_dt, telecommute_total), use.names = TRUE)

setorder(telecommute_freq_dt, AGE_GROUP, -PERSON)
telecommute_freq_dt[,CHART:="TELECOMMUTE_FREQUENCY"]

# telecommute_freq_dt[,.(FREQ, PERSON, PERCENT = scales::percent(prop.table(PERSON))),
#                     by = .(AGE_GROUP)]

rm(telecommute_total)


# Bicycle use frequency by age

bike_labels_ar = c(
  "Missing: Non-response" = -9998,
  "4+ days per week" = 1,
  "4+ days per week" = 2,
  "4+ days per week" = 3,
  "1-3 days per week" = 4,
  "1-3 days per week" = 5,
  "1-3 days per month" = 6,
  "A few times per year" = 7,
  "Never" = 8,
  "Missing: Skip logic" = 995
)

person_dt[,bike_labels:=names(bike_labels_ar)[match(bike_freq,
                                                               bike_labels_ar)]]

bike_freq_dt = person_dt[!bike_freq %in% c(995, -9998) &
                                  age > 4,.(PERSON = sum(weight_household)),
                                by = .(AGE_GROUP = age_group,
                                       FREQ = bike_labels)]

bike_total = person_dt[!bike_freq %in% c(995, -9998) &
                                age > 4,.(AGE_GROUP = "Total",
                                          PERSON = sum(weight_household)),
                              by = .(FREQ = bike_labels)]
bike_freq_dt = rbindlist(list(bike_freq_dt, bike_total), use.names = TRUE)

setorder(bike_freq_dt, AGE_GROUP, -PERSON)
bike_freq_dt[,CHART:="BIKE_USE_FREQUENCY"]

bike_freq_dt[,.(FREQ, PERSON, PERCENT = scales::percent(prop.table(PERSON))),
                    by = .(AGE_GROUP)]

commute_frequency_dt = rbindlist(list(telecommute_freq_dt, bike_freq_dt), use.names = TRUE)

rm(bike_total)

# Deomographics
age_labels_ar = c(
  "16-17" = 1,
  "Under 5" = 3,
  "5 to 15" = 4,
  "18-24" = 5,
  "25-34" = 6,
  "35-44" = 7,
  "45-54" = 8,
  "55-64" = 9,
  "65-74" = 10,
  "75+" = 11
)

person_dt[,age_group:=names(age_labels_ar)[match(age,
                                                 age_labels_ar)]]
# age_group_dt = person_dt[age > 4, .(PERSON=sum(weight_person)),
#                          by=.(AGE_GROUP=age_group)]
age_group_dt = person_dt[age > 4, .(PERSON=.N),
                         by=.(AGE_GROUP=age_group)]

setorder(age_group_dt, AGE_GROUP)
age_group_dt[,CHART:="AGE DISTRIBUTION"]
age_group_dt[,TYPE:="PARTICIPANT"]
setcolorder(age_group_dt, c("TYPE"))
# age_group_dt[,.(AGE_GROUP, PERSON, PERCENT = scales::percent(prop.table(PERSON)))]


gender_labels_ar = c(
  "Missing: Non-response" = -9998,
  "Female" = 1,
  "Male" = 2,
  "Other gender" = 3,
  "Prefer not to answer" = 999
)

person_dt[,gender_label:=names(gender_labels_ar)[match(gender,
                                                       gender_labels_ar)]]
# gender_dt = person_dt[gender %in% c(1,2), .(PERSON=sum(weight_person)),
#                       by=.(GENDER=gender_label)]
gender_dt = person_dt[gender %in% c(1,2), .(PERSON=.N),
                      by=.(GENDER=gender_label)]
setorder(gender_dt, GENDER)
# gender_dt[,CHART:="GENDER DISTRIBUTION"]
# gender_dt[,TYPE:="PARTICIPANT"]
# setcolorder(gender_dt, c("TYPE"))
# gender_dt[,.(GENDER, PERSON, PERCENT = scales::percent(prop.table(PERSON)))]


# County to county trips
taz_dt = data.table(taz_ls$features$properties)
county_dt = data.table(county_ls$features$properties)

county_trips = merge(trip_dt[,.(o_taz=o_taz_2020,d_taz = d_taz_2020, trip_weight_household)],
                     taz_dt,by.x = "o_taz", by.y="id", all.x = TRUE)
county_trips = merge(county_trips,
                     taz_dt,by.x = "d_taz", by.y="id", all.x = TRUE, suffixes = c("_o","_d"))
setkey(county_trips, Cnty_Nm_o, Cnty_Nm_d)
county_trips = county_trips[CJ(Cnty_Nm_o, Cnty_Nm_d, unique = TRUE)][,
                                                                     .(TRIPS = 
                                                                         sum(
                                                                           trip_weight_household)),
                                                                     by = .(Cnty_Nm_o, Cnty_Nm_d)]

stopifnot(all.equal(county_trips[,sum(TRIPS,na.rm = TRUE)],
                    trip_dt[,sum(trip_weight_household)]))
county_trips[,":="(Cnty_Nm_o=ifelse(is.na(Cnty_Nm_o),"External",Cnty_Nm_o),
                   Cnty_Nm_d=ifelse(is.na(Cnty_Nm_d),"External",Cnty_Nm_d),
                   TRIPS=ifelse(is.na(TRIPS),0,TRIPS))]
setnames(county_trips,c("Cnty_Nm_o", "Cnty_Nm_d"), c("FROM", "TO"))

# Trips by mode by county
#recode mode
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
trip_dt[, mode := names(modes)[match(mode_type,modes)]]
trip_dt[, mode := str_to_upper(mode)]
setkey(trip_dt, d_taz_2020, mode)
trips_mode = trip_dt[
  !(is.na(d_taz_2020) | is.na(mode))
  ][CJ(d_taz_2020,
       mode,
       unique = TRUE)][, .(TRIPS = sum(trip_weight_household)),
                       by = .(ZONE = d_taz_2020,
                              MODE = mode)][order(ZONE, MODE)]
trips_mode[is.na(TRIPS), TRIPS:=0]
setkey(taz_dt, id)
trips_mode[,COUNTY:=taz_dt[.(ZONE),Cnty_Nm]]
setcolorder(trips_mode, c("ZONE", "COUNTY"))

# Trip mode by zone
trip_zone = trip_dt[
  !(is.na(d_taz_2020) | is.na(mode))
  ][CJ(d_taz_2020,
       mode,
       unique = TRUE)][, .(TRIPS = sum(trip_weight_household)),
                       by = .(ZONE = d_taz_2020,
                              MODE = mode)][order(ZONE, MODE)]
trip_zone[is.na(TRIPS), TRIPS:=0]


# Seasonal household
trip_dt[,is_seasonal:=0L]
trip_dt[household_dt[seasonal_household==1,.(hh_id)],is_seasonal:=1L,on=.(hh_id)]

seasonal_trips_mode = trip_dt[
  !(is.na(d_taz_2020) | is.na(mode)) & is_seasonal==1
  ][CJ(d_taz_2020,
       mode,
       unique = TRUE)][, .(TRIPS = sum(trip_weight_household)),
                       by = .(ZONE = d_taz_2020,
                              MODE = mode)][order(ZONE, MODE)]
seasonal_trips_mode[is.na(TRIPS), TRIPS:=0]
seasonal_trips_mode[,COUNTY:=taz_dt[.(ZONE),Cnty_Nm]]
setcolorder(seasonal_trips_mode, c("ZONE", "COUNTY"))

# University student
trip_dt[,uni_student:=0L]
trip_dt[person_dt[university_student==1,.(personid)],uni_student:=1L,on=.(personid)]

uni_trips_mode = trip_dt[
  !(is.na(d_taz_2020) | is.na(mode)) & uni_student==1
  ][CJ(d_taz_2020,
       mode,
       unique = TRUE)][, .(TRIPS = sum(trip_weight_household)),
                       by = .(ZONE = d_taz_2020,
                              MODE = mode)][order(ZONE, MODE)]
uni_trips_mode[is.na(TRIPS), TRIPS:=0]
uni_trips_mode[,COUNTY:=taz_dt[.(ZONE),Cnty_Nm]]
setcolorder(uni_trips_mode, c("ZONE", "COUNTY"))

# Day Pattern
# day_pattern_dt = person_dt[,.(personid, age, employment, university_student)][trip_dt[,.(personid,trip_id,d_purpose_category_imputed,trip_weight_household)],on=.(personid)]
day_pattern_dt = person_dt[,.(personid, age, employment, university_student)][day_dt,on=.(personid)]
day_pattern_dt[,person_group:=ifelse(age==1,"CHILD AGE 16 PLUS",
                                     ifelse(age==3, "CHILD AGE 0 TO 4",
                                            ifelse(age==4, "CHILD AGE 5 TO 15",
                                                   ifelse(employment==1, "FULL TIME WORKER",
                                                          ifelse(employment==2, "PART TIME WORKER",
                                                                 ifelse(age >= 10 & employment > 2, "NON WORKER AGE 65 PLUS", "OTHER NON WORKING ADULT"))))))]
day_pattern_dt[university_student==1,person_group:="UNIVERSITY STUDENT"]
day_pattern_dt[num_trips==0 & per_day_iscomplete==1,
               day_pattern:="Home All Day"]
trip_category = c(
  "Home"        = 1L,
  "Work/School" = 2L,
  "Other"       = 3L,
  "Missing/Non-Response" = 4L
)
day_pattern_dt[trip_dt[,.(trip_type=ifelse(any(d_purpose_category_imputed %in% c(2,3,4)),
                                           2L,
                                           ifelse(any(d_purpose_category_imputed %in% c(5,6,7,8,9,10)), 3L, 4L))),by=.(personid, day_num)],
               day_pattern:=ifelse(is.na(day_pattern),
                                   ifelse(i.trip_type==2, "Work and/or School Travel",
                                          ifelse(i.trip_type==3, "Other Travel", "Missing/Non-Response")),
                                   day_pattern),
               on=.(personid, day_num)]

notravel_reason = c("notravel_weather", "notravel_nowork", "notravel_telework", 
                    "notravel_housework", "notravel_kidsbreak", "notravel_homeschool", 
                    "notravel_notransport", "notravel_sick", "notravel_visitor", 
                    "notravel_other")

day_pattern_dt[,reason_notravel:=apply(.SD,1,function(x) ifelse(sum(x==1),1L,0L)),.SDcols=notravel_reason]
day_pattern_dt[reason_notravel==1 & is.na(day_pattern), day_pattern:= "Home All Day"]
setkey(day_pattern_dt, person_group, day_pattern)
day_pattern_dt = day_pattern_dt[CJ(person_group, day_pattern, unique = TRUE)][!(is.na(day_pattern) | (day_pattern=="Missing/Non-Response")),.(`COUNT`= sum(day_weight_person),
                  CHART = "Day Pattern"),
               by = .(`PERSON GROUP`=person_group,
                      `DAY PATTERN` =day_pattern)]

# Time Use
trip_dt[, arr_time:=as.POSIXct(arrival_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")]
trip_dt[, hour := hour(arr_time)]
trip_dt[, min  := minute(arr_time)]
trip_dt[, mpm  := hour * 60 + min]
trip_dt[, mpm_period := as.integer(mpm / 30)]
trip_dt[, mpm_period_shift := mpm_period - 5]
trip_dt[mpm_period_shift < 1, mpm_period_shift := mpm_period_shift + 48]
# trip_dt[, mpm_period_shift := paste0("PER", ifelse(mpm_period_shift<10, "0", ""), mpm_period_shift)]

time_use_dt = person_dt[,.(personid, age, employment, university_student)][trip_dt,on=.(personid)]
time_use_dt[,person_type:=ifelse(age==1,"CHILD AGE 16 PLUS",
                                     ifelse(age==3, "CHILD AGE 0 TO 4",
                                            ifelse(age==4, "CHILD AGE 5 TO 15",
                                                   ifelse(employment==1, "FULL TIME WORKER",
                                                          ifelse(employment==2, "PART TIME WORKER",
                                                                 ifelse(age >= 10 & employment > 2, "NON WORKER AGE 65 PLUS", "OTHER NON WORKING ADULT"))))))]
time_use_dt[university_student==1,person_type:="UNIVERSITY STUDENT"]
setkey(time_use_dt, person_type, mpm_period_shift, destination_labels)
time_use_dt = time_use_dt[!d_purpose_category_imputed %in% c(-9998, 11, 12)][CJ(person_type, mpm_period_shift, destination_labels, unique = TRUE)][,.(QUANTITY = sum(trip_weight_household)),
            by = .(PERSON_TYPE = person_type,
                   PER = mpm_period_shift,
                   ORIG_PURPOSE = destination_labels)]
time_use_dt[is.na(QUANTITY), QUANTITY:=0]
purpose_labels = c(
  "Activity at Home" = "HOME",
  "Work/Work-related" = "WORK",
  "Attending schoool/class" = "SCHOOL",
  "Drop-off/pick-up/accompany another person" = "ESCORT",
  "Shopping/errands/appointments" = "SHOP",
  "Dine out/get coffee or take-out" = "MEAL",
  "Social/Recreation" = "SOC AND REC",
  "Change travel mode" = "CHANGE MODE"
)
time_use_dt[,ORIG_PURPOSE:=purpose_labels[ORIG_PURPOSE]]
time_use_all_dt = time_use_dt[,.(QUANTITY=sum(QUANTITY)),
                              by = .(PER, ORIG_PURPOSE)]
time_use_all_dt[,PERSON_TYPE:="ALL"]
time_use_dt = rbindlist(list(time_use_dt, time_use_all_dt), use.names = TRUE)

### Write output data ############################################################
##################################################################################

# Snapshot
fwrite(snapshot_dt, file = snapshot_file)
# Trips period
fwrite(trip_period_dt, file = trips_period_file)
# Destination
fwrite(trip_destination_dt, file = destination_file)
# Work Mode
fwrite(work_mode_dt, file = work_mode_file)
# Telecommute freq by age
fwrite(telecommute_freq_dt, file = telecommute_file)
# Bicycle frequency by age
fwrite(bike_freq_dt, file = bike_file)
# Commute frequency by age
fwrite(commute_frequency_dt, file = commute_freq_file)
# Demographics
fwrite(age_group_dt, file = age_file)
fwrite(gender_dt, file = gender_file)
# Trips OD
fwrite(county_trips,#[!(Cnty_Nm_o=="External" | Cnty_Nm_d == "External")], 
       file = trip_od_file1)
fwrite(county_trips,#[!(Cnty_Nm_o=="External" | Cnty_Nm_d == "External")], 
       file = trip_od_file2)

# Trips Zone
fwrite(trip_zone, file = trip_zone_file1)
fwrite(trip_zone, file = trip_zone_file2)
fwrite(trip_zone, file = trip_zone_file3)

# Trips Mode
fwrite(trips_mode, file = trip_mode_file)
fwrite(seasonal_trips_mode, file = seasonal_trip_mode_file)
fwrite(uni_trips_mode, file = uni_trip_mode_file)
fwrite(day_pattern_dt, file = day_pattern_file)
fwrite(time_use_dt, file = time_use_file)
