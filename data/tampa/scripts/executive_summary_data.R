##################################################################################
# Create data for Executive Summary
# Author: Aditya Gore
##################################################################################


### Load/Install required packages ###############################################
##################################################################################
library(data.table)
library(jsonlite)
library(stringr)
library(sf)
library(geojsonsf)


### Define Constants #############################################################
##################################################################################

# Input files
data_dir   = getwd()

household_file = file.path(data_dir, "TBRTS-HTS_Household_Table_20191218.csv")
person_file    = file.path(data_dir, "TBRTS-HTS_Person_Table_20191218.csv") 
vehicle_file   = file.path(data_dir, "TBRTS-HTS_Vehicle_Table_20191218.csv")
day_file       = file.path(data_dir, "TBRTS-HTS_Day_Table_20191218.csv") 
trip_file      = file.path(data_dir, "TBRTS-HTS_Trip_Table_20191218.csv")

# Geography input files
taz_file       = file.path(getwd(), "tampa2020.json")
county_file    = file.path(getwd(), "counties.json")
agg_file       = file.path(getwd(), "tampa.csv")

# Output files
es_output_dir = file.path(getwd(), "Executive_Summary")
pd_output_dir = file.path(getwd(), "Passive_Data")
ts_output_dir = file.path(getwd(), "Travel_Survey")

# Executive_Summary
snapshot_file = file.path(es_output_dir, "snapshot.csv")
trips_period_file = file.path(es_output_dir, "trips_period.csv")         # Typical Travel
destination_file = file.path(es_output_dir, "trips_destination.csv")     # Trip Destination
work_mode_file   = file.path(es_output_dir, "work_mode.csv")             # Usual Ways to commute to Work 
telecommute_file = file.path(es_output_dir, "telecommute_freq_age.csv")  # Telecommute freq by age
bike_file        = file.path(es_output_dir, "bike_freq_age.csv")         # Bike use freq by age
commute_freq_file= file.path(es_output_dir, "commute_freq_age.csv")      # Commute freq by age
age_file         = file.path(es_output_dir, "age_distribution.csv")      # Age of participants
gender_file      = file.path(es_output_dir, "gender_distribution.csv")   # Gender of participants

# Passive_Data
trip_od_overall_file         = file.path(pd_output_dir, "trip_od_overall.csv")
trip_od_hillsborough_file    = file.path(pd_output_dir, "trip_od_hillsborough.csv")
trip_od_pinellas_file        = file.path(pd_output_dir, "trip_od_pinellas.csv")
trip_od_pasco_file           = file.path(pd_output_dir, "trip_od_pasco.csv")
trip_od_hernando_citrus_file = file.path(pd_output_dir, "trip_od_hernando_citrus.csv")

# Travel_Survey
trip_zone_file                = file.path(ts_output_dir, "trips_zone.csv")
trip_mode_file                = file.path(ts_output_dir, "trip_mode_zone.csv")
seasonal_trip_mode_file       = file.path(ts_output_dir, "seasonal_trip_mode_zone.csv")
uni_trip_mode_file            = file.path(ts_output_dir, "uni_trip_mode_zone.csv")
day_pattern_file              = file.path(ts_output_dir, "day_pattern.csv")
time_use_file                 = file.path(ts_output_dir, "time_use.csv")
trip_od_overall_file2         = file.path(ts_output_dir, "trip_od_overall.csv")
trip_od_hillsborough_file2    = file.path(ts_output_dir, "trip_od_hillsborough.csv")
trip_od_pinellas_file2        = file.path(ts_output_dir, "trip_od_pinellas.csv")
trip_od_pasco_file2           = file.path(ts_output_dir, "trip_od_pasco.csv")
trip_od_hernando_citrus_file2 = file.path(ts_output_dir, "trip_od_hernando_citrus.csv")

# Geography for chord chart (OD)
overall_geo_file            = file.path(getwd(), "overall.json")
hillsborough_geo_file       = file.path(getwd(), "hillsborough.json")
pinellas_geo_file           = file.path(getwd(), "pinellas.json")
pasco_geo_file              = file.path(getwd(), "pasco.json")
hernando_citrus_geo_file    = file.path(getwd(), "hernandocitrus.json")

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
taz_sf = geojson_sf(taz_file)
taz_dt = data.table(taz_sf)
agg_dt = fread(agg_file)
agg_dt[,HILLSBOROUGH_LBL_3:=gsub("\\&|\\/","",HILLSBOROUGH_LBL_3)]
agg_dt[,HILLSBOROUGH_LBL_3:=gsub("\\s+","_",HILLSBOROUGH_LBL_3)]
agg_dt[,PINELLAS_LBL:=gsub("\\&|\\/","",PINELLAS_LBL)]
agg_dt[,PINELLAS_LBL:=gsub("\\s+","_",PINELLAS_LBL)]
agg_dt[,PASCO_LBL:=gsub("\\&|\\/","",PASCO_LBL)]
agg_dt[,PASCO_LBL:=gsub("\\s+","_",PASCO_LBL)]
agg_dt[,HERNANDO_CITRUS_LBL_2:=gsub("\\&|\\/","",HERNANDO_CITRUS_LBL_2)]
agg_dt[,HERNANDO_CITRUS_LBL_2:=gsub("\\s+","_",HERNANDO_CITRUS_LBL_2)]
agg_sf = st_as_sf(merge(as.data.frame(agg_dt), taz_sf, by.x="TAZ", by.y="id",all.x = TRUE))



### Create output data ###########################################################
##################################################################################

# Create districts.json file for OD chord chart
# Overall
overall_sf = dplyr::summarise(dplyr::group_by(agg_sf, D7_ALL_LBL))
names(overall_sf) = c("NAME", "geometry")

# Hillsborough
hillsborough_sf = dplyr::summarise(dplyr::group_by(agg_sf, HILLSBOROUGH_LBL_3))
names(hillsborough_sf) = c("NAME", "geometry")

# Pinellas
pinellas_sf = dplyr::summarise(dplyr::group_by(agg_sf, PINELLAS_LBL))
names(pinellas_sf) = c("NAME", "geometry")

# Pasco
pasco_sf = dplyr::summarise(dplyr::group_by(agg_sf, PASCO_LBL))
names(pasco_sf) = c("NAME", "geometry")

# Hernando/Citrus
hernando_citrus_sf = dplyr::summarise(dplyr::group_by(agg_sf, HERNANDO_CITRUS_LBL_2))
names(hernando_citrus_sf) = c("NAME", "geometry")

### Executive Summary ############################################################
##################################################################################

# Snapshot Data
snapshot_dt = data.table(Value = c(household_dt[,.N],
                                   person_dt[,.N],
                                   trip_dt[,.N]),
                         Description = c("households participated in the survey",
                                         "total people took part",
                                         "trips recorded as part of the survey"))


# Typical Travel

# Trips start by period
trip_dt[, dep_time:=as.POSIXct(departure_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")]
trip_dt[, hour := hour(dep_time)]

trip_start_dt = trip_dt[,.(TYPE = "TRIP START",
                           TRIPS = round(sum(trip_weight_household), 2)),
                        by = .(HOUR = hour)]
trip_dt[, arr_time:=as.POSIXct(arrival_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")]
trip_dt[, hour := hour(arr_time)]

# Trips end by period 
trip_end_dt = trip_dt[,.(TYPE = "TRIP END",
                         TRIPS = round(sum(trip_weight_household), 2)),
                      by = .(HOUR = hour)]

trip_period_dt = rbindlist(list(trip_start_dt, trip_end_dt), use.names = TRUE)
rm(trip_start_dt, trip_end_dt)

trip_period_dt[,PER:=ifelse(HOUR > 12, paste0(HOUR - 12," PM"),
                            ifelse(HOUR == 12, "12 PM",
                                   ifelse(HOUR == 0, "12 AM", paste0(HOUR, " AM"))))]
setorder(trip_period_dt, TYPE, HOUR)
trip_period_dt[,HOUR:=NULL]
setcolorder(trip_period_dt, c("TYPE", "PER"))
trip_period_dt[,CHART:="TYPICAL TRAVEL"]


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

### Passive Data Scenario ########################################################
##################################################################################

# Chord Diagram
trip_est_dt = trip_dt[,.(TRIPS = sum(trip_weight_household)),
                      by = .(OTAZ = o_taz_2020,
                             DTAZ = d_taz_2020)]
trip_est_dt =  merge(trip_est_dt, agg_dt[,.(TAZ, HILLSBOROUGH_LBL_3, PINELLAS_LBL, PASCO_LBL,
                                            HERNANDO_CITRUS_LBL_2, D7_ALL_LBL)],
                     by.x = "OTAZ", by.y="TAZ", all.x = TRUE)
trip_est_dt =  merge(trip_est_dt, agg_dt[,.(TAZ, HILLSBOROUGH_LBL_3, PINELLAS_LBL, PASCO_LBL,
                                            HERNANDO_CITRUS_LBL_2, D7_ALL_LBL)],
                     by.x = "DTAZ", by.y="TAZ", all.x = TRUE,
                     suffixes = c("_O", "_D"))
# Overall
overall_trip_dt         = trip_est_dt[,.(TRIPS = sum(TRIPS)),.(FROM = D7_ALL_LBL_O,
                                                               TO = D7_ALL_LBL_D)]
overall_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                      TO   = ifelse(is.na(TO), "External", TO))]
# Hillsborough
hillsborough_trip_dt    = trip_est_dt[,.(TRIPS = sum(TRIPS)),.(FROM = HILLSBOROUGH_LBL_3_O,
                                                               TO = HILLSBOROUGH_LBL_3_D)]
hillsborough_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                           TO   = ifelse(is.na(TO), "External", TO))]
# Pinellas
pinellas_trip_dt        = trip_est_dt[,.(TRIPS = sum(TRIPS)),.(FROM = PINELLAS_LBL_O,
                                                               TO = PINELLAS_LBL_D)]
pinellas_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                       TO   = ifelse(is.na(TO), "External", TO))]
# Pasco
pasco_trip_dt           = trip_est_dt[,.(TRIPS = sum(TRIPS)),.(FROM = PASCO_LBL_O,
                                                               TO = PASCO_LBL_D)]
pasco_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                    TO   = ifelse(is.na(TO), "External", TO))]
# Hernando/Citrus
hernando_citrus_trip_dt = trip_est_dt[,.(TRIPS = sum(TRIPS)),.(FROM = HERNANDO_CITRUS_LBL_2_O,
                                                               TO = HERNANDO_CITRUS_LBL_2_D)]
hernando_citrus_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                              TO   = ifelse(is.na(TO), "External", TO))]

### Travel Survey Scenario #######################################################
##################################################################################

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

trip_total = trips_mode[,.(TRIPS=sum(TRIPS), MODE = "ALL_MODES"),.(ZONE, COUNTY)]
trips_mode = rbindlist(list(trips_mode, trip_total), use.names = TRUE)

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

trip_total = seasonal_trips_mode[,.(TRIPS=sum(TRIPS), MODE = "ALL_MODES"),.(ZONE, COUNTY)]
seasonal_trips_mode = rbindlist(list(seasonal_trips_mode, trip_total), use.names = TRUE)

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

trip_total = uni_trips_mode[,.(TRIPS=sum(TRIPS), MODE = "ALL_MODES"),.(ZONE, COUNTY)]
uni_trips_mode = rbindlist(list(uni_trips_mode, trip_total), use.names = TRUE)

# Day Pattern
# day_pattern_dt = person_dt[,.(personid, age, employment, university_student)][trip_dt[,.(personid,trip_id,d_purpose_category_imputed,trip_weight_household)],on=.(personid)]
day_pattern_dt = household_dt[,.(hh_id, seasonal_household, hh_recruitment)][
  person_dt,.(personid, age, employment, university_student, seasonal_household,hh_recruitment),on=.(hh_id)][
    day_dt,on=.(personid)]

categories_ar = c(
  "cat1" = "CHILD AGE 0 TO 4",
  "cat2" = "CHILD AGE 5 TO 15",
  "cat3" = "CHILD AGE 16 PLUS",
  "cat4" = "FULL TIME WORKER",
  "cat5" = "PART TIME WORKER",
  "cat6" = "NON WORKER AGE 65 PLUS",
  "cat7" = "OTHER NON WORKING ADULT",
  "cat8" = "UNIVERSITY STUDENT",
  "cat9" = "SEASONAL RESIDENT",
  "cat10"= "ALL"
)

day_pattern_dt[,c(names(categories_ar)):=0L]
day_pattern_dt[seasonal_household==1,                                   cat9:=1L]
day_pattern_dt[hh_recruitment==6,                                       cat8:=1L]
day_pattern_dt[age==3 & cat8==0 & cat9==0,                              cat1:=1L]
day_pattern_dt[age==4 & cat8==0 & cat9==0,                              cat2:=1L]
day_pattern_dt[age==1 & cat8==0 & cat9==0,                              cat3:=1L]
day_pattern_dt[employment==1 & cat8==0 & cat9==0,                       cat4:=1L]
day_pattern_dt[employment==2 & cat8==0 & cat9==0,                       cat5:=1L]
day_pattern_dt[age>=10 & employment > 2 & cat8==0 & cat9==0,            cat6:=1L]
day_pattern_dt[age < 10 & age > 4 & employment > 2 & cat8==0 & cat9==0, cat7:=1L]
day_pattern_dt[,                                                        cat10:=1L]

# day_pattern_dt[,person_group:=ifelse(age==1,"CHILD AGE 16 PLUS",
#                                      ifelse(age==3, "CHILD AGE 0 TO 4",
#                                             ifelse(age==4, "CHILD AGE 5 TO 15",
#                                                    ifelse(employment==1, "FULL TIME WORKER",
#                                                           ifelse(employment==2, "PART TIME WORKER",
#                                                                  ifelse(age >= 10 & employment > 2, "NON WORKER AGE 65 PLUS", "OTHER NON WORKING ADULT"))))))]
# day_pattern_dt[university_student==1,person_group:="UNIVERSITY STUDENT"]
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

day_pattern_dt = rbindlist(lapply(names(categories_ar), function(x) {tmp = day_pattern_dt[get(x)==1]
tmp[,person_group:=categories_ar[x]] 
tmp[,.(person_group, day_pattern, day_weight_person)]}))

setkey(day_pattern_dt, person_group, day_pattern)
day_pattern_dt = day_pattern_dt[CJ(person_group, day_pattern, unique = TRUE)][!(is.na(day_pattern) | (day_pattern=="Missing/Non-Response")),.(`COUNT`= sum(day_weight_person),
                  CHART = "Day Pattern"),
               by = .(`PERSON GROUP`=person_group,
                      `DAY PATTERN` =day_pattern)]

# Time Use

purpose_labels = c(
  "Activity at Home" = "HOME",
  "Work/Work-related" = "WORK",
  "Attending schoool/class" = "SCHOOL",
  "Drop-off/pick-up/accompany another person" = "ESCORT",
  "Shopping/errands/appointments" = "SHOP",
  "Dine out/get coffee or take-out" = "MEAL",
  "Social/Recreation" = "SOC AND REC",
  "Change travel mode" = "CHANGE MODE",
  "Travel/Other"      = "OTHER OR TRAVEL"
)

setkey(trip_dt, personid, day_num, departure_time)

trip_dt[, arr_time:=as.POSIXct(arrival_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")]
trip_dt[, hour := hour(arr_time)]
trip_dt[, min  := minute(arr_time)]
trip_dt[, mpm  := hour * 60 + min]
trip_dt[, mpm_period := as.integer(mpm / 30)]
trip_dt[, mpm_period_shift := mpm_period - 5]
trip_dt[mpm_period_shift < 1, mpm_period_shift := mpm_period_shift + 48]
trip_dt[,end_time:=mpm_period_shift]
trip_dt[,end_time2:=(day_num-1)*48 + end_time]

trip_dt[, dep_time:=as.POSIXct(departure_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")]
trip_dt[, hour := hour(dep_time)]
trip_dt[, min  := minute(dep_time)]
trip_dt[, mpm  := hour * 60 + min]
trip_dt[, mpm_period := as.integer(mpm / 30)]
trip_dt[, mpm_period_shift := mpm_period - 5]
trip_dt[mpm_period_shift < 1, mpm_period_shift := mpm_period_shift + 48]
trip_dt[,start_time:=mpm_period_shift]
trip_dt[,start_time2:=(day_num-1)*48 + start_time]
trip_dt[order(personid, day_num, departure_time),next_act_time:=shift(start_time2,type = "lead"),.(personid)]
trip_dt[order(personid, day_num, departure_time),next_act_day:=shift(day_num,type = "lead"),.(personid)]


change_day_idx = trip_dt[,.I[end_time2 > next_act_time & !(is.na(end_time2)) & !is.na(next_act_time)]]
change_day_idx = change_day_idx+1L
trip_dt[change_day_idx, start_time2:= (day_num)*48+start_time]
trip_dt[order(personid, day_num, departure_time),next_act_time:=shift(start_time2,type = "lead"),.(personid)]


code_time = function(start_time, end_time){
  if(is.na(start_time)) start_time = end_time
  if(is.na(end_time)) end_time = start_time
  if(end_time >= start_time){
    return(seq(start_time, end_time, 1L))
  } else if(start_time > end_time){
    return(c(seq(start_time, 48, 1L), seq(1, end_time, 1L)))
  }
}

code_day = function(start_time, end_time, start_day_num, end_day_num){
  if(is.na(start_time)) start_time = end_time
  if(is.na(end_time)) end_time = start_time
  if(end_time >= start_time){
    return(rep(start_day_num,length(seq(start_time, end_time, 1L))))
  } else if(start_time > end_time){
    return(c(rep(start_day_num,length(c(seq(start_time, 48, 1L)))), rep(start_day_num+1L,length(seq(1, end_time, 1L)))))
  }
}

# code_duration = function(start_time, end_time, duration, dep_min, arr_min){
#   if(end_time >= start_time){
#     time_seq  = seq(start_time, end_time, 1L)
#   } else if(start_time > end_time){
#     time_seq = c(seq(start_time, 48, 1L), seq(1, end_time, 1L))
#   }
#   
# }


time_use_dt = day_dt[,.(PER=1:48),.(personid, day_num, day_weight_person=day_weight_person/48, num_trips)]
time_use_dt = household_dt[,.(hh_id, seasonal_household, hh_recruitment)][
  person_dt,.(personid, age, employment, university_student, seasonal_household,hh_recruitment),on=.(hh_id)][time_use_dt,on=.(personid)]
# time_use_dt[,person_type:=ifelse(age==1,"CHILD AGE 16 PLUS",
#                                      ifelse(age==3, "CHILD AGE 0 TO 4",
#                                             ifelse(age==4, "CHILD AGE 5 TO 15",
#                                                    ifelse(employment==1, "FULL TIME WORKER",
#                                                           ifelse(employment==2, "PART TIME WORKER",
#                                                                  ifelse(age >= 10 & employment > 2, "NON WORKER AGE 65 PLUS", "OTHER NON WORKING ADULT"))))))]
# time_use_dt[university_student==1,person_type:="UNIVERSITY STUDENT"]
time_use_dt[,personid:=as.numeric(personid)]

time_use_dt[,c(names(categories_ar)):=0L]
time_use_dt[seasonal_household==1,                                   cat9:=1L]
time_use_dt[hh_recruitment==6,                                       cat8:=1L]
time_use_dt[age==3 & cat8==0 & cat9==0,                              cat1:=1L]
time_use_dt[age==4 & cat8==0 & cat9==0,                              cat2:=1L]
time_use_dt[age==1 & cat8==0 & cat9==0,                              cat3:=1L]
time_use_dt[employment==1 & cat8==0 & cat9==0,                       cat4:=1L]
time_use_dt[employment==2 & cat8==0 & cat9==0,                       cat5:=1L]
time_use_dt[age>=10 & employment > 2 & cat8==0 & cat9==0,            cat6:=1L]
time_use_dt[age < 10 & age > 4 & employment > 2 & cat8==0 & cat9==0, cat7:=1L]
time_use_dt[,                                                        cat10:=1L]

time_trip_dt = trip_dt[day_num > 0,.(PER=code_time(end_time2, next_act_time)#,
                          # day_num = code_day(end_time, next_act_time, day_num)
                          ), by=.(trip_id)]
time_trip_dt = trip_dt[,.(trip_id, personid, destination_labels, trip_weight_household)][time_trip_dt,on=.(trip_id)]
time_trip_dt[,trip_id:=NULL]
time_trip_dt[,day_num:=(PER %/% 48)+1]
time_trip_dt[,PER2:=PER]
time_trip_dt[,PER:=(PER %% 48)]
time_trip_dt[PER==0, ":=" (PER=48, day_num=day_num-1)]
# Replace change mode
change_mode = function(activities){
  change_mode_idx = grepl("Change", activities)
  replace_activity_idx = diff(c(FALSE, change_mode_idx))
  while(sum(replace_activity_idx==-1) > 0){
    replace_idx = seq(min(which(replace_activity_idx==1)),
                      min(which(replace_activity_idx==-1))-1, by = 1)
    replace_activity = activities[min(which(replace_activity_idx==-1))]
    activities[replace_idx] = replace_activity
    change_mode_idx = grepl("Change", activities)
    replace_activity_idx = diff(c(FALSE, change_mode_idx))
  }
  return(activities)
}
time_trip_dt[,destination_labels:=change_mode(destination_labels), by=.(personid)]
time_trip_dt = time_trip_dt[,.(TIME_USE = sum(trip_weight_household)),.(personid, day_num, PER, PER2, destination_labels)]

tmp = merge(time_use_dt, time_trip_dt, by = c("personid", "day_num", "PER"), all = TRUE)

setkey(tmp, personid, day_num, PER)

code_home = function(activities, personid){
  if(all(is.na(activities))) return(rep("Activity at Home", length(activities)))
  # cat("Person id ", personid, "\n")
  # Start from home
  act_index = min(which(!is.na(activities)))
  # cat("Start ", act_index, "\n")
  if(act_index >= 2) activities[seq(1,act_index-1, 1)] = "Activity at Home"
  # End at home
  act_index = max(which(!is.na(activities)))
  # cat("End ", act_index, "\n")
  if(act_index <= (length(activities)-1)) activities[seq(act_index+1, length(activities), 1)] = "Activity at Home"
  return(activities)
}

tmp[order(personid, day_num, PER),activity:=code_home(destination_labels, as.numeric(personid)),.(personid)]
tmp[num_trips==0, activity:="Activity at Home"]
tmp[is.na(destination_labels), destination_labels:="Travel/Other"]
tmp[is.na(TIME_USE), TIME_USE:= day_weight_person]
tmp = rbindlist(lapply(names(categories_ar), function(x) {
  tmp_dt = tmp[get(x)==1]
  tmp_dt[, person_type:=categories_ar[x]]
  tmp_dt[,.(person_type, PER, destination_labels, TIME_USE)]
}))

tmp2 = tmp[,.(TIME_USE=sum(TIME_USE)),.(PERSON_TYPE = person_type, PER, ORIG_PURPOSE = destination_labels)]
tmp2 = tmp2[!is.na(PERSON_TYPE)]
tmp2[,ORIG_PURPOSE:=purpose_labels[ORIG_PURPOSE]]
tmp2[is.na(ORIG_PURPOSE), ORIG_PURPOSE:="OTHER"]
time_use_dt = tmp2[,.(TIME_USE=(sum(TIME_USE))),.(PERSON_TYPE, PER, ORIG_PURPOSE)]
setkey(time_use_dt, PERSON_TYPE, PER, ORIG_PURPOSE)
time_use_dt = time_use_dt[CJ(PERSON_TYPE, PER, ORIG_PURPOSE, unique = TRUE)]
time_use_dt[is.na(TIME_USE), TIME_USE:=0]
# fwrite(time_use_dt, "time_use.csv")


# setkey(time_use_dt, person_type, mpm_period_shift, destination_labels)
# time_use_dt = time_use_dt[!d_purpose_category_imputed %in% c(-9998, 11, 12)][CJ(person_type, mpm_period_shift, destination_labels, unique = TRUE)][,.(QUANTITY = sum(trip_weight_household)),
#             by = .(PERSON_TYPE = person_type,
#                    PER = mpm_period_shift,
#                    ORIG_PURPOSE = destination_labels)]
# time_use_dt[is.na(QUANTITY), QUANTITY:=0]
# purpose_labels = c(
#   "Activity at Home" = "HOME",
#   "Work/Work-related" = "WORK",
#   "Attending schoool/class" = "SCHOOL",
#   "Drop-off/pick-up/accompany another person" = "ESCORT",
#   "Shopping/errands/appointments" = "SHOP",
#   "Dine out/get coffee or take-out" = "MEAL",
#   "Social/Recreation" = "SOC AND REC",
#   "Change travel mode" = "CHANGE MODE"
# )
# time_use_dt[,ORIG_PURPOSE:=purpose_labels[ORIG_PURPOSE]]
# time_use_all_dt = time_use_dt[,.(QUANTITY=sum(QUANTITY)),
#                               by = .(PER, ORIG_PURPOSE)]
# time_use_all_dt[,PERSON_TYPE:="ALL"]
# time_use_dt = rbindlist(list(time_use_dt, time_use_all_dt), use.names = TRUE)

### Write output data ############################################################
##################################################################################

## Executive Summary
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
# fwrite(commute_frequency_dt, file = commute_freq_file)
# Demographics
fwrite(age_group_dt, file = age_file)
fwrite(gender_dt, file = gender_file)

## Passive Data
# Geojson files
st_write(overall_sf,         driver = "GeoJSON", dsn = overall_geo_file,         delete_dsn = TRUE)
st_write(hillsborough_sf,    driver = "GeoJSON", dsn = hillsborough_geo_file,    delete_dsn = TRUE)
st_write(pinellas_sf,        driver = "GeoJSON", dsn = pinellas_geo_file,        delete_dsn = TRUE)
st_write(pasco_sf,           driver = "GeoJSON", dsn = pasco_geo_file,           delete_dsn = TRUE)
st_write(hernando_citrus_sf, driver = "GeoJSON", dsn = hernando_citrus_geo_file, delete_dsn = TRUE)

# Trip OD
fwrite(overall_trip_dt,         file = trip_od_overall_file)
fwrite(hillsborough_trip_dt,    file = trip_od_hillsborough_file)
fwrite(pinellas_trip_dt,        file = trip_od_pinellas_file)
fwrite(pasco_trip_dt,           file = trip_od_pasco_file)
fwrite(hernando_citrus_trip_dt, file = trip_od_hernando_citrus_file)

## Travel Survey
# Trip OD
fwrite(overall_trip_dt,         file = trip_od_overall_file2)
fwrite(hillsborough_trip_dt,    file = trip_od_hillsborough_file2)
fwrite(pinellas_trip_dt,        file = trip_od_pinellas_file2)
fwrite(pasco_trip_dt,           file = trip_od_pasco_file2)
fwrite(hernando_citrus_trip_dt, file = trip_od_hernando_citrus_file2)

# Trips Mode
fwrite(trips_mode, file = trip_mode_file)
# Seasonal trips mode
fwrite(seasonal_trips_mode, file = seasonal_trip_mode_file)
# University trips mode
fwrite(uni_trips_mode, file = uni_trip_mode_file)
# Trips Zone
fwrite(trip_zone, file = trip_zone_file)
# Day Pattern
fwrite(day_pattern_dt, file = day_pattern_file)
# Time Use
fwrite(time_use_dt, file = time_use_file)


