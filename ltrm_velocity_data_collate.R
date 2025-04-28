# A. SETUP LTRM DATA MANIPULATIONS ----

#__1. Library Load ----
library(tidyverse)
library(lubridate)

#__2. Read Raw  Data From .csv File(s)----
#downloaded all LTRM SRS data for each component as .csv from LTRM website
#downloaded historical discharge data from: https://www.mvp-wc.usace.army.mil/Data.shtml
#velocity modeling project using all SRS data field stations 1,2, and 3 (Pools 4,8, 13)

#____a) water quality----
dat_wq_fs123 <- read_csv('data/ltrm_water_data.csv')

#____b) fisheries----
dat_fish_fs1 <- read_csv('data/ltrm_fish_data_fs1.csv')
dat_fish_fs2 <- read_csv('data/ltrm_fish_data_fs2.csv')
dat_fish_fs3 <- read_csv('data/ltrm_fish_data_fs3.csv')

#____c) aquatic vegetation----
#note this data os not served online as of Janurary 2025
dat_veg_vel_fs123 <- read.csv('data/ltrm_veg_vel_data_fs123_2224.csv')

#____d) discharge----
#______i) LD4----

### to get the url for raw data, open file (.csv) in Github, then click on "Raw" button ###

dat_q_ld4 <- read_csv("https://raw.githubusercontent.com/Lake-City-LTRM/hydrology/refs/heads/main/data/q_ld4_collated.csv")

#______ii) LD8----
dat_q_ld8 <- read_csv("https://raw.githubusercontent.com/Lake-City-LTRM/hydrology/refs/heads/main/data/q_ld8_collated.csv")

#______iii) LD10----
dat_q_ld10 <- read_csv('https://raw.githubusercontent.com/Lake-City-LTRM/hydrology/refs/heads/main/data/q_ld10_collated.csv')



#B. DATA QAQC AND MANIPULATIONS----

#__1. WQ Velocity Data----


#____a) parse surface data (one row per sheetbar)----
dat_wq_srs_fs123_surf <- dat_wq_fs123  %>% filter (Z <= 0.2)



#____b) parse Non-winter data ----
#______i) create new date fields----

### create new formatted 'date' field in the sites dateframe since...
### by default date is imported as a string of characters ###

dat_wq_srs_fs123_surf$date <-as.POSIXct(dat_wq_srs_fs123_surf$DATE, format = "%m/%d/%Y")

### create new 'year' and 'month' fields from 'date' value ###
### requires 'lubridate' package ###

dat_wq_srs_fs123_surf <- dat_wq_srs_fs123_surf %>%
  mutate(year=  year(date) ) %>%
  mutate(month =  month(date) )


#______ii) filter for non-winter months----
dat_wq_srs_fs123_surf_sprsumfal <- dat_wq_srs_fs123_surf  %>% filter (month != 12 & month != 1 & month != 2)


#____c)  filter relevant site data----

dat_wq_vel_fs123 <- dplyr::select (dat_wq_srs_fs123_surf_sprsumfal, c('SHEETBAR', 'FLDNUM', 'STRATUM', 'DATE', 'NORTHING', 'EASTING',
                                                             'VEL', 'VELQF'))
#____d)  exclude approx 7500 velocity NA values----
dat_wq_vel_fs123 <- subset(dat_wq_vel_fs123, !is.na(VEL))


#____e) exclude all velocity QF codes other than where NA or 3----
### removed approx 175 records ###
dat_wq_vel_fs123 <- subset(dat_wq_vel_fs123 , is.na(VELQF) | VELQF == 3 )

### optional data check ###
#dat_wq_vel_strat_summary <-  dat_wq_vel_fs123 %>%
 # group_by(STRATUM, FLDNUM) %>%
  # summarise(n=n())

#____f) replace STRATUM = NA (provisional 2024 data) to STRATUM 9999----
### replaces approx 750 NA values between 3 field stations ###
dat_wq_vel_fs123$STRATUM[is.na(dat_wq_vel_fs123$STRATUM)] <- 9999

#____g) exclude approx 25 remaining Pepin sites (TDL)----
dat_wq_vel_fs123 <- dat_wq_vel_fs123 %>% filter (STRATUM != 4)

#____h)  add component ('comp') column and populate----
dat_wq_vel_fs123$comp <- "w" 

#____i) add 'pool' column and populate---- 

### based on field station number
dat_wq_vel_fs123$pool[dat_wq_vel_fs123$FLDNUM == '1'] <- 4
dat_wq_vel_fs123$pool[dat_wq_vel_fs123$FLDNUM == '2'] <- 8
dat_wq_vel_fs123$pool[dat_wq_vel_fs123$FLDNUM == '3'] <- 13



#____j) standardize 'stratum' values----

### optional check of unique original STRATUM values in data
# unique(dat_wq_vel_fs123$STRATUM)
# [1]    3    2    1 9999    5    6    9

#replace numeric wq stratum values with alpha codes comparable to other fish and wq components

dat_wq_vel_fs123$STRATUM[dat_wq_vel_fs123$STRATUM == 1] <- 'MC'
dat_wq_vel_fs123$STRATUM[dat_wq_vel_fs123$STRATUM == 2] <- 'SC'
dat_wq_vel_fs123$STRATUM[dat_wq_vel_fs123$STRATUM == 3] <- 'BWC'
dat_wq_vel_fs123$STRATUM[dat_wq_vel_fs123$STRATUM == 9999] <- 'UNK'
dat_wq_vel_fs123$STRATUM[dat_wq_vel_fs123$STRATUM == 5] <- 'IMP'
dat_wq_vel_fs123$STRATUM[dat_wq_vel_fs123$STRATUM == 6] <- 'BWI'
dat_wq_vel_fs123$STRATUM[dat_wq_vel_fs123$STRATUM == 9] <- 'UXO'

### optional check of new STRATUM values in data
# unique(dat_wq_vel_fs123$STRATUM)
# [1] "BWC" "SC"  "MC"  "UNK" "IMP" "BWI" "UXO"

#____k) standardize column names----

# view current column names
# head(dat_wq_vel_fs123)
# "SHEETBAR FLDNUM STRATUM DATE NORTHING EASTING  VEL VELQF comp pool"
# rename to:
# barcode, fstation, stratum, date, z15east, z15north, vel, vel_qf, comp, pool
colnames(dat_wq_vel_fs123) <- c("barcode", "fstation", "stratum", "DATE", "z15north", "z15east", 
                                               "vel", "vel_qf", "comp", "pool")

#____l) standardize column order----
dat_wq_vel_final <- dat_wq_vel_fs123[,c(1, 9, 2, 10, 3, 4, 6, 5, 7, 8)]

### clean environment of temporary WQ dataframes ###
# original data - all
rm(dat_wq_fs123)
# original data - velocity
rm(dat_wq_vel_fs123)
# other temporary dataframes
rm(dat_wq_srs_fs123_surf)
rm(dat_wq_srs_fs123_surf_sprsumfal)
rm(dat_wq_vel_strat_summary)


#__2. Fisheries Velocity Data----
#____a) parse Site Level data ----

# identify one record per barcode as new 'sites' dataframe

dat_fish_unique_barcodes_fs123 <- dat_fish_fs123[!duplicated(dat_fish_fs123[ , c("barcode")]), ]

#filter for only site level data into new dataframe
dat_fish_sites_fs123 <- dplyr::select (dat_fish_unique_barcodes_fs123, c('site', 'barcode', 'fstation', 'sitetype', 'stratum', 
                                                                         'sdate', 'stime', 'fdate', 'ftime', 'pool', 'lcode', 'gear', 'period', 'rep', 
                                                                         'summary', 'project', 'effdist', 'effhr', 'effmin', 'pwrgoal', 'pwrused', 'volts', 'v_qf', 'amps', 'a_qf',
                                                                         'pulses', 'p_qf', 'dutycyc', 'dc_qf', 'utmzone', 'utm_e', 'utm_n', 'gisgrid', 'zone15e', 'zone15n', 
                                                                         'gpsmeth', 'gpsacc', 'secchi', 's_qf', 'temp', 
                                                                         't_qf', 'depth', 'd_qf', 'cond', 'c_qf', 'current', 'cv_qf', 'do', 'do_qf',
                                                                         'stageht', 'sh_qf', 'sveg92', 'substrt', 'snag', 'wingdyke', 'trib', 'riprap', 'inout', 'closing',
                                                                         'flooded', 'othrstrc', 'labind', 'contanrs', 'shtcnt', 'totfishc', 'leader', 'pageno', 'rec_site', 'rownum'))



#____b)  filter relevant site data----

dat_fish_vel_fs123 <- dplyr::select (dat_fish_sites_fs123, c('barcode', 'fstation', 'stratum', 
                                                             'sdate', 'pool', 'zone15e', 'zone15n', 
                                                             'current', 'cv_qf'))

#____c) exclude values----  

#______i)exclude approx 4200 current velocity NA values----
dat_fish_vel_fs123 <- subset(dat_fish_vel_fs123, !is.na(current))



#______ii) exclude 31 current velocity records with any qf code flag----
dat_fish_vel_fs123 <- subset(dat_fish_vel_fs123, is.na(cv_qf))



#____d)  add component ('comp') column and populate with 'f' (fish)----
dat_fish_vel_fs123$comp <- "f" 

#____e) standardize column names----

# view current column names
# head(dat_fish_vel_fs123)
# barcode fstation stratum sdate pool zone15e zone15n current cv_qf comp

#rename 'sdate', 'zone15e', 'zone15n', 'current', 'cv_qf' columns
names(dat_fish_vel_fs123)[names(dat_fish_vel_fs123) == 'sdate'] <- 'DATE'
names(dat_fish_vel_fs123)[names(dat_fish_vel_fs123) == 'zone15e'] <- 'z15east'
names(dat_fish_vel_fs123)[names(dat_fish_vel_fs123) == 'zone15n'] <- 'z15north'
names(dat_fish_vel_fs123)[names(dat_fish_vel_fs123) == 'current'] <- 'vel'
names(dat_fish_vel_fs123)[names(dat_fish_vel_fs123) == 'cv_qf'] <- 'vel_qf'

# view new column names
# head(dat_fish_vel_fs123)
# barcode fstation stratum DATE pool  z15east z15north   vel vel_qf comp 

#____f) standardize column order----
dat_fish_vel_final <- dat_fish_vel_fs123[,c(1, 10, 2, 5, 3, 4, 6, 7, 8, 9)]

#____g) replace pool character values with numeric----
dat_fish_vel_final$pool[dat_fish_vel_final$pool == '04'] <- 4
dat_fish_vel_final$pool[dat_fish_vel_final$pool == '08'] <- 8
dat_fish_vel_final$pool[dat_fish_vel_final$pool == '13'] <- 13

dat_fish_vel_final <- dat_fish_vel_final %>%
  mutate(pool = as.numeric(pool))

# view column names and compare to wq final 
# head(dat_fish_vel_final)
#  barcode comp  fstation pool  stratum DATE       z15east z15north   vel vel_qf
# head(dat_wq_vel_final)
# barcode comp  fstation  pool stratum DATE       z15east z15north   vel vel_qf

### clean environment of temporarty fish dataframes ###
# original raw data - all
rm(dat_fish_fs123)
# original site level data
rm(dat_fish_sites_fs123)
# temporary unique barcodes df
rm(dat_fish_unique_barcodes_fs123)
# temporary original velocity df (prior to QAQC)
rm(dat_fish_vel_fs123)


#__3. AV Velocity Data----

### optional data check - stratum by pool
# dat_veg_vel_strat_summary <- dat_veg_vel_fs123 %>%
  # group_by(POOL, MSTRATUM) %>%
  # summarise(n=n())

#____a)  exclude 252 Pepin (TDL-U and TDL-L) records----
dat_veg_vel_fs123 <- subset(dat_veg_vel_fs123, MSTRATUM != "TDL-U" & MSTRATUM != "TDL-L" )


#____b)  add component ('comp') column and populate with 'v' (veg)----
dat_veg_vel_fs123$comp <- "v"

#____c) standardize column names----

#add 'vel_qf' column (all values null)
dat_veg_vel_fs123$vel_qf <- "" 

#view current column names
# head(dat_veg_vel_fs123)
#   BARCODE FLDNUM POOL MSTRATUM  DATE  EAST1  NORTH1 WATER_VEL comp vel_qf

# renamed to:
# barcode fstation pool stratum date z15east z15north vel comp vel_qf
colnames(dat_veg_vel_fs123) <- c("barcode", "fstation", "pool", "stratum", "DATE", "z15east", "z15north", 
                                           "vel",  "comp", "vel_qf")

#______ii) standardize column order----
dat_veg_vel_final <- dat_veg_vel_fs123[,c(1, 9, 2, 3, 4, 5, 6, 7, 8, 10)]

# view column names and compare to fish final 
# head(dat_veg_vel_final)
# barcode comp  fstation pool  stratum DATE z15east z15north  vel vel_qf

# head(dat_fish_vel_final)
# barcode comp  fstation pool  stratum DATE z15east z15north  vel vel_qf

### clean environment of temporarty fish dataframes ###
# original raw data prior to QAQC
rm(dat_veg_vel_fs123)

#__4. Discharge Data----

#____a) filter data up to year 2024----



# C. MERGE LTRM AND DISCHARGE DATA----

#__1. few more ltrm data manipulations----

# convert "vel_qf" column in veg and wq dataframes to numeric to match fish

# veg
dat_veg_vel_final <- dat_veg_vel_final %>%
  mutate(vel_qf = as.numeric(vel_qf))

# wq
dat_wq_vel_final <- dat_wq_vel_final %>%
  mutate(vel_qf = as.numeric(vel_qf))

# convert fish 'pool' to numeric to match other components
dat_fish_vel_final <- dat_fish_vel_final %>%
  mutate(pool = as.numeric(pool))

#__2. combine 3 LTRM velocity dataframes ----
##with bind_rows function (dplyr package)
dat_vel_p4p8p13_wfv <- dplyr::bind_rows(dat_wq_vel_final, dat_fish_vel_final, dat_veg_vel_final)


#__3. create and format new date columns----

### create new formatted 'date' field in the merged dateframe since...###
###...by default date is imported as a string of characters ###
dat_vel_p4p8p13_wfv$date <-as.POSIXct(dat_vel_p4p8p13_wfv$DATE, format = "%m/%d/%Y")

### create new 'year' 'month' 'day' fields from 'date' value ### 
dat_vel_p4p8p13_wfv <- dat_vel_p4p8p13_wfv %>%
  mutate(year=  year(date) ) %>%
  mutate(month = month(date)) %>%
  mutate(day = day(date))



#____a) add 'reach' column to split p4u and p4l----
### (p8 and p13 not split and remain the same)
dat_vel_p4p8p13_wfv$reach[dat_vel_p4p8p13_wfv$pool == 4 & dat_vel_p4p8p13_wfv$z15east < 546000] <- '4u'
dat_vel_p4p8p13_wfv$reach[dat_vel_p4p8p13_wfv$pool == 4 & dat_vel_p4p8p13_wfv$z15east > 570000] <- '4l'
dat_vel_p4p8p13_wfv$reach[dat_vel_p4p8p13_wfv$pool == 8] <- '8'
dat_vel_p4p8p13_wfv$reach[dat_vel_p4p8p13_wfv$pool == 13] <- '13'

### this yields 19 rows where reach = NA (BWC adjacent to Pepin), so...###
###...remove those records ###

dat_vel_p4p8p13_wfv <- 
  subset(dat_vel_p4p8p13_wfv , !is.na(reach))
  

### optional double check for reach values by pool ###
# dat_vel_p4p8p13_wfv %>%
# group_by(pool, reach) %>%
# summarise(n = n())

# A tibble: 4 × 3
# Groups:   pool [3]
# pool reach     n
# <dbl> <chr> <int>
#     4 4l    10515
#     4 4u     5426
#     8 8     23524
#    13 13    20818

#____b) remove BWI ???----


#__3. Merge discharge data from multiple pools.----
# ***requires dplyr package (in tidyverse)***

#join ld4 and ld8 discharge data
dat_q_ld4_ld8 <- dplyr::left_join(dat_q_ld8, dat_q_ld4, by=c("date"))
#join ld4_ld8 to ld10 discharge data
dat_q_ld4_ld8_ld10 <- dplyr::left_join(dat_q_ld4_ld8, dat_q_ld10, by=c("date"))

#select relevant columns from merged discharge dataset
dat_q_p4p8p10 <- dplyr::select (dat_q_ld4_ld8_ld10, c('date', 'year', 'month', 'cfs_ld4', 'cfs_ld8', 'cfs_ld10'))



#__4. merge velocity and discharge dataframes----
dat_vel_q_p4p8p13 <- dplyr::left_join(dat_vel_p4p8p13_wfv, dat_q_p4p8p10, by=c("date"))



#____a) calculate 0.33, 0.66 percentiles of discharge at each L&D----



#_____i) calculate discharge low, high values ----
###from daily values 1993-2024 excluding winter

#filter for data years 1993-2024
dat_q_p4p8p10_9324 <- dat_q_p4p8p10 %>% filter (year > 1992 & year < 2025)

#filter for non-winter data
dat_q_p4p8p10_9324_sprsumfal <- dat_q_p4p8p10_9324 %>% filter (month > 2 & month < 12)


#assign percentile values to variables

###LD4
qlow_ld4 <- quantile(dat_q_p4p8p10_9324_sprsumfal$cfs_ld4, probs = c(0.33))
# 33% 
# 23800
qhigh_ld4 <- quantile(dat_q_p4p8p10_9324_sprsumfal$cfs_ld4, probs = c(0.66))
# 66%
# 47600

###ld8
qlow_ld8  <- quantile(dat_q_p4p8p10_9324_sprsumfal$cfs_ld8, probs = c(0.33))
#   33% 
# 31700
qhigh_ld8 <- quantile(dat_q_p4p8p10_9324_sprsumfal$cfs_ld8, probs = c(0.66))
#   66% 
# 59400 

###ld10
qlow_ld10  <- quantile(dat_q_p4p8p10_9324_sprsumfal$cfs_ld10, probs = c(0.33))
#   33% 
# 40900
qhigh_ld10 <- quantile(dat_q_p4p8p10_9324_sprsumfal$cfs_ld10, probs = c(0.66))
#   66% 
# 74800


#____b) create new discharge category column (q_cat)----
##and populate based on 33rd, 66th percentile values
###for respective pools

#Pool 4
dat_vel_q_p4p8p13$q_cat[dat_vel_q_p4p8p13$pool == 4 & dat_vel_q_p4p8p13$cfs_ld4 <= qlow_ld4 ] <- 'Low'
dat_vel_q_p4p8p13$q_cat[dat_vel_q_p4p8p13$pool == 4 & dat_vel_q_p4p8p13$cfs_ld4 > qlow_ld4 & 
                          dat_vel_q_p4p8p13$cfs_ld4 <= qhigh_ld4 ] <- 'Mod'
dat_vel_q_p4p8p13$q_cat[dat_vel_q_p4p8p13$pool == 4 & dat_vel_q_p4p8p13$cfs_ld4 > qhigh_ld4 ] <- 'High'

#Pool 8
dat_vel_q_p4p8p13$q_cat[dat_vel_q_p4p8p13$pool == 8 & dat_vel_q_p4p8p13$cfs_ld8 <= qlow_ld8 ] <- 'Low'
dat_vel_q_p4p8p13$q_cat[dat_vel_q_p4p8p13$pool == 8 & dat_vel_q_p4p8p13$cfs_ld8 > qlow_ld8 & 
                          dat_vel_q_p4p8p13$cfs_ld8 <= qhigh_ld8 ] <- 'Mod'
dat_vel_q_p4p8p13$q_cat[dat_vel_q_p4p8p13$pool == 8 & dat_vel_q_p4p8p13$cfs_ld8 > qhigh_ld8 ] <- 'High'

#Pool 13 (based off L&D10 data)
dat_vel_q_p4p8p13$q_cat[dat_vel_q_p4p8p13$pool == 13 & dat_vel_q_p4p8p13$cfs_ld10 <= qlow_ld10 ] <- 'Low'
dat_vel_q_p4p8p13$q_cat[dat_vel_q_p4p8p13$pool == 13 & dat_vel_q_p4p8p13$cfs_ld10 > qlow_ld10 & 
                          dat_vel_q_p4p8p13$cfs_ld10 <= qhigh_ld10 ] <- 'Mod'
dat_vel_q_p4p8p13$q_cat[dat_vel_q_p4p8p13$pool == 13 & dat_vel_q_p4p8p13$cfs_ld10 > qhigh_ld10 ] <- 'High'

#count q_cat values by pool, check for NA
dat_vel_q_p4p8p13 %>%
  group_by(q_cat, pool) %>%
  summarise(m = n())

# A tibble: 9 × 3
# Groups:   q_cat [3]
# q_cat  pool     m
# <chr> <dbl> <int>
# 1 High      4  5089
# 2 High      8  7631
# 3 High     13  6514
# 4 Low       4  5855
# 5 Low       8  8586
# 6 Low      13  8033
# 7 Mod       4  5016
# 8 Mod       8  7307
# 9 Mod      13  6271

#____c) calculate vel:q ratio----

###create new column 'cv_cm' (velocity in cm) and calculate 
dat_vel_q_p4p8p13$cv_cm <- dat_vel_q_p4p8p13$vel * 100

###create new column 'q_10k' (discharge / 10000) and calculate 
dat_vel_q_p4p8p13$q_10k[dat_vel_q_p4p8p13$pool == 4] <-  dat_vel_q_p4p8p13$cfs_ld4 / 10000
dat_vel_q_p4p8p13$q_10k[dat_vel_q_p4p8p13$pool == 8] <-  dat_vel_q_p4p8p13$cfs_ld8 / 10000
dat_vel_q_p4p8p13$q_10k[dat_vel_q_p4p8p13$pool == 13] <-  dat_vel_q_p4p8p13$cfs_ld10 / 10000

###create new 'vq__ratio' (velocity in cm / Q in 10k) column and calculate
dat_vel_q_p4p8p13$vq_ratio <- dat_vel_q_p4p8p13$cv_cm / dat_vel_q_p4p8p13$q_10k

#D.  EXPORT FINAL DATASETS----
##as .csv to plot in GIS, 1 for each reach---

###filter for each reach
dat_pre_gis_r4u <- dat_vel_q_p4p8p13 %>% filter (reach == '4u') ###this will exclude the Pepin adjacent BWC sites
dat_pre_gis_r4l <- dat_vel_q_p4p8p13 %>% filter (reach == '4l') ###this will exclude the Pepin adjacent BWC sites
dat_pre_gis_r8 <- dat_vel_q_p4p8p13 %>% filter (reach == '8')
dat_pre_gis_r13 <- dat_vel_q_p4p8p13 %>% filter (reach == '13')

###write to .csv
write.csv(dat_pre_gis_r4u, paste(path_ltrm_data, "/", 'dat_pre_gis_r4u.csv', sep = ""), row.names = FALSE)
write.csv(dat_pre_gis_r4l, paste(path_ltrm_data, "/", 'dat_pre_gis_r4l.csv', sep = ""), row.names = FALSE)
write.csv(dat_pre_gis_r8, paste(path_ltrm_data, "/", 'dat_pre_gis_r8.csv', sep = ""), row.names = FALSE)
write.csv(dat_pre_gis_r13, paste(path_ltrm_data, "/", 'dat_pre_gis_r13.csv', sep = ""), row.names = FALSE)



#D. PARTITION DATA post-GIS


#X. Optional Misc Data checks----

#scatterplot of discharge between pools to look for outliers
ggplot(dat_q_p4p8, aes(x=cfs_ld4/1000, y=cfs_ld8/1000)) +
  geom_point()

#count records by component and month
dat_vel_p4p8p13_wfv %>%
  group_by(comp, month) %>%
  summarise(m = n())

#double check for vel_qf values by component - only WQ should have "3" values, rest are NA
dat_vel_p4p8p13_wfv %>%
  group_by(comp, vel_qf) %>%
  summarise(m = n())


#count velocity records by qf codes
dat_wq_vel_fs123_exclude_NA %>%
  group_by(VELQF, FLDNUM) %>%
  summarise(m = n())

#count velocity records after excluding QF code flag values
dat_wq_vel_fs123_exclude_qf %>%
  group_by(VELQF, FLDNUM) %>%
  summarise(m = n())

#count velocity records by pool
dat_fish_vel_fs123_exclude_qf %>%
  group_by(pool) %>%
  summarise(m = n())

#Y. To Do: ----

#after GIS spatial analuses (nearest aquatic area and strata polygon), export data tables and
##remove sites outside aqauatic areas and or strata then

#partition data in R

#Z.Some early analysis code s for archive ----

#___1) historical daily Q values (1959-2024)----

#ld4
q_percentile_ld4 <- quantile(dat_q_p4p8$cfs_ld4, probs = c(0.33, 0.66))
#  33%   66% 
# 17300 34000 

#ld8
q_percentile_ld8 <- quantile(dat_q_ld8$cfs_ld8, probs = c(0.33, 0.66))
#  33%   66% 
# 21500 42900 

#ld10
q_percentile_ld10 <- quantile(dat_q_ld10$cfs_ld10, probs = c(0.33, 0.66))
#  33%   66% 
# 30300 53300 

#______a) scatterplot of discharge between pools ----
###to assess use of ld10 for pool 13
ggplot(dat_q_p4p8p10, aes(x=cfs_ld8, y=cfs_ld10)) +
  geom_point()

#______b) Initial attempts to fill in missing discharge data---- 
####source data ultimately updated prior to import into R

#__10/8/2018 LD4----
#add row(s) for data missing data
#for L&D4 on 10/8/2018
#use 57540 (value between 10/7 and 10/9)
#head(dat_q_ld4_orig)

#dat_q_ld4_missing <- data.frame("10/8/2018 12:00", "57540")
#names(dat_q_ld4_missing)<-c("Date.Time", "cfs_ld4")

#dat_q_ld4 <- rbind(dat_q_ld4_orig, dat_q_ld4_missing)

#__7/27/2019 LD4----

#right_join yielded single NA value for 'cfs_ld4' for date = 7/27/2019, so...
#...manually replace with 72650 (difference between 7/26/2019 and 7/28/2019)
#dat_q_p4p8$cfs_ld4[dat_q_p4p8$date == '2019-07-27'] <- 72650

#dat_q_p4p8l$cfs_ld4 <- as.numeric(as.character(dat_q_p4p8l$cfs_ld4))

#__2. not working----
#dat_q_ld4 %>% add_row(Date.Time = "10/8/2018 12:00", cfs_ld4 = 57540)





