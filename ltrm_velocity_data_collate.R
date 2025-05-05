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


### to get the url for data, open file (.csv) in Github, then click on "Raw" button ###
# raw data url method not working all of a sudden : 
# dat_q_ld4 <- read_csv('https://raw.githubusercontent.com/Lake-City-LTRM/hydrology/refs/heads/main/data/q_ld4_collated.csv')
# dat_q_ld8 <- read_csv("https://raw.githubusercontent.com/Lake-City-LTRM/hydrology/refs/heads/main/data/q_ld8_collated.csv")
# dat_q_ld10 <- read_csv('https://raw.githubusercontent.com/Lake-City-LTRM/hydrology/refs/heads/main/data/q_ld10_collated.csv')

#______i) LD4----
dat_q_ld4 <- read_csv('data/q_ld4_collated.csv')

#______ii) LD8----
dat_q_ld8 <- read_csv('data/q_ld8_collated.csv')

#______iii) LD10----
dat_q_ld10 <- read_csv('data/q_ld10_collated.csv')



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



#__2. Fisheries Velocity Data----

#____a) combine data from 3 field stations----

dat_fish_fs123 <- rbind(dat_fish_fs1, dat_fish_fs2, dat_fish_fs3)

#____b) parse Site Level data ----

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



#____c)  filter relevant site data----

dat_fish_vel_fs123 <- dplyr::select (dat_fish_sites_fs123, c('barcode', 'fstation', 'stratum', 
                                                             'sdate', 'pool', 'zone15e', 'zone15n', 
                                                             'current', 'cv_qf'))

#____d) exclude values----  

#______i)exclude approx 4200 current velocity NA values----
dat_fish_vel_fs123 <- subset(dat_fish_vel_fs123, !is.na(current))



#______ii) exclude 31 current velocity records with any qf code flag----
dat_fish_vel_fs123 <- subset(dat_fish_vel_fs123, is.na(cv_qf))



#____e)  add component ('comp') column and populate with 'f' (fish)----
dat_fish_vel_fs123$comp <- "f" 

#____f) standardize column names----

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

#____g) standardize column order----
dat_fish_vel_final <- dat_fish_vel_fs123[,c(1, 10, 2, 5, 3, 4, 6, 7, 8, 9)]

#____h) replace pool character values with numeric----
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
# original raw data - 3 field stations
rm(dat_fish_fs1)
rm(dat_fish_fs2)
rm(dat_fish_fs3)
# original raw data - combined 3 field stations
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

#__2. combine 3 LTRM component velocity dataframes ----
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

#__4. Split velocity datasets by reach----

### Pool 4 ###
dat_vel_wvf_p4 <- dat_vel_p4p8p13_wfv %>%
  filter(pool == 4)

### Pool 4 - Upper ###
dat_vel_wvf_p4u <- dat_vel_p4p8p13_wfv %>%
  filter(reach == '4u')

### Pool 4 - Lower ###
dat_vel_wvf_p4l <- dat_vel_p4p8p13_wfv %>%
  filter(reach == '4l')

### Pool 8 ###
dat_vel_wvf_p8 <- dat_vel_p4p8p13_wfv %>%
  filter(pool == 8)

### Pool 13 ###
dat_vel_wvf_p13 <- dat_vel_p4p8p13_wfv %>%
  filter(pool == 13)

#__6. merge velocity and discharge dataframes----

### Pool 4 (Entire) ###
### need this dataframe for calculating low, mod, high discharge categories...###
### ... to apply to P4u and P4l reaches ###

# select only date and cfs columns from discharge data #
dat_q_ld4_select <- dplyr::select (dat_q_ld4, c('date', 'cfs'))
# join discharge to velocity dataset for entire pool #
dat_vel_q_p4 <- dplyr::left_join(dat_vel_wvf_p4, dat_q_ld4_select, by=c("date"))

### Pool 4 - Upper ###

#join discharge to velocity dataset by date #
dat_vel_q_p4u <- dplyr::left_join(dat_vel_wvf_p4u, dat_q_ld4_select, by=c("date"))

### Pool 4 - Lower ###

# select only date and cfs columns from discharge data #
#join discharge to velocity dataset by date #
dat_vel_q_p4l <- dplyr::left_join(dat_vel_wvf_p4l, dat_q_ld4_select, by=c("date"))

### Pool 8 ###

# select only date and cfs columns from discharge data #
dat_q_ld8_select <- dplyr::select (dat_q_ld8, c('date', 'cfs'))
#join discharge to velocity dataset by date #
dat_vel_q_p8 <- dplyr::left_join(dat_vel_wvf_p8, dat_q_ld8_select, by=c("date"))

### Pool 13 ###

# select only date and cfs columns from discharge data #
dat_q_ld10_select <- dplyr::select (dat_q_ld10, c('date', 'cfs'))
#join discharge to velocity dataset by date #
dat_vel_q_p13 <- dplyr::left_join(dat_vel_wvf_p13, dat_q_ld10_select, by=c("date"))

#____a) clean environment ----
rm(dat_q_ld4_select)
rm(dat_q_ld8_select)
rm(dat_q_ld10_select)
rm(dat_wq_vel_final)
rm(dat_fish_vel_final)
rm(dat_veg_vel_final)
rm(dat_vel_wvf_p4)
rm(dat_vel_wvf_p8)
rm(dat_vel_wvf_p13)


#__7. add discharge 'category' and vel:q ratio columns...----

#____a) calculate discharge low, high values ----
### from daily values 1993-2024 + already excludes winter ###

### assign percentile values to variables ###

### LD4 ###
qlow_ld4 <- quantile(dat_vel_q_p4$cfs, probs = c(0.33), na.rm=TRUE)
# 33% 
# 21300
qhigh_ld4 <- quantile(dat_vel_q_p4$cfs, probs = c(0.66), na.rm=TRUE)
# 66%
# 46000

### ld8 ###
qlow_ld8  <- quantile(dat_vel_q_p8$cfs, probs = c(0.33))
#   33% 
# 30100
qhigh_ld8 <- quantile(dat_vel_q_p8$cfs, probs = c(0.66))
#   66% 
# 57800 
 
### ld10 ###
qlow_ld10  <- quantile(dat_vel_q_p13$cfs, probs = c(0.33))
#   33% 
# 36700
qhigh_ld10 <- quantile(dat_vel_q_p13$cfs, probs = c(0.66))
#   66% 
# 65500


#____b) add discharge category and...----
###...and populate based on 33rd, 66th percentile values for respective pools ###

### Pool 4 - Upper ###
dat_vel_q_p4u <- dat_vel_q_p4u %>% mutate(q_cat = case_when(cfs <= qlow_ld4 ~ 'Low',
                                                          cfs > qlow_ld4 & cfs <= qhigh_ld4 ~ 'Mod',
                                                          cfs > qhigh_ld4 ~ ' High'))

### Pool 4 - Lower ###
dat_vel_q_p4l <- dat_vel_q_p4l %>% mutate(q_cat = case_when(cfs <= qlow_ld4 ~ 'Low',
                                                            cfs > qlow_ld4 & cfs <= qhigh_ld4 ~ 'Mod',
                                                            cfs > qhigh_ld4 ~ ' High'))

### Pool 8 ###
dat_vel_q_p8 <- dat_vel_q_p8 %>% mutate(q_cat = case_when(cfs <= qlow_ld8 ~ 'Low',
                        cfs > qlow_ld8 & cfs <= qhigh_ld8 ~ 'Mod',
                        cfs > qhigh_ld8 ~ ' High'))

### Pool 13 (based off L&D10 data) ###
dat_vel_q_p13 <- dat_vel_q_p13 %>% mutate(q_cat = case_when(cfs <= qlow_ld10 ~ 'Low',
                                                          cfs > qlow_ld10 & cfs <= qhigh_ld10 ~ 'Mod',
                                                          cfs > qhigh_ld10 ~ ' High'))


### optional: count q_cat values by pool, check for NA
# dat_vel_q_p13 %>%
#  group_by(q_cat) %>%
#  summarise(n = n())

### *** pool 4 has 18 NA values - need to look into those ***

#____c) calculate vel:q ratio----

### create new column 'cv_cm' (velocity in cm) and calculate ###

### Pool 4 - Upper ###
dat_vel_q_p4u <- dat_vel_q_p4u %>% mutate(vq_ratio = (vel * 100) / (cfs / 10000))
### Pool 4 - Lower ###
dat_vel_q_p4l <- dat_vel_q_p4l %>% mutate(vq_ratio = (vel * 100) / (cfs / 10000))
### Pool 8 ###
dat_vel_q_p8 <- dat_vel_q_p8 %>% mutate(vq_ratio = (vel * 100) / (cfs / 10000))
### Pool 13 ###
dat_vel_q_p13 <- dat_vel_q_p13 %>% mutate(vq_ratio = (vel * 100) / (cfs / 10000))

#__8. Clean Environment----
rm(dat_q_ld4_selct)
rm(dat_vel_p4p8p13_wfv)
rm(dat_vel_q_p4)
rm(dat_vel_wvf_p4l)
rm(dat_vel_wvf_p4u)
rm(dat_vel_wvf_p4)
rm(dat_vel_wvf_p8)
rm(dat_vel_wvf_p13)
rm(dat_q_ld10)
rm(dat_q_ld4)
rm(dat_q_ld8)

#D. PARTITION DATA----
### Training and Testing Datasets ###

#__1. denote 'train' and 'test' records...----

# sort by discharge category and northing then... #
# every 10th row = 'test' and remaining rows = 'train' in new column 'dataset' #

### Pool 4 - Upper ###
dat_vel_q_p4u <- dat_vel_q_p4u %>%
  arrange(q_cat, z15east) %>%
  mutate(dataset = if_else(row_number() %% 10 == 0, 'test', 'train'))

### Pool 4 - Lower ###
dat_vel_q_p4l <- dat_vel_q_p4l %>%
  arrange(q_cat, desc(z15north)) %>%
  mutate(dataset = if_else(row_number() %% 10 == 0, 'test', 'train'))

### Pool 8 ###
dat_vel_q_p8 <- dat_vel_q_p8 %>%
  arrange(q_cat, desc(z15north)) %>%
  mutate(dataset = if_else(row_number() %% 10 == 0, 'test', 'train'))

### Pool 13 ###
dat_vel_q_p13 <- dat_vel_q_p13 %>%
  arrange(q_cat, desc(z15north)) %>%
  mutate(dataset = if_else(row_number() %% 10 == 0, 'test', 'train'))

# **** note that all pools have BWI records and ultimately we may want to...
# ... remove these from P4u (n=14), P4L (n=3), P13 (n=30) but not P8 (n=915) ****

#E.  EXPORT FINAL DATASETS----

### write to .csv in git velocity repo, data folder ###
### 1 file for each reach ###
write_csv(dat_vel_q_p4u, 'data/dat_vel_traintest_p4u.csv')
write_csv(dat_vel_q_p4l, 'data/dat_vel_traintest_p4l.csv')
write_csv(dat_vel_q_p8, 'data/dat_vel_traintest_p8.csv')
write_csv(dat_vel_q_p13, 'data/dat_vel_traintest_p13.csv')


#F. PARTITION DATA post-GIS


