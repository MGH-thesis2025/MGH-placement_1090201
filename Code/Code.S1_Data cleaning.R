# Please show and unhide document outline to have a cleaner view of each section within this script
# Abbreviations: 
# - abx = antibiotics 
# - MPP = modified per protocol
# - PID = participant ID
# - obs wind = observation window 

rm(list = ls())

############################################################.
######## Data cleaning and preparation #####################.
############################################################.

# load packages
library(pacman)
p_load(
  readxl,
  writexl,
  tidyverse, 
  lubridate, 
  dplyr
)

############################################################.
# Objective 1: Cleaning the medication record ----
# - restrict analysis to MPP population that received 3 primary series doses + 1st booster 
# - calculate time interval between 3rd dose administration and medication start date 
# - calculate time interval between vaccine effective time (14 days after 3rd dose administration) and medication start date 
# - define observation window 
# - restrict analysis to abx prescribed during the observation window
############################################################。

############################################################。
## 1. Finalise cleaning for restriction to MPP population that received 3 primary series doses + 1st booster ----

### a. Load manually cleaned files (MPP population that received 3 primary series doses) ----
# from Data.S1_Raw data_Prior or Concomitant Medications.xlsx 
# manually classified each drug into abx or not (binary indicator 0 or 1) -> Data.S5_Abx classification.xlsx
# then manually cleaned in excel to restrict to MPP population that received 3 primary series doses ->
# Data.S6_Dose3_MPP_abx.xlsx 
# (with information from Data.S2_Raw data_Vaccination date.xlsx,Data.S3_Raw data_Age_Sex_MPP.xlsx)


# File: abx and medication prescription data that has manually cleaned to restrict to MPP population that received 3 doses
xls_file1 <- file.path(
  "Data",
  "Data.S6_Dose3_MPP_abx.xlsx"
)
data1 <- read_excel(xls_file1, sheet = 1)
abx_list <- as.data.frame(data1)
length(unique(abx_list$PID)) #4638

# File: 3rd primary series dose administration date data of MPP population that received 3 primary series doses 
# data is manually filtered and cleaned in excel 
# from Data.S2_Raw data_Vaccination date.xlsx, Data.S3_Raw data_Age_Sex_MPP.xlsx
xls_file2 <- file.path(
  "Data",
  "Data.S7_Dose3_MPP_date.xlsx"
)
data2 <- read_excel(xls_file2, sheet = 1)
dose3_date <- as.data.frame(data2)
length(unique(dose3_date$PID)) #4644

### b. Check for manually cleaning ----
# Make sure PID (participant ID) in both datasets are identical
# check for potential mistakes from manual cleaning in excel 
# i.e. abx list data include all the participants in MPP analysis and received 3 doses, 
# and exclude those haven't

missing_in_dose3_date <- setdiff(abx_list$PID, dose3_date$PID)
missing_in_dose3_date

missing_in_abx_list <- setdiff(dose3_date$PID, abx_list$PID)
missing_in_abx_list

#########################.
# Manually change and update in excel 
# Reload the updated data and check again

xls_file3 <- file.path(
  "Data",
  "Data.S8_Dose3_MPP_abx_updated.xlsx"
)
data3 <- read_excel(xls_file3, sheet = 1)
abx_list <- as.data.frame(data3)
length(unique(abx_list$PID)) #4644

xls_file4 <- file.path(
  "Data",
  "Data.S9_Dose3_MPP_date_updated.xlsx"
)
data4 <- read_excel(xls_file4, sheet = 1)
dose3_date <- as.data.frame(data4)
length(unique(dose3_date$PID)) #4644

# number of PIDs are identical now 

missing_in_dose3_date <- setdiff(abx_list$PID, dose3_date$PID)
missing_in_dose3_date #0

missing_in_abx_list <- setdiff(dose3_date$PID, abx_list$PID)
missing_in_abx_list #0

######################.
### c. Further limit the analysis to population that received first booster and met MPP definition ----

# File: data of individuals that received 1st booster (with 1st booster administration time)
# data is manually filtered in excel from Data.S2_Raw data_Vaccination date.xlsx 
xls_file5 <- file.path(
  "Data",
  "Data.S10_Booster.xlsx"
)
data5 <- read_excel(xls_file5, sheet = 1)
booster <- as.data.frame(data5)
length(booster$PID) #4467

# File: PIDs with respective age, sex and whether individual is in MPP analysis or not
xls_file6 <- file.path(
  "Data",
  "Data.S3_Raw data_Age_Sex_MPP.xlsx"
)
data6 <- read_excel(xls_file6, sheet = 1)
age_sex <- as.data.frame(data6)
length(unique(age_sex$PID)) #4878

#### Restrict from individuals that received 1st booster -> individuals in MPP analysis that received 1st booster (MPP boost1)----
booster <- left_join(
  booster,
  age_sex %>% 
    mutate(PID = as.character(PID)) %>%
    select(PID, `MPP Boost1`),
  by = "PID"
)

# Filter to individuals with MPP Boost 1 =1, remove individuals with MPP Boost 1 =NA (receive booster 1 but not in MPP)
booster <- booster[!is.na(booster$`MPP Boost1`), ]
length(unique(booster$PID)) #4369

######################.
# In abx_list data, and dose3_date data, update the data to 
# limit the analysis to MPP population that received first booster

abx_list <- left_join(
  abx_list,
  booster %>% 
    select(PID, Booster_date),
  by = "PID"
)

length(unique(abx_list$PID)) #4644
# Remove individuals that did not receive booster or not in MPP
abx_list <- abx_list[!is.na(abx_list$Booster_date), ]
length(unique(abx_list$PID)) #4369



dose3_date <- left_join(
  dose3_date,
  booster %>% 
    select(PID, Booster_date),
  by = "PID"
)
length(dose3_date$PID) #4644
# Remove individuals that did not receive booster or not in MPP
dose3_date <- dose3_date[!is.na(dose3_date$Booster_date), ]
dose3_date <- dose3_date %>% 
  rename(Dose3_date=Date_of_vaccination)
length(dose3_date$PID) #4369

############################################################.
## 2. Classify abx into different classes of abx ----
# export in excel to classify and update 
write_xlsx(abx_list, "Data.S11_abx list.xlsx")

xls_file7 <- file.path(
  "Data",
  "Data.S12_abx_list_update_class.xlsx"
)
data7 <- read_excel(xls_file7, sheet = 1)
new_abx_list <- as.data.frame(data7)
length(unique(new_abx_list$PID)) #4369

############################################################.
## 3. Merge the datasets, to add a column of 3rd dose administration date into abx_list data ----
# Join the data based on common PID 
new_abx_list <- left_join(
  new_abx_list,
  dose3_date %>% 
    select(PID, Dose3_date),
  by = "PID"
)

# Rename columns to make it clearer 
new_abx_list <- new_abx_list %>% 
  rename(Meds_start_date = Start_date)%>%
  rename(Meds_stop_date = Stop_date)

length(unique(new_abx_list$PID)) #4369

############################################################.
## 4. Calculate all the time interval (in days) needed ----
# When statistician handle the data to reveal vaccination status, 
# they will assign a random number to each individual and remove PID.
# To avoid knowing the link btw or matching PID and vaccination status, 
# all the dates associated will be removed.

# Therefore, data cleaning will prepare all the time intervals:
# - btw 3rd dose administration and 1st booster administration 
# - btw medication start and stop date
# - btw 3rd dose administration and start date of medication 
# - btw vaccine effective time (14 days after 3rd dose administration date) and start date of medication 
# - btw 1st booster administration and start date of medication 

abx_list_time_interval <- new_abx_list %>%
  mutate(
    Dose3_date = as.Date(Dose3_date),
    Booster_date = as.Date(Booster_date),
    Meds_start_date = as.Date(Meds_start_date),
    Meds_stop_date = as.Date(Meds_stop_date),
    
    Dose3_booster_interval = as.numeric(difftime(Booster_date, Dose3_date, units = "days")),
    Meds_interval = as.numeric(difftime(Meds_stop_date, Meds_start_date, units = "days")),
    Dose3_meds_interval = as.numeric(difftime(Meds_start_date, Dose3_date, units = "days")),                     
    Dose3_14_meds_interval = as.numeric(difftime(Meds_start_date, Dose3_date + 14, units = "days")),
    Booster_meds_interval = as.numeric(difftime(Meds_start_date, Booster_date, units = "days"))
  )
length(unique(abx_list_time_interval$PID)) #4369


############################################################.
## 5. Define observation window ----

### a. Load data of participants that received follow-up visits, and date of visits ----
# manually filter in excel from Data.S4_Raw data_Date of Visit Listing.xlsx 

# File: visit date of 2B0_prevacc (on the day of 2nd booster administration at the time before 2nd booster administration)
xls_file8 <- file.path(
  "Data",
  "Data.S13_2b0_prevacc_visit.xlsx"
)
data8 <- read_excel(xls_file8, sheet = 1)
date_2b0 <- as.data.frame(data8)
length(date_2b0$PID) #4178

# File: visit date of B365 (365 days after 1st booster, for participants that did not take part in the trial extension)
xls_file9 <- file.path(
  "Data",
  "Data.S14_B365_visit.xlsx"
)
data9 <- read_excel(xls_file9, sheet = 1)
date_b365 <- as.data.frame(data9)
length(date_b365$PID) #58

# File: visit date of B1Y (365 days after 1st booster, for participants consented to take part in trial extension)
xls_file10 <- file.path(
  "Data",
  "Data.S15_B1Y_visit.xlsx"
)
data10 <- read_excel(xls_file10, sheet = 1)
date_b1y <- as.data.frame(data10)
length(date_b1y$PID) #2116

# Merge the date of visits with dose3_date data
obs_wind_time <- left_join(
  dose3_date,
  date_2b0 %>% 
    select(PID, `2B0_Visit date`),
  by = "PID"
)

obs_wind_time <- left_join(
  obs_wind_time,
  date_b365 %>% 
    select(PID, `B365_Visit date`),
  by = "PID"
)

obs_wind_time <- left_join(
  obs_wind_time,
  date_b1y %>% 
    select(PID, `B1Y_Visit date`),
  by = "PID"
)

######################.
# add study exit date for participants withdrawn from the study (due to loss to follow up, withdrawal, death...)
age_sex$`Study exit date`[age_sex$`Study exit date` == "."] <- NA
# convert to numeric (for R to interpret the excel serial number)
age_sex$`Study exit date` <- as.numeric(age_sex$`Study exit date`)
# convert to Date (check to match the date in excel)
age_sex$`Study exit date` <- as.Date(age_sex$`Study exit date`, origin = "1899-12-30")


obs_wind_time <- left_join(
  obs_wind_time,
  age_sex %>% 
    mutate(PID = as.character(PID)) %>%
    select(PID, `Study exit date`),
  by = "PID"
)

######################.
# Calculate the time interval between 3rd dose administration date and date of visits (2b0, b365, b1y, exit_date)
# The ending point of observation window should be 2b0, b365, b1y, 
# which is date of 2nd booster administration or 1 year after 1st booster 
obs_wind_time <- obs_wind_time %>%
  mutate(
    `2B0_interval` = difftime(`2B0_Visit date`, Dose3_date, units = "days") ,
    `B365_interval` = difftime(`B365_Visit date`, Dose3_date, units = "days"),
    `B1Y_interval` = difftime(`B1Y_Visit date`, Dose3_date, units = "days"), 
    `exit_interval`= difftime(`Study exit date`, Dose3_date, units = "days")
  )
length(obs_wind_time$PID) #4369

count <- obs_wind_time[is.na(obs_wind_time$`2B0_Visit date`) & is.na(obs_wind_time$`B365_Visit date`) 
                       & is.na(obs_wind_time$`B1Y_Visit date`) & is.na(obs_wind_time$`Study exit date`), ]
length(count$PID) #1, 1 participant no record of any visit (2BO visit scheduled, but not conducted due to serious adverse event)

######################.
# The end of observation window will follow the order of using 2b0, b365, b1y, exit_date
obs_wind_time$visit_date <- obs_wind_time$`2B0_Visit date`
# Fill NAs in visit_date with values from B365_Visit date
obs_wind_time$visit_date[is.na(obs_wind_time$visit_date)] <- 
  obs_wind_time$`B365_Visit date`[is.na(obs_wind_time$visit_date)]
# Fill NAs in visit_date with values from B1Y_Visit date
obs_wind_time$visit_date[is.na(obs_wind_time$visit_date)] <- 
  obs_wind_time$`B1Y_Visit date`[is.na(obs_wind_time$visit_date)]
# Fill NAs in visit_date with values from Study exit date
obs_wind_time$visit_date[is.na(obs_wind_time$visit_date)] <- 
  obs_wind_time$`Study exit date`[is.na(obs_wind_time$visit_date)]

# Identify the row index where visit_date is NA, the participant had no record of any visit 
missing_index <- which(is.na(obs_wind_time$visit_date))
missing_index

# Replace the missing visit_date with booster_date (1st booster) + 365 days
obs_wind_time$Booster_date <- as.Date(obs_wind_time$Booster_date)
obs_wind_time$visit_date <- as.Date(obs_wind_time$visit_date)
obs_wind_time$visit_date[missing_index] <- obs_wind_time$Booster_date[missing_index] + 365

######################,
### b. Calculate the time interval (observation window) ----
# - btw 3rd dose administration and end of observation window
# - btw vaccine effective time (14 days after 3rd dose administration date) and end of observation window 
obs_wind_time$Dose3_date <- as.Date(obs_wind_time$Dose3_date)
obs_wind_time <- obs_wind_time %>%
  mutate(
    Visit_time_interval = difftime(visit_date, Dose3_date, units = "days"), # 2 year after 3rd dose
    Visit_time_interval_14 = difftime(visit_date, Dose3_date+14, units = "days") # time after 14 days after 3rd dose
  )

write_xlsx(obs_wind_time, "Data.S16_obs_window_time.xlsx")

######################.
# Add date of visit/interval to abx_list_time_interval
abx_list_time_interval <- left_join(
  abx_list_time_interval,
  obs_wind_time %>% 
    select(PID, visit_date, Visit_time_interval, Visit_time_interval_14),
  by = "PID"
)


############################################################.
## 6. Restrict analysis to abx prescribed during the observation window: ----
# 14 days after 3rd dose ~ 2nd booster administration or 1 yr after 1st booster

# In new column `Abx_or_not_obs_window`: 
# - mark abx prescription as 0 for abx prescribed out of observation window
# - keeping abx prescription on 14th day after 3rd dose (vaccine start effect)
# - keeping the participants that does not go to clinic and have no abx at all, with meds start date=NA and time_interval=NA

abx_list_time_interval$Visit_time_interval <- as.numeric(abx_list_time_interval$Visit_time_interval)
abx_list_time_interval$Visit_time_interval_14 <- as.numeric(abx_list_time_interval$Visit_time_interval_14)

# create new column 
abx_list_time_interval$Abx_or_not_obs_window <- abx_list_time_interval$Abx_or_not

abx_list_time_interval <- abx_list_time_interval %>%
  mutate(Abx_or_not_obs_window = if_else(
    is.na(Dose3_14_meds_interval), 
    Abx_or_not,
    if_else(
      Dose3_14_meds_interval < 0 | Dose3_14_meds_interval > Visit_time_interval_14, 
      # can be on the same day of visit date because it's prevacc, 
      # even on the same day, but before 2nd booster administration time 
      0,
      Abx_or_not
    )
  ))

length(unique(abx_list_time_interval$PID)) # 4369, constantly checking if the number of individuals remains identical 


############################################################.
## 7. Add gender and sex ----

final_abx_list <- left_join(
  abx_list_time_interval,
  age_sex %>% 
    mutate(PID = as.character(PID)) %>%
    select(PID, Sex, `Age (days) at 1st dose`),
  by = "PID"
)

length(unique(final_abx_list$PID)) #4369

############################################################.
## 8. Export and save ----
write_xlsx(final_abx_list, "Data.S17_final_abx_list.xlsx")

