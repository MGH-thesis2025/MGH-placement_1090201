# Please show and unhide document outline to have a cleaner view of each section within this script
# - abx = antibiotics 
# - PID = participant ID
# - obs wind = observation window 

rm(list = ls())

# load packages
library(pacman)
p_load(
  tidyverse, 
  dplyr
)

# load unblinded dataset (with vaccination status unblinded, PID removed, random ID assigned)
# cleaned data: Data.S17_final_abx_list.xlsx was sent to external statistician for unblinding 

csv_file1 <- file.path(
  "Data",
  "Data.S18_unblinded_abx_list.csv"
)
data1 <- read_csv(csv_file1)
abx_list <- as.data.frame(data1)
length(unique(abx_list$random_id)) #4369

abx_list <- abx_list %>%
  mutate(
    random_id = as.character(random_id)
  )

# convert site number to site name 
site_names <- c(
  "11" = "Nanoro",
  "12" = "Siglé",
  "21" = "Bougouni", 
  "31" = "Dandé", 
  "41" = "Bagamoyo", 
  "51" = "Kilifi"
)

abx_list <- abx_list %>%
  mutate(Site = site_names[as.character(Site)])

# specify vaccine group name in full detail 
vacc_group_name <- c(
  "Control" = "Control", 
  "R21" = "R21/Matrix-M"
)

abx_list <- abx_list %>%
  mutate(group = vacc_group_name[as.character(group)])

abx_list <- abx_list %>%
  mutate(
    group = as.factor(group)
  )

############################################################.
# Objective 1: building a main summary list of abx use per participant (within observation window or in total) ----
# - Received at least 1 abx or not (Prescribed abx or not)
# - Number of abx prescription per person & abx prescription incidence rate per person-year
# - Received >1 abx prescription or not
# - Average time interval btw 3rd dose administration and abx start date 
# - Average time interval btw vaccine effective time (14 days after 3rd dose administration date) and abx start date 
# - Average time interval between each abx prescription
# - Average time to first abx prescription
############################################################.

############################################################.
## 1. Age, sex, vaccination group, observation window ----
# Summarise by random_id

main_df <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    Visit_time_interval = min(Visit_time_interval), # from 3rd dose administration to end of follow-up 
    Visit_time_interval_14 = min(Visit_time_interval_14), # from 14 days after 3rd dose administration to end of follow-up 
    `Agedaysat1stdose`= min(`Agedaysat1stdose`),
    Site=min(Site), 
    Sex=min(Sex), 
    group=first(group)
  )
length(main_df$random_id) #4369

# convert duration of observation window from days to years 
main_df <- main_df %>%
  mutate(
    Visit_time_interval_years = Visit_time_interval / 365.25,
    Visit_time_interval_14_years = Visit_time_interval_14 / 365.25
  )

############################################################.
## 2. Whether receive at least 1 antibiotic? ----
# Summarise by random_id 
# Add column: whether receive at least 1 antibiotic prescription or not 
# (during the observation window or in general)

abx_summary <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    received_abx_or_not = as.integer(any(Abx_or_not == 1)),
    received_abx_or_not_obs_window = as.integer(any(Abx_or_not_obs_window == 1))
  ) 

# Join to main_df dataset 
main_df <- left_join(main_df, abx_summary, by = "random_id")


############################################################.
## 3. Incidence/incidence rate of antibiotic prescriptions ----
# Summarise by random_id 
# Add column: number of abx prescription per person
# (during the observation window or in general)

abx_count <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    Abx_count = sum(Abx_or_not == 1, na.rm = TRUE),
    Abx_count_obs_window = sum(Abx_or_not_obs_window == 1, na.rm = TRUE)
  ) 

# Join to main_df dataset 
main_df <- left_join(main_df, abx_count, by = "random_id")

# Add column: incidence rate of abx prescription (per person-year) 
# = number of abx prescription / total duration of observation window in years
main_df <- main_df %>%
  mutate(
    Abx_rate_obs_window = Abx_count_obs_window/Visit_time_interval_14_years
  )


############################################################.
## 4. Whether receive more than 1 antibiotic? ----
# Summarise by random_id 
# Add column: whether receive more than 1 abx prescription
# (during the observation window or in general)

main_df <- main_df %>%
  mutate(More_than_1_abx = ifelse(Abx_count > 1, 1, 0)) %>%
  mutate(More_than_1_abx_obs_window = ifelse(Abx_count_obs_window > 1, 1, 0)) 

############################################################.
## 5. Average time interval btw vaccine effective time (or 3rd dose administration time) and abx start date ----
# Summarise by random_id 
# Add column: Average time interval btw vaccine effective time and medication start date 
# Restrict the time interval when abx_or_not=1, abx_or_not_obs_window=1
# (during the observation window or in general)

avg_time_interval <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    avg_time_interval_days = mean(as.numeric(Dose3_meds_interval[Abx_or_not == 1])),
    avg_time_interval_days_obs_window = mean(as.numeric(Dose3_meds_interval[Abx_or_not_obs_window == 1])),
    avg_time_interval_days_14 = mean(as.numeric(Dose3_14_meds_interval[Abx_or_not == 1])),
    avg_time_interval_days_obs_window_14 = mean(as.numeric(Dose3_14_meds_interval[Abx_or_not_obs_window == 1])),
  )
# Join to main_df dataset 
main_df <- left_join(main_df, avg_time_interval, by = "random_id")

length(unique(main_df$random_id)) #4369

############################################################.
## 6. Average time interval btw each abx prescription ----
# Summarise by random_id 

avg_abx_intervals <- abx_list %>%
  arrange(random_id, Dose3_meds_interval) %>%
  group_by(random_id) %>%
  summarise(
    avg_abx_interval_days = ifelse(
      sum(Abx_or_not == 1) > 1,
      mean(diff(Dose3_meds_interval[Abx_or_not == 1])),
      NA_real_
    ),
    avg_abx_interval_days_obs_window = ifelse(
      sum(Abx_or_not_obs_window == 1) > 1,
      mean(diff(Dose3_meds_interval[Abx_or_not_obs_window == 1])),
      NA_real_
    ),
    avg_abx_interval_days_14 = ifelse(
      sum(Abx_or_not == 1) > 1,
      mean(diff(Dose3_14_meds_interval[Abx_or_not == 1])),
      NA_real_
    ),
    avg_abx_interval_days_obs_window_14 = ifelse(
      sum(Abx_or_not_obs_window == 1) > 1,
      mean(diff(Dose3_14_meds_interval[Abx_or_not_obs_window == 1])),
      NA_real_
    )
  )

main_df <- left_join(main_df, avg_abx_intervals, by = "random_id")
length(unique(main_df$random_id)) #4369

############################################################.
## 7. Time to 1st abx prescription ----
# Add column: time to 1st abx prescription 
# Summarise by random_id 


# i)
# if censored (no abx prescribed during the obs window), the time would be the end of follow-up (end of obs window)
first_abx_times <- abx_list %>%
  group_by(random_id) %>%
  reframe(
    first_abx_time = if (any(Abx_or_not == 1)) {
      min(Dose3_meds_interval[Abx_or_not == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval)
    },
    first_abx_time_obs_window = if (any(Abx_or_not_obs_window == 1)) {
      min(Dose3_meds_interval[Abx_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval)
    },
    first_abx_time_14 = if (any(Abx_or_not == 1)) {
      min(Dose3_14_meds_interval[Abx_or_not == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval_14)
    },
    first_abx_time_obs_window_14 = if (any(Abx_or_not_obs_window == 1)) {
      min(Dose3_14_meds_interval[Abx_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval_14)
    }
  )

main_df <- left_join(main_df, first_abx_times, by = "random_id")
length(main_df$random_id)

# ii)
# if censored (no abx prescribed during the obs window), keep this column as NA
true_first_abx_times <- abx_list %>%
  group_by(random_id) %>%
  reframe(
    true_first_abx_time = if (any(Abx_or_not == 1)) {
      min(Dose3_meds_interval[Abx_or_not == 1], na.rm = TRUE)
    } else {
      NA_real_
    },
    true_first_abx_time_obs_window = if (any(Abx_or_not_obs_window == 1)) {
      min(Dose3_meds_interval[Abx_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      NA_real_
    },
    true_first_abx_time_14 = if (any(Abx_or_not == 1)) {
      min(Dose3_14_meds_interval[Abx_or_not == 1], na.rm = TRUE)
    } else {
      NA_real_
    },
    true_first_abx_time_obs_window_14 = if (any(Abx_or_not_obs_window == 1)) {
      min(Dose3_14_meds_interval[Abx_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      NA_real_
    }
  )
main_df <- left_join(main_df, true_first_abx_times, by = "random_id")
length(main_df$random_id)


## 8. Export and save final cleaned and organised, summarised datasets for further analysis ----
write_csv(abx_list, "Data.S19_unblinded_final_abx_list.csv")
write_csv(main_df, "Data.S20_main_df.csv")









