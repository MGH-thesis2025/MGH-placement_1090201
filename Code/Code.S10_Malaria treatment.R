############# Sensitivity analysis: malaria treatment ##############.
# the same set of analysis as main analysis is conducted

rm(list = ls())

#####################################################################.
# 1. Data prep ----

# load packages
library(pacman)
p_load(
  tidyverse, 
  dplyr
)

# load unblinded dataset (with vaccination status and PID removed, random ID assigned)
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
    group = as.factor(group) # vaccination group
  )


# classify malaria treatment 
abx_list$mala_or_not <- ifelse(grepl("mala", abx_list$Indication_compiled, ignore.case = TRUE), 1, 0)

# restrict to malaria treatment within observation window (for consistency)
abx_list$mala_or_not_obs_window <- abx_list$mala_or_not

abx_list <- abx_list %>%
  mutate(mala_or_not_obs_window = if_else(
    is.na(Dose3_14_meds_interval), 
    mala_or_not,
    if_else(
      Dose3_14_meds_interval < 0 | Dose3_14_meds_interval > Visit_time_interval_14, # can be on the same day of visit date because it's prevacc, even on the same day, but before administer 2b
      0,
      mala_or_not
    )
  ))


############################################################. 
## Objective 1: building a main summary list of malaria treatment per person (within observation window or in total) ----
# - Received at least 1 malaria treatment or not (Prescribed malaria treatment or not)
# - Number of malaria treatment per person & malaria treatment incidence rate per person-year 
# - Received >1 malaria treatment or not
# - Average time interval btw 3rd dose administration and malaria treatment start date 
# - Average time interval btw vaccine effective time (14 days after 3rd dose administration date)  and malaria treatment start date 
# - Average time interval between each malaria treatment
# - Time to first malaria treatment
############################################################.

############################################################. 
# 1. Summarise by random_id — age, sex, vaccination group, observation window 

mala_df <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    Visit_time_interval = min(Visit_time_interval), # from 3rd dose administration to end of follow up 
    Visit_time_interval_14 = min(Visit_time_interval_14), # from 14 days after 3rd dose administration to end of follow up 
    `Agedaysat1stdose`=min(`Agedaysat1stdose`),
    Site=min(Site), 
    Sex=min(Sex), 
    group=first(group)
  )
length(mala_df$random_id)

# convert duration of observation window from days to years 
mala_df <- mala_df %>%
  mutate(
    Visit_time_interval_years = Visit_time_interval / 365.25,
    Visit_time_interval_14_years = Visit_time_interval_14 / 365.25
  )

############################################################.
# 2. Summarise by random_id — whether receive at least 1 malaria treatment?
# Add column: whether receive at least 1 malaria treatment or not 
# (during the observation window or in general)

mala_summary <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    received_mala_or_not = as.integer(any(mala_or_not == 1)),
    received_mala_or_not_obs_window = as.integer(any(mala_or_not_obs_window == 1))
  ) 

# Join to mala_df dataset 
mala_df <- left_join(mala_df, mala_summary, by = "random_id")


############################################################.
# 3. Summarise by random_id - number of malaria treatment & malaria treatment incidence rate 
# Add column: number of malaria treatment per person 
# (during the observation window or in general)

mala_count <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    mala_count = sum(mala_or_not == 1, na.rm = TRUE),
    mala_count_obs_window = sum(mala_or_not_obs_window == 1, na.rm = TRUE)
  ) 

# Join to mala_df dataset 
mala_df <- left_join(mala_df, mala_count, by = "random_id")

# Add column: incidence rate of malaria treatment (per person-year) 
# = number of malaria treatment / total duration of observation window in years
mala_df <- mala_df %>%
  mutate(
    mala_rate_obs_window = mala_count_obs_window/Visit_time_interval_14_years
  )


############################################################.
# 4. Summarise by random_id - whether receive more than 1 malaria treatment? 
# Add column: whether receive more than 1 malaria treatment
# (during the observation window or in general)

mala_df <- mala_df %>%
  mutate(More_than_1_mala = ifelse(mala_count > 1, 1, 0)) %>%
  mutate(More_than_1_mala_obs_window = ifelse(mala_count_obs_window > 1, 1, 0)) 

############################################################.
# 5. Summarise by random_id - Average time interval btw vaccine effective time (or 3rd dose administration time) and malaria treatment start date 
# Add column: Average time interval btw vaccine effective time and medication start date 
# Restrict the time interval when mala_or_not=1, mala_or_not_obs_window=1
# (during the observation window or in general)

avg_time_interval <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    avg_time_interval_days = mean(as.numeric(Dose3_meds_interval[mala_or_not == 1])),
    avg_time_interval_days_obs_window = mean(as.numeric(Dose3_meds_interval[mala_or_not_obs_window == 1])),
    avg_time_interval_days_14 = mean(as.numeric(Dose3_14_meds_interval[mala_or_not == 1])),
    avg_time_interval_days_obs_window_14 = mean(as.numeric(Dose3_14_meds_interval[mala_or_not_obs_window == 1])),
  )
# Join to mala_df dataset 
mala_df <- left_join(mala_df, avg_time_interval, by = "random_id")

length(unique(mala_df$random_id))

############################################################.
# 6. Summarise by random_id - Average time interval btw each malaria treatment 

avg_mala_intervals <- abx_list %>%
  arrange(random_id, Dose3_meds_interval) %>%
  group_by(random_id) %>%
  summarise(
    avg_mala_interval_days = ifelse(
      sum(mala_or_not == 1) > 1,
      mean(diff(Dose3_meds_interval[mala_or_not == 1])),
      NA_real_
    ),
    avg_mala_interval_days_obs_window = ifelse(
      sum(mala_or_not_obs_window == 1) > 1,
      mean(diff(Dose3_meds_interval[mala_or_not_obs_window == 1])),
      NA_real_
    ),
    avg_mala_interval_days_14 = ifelse(
      sum(mala_or_not == 1) > 1,
      mean(diff(Dose3_14_meds_interval[mala_or_not == 1])),
      NA_real_
    ),
    avg_mala_interval_days_obs_window_14 = ifelse(
      sum(mala_or_not_obs_window == 1) > 1,
      mean(diff(Dose3_14_meds_interval[mala_or_not_obs_window == 1])),
      NA_real_
    )
  )

mala_df <- left_join(mala_df, avg_mala_intervals, by = "random_id")
length(unique(mala_df$random_id)) #4369

############################################################.
# 7. Summarise by random_id -  time to 1st malaria treatment
# Add column: time to 1st malaria treatment


# i)
# if censored (no malaria treatment during the obs window), the time would be the end of follow up day (end of obs window)
first_mala_times <- abx_list %>%
  group_by(random_id) %>%
  reframe(
    first_mala_time = if (any(mala_or_not == 1)) {
      min(Dose3_meds_interval[mala_or_not == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval)
    },
    first_mala_time_obs_window = if (any(mala_or_not_obs_window == 1)) {
      min(Dose3_meds_interval[mala_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval)
    },
    first_mala_time_14 = if (any(mala_or_not == 1)) {
      min(Dose3_14_meds_interval[mala_or_not == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval_14)
    },
    first_mala_time_obs_window_14 = if (any(mala_or_not_obs_window == 1)) {
      min(Dose3_14_meds_interval[mala_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval_14)
    }
  )

mala_df <- left_join(mala_df, first_mala_times, by = "random_id")
length(mala_df$random_id) #4369

# ii)
# if censored (no malaria treatment during the obs window), keep this column as NA
true_first_mala_times <- abx_list %>%
  group_by(random_id) %>%
  reframe(
    true_first_mala_time = if (any(mala_or_not == 1)) {
      min(Dose3_meds_interval[mala_or_not == 1], na.rm = TRUE)
    } else {
      NA_real_
    },
    true_first_mala_time_obs_window = if (any(mala_or_not_obs_window == 1)) {
      min(Dose3_meds_interval[mala_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      NA_real_
    },
    true_first_mala_time_14 = if (any(mala_or_not == 1)) {
      min(Dose3_14_meds_interval[mala_or_not == 1], na.rm = TRUE)
    } else {
      NA_real_
    },
    true_first_mala_time_obs_window_14 = if (any(mala_or_not_obs_window == 1)) {
      min(Dose3_14_meds_interval[mala_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      NA_real_
    }
  )
mala_df <- left_join(mala_df, true_first_mala_times, by = "random_id")
length(mala_df$random_id) #4369

#####################################################################.
# 2. Descriptive stats ----

library(pacman)
p_load(
  tidyverse, 
  dplyr, 
  ggplot2, 
  scales, 
  ggpubr
)

############################################################.
## Objective 1: create summary tables ----
# - malaria treatment
########################################################.

############################################################.
# Summarise by site and vaccination group — malaria treatment

# all sites
table4 <- mala_df %>%
  group_by(group) %>%
  summarise(
    `Total number of individuals` = length(random_id),
    
    `Number of people receive at least 1 malaria treatment` = sum(received_mala_or_not == 1),
    `Number of people receive at least 1 malaria treatment during obs wind` = sum(received_mala_or_not_obs_window == 1),
    
    `% receive at least 1 malaria treatment` = mean(received_mala_or_not == 1),
    `% receive at least 1 malaria treatment during obs wind` = mean(received_mala_or_not_obs_window == 1),
    
    `Number of people receive more than 1 malaria treatment` = sum(More_than_1_mala == 1),
    `Number of people receive more than 1 malaria treatment during obs wind` = sum(More_than_1_mala_obs_window == 1),
    
    `% receive more than 1 malaria treatment` = mean(More_than_1_mala == 1),
    `% receive more than 1 malaria treatment during obs wind` = mean(More_than_1_mala_obs_window == 1),
    
    `Total number of malaria treatment` = sum(mala_count), 
    `Total number of malaria treatment during obs wind` = sum(mala_count_obs_window),
    
    `Total duration of observation window in years` = sum(Visit_time_interval_14_years),
    
    `Average number of malaria treatment per person` = mean(mala_count), 
    `Average number of malaria treatment per person during obs wind` = mean(mala_count_obs_window), 
    
    `Incidence rate of malaria treatment (per person-year) during obs wind` = mean(mala_rate_obs_window), 
    
    `Average time interval until malaria treatment` = mean(avg_time_interval_days, na.rm=TRUE), 
    `Average time interval until malaria treatment during obs wind` = mean(avg_time_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval until malaria treatment_14` = mean(avg_time_interval_days_14, na.rm=TRUE), 
    `Average time interval until malaria treatment during obs wind_14` = mean(avg_time_interval_days_obs_window_14, na.rm=TRUE), 
    
    `Average time to first malaria treatment` = mean(first_mala_time), 
    `Average time to first malaria treatment during obs wind` = mean(first_mala_time_obs_window), 
    `Average time to first malaria treatment_14` = mean(first_mala_time_14), 
    `Average time to first malaria treatment during obs wind_14` = mean(first_mala_time_obs_window_14), 
    
    `True average time to first malaria treatment` = mean(true_first_mala_time, na.rm=TRUE), 
    `True average time to first malaria treatment during obs wind` = mean(true_first_mala_time_obs_window, na.rm=TRUE), 
    `True average time to first malaria treatment_14` = mean(true_first_mala_time_14, na.rm=TRUE), 
    `True average time to first malaria treatment during obs wind_14` = mean(true_first_mala_time_obs_window_14, na.rm=TRUE), 
    
    `Average time interval between each malaria treatment` = mean(avg_mala_interval_days, na.rm=TRUE), 
    `Average time interval between each malaria treatment during obs wind` = mean(avg_mala_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval between each malaria treatment_14` = mean(avg_mala_interval_days_14, na.rm=TRUE), 
    `Average time interval between each malaria treatment during obs wind_14` = mean(avg_mala_interval_days_obs_window_14, na.rm=TRUE), 
  )

View(table4)

# group by sites
table5 <- mala_df %>%
  group_by(Site, group) %>%
  summarise(
    `Total number of individuals` = length(random_id),
    
    `Number of people receive at least 1 malaria treatment` = sum(received_mala_or_not == 1),
    `Number of people receive at least 1 malaria treatment during obs wind` = sum(received_mala_or_not_obs_window == 1),
    
    `% receive at least 1 malaria treatment` = mean(received_mala_or_not == 1),
    `% receive at least 1 malaria treatment during obs wind` = mean(received_mala_or_not_obs_window == 1),
    
    `Number of people receive more than 1 malaria treatment` = sum(More_than_1_mala == 1),
    `Number of people receive more than 1 malaria treatment during obs wind` = sum(More_than_1_mala_obs_window == 1),
    
    `% receive more than 1 malaria treatment` = mean(More_than_1_mala == 1),
    `% receive more than 1 malaria treatment during obs wind` = mean(More_than_1_mala_obs_window == 1),
    
    `Total number of malaria treatment` = sum(mala_count), 
    `Total number of malaria treatment during obs wind` = sum(mala_count_obs_window),
    
    `Total duration of observation window in years` = sum(Visit_time_interval_14_years),
    
    `Average number of malaria treatment per person` = mean(mala_count), 
    `Average number of malaria treatment per person during obs wind` = mean(mala_count_obs_window), 
    
    `Incidence rate of malaria treatment (per person-year) during obs wind` = mean(mala_rate_obs_window), 
    
    `Average time interval until malaria treatment` = mean(avg_time_interval_days, na.rm=TRUE), 
    `Average time interval until malaria treatment during obs wind` = mean(avg_time_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval until malaria treatment_14` = mean(avg_time_interval_days_14, na.rm=TRUE), 
    `Average time interval until malaria treatment during obs wind_14` = mean(avg_time_interval_days_obs_window_14, na.rm=TRUE), 
    
    `Average time to first malaria treatment` = mean(first_mala_time), 
    `Average time to first malaria treatment during obs wind` = mean(first_mala_time_obs_window), 
    `Average time to first malaria treatment_14` = mean(first_mala_time_14), 
    `Average time to first malaria treatment during obs wind_14` = mean(first_mala_time_obs_window_14), 
    
    `True average time to first malaria treatment` = mean(true_first_mala_time, na.rm=TRUE), 
    `True average time to first malaria treatment during obs wind` = mean(true_first_mala_time_obs_window, na.rm=TRUE), 
    `True average time to first malaria treatment_14` = mean(true_first_mala_time_14, na.rm=TRUE), 
    `True average time to first malaria treatment during obs wind_14` = mean(true_first_mala_time_obs_window_14, na.rm=TRUE), 
    
    `Average time interval between each malaria treatment` = mean(avg_mala_interval_days, na.rm=TRUE), 
    `Average time interval between each malaria treatment during obs wind` = mean(avg_mala_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval between each malaria treatment_14` = mean(avg_mala_interval_days_14, na.rm=TRUE), 
    `Average time interval between each malaria treatment during obs wind_14` = mean(avg_mala_interval_days_obs_window_14, na.rm=TRUE), 
  )

View(table5)

# group by setting (seasonal/perennial)
mala_df <- mala_df %>%
  mutate(
    setting = ifelse(Site %in% c("Bougouni", "Nanoro", "Siglé"), "Seasonal", "Perennial")
  )


table6 <- mala_df %>%
  group_by(setting, group) %>%
  summarise(
    `Total number of individuals` = length(random_id),
    
    `Number of people receive at least 1 malaria treatment` = sum(received_mala_or_not == 1),
    `Number of people receive at least 1 malaria treatment during obs wind` = sum(received_mala_or_not_obs_window == 1),
    
    `% receive at least 1 malaria treatment` = mean(received_mala_or_not == 1),
    `% receive at least 1 malaria treatment during obs wind` = mean(received_mala_or_not_obs_window == 1),
    
    `Number of people receive more than 1 malaria treatment` = sum(More_than_1_mala == 1),
    `Number of people receive more than 1 malaria treatment during obs wind` = sum(More_than_1_mala_obs_window == 1),
    
    `% receive more than 1 malaria treatment` = mean(More_than_1_mala == 1),
    `% receive more than 1 malaria treatment during obs wind` = mean(More_than_1_mala_obs_window == 1),
    
    `Total number of malaria treatment` = sum(mala_count), 
    `Total number of malaria treatment during obs wind` = sum(mala_count_obs_window),
    
    `Total duration of observation window in years` = sum(Visit_time_interval_14_years),
    
    `Average number of malaria treatment per person` = mean(mala_count), 
    `Average number of malaria treatment per person during obs wind` = mean(mala_count_obs_window), 
    
    `Incidence rate of malaria treatment (per person-year) during obs wind` = mean(mala_rate_obs_window), 
    
    `Average time interval until malaria treatment` = mean(avg_time_interval_days, na.rm=TRUE), 
    `Average time interval until malaria treatment during obs wind` = mean(avg_time_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval until malaria treatment_14` = mean(avg_time_interval_days_14, na.rm=TRUE), 
    `Average time interval until malaria treatment during obs wind_14` = mean(avg_time_interval_days_obs_window_14, na.rm=TRUE), 
    
    `Average time to first malaria treatment` = mean(first_mala_time), 
    `Average time to first malaria treatment during obs wind` = mean(first_mala_time_obs_window), 
    `Average time to first malaria treatment_14` = mean(first_mala_time_14), 
    `Average time to first malaria treatment during obs wind_14` = mean(first_mala_time_obs_window_14), 
    
    `True average time to first malaria treatment` = mean(true_first_mala_time, na.rm=TRUE), 
    `True average time to first malaria treatment during obs wind` = mean(true_first_mala_time_obs_window, na.rm=TRUE), 
    `True average time to first malaria treatment_14` = mean(true_first_mala_time_14, na.rm=TRUE), 
    `True average time to first malaria treatment during obs wind_14` = mean(true_first_mala_time_obs_window_14, na.rm=TRUE), 
    
    `Average time interval between each malaria treatment` = mean(avg_mala_interval_days, na.rm=TRUE), 
    `Average time interval between each malaria treatment during obs wind` = mean(avg_mala_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval between each malaria treatment_14` = mean(avg_mala_interval_days_14, na.rm=TRUE), 
    `Average time interval between each malaria treatment during obs wind_14` = mean(avg_mala_interval_days_obs_window_14, na.rm=TRUE), 
  )
View(table6)

############################################################.
## Objective 2: create plots ----
############################################################.

# 1. Histogram of percentage of individuals received at least 1 / more than 1 malaria treatment 

hist_data <- mala_df %>%
  group_by(group) %>%
  summarize(
    at_least_1 = mean(received_mala_or_not_obs_window == 1)*100,
    more_than_1 = mean(More_than_1_mala_obs_window == 1)*100
  ) %>%
  pivot_longer(cols = c(at_least_1, more_than_1), 
               names_to = "mala_category", 
               values_to = "percent")

ggplot(hist_data, aes(x = factor(group, labels = c("Control", "R21/Matrix-M")), 
                      y = percent, 
                      fill = mala_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Vaccination status", 
       y = "Percentage of individuals", 
       fill = "Malaria treatment") +
  scale_fill_manual(
    values = c("at_least_1" = "darkblue",  
               "more_than_1" = "lightblue"),
    labels = c("At least 1 malaria treatment", "More than 1 malaria treatment")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal()


hist_data_site <- mala_df %>%
  group_by(group, Site) %>%
  summarize(
    at_least_1 = mean(received_mala_or_not_obs_window == 1)*100,
    more_than_1 = mean(More_than_1_mala_obs_window == 1)*100
  ) %>%
  pivot_longer(cols = c(at_least_1, more_than_1), 
               names_to = "mala_category", 
               values_to = "percent")

ggplot(hist_data_site, aes(x = factor(group, labels = c("Control", "R21/Matrix-M")), 
                           y = percent, 
                           fill = mala_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Site) +
  labs(x = "Vaccination status", 
       y = "Number of individuals", 
       fill = "Malaria treatment") +
  scale_fill_manual(
    values = c("at_least_1" = "darkblue",  
               "more_than_1" = "lightblue"),
    labels = c("At least 1 malaria treatment", "More than 1 malaria treatment")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal()


############################################################.
# 2. Incidence rate of malaria treatment  (per person-year) 

# check if is normal distribution 
hist(mala_df$mala_rate_obs_window) # not normal distribution
hist(mala_df$mala_rate_obs_window[mala_df$group == "Control"]) # not normal distribution
hist(mala_df$mala_rate_obs_window[mala_df$group == "R21/Matrix-M"]) # not normal distribution

# Perform Wilcoxon test 
test_result2 <- wilcox.test(mala_rate_obs_window ~ group, data = mala_df)
p_val2 <- signif(test_result2$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df3 <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(mala_df$mala_rate_obs_window) + 1,
  p = p_val2,
  label = paste0("p = ", p_val2)
)

ggboxplot(mala_df, x = "group", y = "mala_rate_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df3, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination status", 
       y = "Incidence rate of malaria treatment (per person-year)",
       fill = "Vaccination status") +
  theme_classic()

############ by site: 
pvalue_df4 <- mala_df %>%
  group_by(Site) %>%
  summarise(
    p = wilcox.test(mala_rate_obs_window ~ group)$p.value,
    y.position = max(mala_rate_obs_window, na.rm = TRUE) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Control",
    group2 = "R21/Matrix-M",
    label = paste0("p = ", signif(p, 2))
  )

ggboxplot(mala_df, x = "group", y = "mala_rate_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df4, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) + 
  labs(x = "Vaccination status", 
       y = "Incidence rate of malaria treatment (per person-year)",
       fill = "Vaccination status")



############################################################.
# 3: True time to first malaria treatment 
# (not having malaria treatment, time is not replaced as end of follow up)
# (from dose 3 administration)

# check if is normal distribution 
hist(mala_df$true_first_mala_time_obs_window) # not normal distribution
hist(mala_df$true_first_mala_time_obs_window[mala_df$group == "Control"]) # not normal distribution
hist(mala_df$true_first_mala_time_obs_window[mala_df$group == "R21/Matrix-M"]) # not normal distribution

# Perform Wilcoxon test 
test_result3 <- wilcox.test(true_first_mala_time_obs_window ~ group, data = mala_df)
p_val3 <- signif(test_result3$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df5 <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(mala_df$true_first_mala_time_obs_window, na.rm = TRUE) + 1,
  p = p_val3,
  label = paste0("p = ", p_val3)
)

ggboxplot(mala_df, x = "group", y = "true_first_mala_time_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df5, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination status", 
       y = "Time to 1st malaria treatment (days)",
       fill = "Vaccination status") +
  theme_classic()

############ by site: 
pvalue_df6 <- mala_df %>%
  group_by(Site) %>%
  summarise(
    p = wilcox.test(true_first_mala_time_obs_window ~ group)$p.value,
    y.position = max(true_first_mala_time_obs_window, na.rm = TRUE) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Control",
    group2 = "R21/Matrix-M",
    label = paste0("p = ", signif(p, 2))
  )

ggboxplot(mala_df, x = "group", y = "true_first_mala_time_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df6, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) + 
  labs(x = "Vaccination status", 
       y = "Time to 1st malaria treatment (days)", 
       fill = "Vaccination status") 

############################################################.
# 4: Average time interval until malaria treatment (from dose 3 administration)

# check if is normal distribution 
hist(mala_df$avg_time_interval_days_obs_window) # is normal distribution
hist(mala_df$avg_time_interval_days_obs_window[mala_df$group == "Control"]) # is normal distribution
hist(mala_df$avg_time_interval_days_obs_window[mala_df$group == "R21/Matrix-M"]) # is normal distribution

# Perform t test 
test_result4 <- t.test(avg_time_interval_days_obs_window ~ group, data = mala_df)
p_val4 <- signif(test_result4$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df7 <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(mala_df$avg_time_interval_days_obs_window, na.rm = TRUE) + 1,
  p = p_val4,
  label = paste0("p = ", p_val4)
)

ggboxplot(mala_df, x = "group", y = "avg_time_interval_days_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df7, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination Status", 
       y = "Average time interval until each malaria treatment (days)",
       fill = "Vaccination status") +
  theme_classic()

############ by site: 
pvalue_df8 <- mala_df %>%
  group_by(Site) %>%
  summarise(
    p = t.test(avg_time_interval_days_obs_window ~ group)$p.value,
    y.position = max(avg_time_interval_days_obs_window, na.rm = TRUE) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Control",
    group2 = "R21/Matrix-M",
    label = paste0("p = ", signif(p, 2))
  )

ggboxplot(mala_df, x = "group", y = "avg_time_interval_days_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df8, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) +
  labs(x = "Vaccination status", 
       y = "Average time interval until each malaria treatment  (days)",
       fill = "Vaccination status") 


############################################################.
# 5: Average time interval between each malaria treatment 

# check if is normal distribution 
hist(mala_df$avg_mala_interval_days_obs_window) # not normal distribution
hist(mala_df$avg_mala_interval_days_obs_window[mala_df$group == "Control"]) # not normal distribution
hist(mala_df$avg_mala_interval_days_obs_window[mala_df$group == "R21/Matrix-M"]) # not normal distribution

# Perform wilcoxon test 
test_result5 <- wilcox.test(avg_mala_interval_days_obs_window ~ group, data = mala_df)
p_val5 <- signif(test_result5$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df9 <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(mala_df$avg_mala_interval_days_obs_window, na.rm = TRUE) + 1,
  p = p_val5,
  label = paste0("p = ", p_val5)
)

ggboxplot(mala_df, x = "group", y = "avg_mala_interval_days_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df9, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination status", 
       y = "Average time interval between each malaria treatment  (days)", 
       fill = "Vaccination status") +
  theme_classic()

############ by site: 
pvalue_df10 <- mala_df %>%
  group_by(Site) %>%
  summarise(
    p = wilcox.test(avg_mala_interval_days_obs_window ~ group)$p.value,
    y.position = max(avg_mala_interval_days_obs_window, na.rm = TRUE) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Control",
    group2 = "R21/Matrix-M",
    label = paste0("p = ", signif(p, 2))
  )

ggboxplot(mala_df, x = "group", y = "avg_mala_interval_days_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df10, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) + 
  labs(x = "Vaccination status", 
       y = "Average time interval between each malaria treatment (days)",
       fill = "Vaccination status")



############################################################.
# 6: Number of malaria treatment over time 
abx_list <- abx_list %>%
  mutate(
    Visit_time_interval_years = Visit_time_interval / 365.25,
    Visit_time_interval_14_years = Visit_time_interval_14 / 365.25
  )

individual_mala_rates <- abx_list %>%
  filter(mala_or_not_obs_window == 1) %>%
  group_by(random_id, Dose3_meds_interval) %>%
  summarise(
    mala_count = sum(mala_or_not_obs_window),
    group = first(group), 
    follow_up_time = min(Visit_time_interval_14_years), 
    mala_rate = mala_count / follow_up_time, 
    site = first(Site)
  )

rate_summary <- individual_mala_rates %>%
  group_by(group, Dose3_meds_interval) %>%
  summarise(
    mean_mala_rate = mean(mala_rate, na.rm = TRUE)
  )

ggplot(rate_summary, aes(x = Dose3_meds_interval, y = mean_mala_rate, color = factor(group))) +
  geom_line() +
  labs(
    x = "Time since 3rd dose administration (days)",
    y = "Average incidence rate of malaria treatment per person-year",
    title = "Average incidence rate of malaria treatment over time",
    color = "Vaccination status"
  ) +
  theme_minimal()

ggplot(rate_summary, aes(x = Dose3_meds_interval, y = mean_mala_rate, color = factor(group))) +
  geom_point() + 
  labs(
    x = "Time since 3rd dose administration (days)",
    y = "Average incidence rate of malaria treatment per person-year",
    title = "Average incidence rate of malaria treatment over time",
    color = "Vaccination status"
  ) +
  theme_minimal()

# by site: 
rate_summary_site <- individual_mala_rates %>%
  group_by(group, Dose3_meds_interval, site) %>%
  summarise(
    mean_mala_rate = mean(mala_rate, na.rm = TRUE)
  )

ggplot(rate_summary_site, aes(x = Dose3_meds_interval, y = mean_mala_rate, color = factor(group))) +
  geom_line() + 
  labs(
    x = "Time since 3rd dose administration (days)",
    y = "Average incidence rate of malaria treatment per person-year",
    title = "Average incidence rate of malaria treatment over time",
    color = "Vaccination status"
  ) +
  theme_minimal() +
  facet_wrap(~ site, nrow=6) 

ggplot(rate_summary_site, aes(x = Dose3_meds_interval, y = mean_mala_rate, color = factor(group))) +
  geom_point() + 
  labs(
    x = "Time since 3rd dose administration (days)",
    y = "Average incidence rate of malaria treatment per person-year",
    title = "Average incidence rate of malaria treatment over time",
    color = "Vaccination status"
  ) +
  theme_minimal() +
  facet_wrap(~ site) 

#####################################################################.
# 3. GLMM Regression models ----

# load packages
library(pacman)
p_load(
  tidyverse, 
  lubridate, 
  dplyr, 
  ggplot2, 
  lme4, 
  broom, 
  broom.helpers, 
  broom.mixed, 
  gtsummary, 
  forestplot, 
  sjPlot
)


############################################################.
## Objective 1: create age groups ----
########################################################.

# group by biological developmental stages
mala_df <- mala_df %>%
  mutate(age_grp = if_else(Agedaysat1stdose <= 365, "0-1",
                           if_else(Agedaysat1stdose <= 730, "1-2", "2-3")))

############################################################.
## Objective 2: Logistic regression ----
# whether individuals who received the malaria vaccine are less likely to receive malaria treatment 
########################################################.

############################################################.
# a: random intercept only
lm1 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (1 | Site),
  data = mala_df,
  family = binomial(link = "logit")
)
lm1 
summary(lm1) 

############################################################.
# b: random slopes (also with random intercept in build)
lm2 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (age_grp + Sex | Site),
  data = mala_df,
  family = binomial(link = "logit")
)

lm3 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (age_grp | Site),
  data = mala_df,
  family = binomial(link = "logit")
)

lm4 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (Sex | Site),
  data = mala_df,
  family = binomial(link = "logit")
)

############################################################.
# c: model comparison to determine random effect
AIC(lm1,lm2,lm3,lm4) # favour lm1
anova(lm1,lm2,lm3,lm4) # p value not significant, indicating 4 models are not significantly different 


############################################################.
# d: odds ratio, CI, p value 
t1 <- tbl_regression(lm1, exponentiate = TRUE)
t1
tab_model(lm1) 

############################################################.
# e: forest plot 

tidy_model1 <- tidy(lm1, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)

# Create a matrix for display
tabletext1 <- cbind(
  c("Variable", tidy_model1$term),
  c("OR (95% CI)", paste0(round(tidy_model1$estimate, 2), " (",
                          round(tidy_model1$conf.low, 2), "-",
                          round(tidy_model1$conf.high, 2), ")"))
)

# error bar: CI
forestplot(labeltext = tabletext1,
           mean = c(NA, tidy_model1$estimate),
           lower = c(NA, tidy_model1$conf.low),
           upper = c(NA, tidy_model1$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)


############################################################.
# f: model diagnostic, evaluate model fit
plot_model(lm1, type = "diag", show.values = TRUE)

fitted(lm1)
nrow(mala_df)
length(fitted(lm1))

mala_df$prediction <- fitted(lm1)
ggplot(mala_df, aes(x=prediction, y=received_mala_or_not_obs_window)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth() +
  coord_cartesian(xlim=c(0,1)) + 
  labs(
    x = "Predicted probability of receiving antibiotics",
    y = "Received antibiotics (0 = No, 1 = Yes)"
  )
# can see clean upward trend, with most data points in upper right or lower left quadrant 
# Most points with prediction close to 1 correspond to received_abx_or_not_obs_window == 1, 
# and close to 0 correspond to 0

# can also as 0.75, 0.25, maybe add line to the plot 
# Proportion of points with high prediction and received malaria treatment (upper right)
high_pred <- mala_df$prediction > 0.75
high_pred_and_abx <- high_pred & mala_df$received_mala_or_not_obs_window == 1
prop_high_correct <- sum(high_pred_and_abx) / sum(high_pred)
prop_high_correct

# Proportion of points with low prediction and did not receive malaria treatment (lower left)
low_pred <- mala_df$prediction < 0.25
low_pred_and_no_abx <- low_pred & mala_df$received_mala_or_not_obs_window == 0
prop_low_correct <- sum(low_pred_and_no_abx) / sum(low_pred)
prop_low_correct

# Number of such points
sum(high_pred)         # Total predicted > 0.75
sum(high_pred_and_abx) # Predicted > 0.75 and received malaria treatment

sum(low_pred)          # Total predicted < 0.25
sum(low_pred_and_no_abx) # Predicted < 0.25 and did not receive malaria treatment



############################################################.
## Objective 3: Poisson regression for rate ----
# whether individuals who received the malaria vaccine have lower incidence rate of malaria treatment
########################################################.

############################################################.
# a: random intercept only
pm1 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = mala_df,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

############################################################.
# b: random slopes (also with random intercept in build)
pm2 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = mala_df,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

pm3 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = mala_df,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

pm4 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = mala_df,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

############################################################.
# c: model comparison to determine random effect
AIC(pm1, pm2, pm3, pm4) # favour pm3
anova(pm1, pm2, pm3, pm4) # p value favouring pm3 


############################################################.
# d: check overdispersion 

# check variance and mean
dispersionstats <- mala_df %>%
  group_by(group) %>%
  summarise(
    means = mean(mala_count_obs_window),
    variances = var(mala_count_obs_window),
    ratio = variances/means)
dispersionstats # shows that variance bigger than mean 


# Calculate Pearson chisq statistics/dispersion statistic
overdispersion_check <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  ratio <- Pearson.chisq / rdf
  p_value <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  
  cat("Overdispersion statistic:", round(ratio, 2), "\n")
  cat("Chi-square test p-value:", signif(p_value, 4), "\n")
  
  if (ratio > 1.5) {
    cat("-> Likely overdispersion. Consider using a negative binomial model.\n")
  } else {
    cat("-> No major overdispersion detected.\n")
  }
}
overdispersion_check(pm3)

# Score Test
# null hypothesis: no overdispersion 
mu <-predict(pm3, type="response")
z <- ((mala_df$mala_count_obs_window - mu)^2 - mala_df$mala_count_obs_window)/ (mu * sqrt(2))
summary(zscore <- lm(z ~ 1)) # p<0.05: reject null hypothesis

# Lagrange Multiplier Test
# null hypothesis: no overdispersion 
obs <- nrow(mala_df) 
mmu <- mean(mu) 
nybar <- obs*mmu 
musq <- mu*mu
mu2 <- mean(musq)*obs
chival <- (mu2 - nybar)^2/(2*mu2) 
chival
pchisq(chival,1,lower.tail = FALSE) # p<0.05: reject null hypothesis


############################################################.
# e: negative binomial regression for rate (overdispersion is detected) 

pm5 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = mala_df,
  offset=log(Visit_time_interval_14)
)

pm6 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = mala_df,
  offset=log(Visit_time_interval_14)
)

pm7 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = mala_df,
  offset=log(Visit_time_interval_14)
)

pm8 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = mala_df,
  offset=log(Visit_time_interval_14)
)

AIC(pm3, pm5, pm6, pm7, pm8) # favour pm7
anova(pm3, pm5, pm6, pm7, pm8) # favour pm7

summary(pm3)
summary(pm5)

############################################################.
# f: odds ratio, CI, p value 
t2 <- tbl_regression(pm3, exponentiate = TRUE)
t2
tab_model(pm3) 


t3 <- tbl_regression(pm7, exponentiate = TRUE)
t3
tab_model(pm7) 


############################################################.
# e: forest plot 
# error bar: CI
# forest plot 
tidy_model2 <- tidy(pm7, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)

# Create a matrix for display
tabletext2 <- cbind(
  c("Variable", tidy_model2$term),
  c("IRR (95% CI)", paste0(round(tidy_model2$estimate, 2), " (",
                           round(tidy_model2$conf.low, 2), "-",
                           round(tidy_model2$conf.high, 2), ")"))
)

forestplot(labeltext = tabletext2,
           mean = c(NA, tidy_model2$estimate),
           lower = c(NA, tidy_model2$conf.low),
           upper = c(NA, tidy_model2$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)

############################################################.
# f: model diagnostic, evaluate model fit
plot_model(pm7, type = "diag", show.values = TRUE)

fitted(pm7)
nrow(mala_df)
length(fitted(pm7))

mala_df$prediction3 <- fitted(pm7)
ggplot(mala_df, aes(x=prediction3, y=mala_count)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth() + 
  labs(
    x = "Predicted number of antibiotic prescriptions",
    y = "Observed number of antibiotic prescriptions"
  ) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 10))
# can see clean upward trend, roughly linear relationship
# higher predicted values associated with higher observed values 
# smoothed line matched the line y=x (red dashed, perfect model calibration)

#####################################################################.
# 4. Survival analysis ----
# load packages
library(pacman)
p_load(
  tidyverse, 
  lubridate, 
  dplyr, 
  ggplot2, 
  broom, 
  broom.helpers, 
  broom.mixed, 
  survival, 
  survminer,
  forestplot, 
  coxme, 
  sjPlot
)

############################################################.
## Objective 1: time to first malaria treatment ----
# - event: received_mala_or_not_obs_window
# - time: from dose 3 administration to 1st malaria treatment
# - 1st 14 days no event 
############################################################.


#############################################################.
# 1: survival function 
km1 <- survfit(Surv(first_mala_time_obs_window, received_mala_or_not_obs_window) ~ group, data = mala_df)
km1
summary(km1)

#############################################################.
# 2: log rank test
# null hypothesis: survival estimates btw R21 and ctrl are not different 
lrt1 <- survdiff(Surv(first_mala_time_obs_window, received_mala_or_not_obs_window) ~ group, data = mala_df)
lrt1 

#############################################################.
# 3: Kaplan meier curve
# survival probability: P(event=0) = P(not having malaria treatment)
ggsurvplot(km1, 
           data = mala_df, 
           risk.table = TRUE, 
           linetype = c(1,4),
           pval = TRUE,
           pval.coord = c(100,0.3),
           conf.int=TRUE,
           xlab = "Time from 3rd dose administration (days)", 
           ylab = "Survival Probability", 
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M"), 
           surv.median.line = "hv", # median survivals
           break.time.by = 100, 
           risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE
)

#############################################################.
# 4: Cumulative event
# 1-S(t)
# What proportion of individuals have experienced the event by time
ggsurvplot(km1, data = mala_df, fun = "event",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(100,0.6),
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

#############################################################.
# 5: Cumulative hazards
ggsurvplot(km1, data = mala_df, fun = "cumhaz",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(300,0.5),
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

#############################################################.
# 6: Cox model 
# a. model set up 
# strata(site) account for site difference (different baseline hazard)
cox_model <- coxph(Surv(first_mala_time_obs_window, received_mala_or_not_obs_window) 
                   ~ group + Sex + age_grp + strata(Site), data = mala_df) 
summary(cox_model)
tidy(cox_model,
     conf.int = TRUE)
tidy(cox_model, 
     exponentiate = TRUE,
     conf.int = TRUE)

# b. hazard ratio 
t1 <- tbl_regression(cox_model, exp=TRUE)
t1
tab_model(cox_model)

# c. forest plot 
tidy_model1 <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

tabletext1 <- cbind(
  c("Variable", tidy_model1$term),
  c("Hazard Ratio (95% CI)", paste0(round(tidy_model1$estimate, 2), " (",
                                    round(tidy_model1$conf.low, 2), "-",
                                    round(tidy_model1$conf.high, 2), ")"))
)

forestplot(labeltext = tabletext1,
           mean = c(NA, tidy_model1$estimate),
           lower = c(NA, tidy_model1$conf.low),
           upper = c(NA, tidy_model1$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)


# d. model evaluation (model fit)
extractAIC(cox_model) # get aic
cox_model$loglik # log likelihood

# examine proportional hazards assumptions
# Therneau-Grambsch test and Schoenfeld residual analysis
ftest <- cox.zph(cox_model)
ggcoxzph(ftest)
plot(cox.zph(cox_model))

ggcoxdiagnostics(cox_model, type="deviance", ox.scale="linear.predictions")
# assumptions not violated

############################################################.
## Objective 2: Recurrent event survival analysis ----
############################################################.
abx_list <- abx_list %>%
  mutate(
    group = as.factor(group), 
    age_grp = if_else(Agedaysat1stdose <= 365, "0-1",
                      if_else(Agedaysat1stdose <= 730, "1-2", "2-3")))

#############################################################.
# 1: data prep

# keeping those not have malaria treatment at all, have an entry of censoring (event=0, time=final follow up time)
abx_list$Dose3_meds_interval[is.na(abx_list$Dose3_meds_interval)] <- 
  abx_list$Visit_time_interval[is.na(abx_list$Dose3_meds_interval)]
length(unique(abx_list$random_id))

# remove out of window prescription, but keeping rows if all prescriptions are out of window
# keeping as: event =0, time=end of observation window 
abx_list_clean <- abx_list %>%
  group_by(random_id) %>%
  mutate(in_window = Dose3_meds_interval >= 14 & Dose3_meds_interval <= Visit_time_interval) %>%
  group_modify(~ {
    if (any(.x$in_window)) {
      # If there are in-window prescriptions, keep only those
      return(filter(.x, in_window))
    } else {
      # All are out-of-window: set Dose3_14_meds_interval = Visit_time_interval_14
      .x$Dose3_meds_interval <- .x$Visit_time_interval
      return(slice(.x, 1))  # keep only one row
    }
  }) %>%
  ungroup() 
length(unique(abx_list_clean$random_id)) #4369


# To accommodate the following cox model (AG model): 
# - time in start time and stop time format
# - start and stop time can not be identical, 
# - stop time must be > start time

# clean the data -> at each time point, only one record of meds prescription 
# if at this time point, malaria treatment and non-malaria treatment is prescribed (at least 1 malaria treatment) ->  event=1
# if this date, no malaria treatment -> event=0

df <- abx_list_clean %>%
  group_by(random_id, Dose3_meds_interval) %>%
  # Prioritize event == 1
  arrange(desc(mala_or_not_obs_window), .by_group = TRUE) %>%
  # Keep only the first row in each group
  slice(1) %>%
  ungroup()

# create start time stop time format for AG model 
df <- df %>%
  arrange(random_id, Dose3_meds_interval) %>%
  group_by(random_id) %>%
  mutate(
    event_number = row_number(),
    start_time = lag(Dose3_meds_interval, default = 0),
    stop_time = Dose3_meds_interval
  )

# check if there are rows with stop time <= start time (can not be processed by AG model)
check <- df %>%
  filter(stop_time <= start_time)
length(check$random_id)



#############################################################.
# 2: survival function 
km2 <- survfit(Surv(time=start_time, time2=stop_time, event=mala_or_not_obs_window) ~ group, data = df)
km2
summary(km2)

#############################################################.
# 3: cumulative event
# 1-S(t)
# What proportion of individuals have experienced the event by time
ggsurvplot(km2, data = df, fun = "event",
           conf.int = TRUE,
           risk.table = TRUE,
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 


#############################################################.
# 3: cumulative hazards
ggsurvplot(km2, data = df, fun = "cumhaz",
           conf.int = TRUE,
           risk.table = TRUE,
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 


#############################################################.
# 4: Recurrent event cox model - Andersen-Gill model (AG model)

# a. model set up 
# frailty(random_id) account for repeated events within the same individual (heterogeneity) 
# strata(site) account for site difference (different baseline hazard)
cox_ag <- coxph(Surv(time=start_time, time2=stop_time, event=mala_or_not_obs_window) ~ 
                  group + age_grp + Sex + frailty(random_id) + strata(Site), data = df) 
summary(cox_ag)

tidy(cox_ag,
     conf.int = TRUE)
tidy(cox_ag, 
     exponentiate = TRUE,
     conf.int = TRUE)


# b. hazard ratio 
t2 <- tbl_regression(cox_ag, exp=TRUE)
t2
tab_model(cox_ag)


# c. forest plot 

# Tidy the model output
tidy_model2 <- tidy(cox_ag, exponentiate = TRUE, conf.int = TRUE)
tidy_model2 <- tidy_model2[!grepl("frailty", tidy_model2$term), ]

tabletext2 <- cbind(
  c("Variable", tidy_model2$term),
  c("Hazard Ratio (95% CI)", paste0(round(tidy_model2$estimate, 2), " (",
                                    round(tidy_model2$conf.low, 2), "-",
                                    round(tidy_model2$conf.high, 2), ")"))
)

forestplot(labeltext = tabletext2,
           mean = c(NA, tidy_model2$estimate),
           lower = c(NA, tidy_model2$conf.low),
           upper = c(NA, tidy_model2$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)



# d. model evaluation (model fit)
extractAIC(cox_ag) # get aic
cox_ag$loglik # log likelihood

# examine proportional hazards assumptions 
# Therneau-Grambsch test and Schoenfeld residual
ftest <- cox.zph(cox_ag)
ggcoxzph(ftest)
plot(cox.zph(cox_ag))
# assumptions not violated


####################################################################.
# 5. By transmission settings ----
####################################################################.

# separate seasonal and perennial 
seasonal <- mala_df %>%
  filter(Site %in% c("Bougouni", "Nanoro", "Siglé"))

perennial <- mala_df %>%
  filter(Site %in% c("Kilifi", "Bagamoyo", "Dandé"))

#####################################################################.
## 1. GLMM regression ----

############################################################.
### Objective 1: Logistic regression ----
# whether individuals who received the malaria vaccine are less likely to receive malaria treatment  
########################################################. 

############################################################.
# a: random intercept only
lm1 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (1 | Site),
  data = seasonal,
  family = binomial(link = "logit")
)

lm11 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (1 | Site),
  data = perennial,
  family = binomial(link = "logit")
)


############################################################.
# b: random slopes  
lm2 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (age_grp + Sex | Site),
  data = seasonal,
  family = binomial(link = "logit")
)
lm22 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (age_grp + Sex | Site),
  data = perennial,
  family = binomial(link = "logit")
)


lm3 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (age_grp | Site),
  data = seasonal,
  family = binomial(link = "logit")
)
lm33 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (age_grp | Site),
  data = perennial,
  family = binomial(link = "logit")
)


lm4 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (Sex | Site),
  data = seasonal,
  family = binomial(link = "logit")
)
lm44 <- glmer(
  received_mala_or_not_obs_window ~ group + age_grp + Sex + (Sex | Site),
  data = perennial,
  family = binomial(link = "logit")
)


############################################################.
# c: model comparison to determine random effect
AIC(lm1,lm2,lm3,lm4) # favour lm1
anova(lm1,lm2,lm3,lm4) 

AIC(lm11,lm22,lm33,lm44) # favour lm11
anova(lm11,lm22,lm33,lm44)

############################################################.
# d: odds ratio, CI, p value 
t1 <- tbl_regression(lm1, exponentiate = TRUE)
t1
tab_model(lm1) 

t11 <- tbl_regression(lm11, exponentiate = TRUE)
t11
tab_model(lm11) 

############################################################.
# e: forest plot 

tidy_model1 <- tidy(lm1, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)

# Create a matrix for display
tabletext1 <- cbind(
  c("Variable", tidy_model1$term),
  c("OR (95% CI)", paste0(round(tidy_model1$estimate, 2), " (",
                          round(tidy_model1$conf.low, 2), "-",
                          round(tidy_model1$conf.high, 2), ")"))
)

# error bar: CI
forestplot(labeltext = tabletext1,
           mean = c(NA, tidy_model1$estimate),
           lower = c(NA, tidy_model1$conf.low),
           upper = c(NA, tidy_model1$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)


tidy_model2 <- tidy(lm11, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)

# Create a matrix for display
tabletext2 <- cbind(
  c("Variable", tidy_model2$term),
  c("OR (95% CI)", paste0(round(tidy_model2$estimate, 2), " (",
                          round(tidy_model2$conf.low, 2), "-",
                          round(tidy_model2$conf.high, 2), ")"))
)

# error bar: CI
forestplot(labeltext = tabletext2,
           mean = c(NA, tidy_model2$estimate),
           lower = c(NA, tidy_model2$conf.low),
           upper = c(NA, tidy_model2$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)


############################################################.
# f: model diagnostic, evaluate model fit
plot_model(lm1, type = "diag", show.values = TRUE)

fitted(lm1)
nrow(seasonal)
length(fitted(lm1))

seasonal$prediction <- fitted(lm1)
ggplot(seasonal, aes(x=prediction, y=received_mala_or_not_obs_window)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth() +
  coord_cartesian(xlim=c(0,1))  + 
  labs(
    x = "Predicted probability of receiving antibiotics",
    y = "Received antibiotics (0 = No, 1 = Yes)"
  )
# can see clean upward trend, with most data points in upper right or lower left quadrant 
# Most points with prediction close to 1 correspond to received_abx_or_not_obs_window == 1, 
# and close to 0 correspond to 0

plot_model(lm11, type = "diag", show.values = TRUE)

fitted(lm11)
nrow(perennial)
length(fitted(lm11))

perennial$prediction <- fitted(lm11)
ggplot(perennial, aes(x=prediction, y=received_mala_or_not_obs_window)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth() +
  coord_cartesian(xlim=c(0,1)) + 
  labs(
    x = "Predicted probability of receiving antibiotics",
    y = "Received antibiotics (0 = No, 1 = Yes)"
  )
# can see clean upward trend, with most data points in upper right or lower left quadrant 
# Most points with prediction close to 1 correspond to received_abx_or_not_obs_window == 1, 
# and close to 0 correspond to 0

############################################################.
### Objective 2: Poisson regression with rate ----
# whether individuals who received the malaria vaccine have lower incidence rate of malaria treatment 
########################################################.

############################################################.
# a: random intercept only
pm1 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = seasonal,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)
pm11 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = perennial,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

############################################################.
# b: random slopes
pm2 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = seasonal,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)
pm22 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = perennial,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

pm3 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = seasonal,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)
pm33 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = perennial,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

pm4 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = seasonal,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)
pm44 <- glmer(
  mala_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = perennial,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

############################################################.
# c: model comparison to determine random effect
AIC(pm1, pm2, pm3, pm4) # favour pm3
anova(pm1, pm2, pm3, pm4) 

AIC(pm11, pm22, pm33, pm44) # favour pm11
anova(pm11, pm22, pm33, pm44) 


############################################################.
# d: check overdispersion 
# check variance and mean
dispersionstats <- seasonal %>%
  group_by(group) %>%
  summarise(
    means = mean(mala_count_obs_window),
    variances = var(mala_count_obs_window),
    ratio = variances/means)
dispersionstats # shows that variance bigger than mean 

dispersionstats <- perennial %>%
  group_by(group) %>%
  summarise(
    means = mean(mala_count_obs_window),
    variances = var(mala_count_obs_window),
    ratio = variances/means)
dispersionstats # shows that variance bigger than mean 

# Calculate Pearson chisq statistics/dispersion statistic
overdispersion_check <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  ratio <- Pearson.chisq / rdf
  p_value <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  
  cat("Overdispersion statistic:", round(ratio, 2), "\n")
  cat("Chi-square test p-value:", signif(p_value, 4), "\n")
  
  if (ratio > 1.5) {
    cat("-> Likely overdispersion. Consider using a negative binomial model.\n")
  } else {
    cat("-> No major overdispersion detected.\n")
  }
}
overdispersion_check(pm3)
overdispersion_check(pm11)


# Score Test
# null hypothesis: no overdispersion 
mu <-predict(pm3, type="response")
z <- ((seasonal$mala_count_obs_window - mu)^2 - seasonal$mala_count_obs_window)/ (mu * sqrt(2))
summary(zscore <- lm(z ~ 1)) # p<0.05: reject null hypothesis

mu <-predict(pm11, type="response")
z <- ((perennial$mala_count_obs_window - mu)^2 - perennial$mala_count_obs_window)/ (mu * sqrt(2))
summary(zscore <- lm(z ~ 1)) # p<0.05: reject null hypothesis


# Lagrange Multiplier Test
# null hypothesis: no overdispersion 
obs <- nrow(seasonal) 
mmu <- mean(mu) 
nybar <- obs*mmu 
musq <- mu*mu
mu2 <- mean(musq)*obs
chival <- (mu2 - nybar)^2/(2*mu2) 
chival
pchisq(chival,1,lower.tail = FALSE) # p<0.05: reject null hypothesis


obs <- nrow(perennial) 
mmu <- mean(mu) 
nybar <- obs*mmu 
musq <- mu*mu
mu2 <- mean(musq)*obs
chival <- (mu2 - nybar)^2/(2*mu2) 
chival
pchisq(chival,1,lower.tail = FALSE) # p<0.05: reject null hypothesis

############################################################.
# e: negative binomial model (overdispersion is detected) 
pm5 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = seasonal,
  offset=log(Visit_time_interval_14)
)
pm55 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = perennial,
  offset=log(Visit_time_interval_14)
)

pm6 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = seasonal,
  offset=log(Visit_time_interval_14)
)
pm66 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = perennial,
  offset=log(Visit_time_interval_14)
)

pm7 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = seasonal,
  offset=log(Visit_time_interval_14)
)
pm77 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = perennial,
  offset=log(Visit_time_interval_14)
)

pm8 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = seasonal,
  offset=log(Visit_time_interval_14)
)
pm88 <- glmer.nb(
  mala_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = perennial,
  offset=log(Visit_time_interval_14)
)

AIC(pm3, pm5, pm6, pm7, pm8) # favour pm7
anova(pm3, pm5, pm6, pm7, pm8) 

AIC(pm11, pm55, pm66, pm77, pm88) # favour pm77
anova(pm11, pm55, pm66, pm77, pm88) 

summary(pm7)
summary(pm77)

############################################################.
# f: odds ratio, CI, p value 
t2 <- tbl_regression(pm3, exponentiate = TRUE)
t2
tab_model(pm3) 
t3 <- tbl_regression(pm7, exponentiate = TRUE)
t3
tab_model(pm7) 


t22 <- tbl_regression(pm11, exponentiate = TRUE)
t22
tab_model(pm11) 
t33 <- tbl_regression(pm77, exponentiate = TRUE)
t33
tab_model(pm77)


############################################################.
# e: forest plot 
# error bar: CI
# forest plot 
tidy_model3 <- tidy(pm7, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)

# Create a matrix for display
tabletext3 <- cbind(
  c("Variable", tidy_model3$term),
  c("IRR (95% CI)", paste0(round(tidy_model3$estimate, 2), " (",
                           round(tidy_model3$conf.low, 2), "-",
                           round(tidy_model3$conf.high, 2), ")"))
)

forestplot(labeltext = tabletext3,
           mean = c(NA, tidy_model3$estimate),
           lower = c(NA, tidy_model3$conf.low),
           upper = c(NA, tidy_model3$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)



tidy_model4 <- tidy(pm77, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)

# Create a matrix for display
tabletext4 <- cbind(
  c("Variable", tidy_model4$term),
  c("IRR (95% CI)", paste0(round(tidy_model4$estimate, 2), " (",
                           round(tidy_model4$conf.low, 2), "-",
                           round(tidy_model4$conf.high, 2), ")"))
)

forestplot(labeltext = tabletext4,
           mean = c(NA, tidy_model4$estimate),
           lower = c(NA, tidy_model4$conf.low),
           upper = c(NA, tidy_model4$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)

############################################################.
# f: model diagnostic, evaluate model fit
plot_model(pm7, type = "diag", show.values = TRUE)

fitted(pm7)
nrow(seasonal)
length(fitted(pm7))

seasonal$prediction2 <- fitted(pm7)
ggplot(seasonal, aes(x=prediction2, y=mala_count)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth() + 
  labs(
    x = "Predicted number of antibiotic prescriptions",
    y = "Observed number of antibiotic prescriptions"
  ) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 10))
# can see clean upward trend, roughly linear relationship
# higher predicted values associated with higher observed values 
# smoothed line matched the line y=x (red dashed, perfect model calibration)

plot_model(pm77, type = "diag", show.values = TRUE)

fitted(pm77)
nrow(perennial)
length(fitted(pm77))

perennial$prediction3 <- fitted(pm77)
ggplot(perennial, aes(x=prediction3, y=mala_count)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth() + 
  labs(
    x = "Predicted number of antibiotic prescriptions",
    y = "Observed number of antibiotic prescriptions"
  ) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 10))
# can see clean upward trend, roughly linear relationship
# higher predicted values associated with higher observed values 
# smoothed line matched the line y=x (red dashed, perfect model calibration)


#####################################################################.
## 2. Survival analysis  ----

############################################################.
### Objective 1: time to first malaria treatment ----
# - event: received_mala_or_not_obs_window
# - time: from dose 3 administration to 1st malaria treatment 
# - 1st 14 days no event 
############################################################.


#############################################################.
# 1: survival function 
km1 <- survfit(Surv(first_mala_time_obs_window, received_mala_or_not_obs_window) ~ group, data = seasonal)
km11 <- survfit(Surv(first_mala_time_obs_window, received_mala_or_not_obs_window) ~ group, data = perennial)

#############################################################.
# 2: log rank test
# null hypothesis: survival estimates btw R21 and ctrl are not different 
lrt1 <- survdiff(Surv(first_mala_time_obs_window, received_mala_or_not_obs_window) ~ group, data = seasonal)
lrt1 # p values>0.05

lrt11 <- survdiff(Surv(first_mala_time_obs_window, received_mala_or_not_obs_window) ~ group, data = perennial)
lrt11 # p values>0.05

#############################################################.
# 3: Kaplan meier curve
# survival probability: P(event=0) = P(not having malaria treatment)
ggsurvplot(km1, 
           data = seasonal, 
           risk.table = TRUE, 
           linetype = c(1,4),
           pval = TRUE,
           pval.coord = c(300,0.5),
           conf.int=TRUE,
           xlab = "Time from 3rd dose administration (days)", 
           ylab = "Survival Probability", 
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M"), 
           surv.median.line = "hv", # median survivals, 
           break.time.by = 100, 
           risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE
)

ggsurvplot(km11, 
           data = perennial, 
           risk.table = TRUE, 
           linetype = c(1,4),
           pval = TRUE,
           pval.coord = c(300,0.3),
           conf.int=TRUE,
           xlab = "Time from 3rd dose administration (days)", 
           ylab = "Survival Probability", 
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M"), 
           surv.median.line = "hv", # median survivals, 
           break.time.by = 100, 
           risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE
)

#############################################################.
# 4: Cumulative event
# 1-S(t)
# What proportion of individuals have experienced the event by time
ggsurvplot(km1, data = seasonal, fun = "event",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(300,0.5),
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

ggsurvplot(km11, data = perennial, fun = "event",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(750,0.35),
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 
#############################################################.
# 5: Cumulative hazards
ggsurvplot(km1, data = seasonal, fun = "cumhaz",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(300,0.5),
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

ggsurvplot(km11, data = perennial, fun = "cumhaz",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(750,0.5),
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 


#############################################################.
# 6: Cox model 
# a. model set up 
# strata(site) account for site difference (different baseline hazard)
cox_model <- coxph(Surv(first_mala_time_obs_window, received_mala_or_not_obs_window) 
                   ~ group + Sex + age_grp + strata(Site), data = seasonal) 
summary(cox_model)
tidy(cox_model,
     conf.int = TRUE)
tidy(cox_model, 
     exponentiate = TRUE,
     conf.int = TRUE)

cox_model2 <- coxph(Surv(first_mala_time_obs_window, received_mala_or_not_obs_window) 
                    ~ group + Sex + age_grp + strata(Site), data = perennial) 
summary(cox_model2)
tidy(cox_model2,
     conf.int = TRUE)
tidy(cox_model2, 
     exponentiate = TRUE,
     conf.int = TRUE)

# b. hazard ratio 
t1 <- tbl_regression(cox_model, exp=TRUE)
t1
tab_model(cox_model)

t11 <- tbl_regression(cox_model2, exp=TRUE)
t11
tab_model(cox_model2)

# c. forest plot 
tidy_model5 <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

tabletext5 <- cbind(
  c("Variable", tidy_model5$term),
  c("Hazard Ratio (95% CI)", paste0(round(tidy_model5$estimate, 2), " (",
                                    round(tidy_model5$conf.low, 2), "-",
                                    round(tidy_model5$conf.high, 2), ")"))
)

forestplot(labeltext = tabletext5,
           mean = c(NA, tidy_model5$estimate),
           lower = c(NA, tidy_model5$conf.low),
           upper = c(NA, tidy_model5$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)


tidy_model6 <- tidy(cox_model2, exponentiate = TRUE, conf.int = TRUE)

tabletext6 <- cbind(
  c("Variable", tidy_model6$term),
  c("Hazard Ratio (95% CI)", paste0(round(tidy_model6$estimate, 2), " (",
                                    round(tidy_model6$conf.low, 2), "-",
                                    round(tidy_model6$conf.high, 2), ")"))
)

forestplot(labeltext = tabletext6,
           mean = c(NA, tidy_model6$estimate),
           lower = c(NA, tidy_model6$conf.low),
           upper = c(NA, tidy_model6$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)


# d. model evaluation (model fit)
extractAIC(cox_model) # get aic
cox_model$loglik # log likelihood

# examine proportional hazards assumptions
# Therneau-Grambsch test and Schoenfeld residual analysis
ftest <- cox.zph(cox_model)
ggcoxzph(ftest)
plot(cox.zph(cox_model))

ggcoxdiagnostics(cox_model, type="deviance", ox.scale="linear.predictions")
# assumptions not violated


extractAIC(cox_model2) # get aic
cox_model2$loglik # log likelihood

# examine proportional hazards assumptions
# Therneau-Grambsch test and Schoenfeld residual analysis
ftest <- cox.zph(cox_model2)
ggcoxzph(ftest)
plot(cox.zph(cox_model2))

ggcoxdiagnostics(cox_model2, type="deviance", ox.scale="linear.predictions")
# assumptions not violated

############################################################.
### Objective 2: recurrent event survival analysis ----
############################################################.
# 1: data prep

# from above data already being prepared for all site analysis 
# separate seasonal and perennial 
seasonal2 <- df %>%
  filter(Site %in% c("Bougouni", "Nanoro", "Siglé"))

perennial2 <- df %>%
  filter(Site %in% c("Kilifi", "Bagamoyo", "Dandé"))

#############################################################.
# 2: survival function 
km2 <- survfit(Surv(time=start_time, time2=stop_time, event=mala_or_not_obs_window) ~ group, data = seasonal2)

km22 <- survfit(Surv(time=start_time, time2=stop_time, event=mala_or_not_obs_window) ~ group, data = perennial2)

#############################################################.
# 3: cumulative event
# 1-S(t)
# What proportion of individuals have experienced the event by time
ggsurvplot(km2, data = seasonal2, fun = "event",
           conf.int = TRUE,
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

ggsurvplot(km22, data = perennial2, fun = "event",
           conf.int = TRUE,
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

#############################################################.
# 3: cumulative hazards
ggsurvplot(km2, data = seasonal2, fun = "cumhaz",
           conf.int = TRUE,
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

ggsurvplot(km22, data = perennial2, fun = "cumhaz",
           conf.int = TRUE,
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 


#############################################################.
# 4: Recurrent event cox model - Andersen-Gill model (AG model)

# a. model set up 
# frailty(random_id) account for repeated events within the same individual (heterogeneity) 
# strata(site) account for site difference (different baseline hazard)
cox_ag <- coxph(Surv(time=start_time, time2=stop_time, event=mala_or_not_obs_window) ~ 
                  group + age_grp + Sex + frailty(random_id) + strata(Site), data = seasonal2) 
summary(cox_ag)

tidy(cox_ag,
     conf.int = TRUE)
tidy(cox_ag, 
     exponentiate = TRUE,
     conf.int = TRUE)


cox_ag2 <- coxph(Surv(time=start_time, time2=stop_time, event=mala_or_not_obs_window) ~ 
                   group + age_grp + Sex + frailty(random_id) + strata(Site), data = perennial2) 

# b. hazard ratio 
t2 <- tbl_regression(cox_ag, exp=TRUE)
t2
tab_model(cox_ag)

t22 <- tbl_regression(cox_ag2, exp=TRUE)
t22
tab_model(cox_ag2)

# c. forest plot 

# Tidy the model output
tidy_model7 <- tidy(cox_ag, exponentiate = TRUE, conf.int = TRUE)
tidy_model7 <- tidy_model7[!grepl("frailty", tidy_model7$term), ]

tabletext7 <- cbind(
  c("Variable", tidy_model7$term),
  c("Hazard Ratio (95% CI)", paste0(round(tidy_model7$estimate, 2), " (",
                                    round(tidy_model7$conf.low, 2), "-",
                                    round(tidy_model7$conf.high, 2), ")"))
)

forestplot(labeltext = tabletext7,
           mean = c(NA, tidy_model7$estimate),
           lower = c(NA, tidy_model7$conf.low),
           upper = c(NA, tidy_model7$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)



tidy_model8 <- tidy(cox_ag2, exponentiate = TRUE, conf.int = TRUE)
tidy_model8 <- tidy_model8[!grepl("frailty", tidy_model8$term), ]

tabletext8 <- cbind(
  c("Variable", tidy_model8$term),
  c("Hazard Ratio (95% CI)", paste0(round(tidy_model8$estimate, 2), " (",
                                    round(tidy_model8$conf.low, 2), "-",
                                    round(tidy_model8$conf.high, 2), ")"))
)

forestplot(labeltext = tabletext8,
           mean = c(NA, tidy_model8$estimate),
           lower = c(NA, tidy_model8$conf.low),
           upper = c(NA, tidy_model8$conf.high),
           zero = 1,
           boxsize = 0.2,
           lineheight = unit(0.8, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue"),
           xlog = TRUE)



# d. model evaluation (model fit)

# examine proportional hazards assumptions 
# Therneau-Grambsch test and Schoenfeld residual
ftest <- cox.zph(cox_ag)
ggcoxzph(ftest)
plot(cox.zph(cox_ag))
# assumptions not violated

ftest <- cox.zph(cox_ag2)
ggcoxzph(ftest)
plot(cox.zph(cox_ag2))
# assumptions not violated










