# Please show and unhide document outline to have a cleaner view of each section within this script
# - abx = antibiotics 
# - obs wind = observation window 

rm(list = ls())

library(pacman)
p_load(
  tidyverse, 
  dplyr, 
  ggplot2, 
  scales,
  ggpubr
)

csv_file1 <- file.path(
  "Data",
  "Data.S20_main_df.csv"
)
data1 <- read_csv(csv_file1)
main_df <- as.data.frame(data1)
length(unique(main_df$random_id)) #4369

str(main_df)

main_df <- main_df %>%
  mutate(
    group = as.factor(group)
  )

############################################################.
# Objective 1: Create summary tables ----
# - Baseline characteristics 
# - Abx prescription 
########################################################.

############################################################.
## 1. Baseline characteristics ----
# - Age, sex, observation window
# Summarise by site and vaccination group 

### all sites ----
table1 <- main_df %>%
  group_by(group) %>%
  summarise(
    `Total number of participants` = length(random_id),
    
    `Number of males` = sum(Sex=="Male"),
    `Number of females` = sum(Sex=="Female"),
    
    `% of males` = mean(Sex=="Male"),
    `% of females` = mean(Sex=="Female"),
    
    `Average age` = mean(Agedaysat1stdose), 
    
    `Average duration of observation window` = mean(Visit_time_interval_14)
  )
View(table1)

### group by sites ----
table2 <- main_df %>%
  group_by(Site, group) %>%
  summarise(
    `Total number of participants` = length(random_id),
    
    `Number of males` = sum(Sex=="Male"),
    `Number of females` = sum(Sex=="Female"),
    
    `% of males` = mean(Sex=="Male"),
    `% of females` = mean(Sex=="Female"),
    
    `Average age` = mean(Agedaysat1stdose), 
    
    `Average duration of observation window` = mean(Visit_time_interval_14)
  )
View(table2)

### group by setting (seasonal/perennial) ----
main_df <- main_df %>%
  mutate(
    setting = ifelse(Site %in% c("Bougouni", "Nanoro", "Siglé"), "Seasonal", "Perennial")
  )

table3 <- main_df %>%
  group_by(setting, group) %>%
  summarise(
    `Total number of participants` = length(random_id),
    
    `Number of males` = sum(Sex=="Male"),
    `Number of females` = sum(Sex=="Female"),
    
    `% of males` = mean(Sex=="Male"),
    `% of females` = mean(Sex=="Female"),
    
    `Average age` = mean(Agedaysat1stdose), 
    
    `Average duration of observation window` = mean(Visit_time_interval_14)
  )
View(table3)

############################################################.
## 2. Abx prescription ----
# Summarise by site and vaccination group 
# useful information (during obs wind) are extracted into Table.3

### all sites ----
table4 <- main_df %>%
  group_by(group) %>%
  summarise(
    `Total number of individuals` = length(random_id),
    
    `Number of people receive at least 1 abx prescription` = sum(received_abx_or_not == 1),
    `Number of people receive at least 1 abx prescription during obs wind` = sum(received_abx_or_not_obs_window == 1),
    
    `% receive at least 1 abx prescription` = mean(received_abx_or_not == 1),
    `% receive at least 1 abx prescription during obs wind` = mean(received_abx_or_not_obs_window == 1),
    
    `Number of people receive more than 1 abx prescription` = sum(More_than_1_abx == 1),
    `Number of people receive more than 1 abx prescription during obs wind` = sum(More_than_1_abx_obs_window == 1),
    
    `% receive more than 1 abx prescription` = mean(More_than_1_abx == 1),
    `% receive more than 1 abx prescription during obs wind` = mean(More_than_1_abx_obs_window == 1),
    
    `Total number of abx prescription` = sum(Abx_count), 
    `Total number of abx prescription during obs wind` = sum(Abx_count_obs_window),
    
    `Total duration of observation window in years` = sum(Visit_time_interval_14_years),
    
    `Average number of abx prescription per person` = mean(Abx_count), 
    `Average number of abx prescription per person during obs wind` = mean(Abx_count_obs_window), 
    
    `Incidence rate of antibiotic prescriptions (per person-year) during obs wind` = mean(Abx_rate_obs_window), 
    
    `Average time interval until abx prescription` = mean(avg_time_interval_days, na.rm=TRUE), 
    `Average time interval until abx prescription during obs wind` = mean(avg_time_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval until abx prescription_14` = mean(avg_time_interval_days_14, na.rm=TRUE), 
    `Average time interval until abx prescription during obs wind_14` = mean(avg_time_interval_days_obs_window_14, na.rm=TRUE), 
    
    `Average time to first abx prescription` = mean(first_abx_time), 
    `Average time to first abx prescription during obs wind` = mean(first_abx_time_obs_window), 
    `Average time to first abx prescription_14` = mean(first_abx_time_14), 
    `Average time to first abx prescription during obs wind_14` = mean(first_abx_time_obs_window_14), 
    
    `True average time to first abx prescription` = mean(true_first_abx_time, na.rm=TRUE), 
    `True average time to first abx prescription during obs wind` = mean(true_first_abx_time_obs_window, na.rm=TRUE), 
    `True average time to first abx prescription_14` = mean(true_first_abx_time_14, na.rm=TRUE), 
    `True average time to first abx prescription during obs wind_14` = mean(true_first_abx_time_obs_window_14, na.rm=TRUE), 
    
    `Average time interval between each abx prescription` = mean(avg_abx_interval_days, na.rm=TRUE), 
    `Average time interval between each abx prescription during obs wind` = mean(avg_abx_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval between each abx prescription_14` = mean(avg_abx_interval_days_14, na.rm=TRUE), 
    `Average time interval between each abx prescription during obs wind_14` = mean(avg_abx_interval_days_obs_window_14, na.rm=TRUE), 
  )

View(table4)

### group by sites ----
table5 <- main_df %>%
  group_by(Site, group) %>%
  summarise(
    `Total number of individuals` = length(random_id),
    
    `Number of people receive at least 1 abx prescription` = sum(received_abx_or_not == 1),
    `Number of people receive at least 1 abx prescription during obs wind` = sum(received_abx_or_not_obs_window == 1),
    
    `% receive at least 1 abx prescription` = mean(received_abx_or_not == 1),
    `% receive at least 1 abx prescription during obs wind` = mean(received_abx_or_not_obs_window == 1),
    
    `Number of people receive more than 1 abx prescription` = sum(More_than_1_abx == 1),
    `Number of people receive more than 1 abx prescription during obs wind` = sum(More_than_1_abx_obs_window == 1),
    
    `% receive more than 1 abx prescription` = mean(More_than_1_abx == 1),
    `% receive more than 1 abx prescription during obs wind` = mean(More_than_1_abx_obs_window == 1),
    
    `Total number of abx prescription` = sum(Abx_count), 
    `Total number of abx prescription during obs wind` = sum(Abx_count_obs_window),
    
    `Total duration of observation window in years` = sum(Visit_time_interval_14_years),
    
    `Average number of abx prescription per person` = mean(Abx_count), 
    `Average number of abx prescription per person during obs wind` = mean(Abx_count_obs_window), 
    
    `Incidence rate of antibiotic prescriptions (per person-year) during obs wind` = mean(Abx_rate_obs_window), 
    
    `Average time interval until abx prescription` = mean(avg_time_interval_days, na.rm=TRUE), 
    `Average time interval until abx prescription during obs wind` = mean(avg_time_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval until abx prescription_14` = mean(avg_time_interval_days_14, na.rm=TRUE), 
    `Average time interval until abx prescription during obs wind_14` = mean(avg_time_interval_days_obs_window_14, na.rm=TRUE), 
    
    `Average time to first abx prescription` = mean(first_abx_time), 
    `Average time to first abx prescription during obs wind` = mean(first_abx_time_obs_window), 
    `Average time to first abx prescription_14` = mean(first_abx_time_14), 
    `Average time to first abx prescription during obs wind_14` = mean(first_abx_time_obs_window_14), 
    
    `True average time to first abx prescription` = mean(true_first_abx_time, na.rm=TRUE), 
    `True average time to first abx prescription during obs wind` = mean(true_first_abx_time_obs_window, na.rm=TRUE), 
    `True average time to first abx prescription_14` = mean(true_first_abx_time_14, na.rm=TRUE), 
    `True average time to first abx prescription during obs wind_14` = mean(true_first_abx_time_obs_window_14, na.rm=TRUE), 
    
    `Average time interval between each abx prescription` = mean(avg_abx_interval_days, na.rm=TRUE), 
    `Average time interval between each abx prescription during obs wind` = mean(avg_abx_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval between each abx prescription_14` = mean(avg_abx_interval_days_14, na.rm=TRUE), 
    `Average time interval between each abx prescription during obs wind_14` = mean(avg_abx_interval_days_obs_window_14, na.rm=TRUE), 
  )

View(table5)

### group by setting (seasonal/perennial) ----

table6 <- main_df %>%
  group_by(setting, group) %>%
  summarise(
    `Total number of individuals` = length(random_id),
    
    `Number of people receive at least 1 abx prescription` = sum(received_abx_or_not == 1),
    `Number of people receive at least 1 abx prescription during obs wind` = sum(received_abx_or_not_obs_window == 1),
    
    `% receive at least 1 abx prescription` = mean(received_abx_or_not == 1),
    `% receive at least 1 abx prescription during obs wind` = mean(received_abx_or_not_obs_window == 1),
    
    `Number of people receive more than 1 abx prescription` = sum(More_than_1_abx == 1),
    `Number of people receive more than 1 abx prescription during obs wind` = sum(More_than_1_abx_obs_window == 1),
    
    `% receive more than 1 abx prescription` = mean(More_than_1_abx == 1),
    `% receive more than 1 abx prescription during obs wind` = mean(More_than_1_abx_obs_window == 1),
    
    `Total number of abx prescription` = sum(Abx_count), 
    `Total number of abx prescription during obs wind` = sum(Abx_count_obs_window),
    
    `Total duration of observation window in years` = sum(Visit_time_interval_14_years),
    
    `Average number of abx prescription per person` = mean(Abx_count), 
    `Average number of abx prescription per person during obs wind` = mean(Abx_count_obs_window), 
    
    `Incidence rate of antibiotic prescriptions (per person-year) during obs wind` = mean(Abx_rate_obs_window), 
    
    `Average time interval until abx prescription` = mean(avg_time_interval_days, na.rm=TRUE), 
    `Average time interval until abx prescription during obs wind` = mean(avg_time_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval until abx prescription_14` = mean(avg_time_interval_days_14, na.rm=TRUE), 
    `Average time interval until abx prescription during obs wind_14` = mean(avg_time_interval_days_obs_window_14, na.rm=TRUE), 
    
    `Average time to first abx prescription` = mean(first_abx_time), 
    `Average time to first abx prescription during obs wind` = mean(first_abx_time_obs_window), 
    `Average time to first abx prescription_14` = mean(first_abx_time_14), 
    `Average time to first abx prescription during obs wind_14` = mean(first_abx_time_obs_window_14), 
    
    `True average time to first abx prescription` = mean(true_first_abx_time, na.rm=TRUE), 
    `True average time to first abx prescription during obs wind` = mean(true_first_abx_time_obs_window, na.rm=TRUE), 
    `True average time to first abx prescription_14` = mean(true_first_abx_time_14, na.rm=TRUE), 
    `True average time to first abx prescription during obs wind_14` = mean(true_first_abx_time_obs_window_14, na.rm=TRUE), 
    
    `Average time interval between each abx prescription` = mean(avg_abx_interval_days, na.rm=TRUE), 
    `Average time interval between each abx prescription during obs wind` = mean(avg_abx_interval_days_obs_window, na.rm=TRUE), 
    `Average time interval between each abx prescription_14` = mean(avg_abx_interval_days_14, na.rm=TRUE), 
    `Average time interval between each abx prescription during obs wind_14` = mean(avg_abx_interval_days_obs_window_14, na.rm=TRUE), 
  )
View(table6)

############################################################.
# Objective 2: Create plots ----
# - some plots are not included in dissertation, is for exploratory purpose
# - paving the way for next step modelling 
############################################################.

############################################################.
## 1. Age ----
### all sites ----
# check if is normal distribution 
hist(main_df$Agedaysat1stdose) # not normal distribution
hist(main_df$Agedaysat1stdose[main_df$group == "Control"]) # not normal distribution
hist(main_df$Agedaysat1stdose[main_df$group == "R21/Matrix-M"]) # not normal distribution

# Perform Wilcoxon test 
test_result <- wilcox.test(Agedaysat1stdose ~ group, data = main_df)
p_val <- signif(test_result$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(main_df$Agedaysat1stdose) + 1,
  p = p_val,
  label = paste0("p = ", p_val)
)

ggboxplot(main_df, x = "group", y = "Agedaysat1stdose", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination status", 
       y = "Age at 1st dose administration (days)",
       fill = "Vaccination status") +
  theme_classic()
# p value=0.63, no significant difference in age btw control and R21 group

### group by sites ----
pvalue_dff <- main_df %>%
  group_by(Site) %>%
  summarise(
    p = wilcox.test(Agedaysat1stdose ~ group)$p.value,
    y.position = max(Agedaysat1stdose, na.rm = TRUE) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Control",
    group2 = "R21/Matrix-M",
    label = paste0("p = ", signif(p, 2))
  )

ggboxplot(main_df, x = "group", y = "Agedaysat1stdose", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_dff, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) + 
  labs(x = "Vaccination status", 
       y = "Age at 1st dose administration (days)",
       fill = "Vaccination status")

############################################################.
## 2. Observation window ----
### all sites ----
# check if is normal distribution 
hist(main_df$Visit_time_interval_14) # not normal distribution
hist(main_df$Visit_time_interval_14[main_df$group == "Control"]) # not normal distribution
hist(main_df$Visit_time_interval_14[main_df$group == "R21/Matrix-M"]) # not normal distribution

# Perform Wilcoxon test 
test_result1 <- wilcox.test(Visit_time_interval_14 ~ group, data = main_df)
p_val1 <- signif(test_result1$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df1 <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(main_df$Visit_time_interval_14) + 1,
  p = p_val1,
  label = paste0("p = ", p_val1)
)

ggboxplot(main_df, x = "group", y = "Visit_time_interval_14", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df1, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination status", 
       y = "Duration of observation window (days)",
       fill = "Vaccination status") +
  theme_classic()
# p value=0.2, no significant difference in observation window length btw control and R21 group

### group by sites ----
pvalue_df2 <- main_df %>%
  group_by(Site) %>%
  summarise(
    p = wilcox.test(Visit_time_interval_14 ~ group)$p.value,
    y.position = max(Visit_time_interval_14, na.rm = TRUE) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Control",
    group2 = "R21/Matrix-M",
    label = paste0("p = ", signif(p, 2))
  )

ggboxplot(main_df, x = "group", y = "Visit_time_interval_14", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df2, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) + 
  labs(x = "Vaccination status", 
       y = "Duration of observation window (days)",
       fill = "Vaccination status")


############################################################.
## 3. Histogram of percentage of individuals received at least 1 / more than 1 abx prescription ----
### all sites ----
hist_data <- main_df %>%
  group_by(group) %>%
  summarize(
    at_least_1 = mean(received_abx_or_not_obs_window == 1)*100,
    more_than_1 = mean(More_than_1_abx_obs_window == 1)*100
  ) %>%
  pivot_longer(cols = c(at_least_1, more_than_1), 
               names_to = "abx_category", 
               values_to = "percent")

ggplot(hist_data, aes(x = factor(group, labels = c("Control", "R21/Matrix-M")), 
                      y = percent, 
                      fill = abx_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Vaccination status", 
       y = "Percentage of individuals", 
       fill = "Antibiotic prescription") +
  scale_fill_manual(
    values = c("at_least_1" = "darkblue",  
               "more_than_1" = "lightblue"),
    labels = c("At least 1 antibiotic prescription", "More than 1 antibiotic prescriptions")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal()

### group by sites ----
hist_data_site <- main_df %>%
  group_by(group, Site) %>%
  summarize(
    at_least_1 = mean(received_abx_or_not_obs_window == 1)*100,
    more_than_1 = mean(More_than_1_abx_obs_window == 1)*100
  ) %>%
  pivot_longer(cols = c(at_least_1, more_than_1), 
               names_to = "abx_category", 
               values_to = "percent")

ggplot(hist_data_site, aes(x = factor(group, labels = c("Control", "R21/Matrix-M")), 
                           y = percent, 
                           fill = abx_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Site) +
  labs(x = "Vaccination status", 
       y = "Percentage of individuals", 
       fill = "Antibiotic prescription") +
  scale_fill_manual(
    values = c("at_least_1" = "darkblue",  
               "more_than_1" = "lightblue"),
    labels = c("At least 1 antibiotic prescription", "More than 1 antibiotic prescriptions")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal()


############################################################.
## 4. Incidence rate of antibiotic prescriptions (per person-year) ---- 
### all sites ----
# check if is normal distribution 
hist(main_df$Abx_rate_obs_window) # not normal distribution
hist(main_df$Abx_rate_obs_window[main_df$group == "Control"]) # not normal distribution
hist(main_df$Abx_rate_obs_window[main_df$group == "R21/Matrix-M"]) # not normal distribution

# Perform Wilcoxon test 
test_result2 <- wilcox.test(Abx_rate_obs_window ~ group, data = main_df)
p_val2 <- signif(test_result2$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df3 <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(main_df$Abx_rate_obs_window) + 1,
  p = p_val2,
  label = paste0("p = ", p_val2)
)

ggboxplot(main_df, x = "group", y = "Abx_rate_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df3, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination status", 
       y = "Incidence rate of antibiotic prescriptions (per person-year)",
       fill = "Vaccination status") +
  theme_classic()

### group by sites ----
pvalue_df4 <- main_df %>%
  group_by(Site) %>%
  summarise(
    p = wilcox.test(Abx_rate_obs_window ~ group)$p.value,
    y.position = max(Abx_rate_obs_window, na.rm = TRUE) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Control",
    group2 = "R21/Matrix-M",
    label = paste0("p = ", signif(p, 2))
  )

ggboxplot(main_df, x = "group", y = "Abx_rate_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df4, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) + 
  labs(x = "Vaccination status", 
       y = "Incidence rate of antibiotic prescriptions (per person-year)",
       fill = "Vaccination status")



############################################################.
## 5: True time to first abx prescription ----
# (if an individual not having abx at all, time is not replaced as end of follow-up)
# (time from dose 3 administration)

### all sites ----
# check if is normal distribution 
hist(main_df$true_first_abx_time_obs_window) # not normal distribution
hist(main_df$true_first_abx_time_obs_window[main_df$group == "Control"]) # not normal distribution
hist(main_df$true_first_abx_time_obs_window[main_df$group == "R21/Matrix-M"]) # not normal distribution

# Perform Wilcoxon test 
test_result3 <- wilcox.test(true_first_abx_time_obs_window ~ group, data = main_df)
p_val3 <- signif(test_result3$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df5 <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(main_df$true_first_abx_time_obs_window, na.rm = TRUE) + 1,
  p = p_val3,
  label = paste0("p = ", p_val3)
)

ggboxplot(main_df, x = "group", y = "true_first_abx_time_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df5, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination status", 
       y = "Time to 1st antibiotic prescription (days)",
       fill = "Vaccination status") +
  theme_classic()

### group by sites ----
pvalue_df6 <- main_df %>%
  group_by(Site) %>%
  summarise(
    p = wilcox.test(true_first_abx_time_obs_window ~ group)$p.value,
    y.position = max(true_first_abx_time_obs_window, na.rm = TRUE) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Control",
    group2 = "R21/Matrix-M",
    label = paste0("p = ", signif(p, 2))
  )

ggboxplot(main_df, x = "group", y = "true_first_abx_time_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df6, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) + 
  labs(x = "Vaccination status", 
       y = "Time to 1st antibiotic prescription (days)", 
       fill = "Vaccination status") 

############################################################.
## 6: Average time interval until abx prescription (from dose 3 administration) ----

### all sites ----
# check if is normal distribution 
hist(main_df$avg_time_interval_days_obs_window) # is normal distribution
hist(main_df$avg_time_interval_days_obs_window[main_df$group == "Control"]) # is normal distribution
hist(main_df$avg_time_interval_days_obs_window[main_df$group == "R21/Matrix-M"]) # is normal distribution

# Perform t test 
test_result4 <- t.test(avg_time_interval_days_obs_window ~ group, data = main_df)
p_val4 <- signif(test_result4$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df7 <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(main_df$avg_time_interval_days_obs_window, na.rm = TRUE) + 1,
  p = p_val4,
  label = paste0("p = ", p_val4)
)

ggboxplot(main_df, x = "group", y = "avg_time_interval_days_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df7, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination Status", 
       y = "Average time interval until each antibiotic prescription (days)",
       fill = "Vaccination status") +
  theme_classic()

### group by sites ---- 
pvalue_df8 <- main_df %>%
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

ggboxplot(main_df, x = "group", y = "avg_time_interval_days_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df8, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) +
  labs(x = "Vaccination status", 
       y = "Average time interval until each antibiotic prescription (days)",
       fill = "Vaccination status") 


############################################################.
## 7: Average time interval between each abx prescription ----
### all sites ----
# check if is normal distribution 
hist(main_df$avg_abx_interval_days_obs_window) # not normal distribution
hist(main_df$avg_abx_interval_days_obs_window[main_df$group == "Control"]) # not normal distribution
hist(main_df$avg_abx_interval_days_obs_window[main_df$group == "R21/Matrix-M"]) # not normal distribution

# Perform wilcoxon test 
test_result5 <- wilcox.test(avg_abx_interval_days_obs_window ~ group, data = main_df)
p_val5 <- signif(test_result5$p.value, 2)

# Create p value df for annotation on boxplot 
pvalue_df9 <- data.frame(
  group1 = "Control",
  group2 = "R21/Matrix-M",
  y.position = max(main_df$avg_abx_interval_days_obs_window, na.rm = TRUE) + 1,
  p = p_val5,
  label = paste0("p = ", p_val5)
)

ggboxplot(main_df, x = "group", y = "avg_abx_interval_days_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df9, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  labs(x = "Vaccination status", 
       y = "Average time interval between each antibiotic prescription (days)", 
       fill = "Vaccination status") +
  theme_classic()

### group by sites ----
pvalue_df10 <- main_df %>%
  group_by(Site) %>%
  summarise(
    p = wilcox.test(avg_abx_interval_days_obs_window ~ group)$p.value,
    y.position = max(avg_abx_interval_days_obs_window, na.rm = TRUE) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Control",
    group2 = "R21/Matrix-M",
    label = paste0("p = ", signif(p, 2))
  )

ggboxplot(main_df, x = "group", y = "avg_abx_interval_days_obs_window", color = "black", fill = "group",
          palette = c("Control" = "salmon", "R21/Matrix-M" = "skyblue")) +
  stat_pvalue_manual(pvalue_df10, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position",
                     tip.length = 0.01) +
  facet_wrap(~Site) + 
  labs(x = "Vaccination status", 
       y = "Average time interval between each antibiotic prescription (days)",
       fill = "Vaccination status")


############################################################.
## 8: Number of abx prescription over time ----

csv_file2 <- file.path(
  "Data",
  "Data.S19_unblinded_final_abx_list.csv"
)
data2 <- read_csv(csv_file2)
abx_list <- as.data.frame(data2)
length(unique(abx_list$random_id)) #4369

str(abx_list)

abx_list <- abx_list %>%
  mutate(
    group = as.factor(group)
  )

abx_list <- abx_list %>%
  mutate(
    Visit_time_interval_years = Visit_time_interval / 365.25,
    Visit_time_interval_14_years = Visit_time_interval_14 / 365.25
  )

individual_abx_rates <- abx_list %>%
  filter(Abx_or_not_obs_window == 1) %>%
  group_by(random_id, Dose3_meds_interval) %>%
  summarise(
    abx_count = sum(Abx_or_not_obs_window),
    group = first(group), 
    follow_up_time = min(Visit_time_interval_14_years), 
    abx_rate = abx_count / follow_up_time, 
    site = first(Site)
  )

### all sites ----
rate_summary <- individual_abx_rates %>%
  group_by(group, Dose3_meds_interval) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

ggplot(rate_summary, aes(x = Dose3_meds_interval, y = mean_abx_rate, color = factor(group))) +
  geom_line() +
  labs(
    x = "Time since 3rd dose administration (days)",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    title = "Incidence rate of antibiotic prescriptions over time",
    color = "Vaccination status"
  ) +
  theme_minimal()

ggplot(rate_summary, aes(x = Dose3_meds_interval, y = mean_abx_rate, color = factor(group))) +
  geom_point() + 
  labs(
    x = "Time since 3rd dose administration (days)",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    title = "Incidence rate of antibiotic prescriptions over time",
    color = "Vaccination status"
  ) +
  theme_minimal()

### group by sites ----
rate_summary_site <- individual_abx_rates %>%
  group_by(group, Dose3_meds_interval, site) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

ggplot(rate_summary_site, aes(x = Dose3_meds_interval, y = mean_abx_rate, color = factor(group))) +
  geom_line() + 
  labs(
    x = "Time since 3rd dose administration (days)",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    title = "Incidence rate of antibiotic prescriptions over time",
    color = "Vaccination status"
  ) +
  theme_minimal() +
  facet_wrap(~ site, nrow=6) 

ggplot(rate_summary_site, aes(x = Dose3_meds_interval, y = mean_abx_rate, color = factor(group))) +
  geom_line() + 
  labs(
    x = "Time since 3rd dose administration (days)",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    title = "Incidence rate of antibiotic prescriptions over time",
    color = "Vaccination status"
  ) +
  theme_minimal() +
  facet_wrap(~ site) 

ggplot(rate_summary_site, aes(x = Dose3_meds_interval, y = mean_abx_rate, color = factor(group))) +
  geom_point() + 
  labs(
    x = "Time since 3rd dose administration (days)",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    title = "Incidence rate of antibiotic prescriptions over time",
    color = "Vaccination status"
  ) +
  theme_minimal() +
  facet_wrap(~ site) 






