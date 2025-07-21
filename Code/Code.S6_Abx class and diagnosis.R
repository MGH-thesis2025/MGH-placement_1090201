# Please show and unhidedocument outline to have a cleaner view of each section within this script
# - abx = antibiotics 
# - obs wind = observation window 

rm(list = ls())

# load packages
library(pacman)
p_load(
  readxl,
  writexl,
  tidyverse, 
  dplyr, 
  ggplot2, 
  scales
)

csv_file1 <- file.path(
  "Data",
  "Data.S19_unblinded_final_abx_list.csv"
)
data1 <- read_csv(csv_file1)
abx_list <- as.data.frame(data1)
length(unique(abx_list$random_id)) #4369

str(abx_list)

abx_list <- abx_list %>%
  mutate(
    group = as.factor(group) # vaccination group
  )

# extract abx prescription 
new_abx_list <- abx_list %>%
  filter(Abx_or_not_obs_window==1)

length(unique(new_abx_list$random_id)) #3500


# convert observation window to years
new_abx_list <- new_abx_list %>%
  mutate(
    Visit_time_interval_years = Visit_time_interval / 365.25,
    Visit_time_interval_14_years = Visit_time_interval_14 / 365.25, 
    setting = ifelse(Site %in% c("Bougouni", "Nanoro", "Siglé"), "Seasonal", "Perennial")
  )

############################################################.
# Objective 1: Examine abx class ----
########################################################.

# abx class and corresponding number (this is done when classifying abx)
abx_class_names <- c(
  "1" = "Beta-lactams (penicillin)",
  "2" = "Beta-lactams (cephalosporins)",
  "3" = "Beta-lactams (carbapenems)", 
  "4" = "Polyketides (macrolides)", 
  "5" = "Polyketides (tetracyclines)", 
  "6" = "Fusidanes", 
  "7" = "Sulfonamide",
  "8" = "Aminoglycosides",
  "9" = "Fluoroquinolones", 
  "10" = "Nitroimidazoles", 
  "11" = "Mupirocin", 
  "12" = "Glycopeptides", 
  "13" = "Polypeptides", 
  "14" = "Phenicols", 
  "15" = "Trimethoprim", 
  "16" = "Ansamycins (rifamycin)", 
  "7 (malaria)" = "Antimalarial drug with sulfonamide", 
  "15 and 7" = "Trimethoprim & sulfonamide", # drug with 2 class of antibiotics 
  "13 and 8" = "Polypeptides & aminoglycosides", 
  "13 and 5" = "Polypeptides & polyketides (tetracyclines)"
)

new_abx_list <- new_abx_list %>%
  mutate(Abx_class = abx_class_names[as.character(Abx_class)])

## 1. Average incidence rate ----
### all sites: ----
individual_abx_rates1 <- new_abx_list %>%
  group_by(random_id, Abx_class) %>%
  summarise(
    abx_count = sum(Abx_or_not_obs_window),
    group = first(group), 
    follow_up_time = min(Visit_time_interval_14_years), 
    abx_rate = abx_count / follow_up_time, 
    site = first(Site), 
    setting = first(setting)
  )

length(unique(individual_abx_rates1$random_id)) #3500

rate_summary1 <- individual_abx_rates1 %>%
  group_by(group, Abx_class) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

# Ensure all combinations of group and Abx_class are present, even mean_abx_rate is NA or 0
# allow ggplot2 to reserve space if control or R21 group does not have prescription for particular abx class
rate_summary1_complete <- rate_summary1 %>%
  ungroup() %>%
  complete(group, Abx_class, fill = list(mean_abx_rate = 0))

ggplot(rate_summary1_complete, aes(x = Abx_class, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year\nby antibiotic class and vaccination status",
    x = "Antibiotic class",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### by site: ----
rate_summary1_site <- individual_abx_rates1 %>%
  group_by(group, Abx_class, site) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

rate_summary1_site_complete <- rate_summary1_site %>%
  ungroup() %>%
  complete(site, group, Abx_class, fill = list(mean_abx_rate = 0))

ggplot(rate_summary1_site_complete, aes(x = Abx_class, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year\nby antibiotic class and vaccination status",
    x = "Antibiotic class",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ site, nrow=6) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

ggplot(rate_summary1_site_complete, aes(x = site, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year by antibiotic class and vaccination status",
    x = "Site",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Abx_class, nrow=3) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

### by setting (seasonal/perennial): ----
rate_summary1_setting <- individual_abx_rates1 %>%
  group_by(group, Abx_class, setting) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

rate_summary1_setting_complete <- rate_summary1_setting %>%
  ungroup() %>%
  complete(setting, group, Abx_class, fill = list(mean_abx_rate = 0))

ggplot(rate_summary1_setting_complete, aes(x = Abx_class, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year\nby antibiotic class and vaccination status",
    x = "Antibiotic class",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ setting, nrow=2) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))


## 2. percentage ----
### all sites: ----
percentage_df1 <- new_abx_list %>%
  group_by(group, Abx_class) %>%
  summarise(
    total_abx_count = sum(Abx_or_not_obs_window),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  mutate(
    percent = total_abx_count / sum(total_abx_count) * 100
  )

percentage_df1_complete <- percentage_df1 %>%
  ungroup() %>%
  complete(group, Abx_class, fill = list(percent = 0))

ggplot(percentage_df1_complete, aes(x = Abx_class, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions by antibiotic class and vaccination status",
    x = "Antibiotic class",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

### by site: ----
percentage_df1_site <- new_abx_list %>%
  group_by(group, Abx_class, Site) %>%
  summarise(
    total_abx_count = sum(Abx_or_not_obs_window),
    .groups = "drop"
  ) %>%
  group_by(Site, group) %>%
  mutate(
    percent = total_abx_count / sum(total_abx_count) * 100
  )

percentage_df1_site_complete <- percentage_df1_site %>%
  ungroup() %>%
  complete(Site, group, Abx_class, fill = list(percent = 0))

ggplot(percentage_df1_site_complete, aes(x = Abx_class, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby antibiotic class and vaccination status",
    x = "Antibiotic class",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Site, nrow=6) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

ggplot(percentage_df1_site_complete, aes(x = Site, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions by antibiotic class and vaccination status",
    x = "Site",
    y = "Proportion of total prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Abx_class, nrow=3) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

### by setting (seasonal/perennial): ----
percentage_df1_setting <- new_abx_list %>%
  group_by(group, Abx_class, setting) %>%
  summarise(
    total_abx_count = sum(Abx_or_not_obs_window),
    .groups = "drop"
  ) %>%
  group_by(setting, group) %>%
  mutate(
    percent = total_abx_count / sum(total_abx_count) * 100
  )

percentage_df1_setting_complete <- percentage_df1_setting %>%
  ungroup() %>%
  complete(setting, group, Abx_class, fill = list(percent = 0))


ggplot(percentage_df1_setting_complete, aes(x = Abx_class, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby antibiotic class and vaccination status",
    x = "Antibiotic class",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ setting, nrow=2) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))



############################################################.
# Objective 2: Examine illness that required abx prescription ----
# - reasons/diagnosis indications for prescription
########################################################.

diagnosis <- unique(new_abx_list$Indication_compiled) # unique diagnostic reasons
diagnosis <- as.data.frame(diagnosis)
write_xlsx(diagnosis, "diagnosis.xlsx")

# manually updated and classified different illness type into infection class
xls_file2 <- file.path(
  "Data",
  "Data.S21_diagnostic class.xlsx"
)
data2 <- read_excel(xls_file2, sheet = 1)
diagnostic_class <- as.data.frame(data2)

unique(diagnostic_class$class) # granular classification
unique(diagnostic_class$collapsed_class)

diagnostic_class <- diagnostic_class %>%
  rename(Indication_compiled = illness)

new_abx_list <- left_join(
  new_abx_list,
  diagnostic_class %>% 
    select(Indication_compiled, class, collapsed_class),
  by = "Indication_compiled", 
  relationship = "many-to-many"
)

# check for NA
is.na(new_abx_list$class)
na <- new_abx_list[is.na(new_abx_list$class), ] # are all respiratory infection
unique(na$Indication_compiled)
sum(is.na(new_abx_list$class))

new_abx_list$class[is.na(new_abx_list$class)] <- "Respiratory"
sum(is.na(new_abx_list$class))


is.na(new_abx_list$collapsed_class)
na <- new_abx_list[is.na(new_abx_list$collapsed_class), ] # are all respiratory infection
unique(na$Indication_compiled)
sum(is.na(new_abx_list$collapsed_class))

new_abx_list$collapsed_class[is.na(new_abx_list$collapsed_class)] <- "Respiratory"
sum(is.na(new_abx_list$collapsed_class))

## 1. Average incidence rate ----
### all sites: ----
individual_abx_rates2 <- new_abx_list %>%
  group_by(random_id, class) %>%
  summarise(
    abx_count = sum(Abx_or_not_obs_window),
    group = first(group), 
    follow_up_time = min(Visit_time_interval_14_years), 
    abx_rate = abx_count / follow_up_time, 
    site = first(Site), 
    setting = first(setting)
  )

rate_summary2 <- individual_abx_rates2 %>%
  group_by(group, class) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

rate_summary2_complete <- rate_summary2 %>%
  ungroup() %>%
  complete(group, class, fill = list(mean_abx_rate = 0))


# move class "other" to the end 
rate_summary2_complete$class <- factor(rate_summary2_complete$class)
rate_summary2_complete$class <- fct_relevel(rate_summary2_complete$class, "Other", after = Inf)

ggplot(rate_summary2_complete, aes(x = class, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Incidence rateof antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### collapsed to less granular classifications: ----
individual_abx_rates3 <- new_abx_list %>%
  group_by(random_id, collapsed_class) %>%
  summarise(
    abx_count = sum(Abx_or_not_obs_window),
    group = first(group), 
    follow_up_time = min(Visit_time_interval_14_years), 
    abx_rate = abx_count / follow_up_time, 
    site = first(Site), 
    setting = first(setting)
  )

rate_summary3 <- individual_abx_rates3 %>%
  group_by(group, collapsed_class) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

# move class "other" to the end 
rate_summary3$collapsed_class <- factor(rate_summary3$collapsed_class)
rate_summary3$collapsed_class <- fct_relevel(rate_summary3$collapsed_class, "Other", after = Inf)

ggplot(rate_summary3, aes(x = collapsed_class, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 50, hjust = 1))

### by site: ----
rate_summary2_site <- individual_abx_rates2 %>%
  group_by(group, class, site) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

rate_summary2_site_complete <- rate_summary2_site %>%
  ungroup() %>%
  complete(site, group, class, fill = list(mean_abx_rate = 0))

# move class "other" to the end 
rate_summary2_site_complete$class <- factor(rate_summary2_site_complete$class)
rate_summary2_site_complete$class <- fct_relevel(rate_summary2_site_complete$class, "Other", after = Inf)

ggplot(rate_summary2_site_complete, aes(x = class, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ site, nrow=6) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

ggplot(rate_summary2_site_complete, aes(x = site, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year by diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ class) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

#### collapsed to less granular classifications: ----
rate_summary3_site <- individual_abx_rates3 %>%
  group_by(group, collapsed_class, site) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

# move class "other" to the end 
rate_summary3_site$collapsed_class <- factor(rate_summary3_site$collapsed_class)
rate_summary3_site$collapsed_class <- fct_relevel(rate_summary3_site$collapsed_class, "Other", after = Inf)

ggplot(rate_summary3_site, aes(x = collapsed_class, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ site, nrow=6) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))


ggplot(rate_summary3_site, aes(x = site, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year by diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ collapsed_class) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

### by setting: ----
rate_summary2_setting <- individual_abx_rates2 %>%
  group_by(group, class, setting) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

rate_summary2_setting_complete <- rate_summary2_setting %>%
  ungroup() %>%
  complete(setting, group, class, fill = list(mean_abx_rate = 0))

# move class "other" to the end 
rate_summary2_setting_complete$class <- factor(rate_summary2_setting_complete$class)
rate_summary2_setting_complete$class <- fct_relevel(rate_summary2_setting_complete$class, "Other", after = Inf)

ggplot(rate_summary2_setting_complete, aes(x = class, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ setting, nrow=2) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

#### collapsed to less granular classifications: ----
rate_summary3_setting <- individual_abx_rates3 %>%
  group_by(group, collapsed_class, setting) %>%
  summarise(
    mean_abx_rate = mean(abx_rate, na.rm = TRUE)
  )

# move class "other" to the end 
rate_summary3_setting$collapsed_class <- factor(rate_summary3_setting$collapsed_class)
rate_summary3_setting$collapsed_class <- fct_relevel(rate_summary3_setting$collapsed_class, "Other", after = Inf)

ggplot(rate_summary3_setting, aes(x = collapsed_class, y = mean_abx_rate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence rate of antibiotic prescriptions per person-year\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Incidence rate of antibiotic prescriptions per person-year",
    fill = "Vaccination group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ setting, nrow=6) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

## 2. percentage ----
### all sites: ----
percentage_df2 <- new_abx_list %>%
  group_by(group, class) %>%
  summarise(
    total_abx_count = sum(Abx_or_not_obs_window),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  mutate(
    percent = total_abx_count / sum(total_abx_count) * 100
  )

percentage_df2_complete <- percentage_df2 %>%
  ungroup() %>%
  complete(group, class, fill = list(percent = 0))

# move class "other" to the end 
percentage_df2_complete$class <- factor(percentage_df2_complete$class)
percentage_df2_complete$class <- fct_relevel(percentage_df2_complete$class, "Other", after = Inf)


ggplot(percentage_df2_complete, aes(x = class, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#### collapsed to less granular classifications: ----
percentage_df3 <- new_abx_list %>%
  group_by(group, collapsed_class) %>%
  summarise(
    total_abx_count = sum(Abx_or_not_obs_window),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  mutate(
    percent = total_abx_count / sum(total_abx_count) * 100
  )

# move class "other" to the end 
percentage_df3$collapsed_class <- factor(percentage_df3$collapsed_class)
percentage_df3$collapsed_class <- fct_relevel(percentage_df3$collapsed_class, "Other", after = Inf)


ggplot(percentage_df3, aes(x = collapsed_class, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 50, hjust = 1)) 


### by site: ----
percentage_df2_site <- new_abx_list %>%
  group_by(group, class, Site) %>%
  summarise(
    total_abx_count = sum(Abx_or_not_obs_window),
    .groups = "drop"
  ) %>%
  group_by(Site, group) %>%
  mutate(
    percent = total_abx_count / sum(total_abx_count) * 100
  )

percentage_df2_site_complete <- percentage_df2_site %>%
  ungroup() %>%
  complete(Site, group, class, fill = list(percent = 0))


# move class "other" to the end 
percentage_df2_site_complete$class <- factor(percentage_df2_site_complete$class)
percentage_df2_site_complete$class <- fct_relevel(percentage_df2_site_complete$class, "Other", after = Inf)


ggplot(percentage_df2_site_complete, aes(x = class, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Site, nrow=6) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))


ggplot(percentage_df2_site_complete, aes(x = Site, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ class) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

#### collapsed to less granular classifications: ----
percentage_df3_site <- new_abx_list %>%
  group_by(group, collapsed_class, Site) %>%
  summarise(
    total_abx_count = sum(Abx_or_not_obs_window),
    .groups = "drop"
  ) %>%
  group_by(Site, group) %>%
  mutate(
    percent = total_abx_count / sum(total_abx_count) * 100
  )


# move class "other" to the end 
percentage_df3_site$collapsed_class <- factor(percentage_df3_site$collapsed_class)
percentage_df3_site$collapsed_class <- fct_relevel(percentage_df3_site$collapsed_class, "Other", after = Inf)

ggplot(percentage_df3_site, aes(x = collapsed_class, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Site, nrow=6) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))


ggplot(percentage_df3_site, aes(x = Site, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ collapsed_class) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

### by setting: ----
percentage_df2_setting <- new_abx_list %>%
  group_by(group, class, setting) %>%
  summarise(
    total_abx_count = sum(Abx_or_not_obs_window),
    .groups = "drop"
  ) %>%
  group_by(setting, group) %>%
  mutate(
    percent = total_abx_count / sum(total_abx_count) * 100
  )

percentage_df2_setting_complete <- percentage_df2_setting %>%
  ungroup() %>%
  complete(setting, group, class, fill = list(percent = 0))


# move class "other" to the end 
percentage_df2_setting_complete$class <- factor(percentage_df2_setting_complete$class)
percentage_df2_setting_complete$class <- fct_relevel(percentage_df2_setting_complete$class, "Other", after = Inf)


ggplot(percentage_df2_setting_complete, aes(x = class, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ setting, nrow=6) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))

#### collapsed to less granular classifications: ----
percentage_df3_setting <- new_abx_list %>%
  group_by(group, collapsed_class, setting) %>%
  summarise(
    total_abx_count = sum(Abx_or_not_obs_window),
    .groups = "drop"
  ) %>%
  group_by(setting, group) %>%
  mutate(
    percent = total_abx_count / sum(total_abx_count) * 100
  )


# move class "other" to the end 
percentage_df3_setting$collapsed_class <- factor(percentage_df3_setting$collapsed_class)
percentage_df3_setting$collapsed_class <- fct_relevel(percentage_df3_setting$collapsed_class, "Other", after = Inf)

ggplot(percentage_df3_setting, aes(x = collapsed_class, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of antibiotic prescriptions\nby diagnostic categories and vaccination status",
    x = "Diagnostic categories",
    y = "Proportion of total antibiotic prescriptions",
    fill = "Vaccination group"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ setting, nrow=6) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 60))



