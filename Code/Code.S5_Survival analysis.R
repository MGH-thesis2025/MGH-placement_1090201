# Please show and unhide document outline to have a cleaner view of each section within this script
# - abx = antibiotics 
# - obs wind = observation window 

rm(list = ls())

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
  sjPlot, 
  gtsummary
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
  rename(Vaccination_group = group)%>%
  mutate(
    Vaccination_group = as.factor(Vaccination_group)
  )

main_df <- main_df %>%
  mutate(Age_group = if_else(Agedaysat1stdose <= 365, "0-1",
                           if_else(Agedaysat1stdose <= 730, "1-2", "2-3")))


#################################.
# Survival analysis #
#################################.

############################################################.
# Objective 1: time to first antibiotic prescription ----
# - event: received_abx_or_not_obs_window
# - time: from dose 3 administration to 1st antibiotic prescription 
# - first 14 days no event 
############################################################.


#############################################################.
## 1: Survival function ----
km1 <- survfit(Surv(first_abx_time_obs_window, received_abx_or_not_obs_window) ~ Vaccination_group, data = main_df)
km1
summary(km1)

#############################################################.
## 2: Log rank test ----
# null hypothesis: survival estimates btw R21 and ctrl are not different 
lrt1 <- survdiff(Surv(first_abx_time_obs_window, received_abx_or_not_obs_window) ~ Vaccination_group, data = main_df)
lrt1 # p values

#############################################################.
## 3: Kaplan meier curve ----
# survival probability
# P(event=0) = P(not having abx prescription)
ggsurvplot(km1, 
           data = main_df, 
           risk.table = TRUE, 
           linetype = c(1,4),
           pval = TRUE,
           pval.coord = c(300,0.5),
           conf.int=TRUE,
           xlab = "Time from third dose administration (days)", 
           ylab = "Survival Probability", 
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M"), 
           surv.median.line = "hv", # median survivals 
           break.time.by = 100, 
           risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE
)

#############################################################.
## 4: Cumulative event ----
# 1-S(t)
# What proportion of individuals have experienced the event by time
ggsurvplot(km1, data = main_df, fun = "event",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(300,0.5),
           linetype = c(1,4),
           xlab = "Time from third dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

#############################################################.
## 5: Cumulative hazards ----
# H(t)
# accumulated risk over time
# What is the total hazard accumulated by time 
ggsurvplot(km1, data = main_df, fun = "cumhaz",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(300,0.5),
           linetype = c(1,4),
           xlab = "Time from third dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

#############################################################.
## 6: Cox model ----
### a. model set up ----
# adjust for covariates: sex and age 
# strata(site) account for site difference (different baseline hazard)
cox_model <- coxph(Surv(first_abx_time_obs_window, received_abx_or_not_obs_window) 
                    ~ Vaccination_group + Age_group + Sex + strata(Site), data = main_df) 
summary(cox_model)
tidy(cox_model,
     conf.int = TRUE)
tidy(cox_model, 
     exponentiate = TRUE,
     conf.int = TRUE)

### b. hazard ratio ----
t1 <- tbl_regression(cox_model, exp=TRUE)
t1
tab_model(cox_model)

### c. forest plot ----
tidy_model1 <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

term_labels <- c(
  "SexMale" = "Sex: Male",
  "Age_group1-2" = "Age group: 1-2 years old",
  "Age_group2-3" = "Age group: 2-3 years old",
  "Vaccination_groupR21/Matrix-M" = "Vaccination group: R21/Matrix-M"
)
tidy_model1$term <- term_labels[tidy_model1$term]


tabletext1 <- cbind(
  c("Variable", tidy_model1$term),
  c("Adjusted Hazard Ratio (95% CI)", paste0(round(tidy_model1$estimate, 2), " (",
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
           xlog = TRUE,
           xticks = c(0.5, 0.75, 1, 1.25, 1.5, 2))

# Add right arrow and text
grid.lines(x = unit(c(0.765, 0.9), "npc"), y = unit(0.22, "npc"),
           arrow = arrow(type = "open", length = unit(0.1, "inches")),
           gp = gpar(col = "black", lwd = 1))
grid.text("Higher hazard", x = unit(0.88, "npc"), y = unit(0.16, "npc"),
          just = "right", gp = gpar(fontsize = 10))

# Add left arrow and text
grid.lines(x = unit(c(0.75, 0.615), "npc"), y = unit(0.22, "npc"),
           arrow = arrow(type = "open", length = unit(0.1, "inches")),
           gp = gpar(col = "black", lwd = 1))
grid.text("Lower hazard", x = unit(0.635, "npc"), y = unit(0.16, "npc"),
          just = "left", gp = gpar(fontsize = 10))


### d. model evaluation (model fit) ----
extractAIC(cox_model) # get aic
cox_model$loglik # log likelihood

# examine proportional hazards assumptions
# Therneau-Grambsch test and Schoenfeld residual analysis
# Null hypothesis: proportional hazards assumptions is true, the slope of hazard ratio against time is 0
ftest <- cox.zph(cox_model)
ftest # p value > 0.05, accept null hypothesis, assumption not violated
ggcoxzph(ftest) 
plot(cox.zph(cox_model))
ggcoxdiagnostics(cox_model, type="deviance", ox.scale="linear.predictions")
# assumptions not violated


############################################################.
# Objective 2: Recurrent event survival analysis ----
############################################################.
csv_file2 <- file.path(
  "Data",
  "Data.S19_unblinded_final_abx_list.csv"
)
data2 <- read_csv(csv_file2)
abx_list <- as.data.frame(data2)
length(unique(abx_list$random_id)) #4369

str(abx_list)

abx_list <- abx_list %>%
  rename(Vaccination_group = group)%>%
  mutate(
    Vaccination_group = as.factor(Vaccination_group), 
    Age_group = if_else(Agedaysat1stdose <= 365, "0-1",
                             if_else(Agedaysat1stdose <= 730, "1-2", "2-3")))

#############################################################.
## 1: Data prep ----
# prep for Cox andersen gill model (AG model) set up 

# For individuals not having abx prescription at all during observation window: 
# have an entry of censoring (event=0, time=final follow up time)
abx_list$Dose3_meds_interval[is.na(abx_list$Dose3_meds_interval)] <- 
  abx_list$Visit_time_interval[is.na(abx_list$Dose3_meds_interval)]
length(unique(abx_list$random_id))

# Remove out of window prescription, but keeping rows if all prescriptions are out of window
# keeping as: event =0, time=end of observation window 
abx_list_clean <- abx_list %>%
  group_by(random_id) %>%
  mutate(in_window = Dose3_meds_interval >= 14 & Dose3_meds_interval <= Visit_time_interval) %>%
  group_modify(~ {
    if (any(.x$in_window)) {
      # If there are in-window prescriptions, keep only those
      return(filter(.x, in_window))
    } else {
      # All are out-of-window: set Dose3_meds_interval = Visit_time_interval
      .x$Dose3_meds_interval <- .x$Visit_time_interval
      return(slice(.x, 1))  # keep only one row
    }
  }) %>%
  ungroup() 
length(unique(abx_list_clean$random_id))


# To accommodate the following cox model (AG model): 
# - time in start time and stop time format
# - start and stop time can not be identical, 
# - stop time must be > start time

# clean the data -> at each time point, only one record of meds prescription 
# if at this time point, abx and non-abx is prescribed (at least 1 abx prescribed) ->  event=1
# if this date, no abx prescribed -> event=0

df <- abx_list_clean %>%
  group_by(random_id, Dose3_meds_interval) %>%
  # Prioritize event == 1
  arrange(desc(Abx_or_not_obs_window), .by_group = TRUE) %>%
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
length(check$random_id) #0

#############################################################.
## 2: Survival function ----
km2 <- survfit(Surv(time=start_time, time2=stop_time, event=Abx_or_not_obs_window) ~ Vaccination_group, data = df)
km2
summary(km2)

#############################################################.
## 3: Cumulative event ----
# 1-S(t)
# What proportion of individuals have experienced the event by time
ggsurvplot(km2, data = df, fun = "event",
           conf.int = TRUE,
           linetype = c(1,4),
           xlab = "Time from third dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 


#############################################################.
## 4: Cumulative hazards ----
# H(t)
# accumulated risk over time
# What is the total hazard accumulated by time 
ggsurvplot(km2, data = df, fun = "cumhaz",
           conf.int = TRUE,
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 


#############################################################.
## 5: Recurrent event cox model - Andersen-Gill model (AG model) ----

### a. model set up ----
# adjust for covariates: sex and age 
# frailty(random_id) account for repeated events within the same individual (heterogeneity) 
# strata(site) account for site difference (different baseline hazard)

cox_ag <- coxph(Surv(time=start_time, time2=stop_time, event=Abx_or_not_obs_window) ~ 
                  Vaccination_group + Age_group + Sex + frailty(random_id) + strata(Site), data = df) 
summary(cox_ag)

tidy(cox_ag,
     conf.int = TRUE)
tidy(cox_ag, 
     exponentiate = TRUE,
     conf.int = TRUE)

### b. hazard ratio ----
t2 <- tbl_regression(cox_ag, exp=TRUE)
t2
tab_model(cox_ag)


### c. forest plot ----

# Tidy the model output
tidy_model2 <- tidy(cox_ag, exponentiate = TRUE, conf.int = TRUE)
tidy_model2 <- tidy_model2[!grepl("frailty", tidy_model2$term), ]

term_labels <- c(
  "SexMale" = "Sex: Male",
  "Age_group1-2" = "Age group: 1-2 years old",
  "Age_group2-3" = "Age group: 2-3 years old",
  "Vaccination_groupR21/Matrix-M" = "Vaccination group: R21/Matrix-M"
)
tidy_model2$term <- term_labels[tidy_model2$term]


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



### d. model evaluation (model fit) ----
extractAIC(cox_ag) # get aic
cox_ag$loglik # log likelihood

# examine proportional hazards assumptions 
# Therneau-Grambsch test and Schoenfeld residual
# Null hypothesis: proportional hazards assumptions is true, the slope of hazard ratio against time is 0
ftest <- cox.zph(cox_ag)
ftest # p value > 0.05, accept null hypothesis 
ggcoxzph(ftest)
plot(cox.zph(cox_ag))
# assumptions not violated







