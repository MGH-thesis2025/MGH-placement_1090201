############# Sensitivity analysis: systemic abx ##############.
# the same set of analysis as main primary analysis is conducted

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

abx_list$Systemic_abx_or_not_obs_window <- abx_list$Abx_or_not_obs_window

# keep only systemic abx (remove topical, rectal, and other abx with unknown route)
abx_list <- abx_list %>%
  mutate(
    Systemic_abx_or_not_obs_window = if_else(
      is.na(Route_of_administration), 
      0,
      ifelse(
        Route_of_administration %in% c("Topical (includes skin, eye, ear)", 
                                       "Other", 
                                       "Rectal", 
                                       "Unknown", "NA"),
        0,
        Systemic_abx_or_not_obs_window
      )))
length(unique(abx_list$random_id)) #4369

############################################################.
## Objective 1: building a main summary list of systemic abx use per individual (within observation window or in total) ----
# - Received at least 1 systemic abx or not (Prescribed abx or not)
# - Number of systemic abx prescription per person & 
# systemic abx prescription incidence rate per person-year 
# - Received >1 systemic abx prescription or not
# - Average time interval btw 3rd dose administration and systemic abx start date 
# - Average time interval btw vaccine effective time (14 days after 3rd dose administration date) and systemic abx start date 
# - Average time interval between each systemic abx prescription
# - Time to first systemic abx prescription
############################################################.

############################################################.
# 1. Summarise by random_id — age, sex, vaccination group, observation window 

main_df <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    Visit_time_interval = min(Visit_time_interval), # from 3rd dose administration to end of follow up 
    Visit_time_interval_14 = min(Visit_time_interval_14), # from 14 days after 3rd dose administration to end of follow up 
    `Agedaysat1stdose`=min(`Agedaysat1stdose`),
    Site=min(Site), 
    Sex=min(Sex), 
    group=first(group)
  )
length(main_df$random_id)

# convert duration of observation window from days to years 
main_df <- main_df %>%
  mutate(
    Visit_time_interval_years = Visit_time_interval / 365.25,
    Visit_time_interval_14_years = Visit_time_interval_14 / 365.25
  )

############################################################.
# 2. Summarise by random_id — whether receive at least 1 systemic antibiotic?
# Add column: whether receive at least 1 systemic antibiotic prescription or not 
# (during the observation window)

abx_summary <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    received_abx_or_not_obs_window = as.integer(any(Systemic_abx_or_not_obs_window == 1))
  ) 

# Join to main_df dataset 
main_df <- left_join(main_df, abx_summary, by = "random_id")


############################################################.
# 3. Summarise by random_id - number of systemic antibiotic prescriptions & systemic abx prescription incidence rate 
# Add column: number of abx prescription per person 
# (during the observation window)

abx_count <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    Abx_count_obs_window = sum(Systemic_abx_or_not_obs_window == 1, na.rm = TRUE)
  ) 

# Join to main_df dataset 
main_df <- left_join(main_df, abx_count, by = "random_id")

# Add column: incidence rate of systemic abx prescription (per person-year) 
# = number of systemic abx prescription / total duration of observation window in years
main_df <- main_df %>%
  mutate(
    Abx_rate_obs_window = Abx_count_obs_window/Visit_time_interval_14_years
  )


############################################################.
# 4. Summarise by random_id - whether receive more than 1 systemic antibiotic? 
# Add column: whether receive more than 1 systemic abx prescription
# (during the observation window)

main_df <- main_df %>%
  mutate(More_than_1_abx_obs_window = ifelse(Abx_count_obs_window > 1, 1, 0)) 

############################################################.
# 5. Summarise by random_id - Average time interval btw vaccine effective time (or 3rd dose administration time) and systemic abx start date 
# Add column: Average time interval btw vaccine effective time and medication start date 
# Restrict the time interval when Systemic_abx_or_not_obs_window=1
# (during the observation window )

avg_time_interval <- abx_list %>%
  group_by(random_id) %>%
  summarise(
    avg_time_interval_days_obs_window = mean(as.numeric(Dose3_meds_interval[Systemic_abx_or_not_obs_window == 1])),
    avg_time_interval_days_obs_window_14 = mean(as.numeric(Dose3_14_meds_interval[Systemic_abx_or_not_obs_window == 1])),
  )
# Join to main_df dataset 
main_df <- left_join(main_df, avg_time_interval, by = "random_id")

length(unique(main_df$random_id)) #4369

############################################################.
# 6. Summarise by random_id - Average time interval btw each systemic abx prescription 

avg_abx_intervals <- abx_list %>%
  arrange(random_id, Dose3_meds_interval) %>%
  group_by(random_id) %>%
  summarise(
    avg_abx_interval_days_obs_window = ifelse(
      sum(Systemic_abx_or_not_obs_window == 1) > 1,
      mean(diff(Dose3_meds_interval[Systemic_abx_or_not_obs_window == 1])),
      NA_real_
    ),
    avg_abx_interval_days_obs_window_14 = ifelse(
      sum(Systemic_abx_or_not_obs_window == 1) > 1,
      mean(diff(Dose3_14_meds_interval[Systemic_abx_or_not_obs_window == 1])),
      NA_real_
    )
  )

main_df <- left_join(main_df, avg_abx_intervals, by = "random_id")
length(unique(main_df$random_id)) #4369

############################################################.
# 7. Summarise by random_id -  time to 1st systemic abx prescription 
# Add column: time to 1st systemic abx prescription 


# i)
# if censored (no systemic abx prescribed during the obs window), the time would be the end of follow up day (end of obs window)
first_abx_times <- abx_list %>%
  group_by(random_id) %>%
  reframe(
    
    first_abx_time_obs_window = if (any(Systemic_abx_or_not_obs_window == 1)) {
      min(Dose3_meds_interval[Systemic_abx_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval)
    },

    first_abx_time_obs_window_14 = if (any(Systemic_abx_or_not_obs_window == 1)) {
      min(Dose3_14_meds_interval[Systemic_abx_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      unique(Visit_time_interval_14)
    }
  )

main_df <- left_join(main_df, first_abx_times, by = "random_id")
length(main_df$random_id)

# ii)
# if censored (no systemic abx prescribed during the obs window), keep this column as NA
true_first_abx_times <- abx_list %>%
  group_by(random_id) %>%
  reframe(
    
    true_first_abx_time_obs_window = if (any(Systemic_abx_or_not_obs_window == 1)) {
      min(Dose3_meds_interval[Systemic_abx_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      NA_real_
    },
    
    true_first_abx_time_obs_window_14 = if (any(Systemic_abx_or_not_obs_window == 1)) {
      min(Dose3_14_meds_interval[Systemic_abx_or_not_obs_window == 1], na.rm = TRUE)
    } else {
      NA_real_
    }
  )
main_df <- left_join(main_df, true_first_abx_times, by = "random_id")
length(main_df$random_id)


#####################################################################.
# 2. GLMM Regression models ----
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
main_df <- main_df %>%
  mutate(age_grp = if_else(Agedaysat1stdose <= 365, "0-1",
                           if_else(Agedaysat1stdose <= 730, "1-2", "2-3")))

############################################################.
## Objective 2: Logistic regression ----
# whether individuals who received the malaria vaccine are less likely to receive systemic antibiotics 
########################################################.

############################################################.
# a: random intercept only
lm1 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (1 | Site),
  data = main_df,
  family = binomial(link = "logit")
)
lm1 
summary(lm1) 

############################################################.
# b: random slopes  
lm2 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (age_grp + Sex | Site),
  data = main_df,
  family = binomial(link = "logit")
)

lm3 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (age_grp | Site),
  data = main_df,
  family = binomial(link = "logit")
)

lm4 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (Sex | Site),
  data = main_df,
  family = binomial(link = "logit")
)

############################################################.
# c: model comparison to determine random effect
AIC(lm1,lm2,lm3,lm4) # favour lm1
anova(lm1,lm2,lm3,lm4) 

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
nrow(main_df)
length(fitted(lm1))

main_df$prediction <- fitted(lm1)
ggplot(main_df, aes(x=prediction, y=received_abx_or_not_obs_window)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth() +
  coord_cartesian(xlim=c(0,1))+ 
  labs(
    x = "Predicted probability of receiving antibiotics",
    y = "Received antibiotics (0 = No, 1 = Yes)"
  )
# can see clean upward trend, with most data points in upper right or lower left quadrant 
# Most points with prediction close to 1 correspond to received_abx_or_not_obs_window == 1, 
# and close to 0 correspond to 0

# can also as 0.75, 0.25, maybe add line to the plot 
# Proportion of points with high prediction and received antibiotics (upper right)
high_pred <- main_df$prediction > 0.75
high_pred_and_abx <- high_pred & main_df$received_abx_or_not_obs_window == 1
prop_high_correct <- sum(high_pred_and_abx) / sum(high_pred)
prop_high_correct

# Proportion of points with low prediction and did not receive antibiotics (lower left)
low_pred <- main_df$prediction < 0.25
low_pred_and_no_abx <- low_pred & main_df$received_abx_or_not_obs_window == 0
prop_low_correct <- sum(low_pred_and_no_abx) / sum(low_pred)
prop_low_correct

# Number of such points
sum(high_pred)         # Total predicted > 0.75
sum(high_pred_and_abx) # Predicted > 0.75 and received antibiotics

sum(low_pred)          # Total predicted < 0.25
sum(low_pred_and_no_abx) # Predicted < 0.25 and did not receive antibiotics



############################################################.
## Objective 3: Poisson regression for rate ----
# whether individuals who received the malaria vaccine have lower incidence rate of systemic abx prescription
########################################################.

############################################################.
# a: random intercept only
pm1 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = main_df,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

############################################################.
# b: random slopes (also with random intercept in built)
pm2 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = main_df,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

pm3 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = main_df,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

pm4 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = main_df,
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
dispersionstats <- main_df %>%
  group_by(group) %>%
  summarise(
    means = mean(Abx_count_obs_window),
    variances = var(Abx_count_obs_window),
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
    cat("-> Likely overdispersion. \n")
  } else {
    cat("-> No major overdispersion detected.\n")
  }
}
overdispersion_check(pm3)


# Score Test
# null hypothesis: no overdispersion 
mu <-predict(pm3, type="response")
z <- ((main_df$Abx_count_obs_window - mu)^2 - main_df$Abx_count_obs_window)/ (mu * sqrt(2))
summary(zscore <- lm(z ~ 1)) # p<0.05: reject null hypothesis

# Lagrange Multiplier Test
# null hypothesis: no overdispersion 
obs <- nrow(main_df) 
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
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = main_df,
  offset=log(Visit_time_interval_14)
)

pm6 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = main_df,
  offset=log(Visit_time_interval_14)
)

pm7 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = main_df,
  offset=log(Visit_time_interval_14)
)

pm8 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = main_df,
  offset=log(Visit_time_interval_14)
)

AIC(pm3, pm5, pm6, pm7, pm8) # favour pm5
anova(pm3, pm5, pm6, pm7, pm8) # favour pm5

summary(pm3)
summary(pm5)

############################################################.
# f: odds ratio, CI, p value 
t2 <- tbl_regression(pm3, exponentiate = TRUE)
t2
tab_model(pm3) 


t3 <- tbl_regression(pm5, exponentiate = TRUE)
t3
tab_model(pm5) 


############################################################.
# e: forest plot 
# error bar: CI
tidy_model2 <- tidy(pm5, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)

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
plot_model(pm5, type = "diag", show.values = TRUE)

fitted(pm5)
nrow(main_df)
length(fitted(pm5))

main_df$prediction3 <- fitted(pm5)
ggplot(main_df, aes(x=prediction3, y=Abx_count_obs_window)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth()+ 
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
# 3. Survival analysis ----
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
## Objective 1: time to first systemic antibiotic prescription ----
# - event: received_abx_or_not_obs_window
# - time: from dose 3 administration to 1st systemic antibiotic prescription 
# - 1st 14 days no event 
############################################################.


#############################################################.
# 1: survival function 
km1 <- survfit(Surv(first_abx_time_obs_window, received_abx_or_not_obs_window) ~ group, data = main_df)
km1
summary(km1)

#############################################################.
# 2: log rank test
# null hypothesis: survival estimates btw R21 and ctrl are not different 
lrt1 <- survdiff(Surv(first_abx_time_obs_window, received_abx_or_not_obs_window) ~ group, data = main_df)
lrt1 

#############################################################.
# 3: Kaplan meier curve
# survival probability: P(event=0) = P(not having abx prescription)
ggsurvplot(km1, 
           data = main_df, 
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

#############################################################.
# 4: Cumulative event
# 1-S(t)
# What proportion of individuals have experienced the event by time
ggsurvplot(km1, data = main_df, fun = "event",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(300,0.5),
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 

#############################################################.
# 5: Cumulative hazards
ggsurvplot(km1, data = main_df, fun = "cumhaz",
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
cox_model <- coxph(Surv(first_abx_time_obs_window, received_abx_or_not_obs_window) 
                   ~ group + Sex + age_grp + strata(Site), data = main_df) 
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
tidy_model3 <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

tabletext3 <- cbind(
  c("Variable", tidy_model3$term),
  c("Hazard Ratio (95% CI)", paste0(round(tidy_model3$estimate, 2), " (",
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


# d. model evaluation (model fit)
extractAIC(cox_model) # get aic
cox_model$loglik # log likelihood

# examine proportional hazards assumptions
# Therneau-Grambsch test and Schoenfeld residual analysis
ftest <- cox.zph(cox_model)
ftest 
ggcoxzph(ftest)
plot(cox.zph(cox_model))

ggcoxdiagnostics(cox_model, type="deviance", ox.scale="linear.predictions")
# assumptions not violated

############################################################.
## Objective 2: recurrent event survival analysis ----
############################################################.

abx_list <- abx_list %>%
  mutate(
    group = as.factor(group), 
    age_grp = if_else(Agedaysat1stdose <= 365, "0-1",
                      if_else(Agedaysat1stdose <= 730, "1-2", "2-3")))

#############################################################.
# 1: data prep

# keeping those not have systemic abx at all, have an entry of censoring (event=0, time=final follow up time)
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
length(unique(abx_list_clean$random_id))


# To accommodate the following cox model (AG model): 
# - time in start time and stop time format
# - start and stop time can not be identical, 
# - stop time must be > start time

# clean the data -> at each time point, only one record of meds prescription 
# if at this time point, systemic abx and non-systemic abx is prescribed (at least 1 systemic abx prescribed) ->  event=1
# if this date, no abx prescribed -> event=0

df <- abx_list_clean %>%
  group_by(random_id, Dose3_meds_interval) %>%
  # Prioritize event == 1
  arrange(desc(Systemic_abx_or_not_obs_window), .by_group = TRUE) %>%
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
km2 <- survfit(Surv(time=start_time, time2=stop_time, event=Systemic_abx_or_not_obs_window) ~ group, data = df)
km2
summary(km2)

#############################################################.
# 3: cumulative event
# 1-S(t)
# What proportion of individuals have experienced the event by time
ggsurvplot(km2, data = df, fun = "event",
           conf.int = TRUE,
           linetype = c(1,4),
           xlab = "Time from 3rd dose administration (days)",
           legend.title = "Vaccination status",
           legend.labs = c("Control", "R21/Matrix-M")) 


#############################################################.
# 3: cumulative hazards
ggsurvplot(km2, data = df, fun = "cumhaz",
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
cox_ag <- coxph(Surv(time=start_time, time2=stop_time, event=Systemic_abx_or_not_obs_window) ~ 
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
tidy_model4 <- tidy(cox_ag, exponentiate = TRUE, conf.int = TRUE)
tidy_model4 <- tidy_model4[!grepl("frailty", tidy_model4$term), ]

tabletext4 <- cbind(
  c("Variable", tidy_model4$term),
  c("Hazard Ratio (95% CI)", paste0(round(tidy_model4$estimate, 2), " (",
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



# d. model evaluation (model fit)
extractAIC(cox_ag) # get aic
cox_ag$loglik # log likelihood

# examine proportional hazards assumptions 
# Therneau-Grambsch test and Schoenfeld residual
ftest <- cox.zph(cox_ag)
ftest
ggcoxzph(ftest)
plot(cox.zph(cox_ag))
# assumptions not violated














