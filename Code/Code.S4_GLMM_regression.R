# Please show document outline to have a cleaner view of each section within this script
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
  lme4, 
  broom, 
  broom.helpers, 
  broom.mixed, 
  gtsummary, 
  forestplot, 
  sjPlot
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
    group = as.factor(group) # vaccination group 
  )

############################################################.
# Objective 1: Create age groups ----
########################################################. 

# group by biological developmental stages
main_df <- main_df %>%
  mutate(age_grp = if_else(Agedaysat1stdose <= 365, "0-1",
                           if_else(Agedaysat1stdose <= 730, "1-2", "2-3")))


# explore if age group is linearly correlated with abx prescription 
ggplot(main_df, aes(x = age_grp, y = Abx_rate_obs_window)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "Age Group", y = "Incidence rate of antibiotic prescriptions (per person-year)") +
  theme_minimal()

ggplot(main_df, aes(x = age_grp, y = Abx_rate_obs_window)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  facet_wrap(~ Site) +  # or facet_grid(. ~ site)
  labs(x = "Age Group", y = "Incidence rate of antibiotic prescriptions (per person-year)") +
  theme_minimal()

# seems like younger children are prescribed more, and prescribed earlier 


ggplot(main_df, aes(x = age_grp, y = true_first_abx_time_obs_window)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "Age Group", y = "Time to 1st abx prescription (days)") +
  theme_minimal()

# Bagamoyo site, age group 1-2 have longer time to 1st abx prescription (non-linearity)
ggplot(main_df, aes(x = age_grp, y = true_first_abx_time_obs_window)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  facet_wrap(~ Site)+
  labs(x = "Age Group", y = "Time to 1st abx prescription (days)") +
  theme_minimal()

# Bagamoyo site, age group 1-2 have longer average time interval (non-linearity)
# so treat age as categorical variable (3 age groups) instead of continuous variable
ggplot(main_df, aes(x = age_grp, y = avg_time_interval_days_obs_window)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "Age Group", y = "Average time interval until each abx prescription (days)") +
  theme_minimal()

ggplot(main_df, aes(x = age_grp, y = avg_time_interval_days_obs_window)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  facet_wrap(~ Site)+
  labs(x = "Age Group", y = "Average time interval until each abx prescription (days)") +
  theme_minimal()

ggplot(main_df, aes(x = age_grp, y = avg_abx_interval_days_obs_window)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "Age Group", y = "Average time interval between each antibiotic prescription (days)") +
  theme_minimal()

ggplot(main_df, aes(x = age_grp, y = avg_abx_interval_days_obs_window)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  facet_wrap(~ Site)+
  labs(x = "Age Group", y = "Average time interval between each antibiotic prescription (days)") +
  theme_minimal()

############################################################.
# Objective 2: Logistic regression ---- 
# whether individuals who received the malaria vaccine were less likely to receive antibiotics
# - adjust for covariates: sex, age 
# - site-level random effects
########################################################.

############################################################.
## a: random intercept only ----
lm1 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (1 | Site), 
  data = main_df,
  family = binomial(link = "logit")
)
lm1 
summary(lm1) 

############################################################.
## b: random slopes (also with random intercept in built) ----
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
## c: model comparison to determine random effect ----
AIC(lm1,lm2,lm3,lm4) # favour lm1
anova(lm1,lm2,lm3,lm4) 

############################################################.
## d: odds ratio, CI, p value ----
summary(lm1)
t1 <- tbl_regression(lm1, exponentiate = TRUE)
t1
tab_model(lm1) 

############################################################.
## e: forest plot ----
tidy_model1 <- tidy(lm1, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
tidy_model1 <- tidy_model1 %>%
  filter(term != "(Intercept)")

term_labels <- c(
  "SexMale" = "Sex: Male",
  "age_grp1-2" = "Age group: 1-2 years old",
  "age_grp2-3" = "Age group: 2-3 years old",
  "groupR21/Matrix-M" = "Vaccination group: R21/Matrix-M"
)
tidy_model1$term <- term_labels[tidy_model1$term]

# Create a matrix for display
tabletext1 <- cbind(
  c("Variable", tidy_model1$term),
  c("Odds Ratio (95% CI)", paste0(round(tidy_model1$estimate, 2), " (",
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
## f: model diagnostic, evaluate model fit ----
plot_model(lm1, type = "diag", show.values = TRUE)

fitted(lm1)
nrow(main_df)
length(fitted(lm1))

main_df$prediction <- fitted(lm1) # model's predicted probability 
ggplot(main_df, aes(x=prediction, y=received_abx_or_not_obs_window)) +
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

# Proportion of points with high prediction and received antibiotics (upper right)
high_pred <- main_df$prediction > 0.75
high_pred_and_abx <- high_pred & main_df$received_abx_or_not_obs_window == 1
prop_high_correct <- sum(high_pred_and_abx) / sum(high_pred)
prop_high_correct # 94%

# Proportion of points with low prediction and did not receive antibiotics (lower left)
low_pred <- main_df$prediction < 0.25
low_pred_and_no_abx <- low_pred & main_df$received_abx_or_not_obs_window == 0
prop_low_correct <- sum(low_pred_and_no_abx) / sum(low_pred)
prop_low_correct #81%

# Number of such points
sum(high_pred)         # Total predicted > 0.75
sum(high_pred_and_abx) # Predicted > 0.75 and received antibiotics

sum(low_pred)          # Total predicted < 0.25
sum(low_pred_and_no_abx) # Predicted < 0.25 and did not receive antibiotics

############################################################.
# Objective 3: Poisson regression for rate ----
# whether individuals who received the malaria vaccine had lower incidence rate of abx prescription
# - adjust for covariates: sex, age 
# - site-level random effects
########################################################.

############################################################.
## a: random intercept only ----
pm1 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = main_df,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

############################################################.
## b: random slopes (also with random intercept in built) ----
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
## c: model comparison to determine random effect ----
AIC(pm1, pm2, pm3, pm4) # favour pm3
anova(pm1, pm2, pm3, pm4) # p value favouring pm3 


############################################################.
## d: check overdispersion ----

# check variance and mean
dispersionstats <- main_df %>%
  group_by(group) %>%
  summarise(
    means = mean(Abx_count_obs_window),
    variances = var(Abx_count_obs_window),
    ratio = variances/means)
dispersionstats # shows variance bigger than mean 

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
## e: Negative binomial regression for rate (overdispersion is detected) ----
# this might takes a bit of time to run

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

############################################################.
## f: odds ratio, CI, p value ----
summary(pm3)
summary(pm5)

t2 <- tbl_regression(pm3, exponentiate = TRUE)
t2
tab_model(pm3) 


t3 <- tbl_regression(pm5, exponentiate = TRUE)
t3
tab_model(pm5) 
############################################################.
## g: forest plot ----
tidy_model2 <- tidy(pm5, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
tidy_model2 <- tidy_model2 %>%
  filter(term != "(Intercept)")

term_labels <- c(
  "SexMale" = "Sex: Male",
  "age_grp1-2" = "Age group: 1-2 years old",
  "age_grp2-3" = "Age group: 2-3 years old",
  "groupR21/Matrix-M" = "Vaccination group: R21/Matrix-M"
)
tidy_model2$term <- term_labels[tidy_model2$term]

# Create a matrix for display
tabletext2 <- cbind(
  c("Variable", tidy_model2$term),
  c("Adjusted Incidence Rate Ratio (95% CI)", paste0(round(tidy_model2$estimate, 2), " (",
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
           xlog = TRUE,
           xticks = c(0.5, 0.75, 1, 1.25, 1.5, 2))


# Add right arrow and text
grid.lines(x = unit(c(0.765, 0.9), "npc"), y = unit(0.22, "npc"),
           arrow = arrow(type = "open", length = unit(0.1, "inches")),
           gp = gpar(col = "black", lwd = 1))
grid.text("Higher incidence", x = unit(0.88, "npc"), y = unit(0.16, "npc"),
          just = "right", gp = gpar(fontsize = 10))

# Add left arrow and text
grid.lines(x = unit(c(0.75, 0.615), "npc"), y = unit(0.22, "npc"),
           arrow = arrow(type = "open", length = unit(0.1, "inches")),
           gp = gpar(col = "black", lwd = 1))
grid.text("Lower incidence", x = unit(0.635, "npc"), y = unit(0.16, "npc"),
          just = "left", gp = gpar(fontsize = 10))


############################################################.
## h: model diagnostic, evaluate model fit ----
plot_model(pm5, type = "diag", show.values = TRUE)

fitted(pm5)
nrow(main_df)
length(fitted(pm5))

main_df$prediction2 <- fitted(pm5) # predicted, expected abx counts

ggplot(main_df, aes(x=prediction2, y=Abx_count_obs_window)) +
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
 














