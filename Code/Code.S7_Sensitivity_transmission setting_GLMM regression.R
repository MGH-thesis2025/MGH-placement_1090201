# Please show and unhide document outline to have a cleaner view of each section within this script
# the same set of analysis as main analysis is conducted

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
# Objective 1: create age groups ----
########################################################. 

# group by biological developmental stages
main_df <- main_df %>%
  mutate(age_grp = if_else(Agedaysat1stdose <= 365, "0-1",
                           if_else(Agedaysat1stdose <= 730, "1-2", "2-3")))

# separate seasonal and perennial 
seasonal <- main_df %>%
  filter(Site %in% c("Bougouni", "Nanoro", "Siglé"))

perennial <- main_df %>%
  filter(Site %in% c("Kilifi", "Bagamoyo", "Dandé"))

############################################################.
# Objective 2: Logistic regression ---- 
# whether individuals who received the malaria vaccine are less likely to receive antibiotics 
########################################################.

############################################################.
## a: random intercept only ----
lm1 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (1 | Site),
  data = seasonal,
  family = binomial(link = "logit")
)

lm11 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (1 | Site),
  data = perennial,
  family = binomial(link = "logit")
)

############################################################.
## b: random slopes ----
lm2 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (age_grp + Sex | Site),
  data = seasonal,
  family = binomial(link = "logit")
)
lm22 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (age_grp + Sex | Site),
  data = perennial,
  family = binomial(link = "logit")
)


lm3 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (age_grp | Site),
  data = seasonal,
  family = binomial(link = "logit")
)
lm33 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (age_grp | Site),
  data = perennial,
  family = binomial(link = "logit")
)


lm4 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (Sex | Site),
  data = seasonal,
  family = binomial(link = "logit")
)
lm44 <- glmer(
  received_abx_or_not_obs_window ~ group + age_grp + Sex + (Sex | Site),
  data = perennial,
  family = binomial(link = "logit")
)


############################################################.
## c: model comparison to determine random effect ----
AIC(lm1,lm2,lm3,lm4) # favour lm1
anova(lm1,lm2,lm3,lm4)  

AIC(lm11,lm22,lm33,lm44) # favour lm11
anova(lm11,lm22,lm33,lm44)

############################################################.
## d: odds ratio, CI, p value ----
t1 <- tbl_regression(lm1, exponentiate = TRUE)
t1
tab_model(lm1) 

t11 <- tbl_regression(lm11, exponentiate = TRUE)
t11
tab_model(lm11) 

############################################################. 
## e: model diagnostic, evaluate model fit ----
plot_model(lm1, type = "diag", show.values = TRUE)

fitted(lm1)
nrow(seasonal)
length(fitted(lm1))

seasonal$prediction <- fitted(lm1) # model's predicted probability 
ggplot(seasonal, aes(x=prediction, y=received_abx_or_not_obs_window)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth() +
  coord_cartesian(xlim=c(0,1))
# Most points with prediction close to 1 correspond to received_abx_or_not_obs_window == 1, 
# and close to 0 correspond to 0

# Proportion of points with high prediction and received antibiotics (upper right)
high_pred <- seasonal$prediction > 0.75
high_pred_and_abx <- high_pred & seasonal$received_abx_or_not_obs_window == 1
prop_high_correct <- sum(high_pred_and_abx) / sum(high_pred)
prop_high_correct #94%

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
plot_model(lm11, type = "diag", show.values = TRUE)

fitted(lm11)
nrow(perennial)
length(fitted(lm11))

perennial$prediction <- fitted(lm11) # model's predicted probability 
ggplot(perennial, aes(x=prediction, y=received_abx_or_not_obs_window)) +
  geom_jitter(height=0.1, alpha=0.1) + 
  geom_smooth() +
  coord_cartesian(xlim=c(0,1))
# can see clean upward trend, with most data points in upper right or lower left quadrant 
# Most points with prediction close to 1 correspond to received_abx_or_not_obs_window == 1, 
# and close to 0 correspond to 0

# Proportion of points with high prediction and received antibiotics (upper right)
high_pred <- perennial$prediction > 0.75
high_pred_and_abx <- high_pred & perennial$received_abx_or_not_obs_window == 1
prop_high_correct <- sum(high_pred_and_abx) / sum(high_pred)
prop_high_correct #92%

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
# Objective 3: Poisson regression for rate ----
# whether individuals who received the malaria vaccine have lower incidence rate of abx prescription
########################################################.

############################################################.
## a: random intercept only ----
pm1 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = seasonal,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)
pm11 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = perennial,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

############################################################.
## b: random slopes ----
pm2 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = seasonal,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)
pm22 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = perennial,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

pm3 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = seasonal,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)
pm33 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = perennial,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

pm4 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = seasonal,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)
pm44 <- glmer(
  Abx_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = perennial,
  family = poisson(link = "log"),
  offset=log(Visit_time_interval_14)
)

############################################################.
## c: model comparison to determine random effect ----
AIC(pm1, pm2, pm3, pm4) # favour pm1
anova(pm1, pm2, pm3, pm4) 

AIC(pm11, pm22, pm33, pm44) # favour pm33
anova(pm11, pm22, pm33, pm44) # p value favouring pm33


############################################################.
## d: check overdispersion ----

# check variance and mean
dispersionstats <- seasonal %>%
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
overdispersion_check(pm1)
overdispersion_check(pm33)


# Score Test
# null hypothesis: no overdispersion 
mu <-predict(pm1, type="response")
z <- ((seasonal$Abx_count_obs_window - mu)^2 - seasonal$Abx_count_obs_window)/ (mu * sqrt(2))
summary(zscore <- lm(z ~ 1)) # p<0.05: reject null hypothesis

mu <-predict(pm33, type="response")
z <- ((perennial$Abx_count_obs_window - mu)^2 - perennial$Abx_count_obs_window)/ (mu * sqrt(2))
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
## e: Negative binomial regression for rate (overdispersion is detected) ----

pm5 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = seasonal,
  offset=log(Visit_time_interval_14)
)
pm6 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = seasonal,
  offset=log(Visit_time_interval_14)
)
pm7 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = seasonal,
  offset=log(Visit_time_interval_14)
)
pm8 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = seasonal,
  offset=log(Visit_time_interval_14)
)


pm55 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (1 | Site),
  data = perennial,
  offset=log(Visit_time_interval_14)
)
pm66 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp + Sex | Site),
  data = perennial,
  offset=log(Visit_time_interval_14)
)
pm77 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (age_grp | Site),
  data = perennial,
  offset=log(Visit_time_interval_14)
)
pm88 <- glmer.nb(
  Abx_count_obs_window ~  group + age_grp + Sex + (Sex | Site),
  data = perennial,
  offset=log(Visit_time_interval_14)
)


AIC(pm1, pm5, pm6, pm7, pm8) # favour pm5
anova(pm1, pm5, pm6, pm7, pm8) # favour pm5

AIC(pm33, pm55, pm66, pm77, pm88) # favour pm77
anova(pm33, pm55, pm66, pm77, pm88) # favour pm77


############################################################.
## f: odds ratio, CI, p value ----
summary(pm1)
summary(pm5)

t2 <- tbl_regression(pm1, exponentiate = TRUE)
t2
tab_model(pm1) 
t3 <- tbl_regression(pm5, exponentiate = TRUE)
t3
tab_model(pm5) 

summary(pm33)
summary(pm77)
t22 <- tbl_regression(pm33, exponentiate = TRUE)
t22
tab_model(pm33) 
t33 <- tbl_regression(pm77, exponentiate = TRUE)
t33
tab_model(pm77)

############################################################.
## g: model diagnostic, evaluate model fit ----
plot_model(pm5, type = "diag", show.values = TRUE)

fitted(pm5)
nrow(seasonal)
length(fitted(pm5))

seasonal$prediction2 <- fitted(pm5)
ggplot(seasonal, aes(x=prediction2, y=Abx_count_obs_window)) +
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

############################################################.
plot_model(pm77, type = "diag", show.values = TRUE)

fitted(pm77)
nrow(perennial)
length(fitted(pm77))

perennial$prediction2 <- fitted(pm77)
ggplot(perennial, aes(x=prediction2, y=Abx_count_obs_window)) +
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




