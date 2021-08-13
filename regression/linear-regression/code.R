library(ggplot2)
# Load in the data
plasma <- read.delim("Data/plasma.txt")
head(plasma)
summary(plasma)


### PART 1 
### Plasma beta-carotene and age

### (a) model how plasma beta-carotene varies with age
# Save plasma beta-carotene in betaplasma-vector and age in vector
betaplasma <- plasma[, "betaplasma"]
age <- plasma[, "age"]

# Plot all observations betaplasma vs age
plot(age, betaplasma)

# Problem: one plasma beta-carotene observation = 0
# Take observation out
plasma.new <- plasma[plasma$betaplasma > 0, ]

# Save plasma beta-carotene in betaplasma-vector and age in vector
betaplasma <- plasma.new[, "betaplasma"]
#log.betaplasma <- log(betaplasma)
age <- plasma.new[, "age"]

# Plot betaplasma vs age where observations have betaplasma > 0 
plot(age, betaplasma)

# Plot log(betaplasma) vs age where observations have betaplasma > 0 
plot(age, log(betaplasma))

### 1. Betaplasma as linear function of age 
#------ Fit a linear regression model, Y_i = beta_0 + beta_1*x_1 + ebs
(betaplasma.model <- lm(betaplasma ~ age, data = plasma.new))

# Get and save standard errors, etc
(betaplasma.summary <- summary(betaplasma.model))

# Extract and save the sigma estimate = standard deviation from the summary
(betaplasma.sigma <- betaplasma.summary$sigma)

# Extract the parameter estimates table from the summary
betaplasma.summary$coefficients

# Get a 95% confidence interval for the parameter estimates
# CI = +- 1.96 * <Standard error>
confint(betaplasma.model)

#------ Model summary
# Parameter estimates
# b0 = 128.141802
# b1 = 1.242668

# Standard error of the parameter estimates
# S0 = 36.9220833
# S1 = 0.7066541

# Confidence interval
# CI0 = [55.4940399, 200.789564]
# CI1 = [-0.1477424, 2.633078]

# T-value 
# t0 = 3.470601
# t1 = 1.758523

# (T value for 95 % confidence interval is 2.262. Everything above that is MORE 
# significant)


# ------ Plot the data, fitted line and 95% CI
# Make a new data frame by adding the confidence and
# prediction intervals for each of the observations

betaplasma.pred <- 
  cbind(plasma.new, 
        fit = predict(betaplasma.model),
        conf = predict(betaplasma.model, interval = "confidence"),
        pred = predict(betaplasma.model, interval = "prediction"))
head(betaplasma.pred)
# get rid of the extra fits
betaplasma.pred$conf.fit <- betaplasma.pred$pred.fit <- NULL
head(betaplasma.pred)

# Make the basic plot, add some modifications and
# save it so that we can add other things to it later:
(
  plot.data <- 
    ggplot(data = betaplasma.pred, aes(x = age, y = betaplasma)) + 
    geom_point(size = 3) +
    xlab("age") +
    ylab("betaplasma") +
    labs(title = "betaplasma against age") +
    theme(text = element_text(size = 18))
)

### Add the fitted line to the data plot####
(
  plot.line <- plot.data + 
    geom_line(aes(y = fit), color = "blue", size = 1) +
    labs(caption = "data and fitted line")
)

### Add confidence interval####
(
  plot.conf <- plot.line + 
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.6) +
    labs(caption = "data, fitted line and 95% confidence interval")
)

### Add prediction interval####
plot.conf +
  geom_line(aes(y = pred.lwr),
            color = "red", linetype = "dashed", size = 1) +
  geom_line(aes(y = pred.upr),
            color = "red", linetype = "dashed", size = 1) +
  labs(caption = "data, fitted line, 95% confidence and prediction intervals")


#------ Basic residual analysis
# Add the residuals to the predicted data
betaplasma.pred$e <- betaplasma.model$residuals
head(betaplasma.pred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max.e <- max(abs(betaplasma.pred$e)))

(betaplasma.elims <- c(-max.e, max.e))

# Plot residuals against x, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = betaplasma.pred, 
       aes(x = fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = Pb.elims) +
  geom_smooth() +
  xlab("fit") +
  ylab("e") +
  labs(title = "Linear model residuals") +
  theme(text = element_text(size = 18))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = betaplasma.pred, aes(x = fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = c(-1200,1200)) +
  geom_smooth() +
  xlab("fit") +
  ylab("e") +
  labs(title = "Linear model residuals vs Yhat") +
  theme(text = element_text(size = 18))

# Make a normal qq-plot of the residuals.
ggplot(data = betaplasma.pred, aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(title = "Linear model residuals Q-Q-plot") +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = betaplasma.pred, aes(x = e)) +
  geom_histogram(bins = 10) +
  xlab("e") +
  geom_smooth() +
  expand_limits(x = c(-500,1500)) +
  labs(title = "Linear model residuals histogram") +
  theme(text = element_text(size = 18))

# Residual analysis:
# 1. The residual variance sigma^2 is not constant. It increases with increasing Y_hat_i
# 2. The residuals are not normally distributed
# 3. The residuals are not randomly distributed around zero. 
# Betaplasma have a non-linear trend



### 2. log(betaplasma) as linear function of age 
#------ Fit a linear regression model, log(Y_i) = b0 + b1*x_1 + ebs
(log.betaplasma.model <- lm(log(betaplasma) ~ age, data = plasma.new))

# Get and save standard errors, etc
(log.betaplasma.summary <- summary(log.betaplasma.model))

# Extract and save the sigma estimate = standard deviation from the summary
(logbetaplasma.sigma <- log.betaplasma.summary$sigma)

# Extract the parameter estimates table from the summary
log.betaplasma.summary$coefficients

# Get a 95% confidence interval for the parameter estimates
# CI = +- 1.96 * <Standard error>
confint(log.betaplasma.model)

#------ Model summary
# Parameter estimates
# b0 = 4.610011266
# b1 = 0.006965795

# Standard error of the parameter estimates
# S0 = 0.150212008
# S1 = 0.002874917

# Confidence interval
# CI0 = [4.314454644, 4.90556789]
# CI1 = [0.001309118, 0.01262247]

# T-value 
# t0 = 30.690031
# t1 = 2.422955

# (T value for 95 % confidence interval is 2.262. Everything above that is MORE 
# significant)


# ------ Plot the data, fitted line and 95% CI
# Make a new data frame by adding the confidence and
# prediction intervals for each of the observations

log.betaplasma.pred <- 
  cbind(plasma.new, 
        fit = predict(log.betaplasma.model),
        conf = predict(log.betaplasma.model, interval = "confidence"),
        pred = predict(log.betaplasma.model, interval = "prediction"))
head(log.betaplasma.pred)
# get rid of the extra fits
log.betaplasma.pred$conf.fit <- log.betaplasma.pred$pred.fit <- NULL
head(log.betaplasma.pred)


#------ Basic residual analysis
# Add the residuals to the predicted data
log.betaplasma.pred$e <- log.betaplasma.model$residuals
head(log.betaplasma.pred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max.e <- max(abs(log.betaplasma.pred$e)))
(log.betaplasma.elims <- c(-max.e, max.e))

### Plot residuals against x, add a horizontal line at y=0,
### and expand the y-axis to include +/- max residual.

ggplot(data = log.betaplasma.pred, 
       aes(x = fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = c(-6, 6)) +
  geom_smooth() +
  xlab("fit") +
  ylab("e") +
  labs(title = "Log-linear model residuals vs Yhat") +
  theme(text = element_text(size = 18))

### Plot residuals against yhat, add a horizontal line at y=0,
### and expand the y-axis to include +/- max residual.

ggplot(data = log.betaplasma.pred, aes(x = fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = c(-4,4)) +
  xlab("Predicted betaplasma") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

### Make a normal qq-plot of the residuals ###
ggplot(data = log.betaplasma.pred, aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(title = "Log-linear model residuals Q-Q-plot") +
  theme(text = element_text(size = 18))

### Histogram of the residuals ###
ggplot(data = log.betaplasma.pred, aes(x = e)) +
  geom_histogram(bins = 10) +
  xlab("Residuals") +
  labs(title = "Log-linear model residuals histogram") +
  theme(text = element_text(size = 18))

# ------ Residual analysis:
# 1. The residual variance sigma^2 is constant and doesnt increases with increasing Y_hat_i
# 2. The residuals are normally distributed
# 3. The residuals are randomly distributed around zero. 
# log(betaplasma) have a linear trend

# Residual analysis show that
# model 2 log(betaplasma) as a linear function of age is a better model 
# than model 1 betaplasma as a linear function of age


### PART 1
### (b) Plot plasma beta-carotene against age with estimated relationship, a 
### confidence interval for fitted line and prediction interval for new observations

### Plot observations ###
(
  plot.data <- 
    ggplot(data = log.betaplasma.pred, aes(x = age, y = betaplasma)) + 
    geom_point(size = 1) +
    expand_limits(y = c(1,8)) +
    xlab("age") +
    ylab("log(beta-carotene)") +
    labs(title = "Log-linear model") +
    theme(text = element_text(size = 18))
)

### Add the fitted line to the data plot ####
(
  plot.line <- plot.data + 
    geom_line(aes(y = exp(fit)), color = "blue", size = 1) +
    labs(caption = "data and fitted line")
)

### Add confidence interval ####
(
  plot.conf <- plot.line + 
    geom_ribbon(aes(ymin = exp(conf.lwr), ymax = exp(conf.upr)), alpha = 0.6) +
    labs(caption = "data, fitted line and 95% confidence interval")
)

### Add prediction interval ####
plot.conf +
  geom_line(aes(y = exp(pred.lwr)),
            color = "red", linetype = "dashed", size = 1) +
  geom_line(aes(y = exp(pred.upr)),
            color = "red", linetype = "dashed", size = 1) +
  labs(caption = "data, fitted line, 95% confidence and prediction intervals")


### What happens on average to the plasma beta-carotene level if we increase
### the age by 1 year? Include a 95 % confidence interval for the change rate

# Look at the slope b1 = 0.006965795
(exp(0.006965795))

# On average the plasma beta-carotene level increase with exp(b1)=1.00699 ng/ml 
# per year

# Look at confidence interval for b1: CI1 = [0.001309118, 0.01262247]
exp((0.001309118))
exp((0.01262247))

# 95% CI for the change rate: CI = [1.00131, 1.012702]


### Does the size of the change (measured in ng/ml) depend on age? 

# log(Y_i) = b0 + b1*x_1
# --> delta_31_30 = e^(log(Y_31))-e^(log(Y_30)) = e^(b0+b1*31)-e^(b0+b1*30)
# --> delta_70_71 = e^(log(Y_71))-e^(log(Y_70)) = e^(b0+b1*71)-e^(b0+b1*70)

# Size of change between 30 year old and a 31 year old
(delta_31_30 <- exp(4.610011266 +0.006965795*31)-exp(4.610011266 +0.006965795*30))

# Size of change between 30 year old and a 31 year old
(delta_71_70 <- exp(4.610011266 +0.006965795*71)-exp(4.610011266 +0.006965795*70))

# delta_31_30 != delta_70_71 --> change (measured in ng/ml) depend on age.
# This is due to the logarithmic behavior of the function

# Change per year [ng/ml] for a 30 year old person is LOWER than average 
# Change per year [ng/ml] for a 70 year old person is HIGHER than average



### PART 1
### (c) Calculate a 95% prediction interval for the observed plasma beta-
# carotene of a 30 year old person, as well as for a 70 year old person
(log.betaplasma.x0 <- data.frame(age = c(30)))

pred.int <- cbind(log.betaplasma.x0,
      round(
        predict(log.betaplasma.model, newdata = log.betaplasma.x0, interval = "prediction"),
        digits = 6))

pred.int
exp(pred.int)

(log.betaplasma.x0 <- data.frame(age = c(70)))
pred.int <- cbind(log.betaplasma.x0,
                  round(
                    predict(log.betaplasma.model, newdata = log.betaplasma.x0, interval = "prediction"),
                    digits = 6))
pred.int
exp(pred.int)


### PART 2
### (a) Turn the categorical variables into factors and present a frequency table

# Change sex from {1, 2} to {Male, Female}
plasma.new$sex <- factor(plasma.new$sex,
                     levels = c(1, 2),
                     labels = c("Male", "Female"))

# Change smoking status from {1, 2, 3} to {Never, Former, Current Smoker}
plasma.new$smokstat <- factor(plasma.new$smokstat,
                         levels = c(1, 2, 3),
                         labels = c("Never", "Former", "Current Smoker"))

# Change BMI category from {1, 2, 3, 4}
plasma.new$bmicat <- factor(plasma.new$bmicat,
                              levels = c(1, 2, 3, 4),
                              labels = c("Underweight", "Normal", "Overweight", "Obese"))


# Present in frequency tables
# The largest category should always be the reference category

# Frequency table for sex
summary(plasma.new$sex)        # Largest category: Female

# Frequency table for smoking status 
summary(plasma.new$smokstat)   # Largest category: Never

# Frequency table for BMI category
summary(plasma.new$bmicat)     # Largest category: Normal



### PART 2 
### (b) Fit a model where plasma beta-carotene depends only on BMI

# Plot log(betaplasma) vs BMI
bmicat <- plasma.new[, "bmicat"]
ggplot(plasma.new, aes(x = bmicat, y = log.betaplasma)) + geom_point() + 
         labs(title = "log(betaplasma) vs BMI")

# Using default "Underweight" as reference category 
# Y = b0 + b1*bmicat_Normal + b2*bmicat_Overweight + b3*bmicat_Obese
(log.betaplasma.bmi.model <- lm(log.betaplasma ~ bmicat, data = plasma.new))

# Get and save standard errors, etc
(log.betaplasma.bmi.summary <- summary(log.betaplasma.bmi.model))

# Extract and save the sigma estimate = standard deviation from the summary
log.betaplasma.bmi.sigma <- log.betaplasma.bmi.summary$sigma

# Extract the parameter estimates table from the summary
log.betaplasma.bmi.summary$coefficients

#------ Model summary: reference category "Underweight"
# Parameter estimates
# b0 = 5.3601778
# b1 = -0.2324427
# b2 = -0.4869572
# b3 = -0.7421181

# Standard error of the parameter estimates
# S0 = 0.3614882
# S1 = 0.3659789
# S2 = 0.3695223
# S3 = 0.3731521

# Changing reference category to "Normal"
plasma.new$bmicat <- relevel(plasma.new$bmicat, "Normal")

# Y = b0 + b1*bmicat_Underweight + b2*bmicat_Overweight + b3*bmicat_Obese
(log.betaplasma.bmi.model <- lm(log.betaplasma ~ bmicat, data = plasma.new))

# Get and save standard errors, etc
(log.betaplasma.bmi.summary <- summary(log.betaplasma.bmi.model))

# Extract and save the sigma estimate = standard deviation from the summary
log.betaplasma.bmi.sigma <- log.betaplasma.bmi.summary$sigma

# Extract the parameter estimates table from the summary
log.betaplasma.bmi.summary$coefficients

#------ Model summary: reference category "Underweight"
# Parameter estimates
# b0 = 5.1277351
# b1 = 0.2324427
# b2 = -0.2545145
# b3 = -0.5096754

# Standard error of the parameter estimates
# S0 = 0.0571563
# S1 = 0.3659789
# S2 = 0.0956024 
# S3 = 0.1087916 


# COMPARE 
# Models: reference category 1. "Underweight" vs 2. "Normal"
# Standard error is much larger when "Underweight" is used as reference category
# The reference category is used as category of comparison to the other categories. 
# When comparing everything with a category that is not representative (only 4 observations) 
# it will be larger errors



### PART 2
### (c) Fit a model where plasma beta-carotene depends on age, sex, smokstat and 
### bmicat 

# Changing reference category for sex to "Female"
plasma.new$sex <- relevel(plasma.new$sex, "Female")

# Changing reference category for smokstat to "Never"
plasma.new$smokstat <- relevel(plasma.new$smokstat, "Never")

# Fit a model where plasma beta-carotene depend on age, sex, smokstat and bmicat
# Y_i = b0 + b1*age + b2*sex_male + b3*smokstat_Former + b4*smokstat_Current 
# + b5*bmicat_Underweight + b6*bmicat*Overweight + b7*Obese
(full.model <- lm(log.betaplasma ~ age + sex + smokstat + bmicat, data = plasma.new))

# Get and save standard errors, etc
(full.summary <- summary(full.model))

# Extract and save the sigma estimate = standard deviation from the summary
full.sigma <- full.summary$sigma

# Extract the parameter estimates table from the summary
full.summary$coefficients

# 95% confidence interval
confint(full.model)

# exp(parameter estimates) and exp(CI)
exp(full.summary$coefficients)
exp(confint(full.model))


### Perform some tests

# TEST 1) Is this model significantly better than a model with only an intercept, meaning
# is the model better than nothing?
#
# Solution
# Null hypothesis H_0: b1 = b2 = b3 = b4 = b5 = b6 = b7 = 0
# GLobal F-test to test H_0. If H_0 is true then model is worthless
(full.summary <- summary(full.model))
# F = MS(Regr)/MS(Error) = 8.187 (from summary)
# df = 306
# F follows an F(7, 306)-distribution if H_0 is true
# P-value = P(F(7, 306) > 8.187) = 3.786*10^-9 if H_0 is true
# Since P-value < 0.05 = alpha we can reject H_0
#
# Conclusion: our data suggest that the model contain at least one relevant 
# covariate 


# TEST 2) Is this model significantly better than the model in Part 1, 
# using only age?
# 
# Solution
# Null hypothesis H_0: b2 = b3 = b4 = b5 = b6 = b7 = 0
# Partial F-test to test H_0 against H_1: b2! = 0 and/or b3!= 0 etc.
# If H_0 is true then the model using only age can be used instead
# 
# Estimate model without b2,b3,b4,b5,b6,b7. Only age
reduced.model <- lm(log.betaplasma ~ age, data = plasma.new)

# Compare full model against reduced model
anova(reduced.model, full.model)

# Calculating the increase in SS(Error)
# Q = SS(Error_reduced)- SS(Error_full) = (171.75-147.38)
# df = n - (7+1-6) -(n-(7+1)) = 6
# df_full = 306
# s_full = SS(Error_full) / df = 147.38/ 306
#
# Reject H_0 if 
# F = (Q/k)/(s_full^2) > F_{alpha, 6, n - (7+1)}
# F = ((171.75 -147.38)/6)/ (147.38/306)^2 = 8.4334
# 
# P-value = 1.763*10^-8 < 0.05 and we can reject H_0
# 
# Conclusion: our data suggest that the full model is significantly better
# than the model using only age


# TEST 3) For each variable (age, sex, smokstat and bmicat), is a model with that 
# variable significantly better than a model without that variable, keeping all 
# other variables?
# 
# Solution: 

(full.summary <- summary(full.model))
# t_{0.05/2, 306} = 1.650 (from look-up table)

## Variable: age
# Null hypothesis H_0: b1 = 0
# Test: t-test to test the effect of one variable
# Parameter: b1
# t-value = 2.608 > 1.650 --> reject H_0: b1 = 0 at significant level 0.05
# P-value = P(|t(306)| > 2.608) = 0.009560 < 0.05 --> reject H_0: b1 = 0
#
## Variable: Sex: Male
# Null hypothesis H0: b2 = 0
# Test: t-test to test the effect of one variable
# Parameter: b2
# t-value = |-2.778| > 1.650 --> reject H_0: b2 = 0 at significant level 0.05
# P-value = P(|t(306)| > -2.778) = 0.005799 < 0.05 --> reject H_0: b2 = 0
#
#
## Variable: Smokstat 
# Null hypothesis H0: b3 = b4 = 0
# Test: partial F-test to test several parameters at the same time
reduce.smokstat.model <- lm(log.betaplasma ~ age + sex + bmicat, data = plasma.new)
anova(reduce.smokstat.model, full.model)
# F = 6.499
# P = 0.001721 < 0.05 --> reject H_0
#
#
## Variable: bmicat
# Null hypothesis: b5 = b6 = b7 = 0
# Test: Partial F-test to test several parameters at the same time
reduce.bmicat.model <- lm(log.betaplasma ~ age + sex + smokstat, data = plasma.new)
anova(reduce.bmicat.model, full.model)
# F = 9.8373
# P = 2.66e-06 < 0.05 --> reject H_0
#
# Conclusion:
# Model will be significantly BETTER with ALL variables: age, sex, smokstat, bmicat


# TEST 4) Is the "Underweight" BMI category significantly different from the 
# reference category, given all other variables?
# 
# Solution: 
(full.summary <- summary(full.model))
# t_{0.05/2, 306} = 1.650 (from look-up table)
#
## Variable: bmicat - Underweight
# Null hypothesis H_0: b5 = 0
# Test: t-test will test one of the categories against the reference
# Parameter: b5
# t-value = 0.864 !> 1.650 --> cannot reject H_0: b5 = 0 at significant level 0.05
# P-value = P(|t(306)| > 2.608) = 0.388193 < 0.05 --> cannot reject H_0: b1 = 0


### PART 2 
### (d) Calculate fitted values, CI and prediction interval for model
sex <- plasma.new[, "sex"]

ggplot(data = plasma.new, aes(x = age, y = log.betaplasma, color = sex)) +
  geom_point() + labs(title = "log plasma beta-carotene against age")

# ------ Plot the data, fitted line and 95% CI
# Make a new data frame by adding the confidence and
# prediction intervals for each of the observations
bp.pred <- cbind(
  plasma.new,
  fit = predict(full.model),
  conf = predict(full.model, interval = "confidence"),
  pred = predict(full.model, interval = "prediction"))
  
# Betaplasma against age, color = sex
(
  plot.data <- 
    ggplot(plasma.new, aes(x = age, y = log.betaplasma, color = sex))) + 
    geom_point() +
    xlab("age") +
    ylab("log betaplasma") +
    labs(title = "betaplasma against age") 


# log Betaplasma against age, confidence interval and prediction interval. Pink = female, 
# black = men. Subplots show different smoke status (1-3) and different BMI category (1-4)
ggplot(bp.pred, aes(x = age, y = log.betaplasma, color = sex)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  geom_line(aes(y = pred.lwr), linetype = 2) +
  geom_line(aes(y = pred.upr), linetype = 2) +
  facet_wrap(~ bmicat + smokstat) +
  xlab("age") +
  ylab("log(plasma beta-carotene)") +
  theme(text = element_text(size = 18)) + 
  labs(title = "Log-linear model, categorical variables") +
  labs(caption = "data, fitted line, 95% confidence and prediction intervals") +
  facet_grid(smokstat ~ relevel(bmicat, "Underweight"))

# Any underweight men in the data?
# No
# 
# Any underweight former smokers of either sex? 
# No
#
# What happens to the (log) plasma beta-carotene levels when the BMI increases? 
# The levels are lower the higher the BMI
#
# Use the model to estimate the plasma beta-carotene level with CI and prediction 
# intervals for an underweight former smoking male of average age. Should we 
# rely on this estimate?
#
### Get the average age
summary(age)

### New dataframe with x0 = {average age} = 50, sex = Male, smokstat = Former,
### bmicat = Underweight
(log.betaplasma.x0 <- data.frame(age = c(50), sex = "Male", smokstat = "Former", bmicat = "Underweight"))


### Make prediction using the full model
(log.betaplasma.y0.pred <- cbind(
  log.betaplasma.x0,
  predict(full.model, log.betaplasma.x0, se.fit = TRUE)
)
)

### Confidence interval for age=50, sex=Male etc.
conf = predict(full.model, log.betaplasma.x0, interval = "confidence")


### Prediction interval for age=50, sex=Male, etc. 
### = the interval that will be expected to contain 95% of the observations
pred = predict(full.model, log.betaplasma.x0, interval = "prediction")

### Prediction, CI and prediction interval
exp(conf)
exp(pred)

### ANSWER
# The estimated level of plasma beta-carotene is 168.7177
# Confidence interval = [80.32119, 354.3979]
# Prediction interval = [35.65778, 798.3016]
# The intervals are very large and therefore the estimates should not be relied on

### PART 2
### (e) Use the continuous BMI-variable quetelet to fit the model
# Fit a model where plasma beta-carotene depend on age, sex, smokstat and quetelet
# Y_i = b0 + b1*age + b2*sex_male + b3*smokstat_Former + b4*smokstat_Current 
# + b5*quetelet 
(full.model.2 <- lm(log.betaplasma ~ age + sex + smokstat + quetelet, data = plasma.new))

# Get and save standard errors, etc
(full.summary.2 <- summary(full.model.2))

# Extract and save the sigma estimate = standard deviation from the summary
full.sigma.2 <- full.summary.2$sigma

# Parameter (beta) estimates and 95% confidence interval
full.summary.2$coefficients
confint(full.model.2)

# exp(beta) and exp(CI)
exp(full.summary.2$coefficients)
exp(confint(full.model.2))

# Did the parameters for the other variables change in any substantial way compared
# to model 1?

full.summary$coefficients
full.summary.2$coefficients


# The other parameters (age, sexMale, smokstatFormet, smokstarCurrent) did not
# change in any substantial way. The results are very similar. The P-value is a
# little lower for the parameters in model 2 which indicates that the model is 
# better since the parameter estimates are more significant


### Use model 1 and model 2 to estimate the average (median) plasma beta-carotene
### level in a man and in a woman, both 30-year old former smokers with a normal
### BMI of 20


### New dataframes with x0 =  30, sex = Male/Female, smokstat = Former, BMI = 20/ Normal
(log.bp.m.1.x0 <- data.frame(age = c(30), sex = "Male", smokstat = "Former", bmicat = "Normal"))
(log.bp.f.1.x0 <- data.frame(age = c(30), sex = "Female", smokstat = "Former", bmicat = "Normal"))

(log.bp.m.2.x0 <- data.frame(age = c(30), sex = "Male", smokstat = "Former", quetelet = 20))
(log.bp.f.2.x0 <- data.frame(age = c(30), sex = "Female", smokstat = "Former", quetelet = 20))


##### Sex = Male
### Prediction using model 1
(log.bp.m.y0.pred.1 <- cbind(
  log.bp.m.1.x0,
  predict(full.model, log.bp.m.1.x0, se.fit = TRUE)))
conf.1.1 = predict(full.model, log.bp.m.1.x0, interval = "confidence")
exp(conf.1.1)

### Prediction using model 2
(log.bp.m.y0.pred.2 <- cbind(
  log.bp.m.2.x0,
  predict(full.model.2, log.bp.m.2.x0, se.fit = TRUE)))
conf.1.2 = predict(full.model.2, log.bp.m.2.x0, interval = "confidence")
exp(conf.1.2)

#### Sex = Female
### Prediction using model 1
(log.bp.f.y0.pred.1 <- cbind(
  log.bp.f.1.x0,
  predict(full.model, log.bp.f.1.x0, se.fit = TRUE)))
conf.1.3 = predict(full.model, log.bp.f.1.x0, interval = "confidence")
exp(conf.1.3)

### Prediction using model 2
(log.bp.f.y0.pred.2 <- cbind(
  log.bp.f.2.x0,
  predict(full.model.2, log.bp.f.2.x0, se.fit = TRUE)))
conf.1.4 = predict(full.model.2, log.bp.f.2.x0, interval = "confidence")
exp(conf.1.4)

### RESULTS
### Estimates of plasma beta-carotene level in man and woman, 30 years old former 
### smoker with normal BMI (20)
#
## Men
# Model 1 estimate y_30 = 106.9427, CI = [79.55344, 143.7617]
# Model 2 estimate y_30 = 113.1035, CI = [84.21658, 151.8988]
#
## Women
# Model 1 estimate y_30 = 150.1146, CI = [124.8736, 180.4576]
# Model 2 estimate y_30 = 159.4749, CI = [132.4042, 192.0803]


### Explain why CI for men are wider than CI for women?
# Looking at the previous plot with BMI-Normal and Smoker-Former, there is a lot 
# of observations of (ish) 30 year old women and the spread of data is not so large, 
# compared to the spread of the data points of men within this group.

### How would expected plasma beta-carotene level change if we use BMI 32 (obese)
### instead of Normal (20) for the both models?

### New dataframes with x0 =  30, sex = Male/Female, smokstat = Former, BMI = 32/ Obese
(log.bp.m.1.x0 <- data.frame(age = c(30), sex = "Male", smokstat = "Former", bmicat = "Obese"))
(log.bp.f.1.x0 <- data.frame(age = c(30), sex = "Female", smokstat = "Former", bmicat = "Obese"))

(log.bp.m.2.x0 <- data.frame(age = c(30), sex = "Male", smokstat = "Former", quetelet = 32))
(log.bp.f.2.x0 <- data.frame(age = c(30), sex = "Female", smokstat = "Former", quetelet = 32))

##### Sex = Male
### Prediction using model 1
(log.bp.m.y0.pred.1 <- cbind(
  log.bp.m.1.x0,
  predict(full.model, log.bp.m.1.x0, se.fit = TRUE)))
conf.2.1 = predict(full.model, log.bp.m.1.x0, interval = "confidence")
exp(conf.2.1)

### Prediction using model 2
(log.bp.m.y0.pred.2 <- cbind(
  log.bp.m.2.x0,
  predict(full.model.2, log.bp.m.2.x0, se.fit = TRUE)))
conf.2.2 = predict(full.model.2, log.bp.m.2.x0, interval = "confidence")
exp(conf.2.2)

#### Sex = Female
### Prediction using model 1
(log.bp.f.y0.pred.1 <- cbind(
  log.bp.f.1.x0,
  predict(full.model, log.bp.f.1.x0, se.fit = TRUE)))
conf.2.3 = predict(full.model, log.bp.f.1.x0, interval = "confidence")
exp(conf.2.3)

### Prediction using model 2
(log.bp.f.y0.pred.2 <- cbind(
  log.bp.f.2.x0,
  predict(full.model.2, log.bp.f.2.x0, se.fit = TRUE)))
conf.2.4 = predict(full.model.2, log.bp.f.2.x0, interval = "confidence")
exp(conf.2.4)

### RESULTS
### Estimates of plasma beta-carotene level in man and woman, 30 years old former 
### smoker with high BMI-Obese (32)
#
## Men
# Model 1 estimate y_30 = 61.84001, CI = [44.70763, 85.53768]
# Model 2 estimate y_30 = 72.49152, CI = [54.25493, 96.85794]
#
## Women
# Model 1 estimate y_30 = 86.80432, CI= [68.84475, 109.449]
# Model 2 estimate y_30 = 102.2124, CI = [85.02067, 122.8803]

# Difference in plasma beta-carotene levels depending on BMI
## Men
# Model 1
exp(conf.1.1) - exp(conf.2.1)
# Model 2
exp(conf.1.2) - exp(conf.2.2)

## Women
# Model 1
exp(conf.1.3) - exp(conf.2.3)
# Model 2 
exp(conf.1.4) - exp(conf.2.4)

# The model will estimate the levels to be lower for high BMI
# Model 1 will estimate a larger difference between BMI Normal/ Obese than model 2

### Relative difference as a function of the relevant beta-parameters
# The models
# Y_1_{Obese} = b0 + b1*age + b2*SexMale + b3*SmokerFormer + b7*bmicatObese
# Y_1_{Normal} = b0 + b1*age + b2*SexMale + b3*SmokerFormer

# Y_2_{bmi=32} = b0 + b1*age + b2*SexMale + b3*SmokerFormer + b4*quetelet=32
# Y_2_{bmi=20} = b0 + b1*age + b2*SexMale + b3*SmokerFormer + b4*quetelet=20

### Express the relative difference as a function of the relevant b-parameters 
### and estimate it, with confidence intervals

# Model 1
# log(Y_1_{Obese}) - log(Y_1_{Normal}) = b7
# Men
conf.2.1 - conf.1.1
# b7 estimate = -0.54774261
# CI = [-0.5762849, -0.5192004]

# Model 1
# log(Y_1_{Obese}) - log(Y_1_{Normal}) = b7
# Women
conf.2.3 - conf.1.3
# b7 estimate = -0.5477426
# CI = [-0.5954483, -0.500037]

# Model 2
# Y_1_{bmi=32} - Y_1_{bmi=20} = b4*32 - b4*20 --> b4 = (Y_1_{bmi=32} - Y_1_{bmi=20})/(32-20)
# Men
full.summary.2
(conf.2.2 - conf.1.2)/12
# b4 estimate = -0.03706947
# CI = [-0.03664149, -0.03749745]


# Model 2
# Y_1_{bmi=32} - Y_1_{bmi=20} = b4*32 - b4*20 --> b4 = (Y_1_{bmi=32} - Y_1_{bmi=20})/(32-20)
# Women
full.summary.2
(conf.2.4 - conf.1.4)/12
# b4 estimate = -0.03706947
# CI = [ -0.03691374, -0.03722521]

full.model.2$coefficients/12
confint(full.model.2)/12

### PART 2
### (f) What happens if we use both quetelet and bmicat to fit the model?
# Fit a model where plasma beta-carotene depend on age, sex, smokstat, bmicat and quetelet
# Y_i = b0 + b1*age + b2*sex_male + b3*smokstat_Former + b4*smokstat_Current 
# + b5*quetelet + b6*bmicat_Underweight + b7*bmicat_Overweight + b8*bmicat*Obese

(full.model.3 <- lm(log.betaplasma ~ age + sex + smokstat + quetelet + bmicat, data = plasma.new))

# Get and save standard errors, etc
summary(full.model.3)

# t-test to see if quetelet is significant 

## Variable: quetelet
# Null hypothesis H_0: b5 = 0
# Test: t-test to test the effect of one variable
# Parameter: b5
# P-value = P(|t(305)| > -1.775) = 0.076828 !< 0.05 --> cannot reject H_0: b5 = 0
#
# Conclusion:
# Model will NOT be significantly BETTER with quetelet when we have bmicat
#
# Partial f-test to test if bmicat is significant
## Variable: bmicat
# Null hypothesis: b6 = b7 = b8 = 0
# Test: Partial F-test to test several parameters at the same time
reduce.bmicat.model.3 <- lm(log.betaplasma ~ age + sex + smokstat + quetelet, data = plasma.new)
anova(reduce.bmicat.model.3, full.model.3)
# F = 0.2659
# P = 0.85 !< 0.05 --> cannot reject H_0: b6 = b7 = b8 = 0
#
# Conclusion:
# Model will NOT be significantly BETTER with bmicat when we have quetelet

# Explain why it is problematic to use both variables in model?
# The parameters doesnt know about each other --> leads to overestimating/ 
# overcompensation (?)


### PART 3: Model validation and selection
### (a) Compare the two models from Part 2 and decide which one is better

## Model 1 
model.1 <- lm(log.betaplasma ~ age + sex + smokstat + bmicat, data = plasma.new)
sum.1 <- summary(model.1)

## Model 2
model.2 <- lm(log.betaplasma ~ age + sex + smokstat + quetelet, data = plasma.new)
sum.2 <- summary(model.2)

# Model 1 and 2 are not nested -> cannot use anova or Partial F-test.
# "Principle of parcimony": Select a model with small residual sum of squares with 
# as few parameters as possible (Lecture 6 slide 2-3)

## Calculating the coefficient of determination - R^2
## Because we have different number of parameters we look at the adjusted R^2 R_{adj}^2
(collect.R2s <- data.frame(
  nr = seq(1, 2),
  model = c("1.bmicat", "2.quetelet"),
  R2.adj = c(sum.1$adj.r.squared,
             sum.2$adj.r.squared)))

# The "best model" is the one with the highest R_{adj}^2, i.e. Model 2

## Model 2 as the "background model"
background.model <- lm(log.betaplasma ~ age + sex + smokstat + quetelet, data = plasma.new)

## Model with only age as the "age model"
age.model <- lm(log.betaplasma ~ age, data = plasma.new)


### PART 3
### (b) Check that the dietary.full x-variables are not highly correlated to each other

## Create dataframe with all continuous x-variables
contx <- plasma.new[, c("age", "quetelet", "calories", "fat", "fiber",
                    "alcohol", "cholesterol", "betadiet")]

## Calculate all pairwise correlations between them
(all.pairwise.cor <- cor(contx))

## Plot the correlations against each other
pairs(contx)

## All pairs where the correlation is stronger than +-0.7 and present plots for 
## these pairs
all.pairwise.cor[(all.pairwise.cor[,] > 0.7)]
all.pairwise.cor[(all.pairwise.cor[,] < -0.7)]

# Strong correlations (> 0.7)
# 1. fat and calories = 0.87087984
# 2. fat and cholesterol = 0.7040940
 
## Any other potential problems?
# 1. It seems to be a problem with the alcohol variable
alcohol <- plasma.new[, "alcohol"]
plot(alcohol, betaplasma)


# One observation is very off compared to the other and may disturb the data
alcohol.strange <- which(plasma.new$alcohol > 200)


# 2. Calories and fat also have observations that is off compared to the other
calories <- plasma.new[, "calories"]
plot(calories, betaplasma)

fat <- plasma.new[, "fat"]
plot(fat, betaplasma)

### Report a frequency table for the categorical value virtuse
vituse <- plasma.new[, "vituse"]
table(vituse)

## Frequencies:
# 1: 121
# 2: 82
# 3: 111
# The largest category should be the reference category, i.e. the category "1"

# Set vituse: 1 to reference
plasma.new$vituse <-
  factor(plasma.new$vituse,
         levels = c("1", "2", "3"))


### PART 3
### (c) Fit a model using all the dietaryfull variables
dietary.full.model <- lm(log.betaplasma ~ vituse + calories + fat + fiber + alcohol  
                      + cholesterol + betadiet, data = plasma.new)

summary(dietary.full.model)

## Leverage for this set of covariates 
(dietary.full.pred <- cbind(
  plasma.new,
  fit = predict(dietary.full.model),
  r = rstudent(dietary.full.model)))


dietary.full.pred[alcohol.strange, ]

(dietary.full.pred$v <- influence(dietary.full.model)$hat)
head(dietary.full.pred)
min(dietary.full.pred$v)

# n = number of observations = 314 
# p + 1 = number of beta parameters+1 = 9
length(dietary.full.model$coefficients)
largest.leverage <- which(dietary.full.pred$v > 0.8)

# Plot leverage against year
# with 1/n and 2(p+1)/n horizontal lines:
ggplot(
  cbind(dietary.full.pred), aes(x = log.betaplasma, y = v)) +
  geom_point() +
  geom_hline(yintercept = 1/nrow(plasma.new)) +
  geom_hline(yintercept = 2*length(dietary.full.model$coefficients)/nrow(plasma.new), 
             color = "red") +
  xlab("log(plasma beta-carotene)") +
  ylab("leverage") +
  labs(title = "Leverage, all dietary variables") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))


largest.leverage <- which(dietary.full.pred$v > 0.8)

## There is one observation with alarmingly high leverage > 0.8 (see plot)
## But high leverage doesnt mean that it has any influence
## Leverage = measure of how far away the independent variable values of an 
## observation are from those of the other observations

## Take away alcohol from model and plot leverage again
dietary.full.model.2 <- lm(log.betaplasma ~ vituse + calories + fat + fiber + cholesterol 
                      + betadiet, data = plasma.new)

dietary.full.pred.2 <- cbind(
  plasma.new,
  fit = predict(dietary.full.model),
  r = rstudent(dietary.full.model))

dietary.full.pred.2$v <- influence(dietary.full.model.2)$hat

ggplot(
  cbind(dietary.full.pred.2), aes(x = log.betaplasma, y = v)) +
  geom_point(width = 3) +
  geom_hline(yintercept = 1/nrow(plasma.new)) +
  geom_hline(yintercept = 2*length(dietary.full.model.2$coefficients)/nrow(plasma.new), 
             color = "red") +
  labs(title = "Pb: leverage vs Pb") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

alcohol <- plasma.new[, "alcohol"]
log.alcohol <- log(alcohol)

# Look at alcohol and betaplasma
plot(alcohol, log.betaplasma)
plot(log.alcohol, log.betaplasma)
table(alcohol)

# The log(alcohol) plot look better
# There are 110 observations (out of 314) consuming NO alcohol at all

# Look at alcohol against the other variables to see if the extreme observation
# is extreme in any other way aswell

fiber <- plasma.new[, "fiber"]
cholesterol <- plasma.new[, "cholesterol"]
betadiet <- plasma.new[, "betadiet"]

ggplot(
  cbind(dietary.full.pred.2), aes(x = alcohol, y = betaplasma)) +
  geom_point(width = 3) +
  labs(title = "plasma beta-carotene vs alcohol") +
  geom_point(data = dietary.full.pred[largest.leverage,], 
             color = "red", size = 4, shape = 24) +
  xlab("alcohol consumption [mg/day]") +
  ylab("plasma beta-carotene [ng/ml] ") +
  theme(text = element_text(size = 18))


plot(alcohol, vituse)
ggplot(alcohol, calories)
plot(alcohol, fat)
plot(alcohol, fiber)
plot(alcohol, cholesterol)
plot(alcohol, betadiet)

# The observation has an extreme intake of calories compared to other
# Pretty high fat and cholesterol as well
# Mark this strange observation


### PART 3
### (d) Visual inspection of the studentized residuals

# plot residuals
ggplot(dietary.full.pred, aes(x = fit, y = r)) +
  geom_point(size = 3) +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_smooth() +
  geom_point(data = dietary.full.pred[largest.leverage,], 
             color = "red", size = 4, shape = 24) +
  labs(title = "Residuals vs fitted values") +
  geom_point(data = dietary.full.pred[abs(dietary.full.pred$r) > 3,], 
             color = "red", size = 3) +
  xlab("fitted values log(plasma beta-carotene)") +
  ylab("studentized residuals") +
  theme(text = element_text(size = 18))

# The alcohol observation doesnt have a large studentized residual 
# Student residuals = Important technique to detect outliers = Form of student's 
# t-statistic, with the estimate of error varying between points

# We only have one problematic observation in terms of student residuals. It indicated
# that we have the right model

# Identify the observation with unusually high studentized resiudal
largest.student <- which(abs(dietary.full.pred$r) > 3)


### PART 3 
### (e) Visual inspection of the Cook's distance

dietary.full.pred$D <- cooks.distance(dietary.full.model)
head(dietary.full.pred)

summary(dietary.full.model)

(f1 <- length(dietary.full.model$coefficients))
(f2 <- dietary.full.model$df.residual)
(cook.limit <- qf(0.5, f1, f2))
ggplot(dietary.full.pred, aes(fit, D)) + 
  geom_point(size = 3) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_hline(yintercept = cook.limit, color = "red") +
  geom_point(data = dietary.full.pred[largest.student,], 
                       color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[dietary.full.pred$D > 0.05,],
            color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 4/nrow(dietary.full.pred), linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Cook's D") +
  labs(caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)") +
  theme(text = element_text(size = 18))

# Observations can be considered to have a high influence on the estimation if
# D_I > Solid line, which none of the observations are

ggplot(dietary.full.pred, aes(fit, D)) + 
  geom_point(size = 3) +
  geom_point(data = dietary.full.pred[dietary.full.pred$alcohol > 200, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[abs(dietary.full.pred$r) > 3,], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[dietary.full.pred$D > 0.05,],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 4/nrow(dietary.full.pred), linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Cook's D") +
  labs(caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)") +
  theme(text = element_text(size = 18))

# If D_i < dashed there is no problem with that observation having high influence
# observations that have Di considerably higher than the rest may be problematic
# The high alcohol observations doesnt have a large impact, the one with high studentized
# residual have higher influence

## Observations with largest Cook's D
largest.cooks <- which(dietary.full.pred$D > 0.05)
largest.cooks

plasma.new[36,]
summary(cholesterol)
summary(betaplasma)
## DFBETAS 
# DFBETAS####
head(dfbetas(dietary.full.model))
dietary.full.pred$df0 <- dfbetas(dietary.full.model)[, "(Intercept)"]
dietary.full.pred$df1 <- dfbetas(dietary.full.model)[, "vituse2"]
dietary.full.pred$df2 <- dfbetas(dietary.full.model)[, "vituse3"]
dietary.full.pred$df3 <- dfbetas(dietary.full.model)[, "calories"]
dietary.full.pred$df4 <- dfbetas(dietary.full.model)[, "fat"]
dietary.full.pred$df5 <- dfbetas(dietary.full.model)[, "fiber"]
dietary.full.pred$df6 <- dfbetas(dietary.full.model)[, "alcohol"]
dietary.full.pred$df7 <- dfbetas(dietary.full.model)[, "cholesterol"]
dietary.full.pred$df8 <- dfbetas(dietary.full.model)[, "betadiet"]

#(dietary.full.pred[, dietary.full.pred$vituse2 ])

#dfbetas for beta_0:
ggplot(dietary.full.pred, aes(x = fit, y = df0)) +
  geom_point(size = 2) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[largest.student, ], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[largest.cooks, ],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(plasma.new))*c(-1, 1), color = "red", linetype = "dashed") +
  ylab("DFBETAS_0(i)") +
  xlab("Fitted values") +
  labs(title = "DFBETAS_0: impact on the intercept") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# beta0: No influence detected


# dfbetas for beta1:
vituse <- dietary.full.full.pred[dietary.full.pred$vituse, ]
ggplot(dietary.full.pred, aes(x = vituse, y = df1)) +
  geom_point(size = 2) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[largest.student, ], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[largest.cooks, ],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(plasma.new))*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("DFBETAS_1(i)") +
  labs(title = "impact on vituse: Yes, not often") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# beta1: Influence detected from largest leverage


# dfbetas for beta2:
ggplot(dietary.full.pred, aes(x = vituse, y = df2)) +
  geom_point(size = 2) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[largest.student, ], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[largest.cooks, ],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(plasma.new))*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("DFBETAS_2(i)") +
  labs(title = "impact on vituse: No") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# beta2: Influence detected from largest Cook's D


# dfbetas for beta3:
calories <- dietary.full.pred[dietary.full.pred$calories, ]

ggplot(dietary.full.pred, aes(x = calories, y = df3)) +
  geom_point(size = 2) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[largest.student, ], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[largest.cooks, ],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(plasma.new))*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("DFBETAS_3(i)") +
  labs(title = "impact on calories") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# beta3: Influence detected from largest cooks d


# dfbetas for beta4:
fat <- dietary.full.pred[dietary.full.pred$fat, ]
ggplot(dietary.full.pred, aes(x = fat, y = df4)) +
  geom_point(size = 2) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[largest.student, ], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[largest.cooks, ],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(plasma.new))*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("DFBETAS_4(i)") +
  labs(title = "impact on fat") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# beta4: Influence detected from largest cooks d


# dfbetas for beta5:
fiber <- dietary.full.pred[dietary.full.pred$fiber, ]
ggplot(dietary.full.pred, aes(x = fiber, y = df5)) +
  geom_point(size = 2) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[largest.student, ], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[largest.cooks, ],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(plasma.new))*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("DFBETAS_1(i)") +
  labs(title = "DFBETAS_5: impact on fiber") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# beta5: No influence detected


# dfbetas for beta6:
alcohol <- dietary.full.pred[dietary.full.pred$alcohol, ]
ggplot(dietary.full.pred, aes(x = alcohol, y = df6)) +
  geom_point(size = 2) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[largest.student, ], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[largest.cooks, ],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(plasma.new))*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("DFBETAS_6(i)") +
  labs(title = "impact on alcohol") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# beta6: Influence detected from largest cooks d and largest students residual


# dfbetas for beta7:
cholesterol <- dietary.full.pred[dietary.full.pred$cholesterol, ]
ggplot(dietary.full.pred, aes(x = cholesterol, y = df7)) +
  geom_point(size = 2) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[largest.student, ], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[largest.cooks, ],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(plasma.new))*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("DFBETAS_7(i)") +
  labs(title = "DFBETAS_7: impact on cholesterol") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# beta7: No influence detected


# dfbetas for beta8:
betadiet <- dietary.full.pred[dietary.full.pred$betadiet, ]
ggplot(dietary.full.pred, aes(x = betadiet, y = df8)) +
  geom_point(size = 2) +
  geom_point(data = dietary.full.pred[largest.leverage, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = dietary.full.pred[largest.student, ], 
             color = "red", size = 4, shape = 20) +
  geom_point(data = dietary.full.pred[largest.cooks, ],
             color = "red", size = 4, shape = 6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(plasma.new))*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("DFBETAS_8(i)") +
  labs(title = "impact on dietary beta-carotene") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# beta8: Influence detected from largest cooks d


# The largest cooks D influence the most parameters. Largest leverage and largest 
# students residual only influence one parameter each


### PART 3
### (f) Backward elimination to reduce the model (determine which variables should 
### be in the model)

## Backward elimination
## Default BIC (k = 2):
dietary.model.1 <- step(dietary.full.model)

# Model with vituse, calories, fiber, alcohol, betadiet
summary(dietary.model.1)
confint(dietary.model.1)


## BIC (k = ln(n)):
dietary.model.2 <- step(dietary.full.model, k = log(nrow(plasma.new)))

# Model with vituse, calories, fiber
summary(dietary.model.2)
confint(dietary.model.2)

# choosing the BIC model, dietary.model.2 because 
# 1. Simpler (less parameters)
# 2. All parameters are significant (t-test) except 1, other model had 3 non 
# significant 

# Compare to 3(b) findings: Strong correlations (> 0.7) for
# 1. fat and calories = 0.87087984
# 2. fat and cholesterol = 0.7040940
#
# Therefor, fat is not in the model

## Choosing model 2 as dietary model
dietary.model <- dietary.model.2

## Create table with dietary model beta estimates, exp(beta estimates) and CI
beta.dietary <- cbind(summary(dietary.model)$coefficients,
                  ci = confint(dietary.model), 
                  exp.beta = exp(summary(dietary.model)$coefficients), 
                  exp.conf = exp(confint(dietary.model)))
colnames(beta.dietary) <- c('Estimate','Std. Error','t.value', '..', '2.5%', '97.5 %',
                            'exp(Estimate)', '..', '..', '..', 'exp(2.5 %)', 'exp(97.5 %)')
round(beta.dietary[, c(1, 2, 3, 4, 5, 6, 7, 11, 12)], digits = 3)


### PART 3
### (g) Combine Background and Dietary model using stepwise procedure

# Start with the Dietary model
# Specify the scope
# lower = smallest model allowed = null model = intercept only 
null.model <- lm(log.betaplasma ~ 1, data = plasma.new)
# upper = largest model allowed = all variables present in Background or Dietary model
summary(background.model)
summary(dietary.model)

full.model.upr <- lm(log.betaplasma ~ age + sex + smokstat + quetelet + vituse
                     + calories +fiber, data = plasma.new)


## Create the stepwise AIC model
step.AIC <- step(dietary.model, 
        scope = list(lower = null.model, upper = full.model.upr),
        direction = "both")


## Create the stepwise BIC model
step.BIC <- step(dietary.model, 
     scope = list(lower = null.model, upper = full.model.upr),
     direction = "both",
     k = log(nrow(plasma.new)))


## Table exp(beta) and 95% CI intervals
## Create table with dietary model beta estimates, exp(beta estimates) and CI
beta.step.AIC <- cbind(exp.beta = exp(summary(step.AIC)$coefficients), 
                      exp.conf = exp(confint(step.AIC)))
colnames(beta.step.AIC) <- c('exp(Estimate)', '..', '..', '..', 'exp(2.5 %)', 'exp(97.5 %)')
beta.step.AIC[, c(1, 5, 6)]

round(beta.step.AIC[, c(1, 5, 6)], digits = 3)

## Table exp(beta) and 95% CI intervals
## Create table with dietary model beta estimates, exp(beta estimates) and CI
beta.step.AIC <- cbind(exp.beta = summary(step.AIC)$coefficients, 
                       exp.conf = confint(step.AIC))
beta.step.AIC[, c(1, 5, 6)]

round(beta.step.AIC[, ], digits = 3)


## Table exp(beta) and 95% CI intervals
## Create table with dietary model beta estimates, exp(beta estimates) and CI
beta.step.BIC <- cbind(exp.beta = exp(summary(step.BIC)$coefficients), 
                       exp.conf = exp(confint(step.BIC)))
colnames(beta.step.BIC) <- c('exp(Estimate)', '..', '..', '..', 'exp(2.5 %)', 'exp(97.5 %)')
beta.step.BIC[, c(1, 5, 6)]

round(beta.step.BIC[, c(1, 5, 6)], digits = 3)


# Table exp(beta) and 95% CI intervals
## Create table with dietary model beta estimates, exp(beta estimates) and CI
beta.step.BIC <- cbind(exp.beta = summary(step.BIC)$coefficients, 
                       exp.conf = confint(step.BIC))
beta.step.BIC[, c(1, 5, 6)]

round(beta.step.BIC[, ], digits = 3)

beta.step.BIC <- rbind(a = beta.step.BIC,
                       smokstatFormer = 0, 
                       'smokstatCurrent Smoker' = 0,
                       age = 0,
                       sexMale = 0)
beta.step <- cbind(a = beta.step.BIC, 
                   b = beta.step.AIC)

colnames(beta.step) <- c('BIC exp(Estimate)', '..', '..', '..', 'BIC exp(2.5 %)', 'BIC exp(97.5 %)',
                         'AIC exp(Estimate)', '..', '..', '..', 'AIC exp(2.5 %)', 'AIC exp(97.5 %)')
round(beta.step[, c(1, 5, 6, 7, 11, 12)], digits = 3)

## Findings: 
# 1. Intercept differs 
# 2. BIC has larger 95% CI than AIC on the common parameters except intercept
# 3. The common parameters have very similar estimates even though AIC has a lot 
# more parameters


### PART 3
### (h) Compare the five models: Age, Background, Dietary, Step AIC, Step BIC in 
### a table presenting R^2 and R_{adj}^2

sum.age <- summary(age.model)
sum.background <- summary(background.model)
sum.dietary <- summary(dietary.model)
sum.step.AIC <- summary(step.AIC)
sum.step.BIC <- summary(step.BIC)

# collect the R2-s 
(collect.R2s <- data.frame(
  model = c("age", "background", "dietary", "step.AIC", "step.BIC"),
  R2 = c(sum.age$r.squared,
         sum.background$r.squared,
         sum.dietary$r.squared, 
         sum.step.AIC$r.squared, 
         sum.step.BIC$r.squared),
  R2.adj = c(sum.age$adj.r.squared,
             sum.background$adj.r.squared,
             sum.dietary$adj.r.squared, 
             sum.step.AIC$adj.r.squared, 
             sum.step.BIC$adj.r.squared)))

## How much of the variability of log plasma beta-carotene can be explained using 
## only the background variables?
# 16 % 
#
## Using only the dietary variables?
# 14 % 
#
## Which model is best according to R2adj?
# The Step AIC model is the best, highest R2adj

## Final model = Step AIC model
