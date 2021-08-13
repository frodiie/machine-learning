library(ggplot2)
library(pROC)
library(ResourceSelection)


plasma <- read.delim("Data/plasma.txt")     # Load in data
head(plasma)                                
summary(plasma)


### PART 0
###   - Creating a variable for low plasma beta-carotene
###   (a) Calculate cut-off value a

## Known:
# Cut off value = 0.42 [micro*mol/litre] 
# Molar mass of beta-carotene = 536.888 g/mol 

## Translate the cut-off value into unit [ng/ml]
#    - formula:
# (0.42 [micro *mol/litre] * 536.888 [g/mol] * 10e3) / (litre *10e3) = a [ng/ml]

a <- 0.42 * 536.888       # Cut-off value a = 225.493 [ng/ml]

### PART 0
###   (b) Create new variable lowplasma that is 1 when betaplasma < a ng/ml, and
###       0 otherwise

plasma$lowplasma <- as.numeric(plasma$betaplasma < a) # New variable lowplasma

plasma$plasmacat <- factor(plasma$lowplasma,          # Separate factor version 
                           levels = c(0, 1),          # of variable
                           labels = c("high", "low")) # 0 = high, 1 = low


### PART 1
###   - Introduction to logistic regression
###   (a) Relationship between low beta-carotene and smoking status

(table.1 <- table(plasma$smokstat, plasma$lowplasma)) # Cross-tabulation


## Estimate probabilities for having low beta-carotene for each smoking category 
## Pr(Y_i = 1) = p_j = (nbr of low level observation) / (all observation)


p1 = table.1[1,2] / (table.1[1,2] + table.1[1,1])  # probability: low & smokstat1
p2 = table.1[2,2] / (table.1[2,1] + table.1[2,2]) # probability: low & smokstat2
p3 = table.1[3,2] / (table.1[3,1] + table.1[3,2]) # probability: low & smokstat3
p <- c(p1, p2, p3)    # Probability vector
table.1 <- cbind(table.1, p = p)     # add probabilities to table


## Estimate odds for having low beta-carotene for each smoking category 
## odds_j = p_j / (1 - p_j)

odds1 = p1 / (1 - p1)    # odds: low & smokstat1
odds2 = p2 / (1 - p2)    # odds: low & smokstat2  
odds3 = p3 / (1 - p3)    # odds: low & smokstat3
odds <- c(odds1, odds2, odds3)    # odds vector
table.1 <- cbind(table.1, odds = odds)    # add odds to table


## Calculate odds ratios for each of the categories, compared to a suitable 
## reference category

# Choosing the largest and most normative group as reference category, 
# i.e. smokstat1 = no smoking
# OR = odds_i / odds_ref

OR1 = odds1 / odds1    # OR: low & smokstat1. 1.0 (reference)
OR2 = odds2 / odds1    # OR: low & smokstat2. 1.305916 (risk of low increase with 31%)
OR3 = odds3 / odds1    # OR: low & smokstat2. 5.871560 (risk of low increase with 487%)

OR <- c(OR1, OR2, OR3)                # odds ratio vector
table.1 <- cbind(table.1, OR = OR)    # add OR to table

table.1 # probability, odds and OR table


## Fit a logistic regression model for lowplasma with smokstat as explanatory 
## variable

plasma$smokstat <- factor(plasma$smokstat,         # Change smoking status to 
                              levels = c(1, 2, 3), # categorical variable 
                              labels = c("1", "2", "3")) # smokstat1 = reference

(model.1 <- glm(lowplasma ~ smokstat, family = "binomial", data = plasma))

## beta-estimates and CI
model.1.sum <- summary(model.1)    # summary of model
model.1.sum$coefficients           # extract parameter estimates
confint(model.1)                   # get confidence interval

## exp(beta)-estimates and CI
exp(model.1.sum$coefficients)      # extract parameter estimates
exp(confint(model.1))              # get confidence interval


## Express the odds and the probabilities for each of the categories as functions 
## of the beta-parameters

# odds_i = exp(beta_0 + beta_1*smokstat2 + beta_2*smokstat3)
# p_i = odds_i / (1 + odds_i)

beta_0 = model.1.sum$coefficients[1,1]   # get parameter beta_0
beta_1 = model.1.sum$coefficients[2,1]   # get parameter beta_1
beta_2 = model.1.sum$coefficients[3,1]   # get parameter beta_2

odds_1 = exp(beta_0 + beta_1*0 + beta_2*0)   # odds_i as function of parameters
odds_2 = exp(beta_0 + beta_1*1 + beta_2*0)
odds_3 = exp(beta_0 + beta_1*0 + beta_2*1)

p_1 = odds_1 / (1 + odds_1)    # p_i as function of parameters
p_2 = odds_2 / (1 + odds_2)
p_3 = odds_3 / (1 + odds_3)

##    - odds and probabilities same as in table.1


## Use model.1 to estimate the probability of having a low plasma beta-carotene 
## concentration for each smoking category, together with 95% CI

# estimate probabilities phat
pred.1 <- cbind(
  plasma,
  phat = predict(model.1, type = "response")) # phat = estimated probabilities p

pred.1
# get log-odds with s.e. to construct C.I
pred.1 <- cbind(
  pred.1,
  logit = predict(model.1, se.fit = TRUE)) # logit = logodds with s.e.

# calculate CI for log-odds
(lambda <- qnorm(1 - 0.05/2))    # standard normal quantile
pred.1$logit.lwr <- pred.1$logit.fit - lambda*pred.1$logit.se.fit
pred.1$logit.upr <- pred.1$logit.fit + lambda*pred.1$logit.se.fit
head(pred.1)

# transform the log-odds intervals into C.I. for odds
pred.1$odds.lwr <- exp(pred.1$logit.lwr)
pred.1$odds.upr <- exp(pred.1$logit.upr)
head(pred.1)

# transform the odds intervals into C.I. for p####
pred.1$p.lwr <- pred.1$odds.lwr/(1 + pred.1$odds.lwr)
pred.1$p.upr <- pred.1$odds.upr/(1 + pred.1$odds.upr)
head(pred.1)

pred.1    # Look at all predictions

## Smoking category, p_hat, CI
## 1, 0.6942675, [0.6178961, 0.7612720]
## 2, 0.7478261, [0.6606474, 0.8187531]
## 3, 0.9302326, [0.8048702, 0.9773242]


## Test to determine whether there are any significant differences 
## between smoking categories in the model

summary(model.1)$coefficients

## Likelihood ratio global test

# Null model
(model.0 <- glm(lowplasma ~ 1, family = "binomial", data = plasma))

(sum.model.1 <- summary(model.1))
(beta.model.1 <- cbind(model.1$coefficients, ci = confint(model.1)))
round(exp(beta.model.1), digits = 2)

# Average Y:
mean(plasma$lowplasma)
# or prediction in the null model;
predict(model.0, data.frame(x = NA), type = "response")

# compare with Null model using the summary output:
(D_diff <- sum.model.1$null.deviance - sum.model.1$deviance)
(df_diff <- sum.model.1$df.null - sum.model.1$df.residual)

# compare with Null model using the anova funktion:
(anova.0.model.1 <- anova(model.0, model.1))
(D_diff <- anova.0.model.1$Deviance[2])
(df_diff <- anova.0.model.1$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, df_diff)
# or P-value:
pchisq(D_diff, df_diff, lower.tail = FALSE)


# - Test type: Wald test for beta_j
# - Null hypothesis H0: beta_1 = 0
# - z-value_1 = |0.97| >! lambda_0.025 = 1.96
# - Distribution: Z ~ N(0,1)
# - P-value_1 = 3.33e-01 > 0.05
# - Cannot reject H0
# - Conclusion: The category smokstat2 does not have a significant impact on the 
#   probability of low beta-carotene levels compared to smokstat1

# - Test type: Wald test for beta_j
# - H0: beta_2 = 0
# - z_value_2 = |2.84| > lambda_0.025 = 1.96
# - Distribution: Z ~ N(0,1)
# - P-value_2 = 4.504e-03 < 0.05
# - Reject H0
# - Conclusion: The category smokstat3 have a significant impact on the 
#   probability of low beta-carotene levels compared to smokstat1

# - Why Wald? Wald test tell us if variable x_j (which category) have a significant effect 
# on the probability of low levels on beta-carotene, i.e., does it change the 
# log-odds of having low levels?


### PART 1
###   (b) Relationship between lowplasma and age

## plot lowplasma against age with moving average
ggplot(plasma, aes(age, lowplasma)) +
  geom_point() +                                  # Add data points
  geom_smooth(se = FALSE, linetype = "dashed") +  # Add moving average
  xlab("age") +
  ylab("lowplasma" ) +
  labs(title = "plasma beta-carotene levels vs age", 
       subtitle = "low levels (=1), high levels (=0)", 
       caption = "blue dashed = moving average") +
  theme(text = element_text(size = 16))

# plot show that:
#   - Probability of having low plasma beta-carotene levels decrease with age
#   - In Project 1: higher age corresponds to increase in beta-carotene levels
#   i.e. foundings here agrees with foundings in Project 2


## fit a logistic regression for lowplasma as a function of age
(age.model <- glm(lowplasma ~ age, family = "binomial", data = plasma))

## beta-estimates and CI
age.model.sum <- summary(age.model)  # summary of Age model
age.model.sum$coefficients           # extract parameter estimates
confint(age.model)                   # get confidence interval

## exp(beta)-estimates and CI
exp(age.model.sum$coefficients)      # extract parameter estimates
exp(confint(age.model))              # get confidence interval


## test if probability of having low plasma beta-carotene changes with age
age.model.sum$coefficients

# - Test type: Wald test for beta_age
# - Null hypothesis H0: beta_age = 0
# - z-value = |-3.17| > lambda_0.025 = 1.96
# - Distribution: Z ~ N(0,1)
# - P-value = 1.537e-03 < 0.05
# - Reject H0

# - Conclusion: probability of having low plasma beta-carotene changes with age

# - Why Wald? Wald test tell us if variable x_age have a significant effect 
# on the probability of low levels on beta-carotene, i.e., does it change the 
# log-odds of having low levels?


## what happens to odds of having low levels when age increase?
##   - increase with 1 year

exp(age.model.sum$coefficients)      # extract parameter estimates
exp(confint(age.model))              # get confidence interval


# Odds ratio = OR = exp(beta0 + beta1(age + 1))/exp(beta0 + beta1(age))
#                 = exp(beta_1) = 0.97184

(1 - 0.97184)*100      # decrease in %
(1 - 0.9545734)*100    # lwr
(1 - 0.9890133)*100    # upr

#    - OR = 0.97184, CI= [0.9545734, 0.9890133]
#    - Odds of having low levels decrease with 3 % when age increase +1 year
#    - CI = (decrease) [1 %, 5 %]


## what happens to odds of having low levels when age increase?
##   - increase with 10 year

(age.model.sum$coefficients)      # extract parameter estimates
confint(age.model)                # get confidence interval

# Odds ratio = OR = exp(beta0 + beta1(age + 10))/exp(beta0 + beta1(age))
#                 = exp(10*beta_1)

exp(10*(-0.02856521))    # calculate OR
exp(10*(-0.0464907))     # lwr
exp(10*(-0.01104754))    # upr


(1 - 0.751524)*100        # decrease in %
(1 - 0.6281935)*100       # lwr
(1 - 0.8954084)*100       # upr


#    - OR = 0.751524, CI= [0.6281935, 0.8954084]
#    - Odds of having low levels decrease with 25 % when age increase +1 year
#    - CI = (decrease) [11 %, 37 %]


## - plot lowplasma against age together with predicted probabilities and 95% CI

## predict using Age model
age.model.pred <- cbind(                          # predict for plotting
  plasma,                                         # phat = estimated probabilities p
  phat = predict(age.model, type = "response"))  

age.model.pred <- cbind(                          # logit = logodds with s.e. 
  age.model.pred,                                 # for constructing C.I.
  logit = predict(age.model, se.fit = TRUE))

(age.model.pred$logit.residual.scale <- NULL)     # remove unnecessary variable

(lambda <- qnorm(1 - 0.05/2))                     # standard normal quantile

age.model.pred$logit.lwr <- age.model.pred$logit.fit - 
  lambda*age.model.pred$logit.se.fit              # CI for log odds
age.model.pred$logit.upr <- age.model.pred$logit.fit + 
  lambda*age.model.pred$logit.se.fit

age.model.pred$odds.lwr <-                        # transform log-odds intervals 
  exp(age.model.pred$logit.lwr)                   # into C.I. for odds
age.model.pred$odds.upr <- exp(age.model.pred$logit.upr)

age.model.pred$p.lwr <- age.model.pred$odds.lwr / # transform the odds intervals
  (1 + age.model.pred$odds.lwr)                   # into C.I. for p
age.model.pred$p.upr <- age.model.pred$odds.upr /
  (1 + age.model.pred$odds.upr)
head(age.model.pred)

(age.plot <- ggplot(age.model.pred, aes(age, lowplasma)) +     
  geom_point() +                             
  geom_line(aes(y = phat), color = "red", size = 1) +         # plot predicted p
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) + # plot CI
  xlab("age") +
  ylab("lowplasma") +
  labs(title = "Plasma beta-carotene levels vs age", 
       subtitle = "Low levels (=1), High levels (=0)",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 16)))


## - Calculate the difference by subtraction probability of having low plasma 
##   beta-carotene for 30- and 31-year-old and 70- and 71-year-old

beta0 <- age.model.sum$coefficients[1, 1]      # save parameter estimates
beta1 <- age.model.sum$coefficients[2, 1]      # save parameter estimates

(delta_p_31_30 = exp(beta0 + beta1*31)/(1 + exp(beta0 + beta1*31)) - 
    exp(beta0 + beta1*30)/(exp(beta0 + beta1*30) + 1))
# Probability will decrease with 0.4%

(delta_p_71_70 = exp(beta0 + beta1*71)/(1 + exp(beta0 + beta1*71)) - 
    exp(beta0 + beta1*70)/(exp(beta0 + beta1*70) + 1))
# Probability will decrease with 0.7% 

##    - The slope is steeper for a 70 year old (more change in probability)


### PART 1
###   (c) Leverage for the Age model, logistic and linear

## linear regression model 
linear.age.model <- lm(lowplasma ~ age, data = plasma)

## leverage values for logistic and linear Age model
leverage.values <- cbind(plasma,   
                         v.log = influence(age.model)$hat,
                         v.lin = influence(linear.age.model)$hat)

## plot
(plot.v <- ggplot(leverage.values, aes(x = age, y = v.log, colour = "logistic")) +    # plot against age
    geom_point() +
    geom_point(aes(age, v.lin, colour = "linear")) +
    geom_hline(yintercept = 2*length(age.model$coefficients)/nrow(plasma), 
               color = "red", size = 1) +      # suitable line
    labs(title = "leverage vs age",
         caption = "2(p+1)/n in red", 
         color = "model") +
    xlab("age") +
    expand_limits(y = c(0,0.03)) +
    ylab("v") +
    theme(text = element_text(size = 16)))


# Leverage measures how far away values of observation are from those of the o
# other observations 
#    - Both models: high leverage for age 75 and older. Linear model a bit better
#    - Logistic model have lower leverage for young people, age < 25 
# 
#    - looking at leverage, logistic model performs better for young people. 
#      for the other ages the two models perform quite similar

## Identify observations with the highest leverage

max.leverage <- max(leverage.values$v.log)
(highest.leverage <- which(leverage.values$v.log == max.leverage))
leverage.values[highest.leverage,]    # all 3 observations with highest leverage 
                                      # have age = 83 

## plot
(age.plot <- ggplot(age.model.pred, aes(age, lowplasma)) +     
    geom_point() +                             
    geom_line(aes(y = phat), color = "red", size = 1) +         # plot predicted p
    geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) + # plot CI
    geom_point(data = leverage.values[highest.leverage,],
                  color = "red", size = 5, shape = 24) +
    xlab("age") +
    ylab("lowplasma") +
    labs(title = "plasma beta-carotene levels vs age", 
         subtitle = "low levels (=1), high levels (=0)",
         caption = "red = fitted line, with 95% confidence interval 
         red triangle = highest leverage observations") +
    theme(text = element_text(size = 16)))


### PART 1
###   (d) Standardized deviance residuals for the Age model

## calculate standardized deviance residuals

age.model.pred <-  cbind(plasma,
                         v = influence(age.model)$hat)
age.model.pred$devres <- influence(age.model)$dev.res
age.model.pred$devstd <- age.model.pred$devres/sqrt(1 - age.model.pred$v)
head(age.model.pred)


## plot
ggplot(age.model.pred, aes(age, devstd, color = as.factor(lowplasma))) +
  geom_point() +
  geom_point(data = age.model.pred[highest.leverage,],   # Mark highest leverage
             color = "red", size = 4, shape = 24) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "standardized deviance residuals vs age",
       color = "levels",
       subtitle = "low levels (=1), high levels (=0)") +
  theme(text = element_text(size = 16))

## qq plot

ggplot(age.model.pred, aes(sample = devstd)) +
  geom_qq() + geom_qq_line()

## Identify the observation with largest residual

max.res <- min(age.model.pred$devstd)
largest.res <- which(age.model.pred$devstd == max.res)
age.model.pred[largest.res,]

## plot
(age.plot <- ggplot(age.model.pred, aes(age, lowplasma)) +     
    geom_point() +                             
    geom_line(aes(y = phat), color = "red", size = 1) +         # plot predicted p
    geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) + # plot CI
    geom_point(data = age.model.pred[largest.res,],
               color = "red", size = 4, shape = 24) +
    xlab("age") +
    ylab("lowplasma") +
    labs(title = "Plasma beta-carotene levels vs age", 
         subtitle = "low levels (=1), high levels (=0)",
         caption = "red = fitted line, with 95% confidence interval 
         triangle = highest leverage observations") +
    theme(text = element_text(size = 16)))


### PART 1
###   (e) Cook's distance for the Age model

age.model.pred$Dcook <- cooks.distance(age.model)
head(age.model.pred)

## plot
ggplot(age.model.pred, aes(age, Dcook, color = as.factor(lowplasma))) +
  geom_point() +
  geom_hline(yintercept = 4/nrow(plasma), linetype = "dotted",
             size = 1) +
  geom_point(data = age.model.pred[highest.leverage,],   # Mark highest leverage
             color = "red", size = 4, shape = 6) +
  geom_point(data = age.model.pred[largest.res,],
             color = "red", size = 4, shape = 2) +
  xlab("age") +
  ylab("Cook's D") +
  labs(title = "Cook's distance vs age",
       color = "levels",
       subtitle = "low levels (=1), high levels (=0)",
       caption = "4/n in black
       triangle = largest residual observation
       downward triangle = highest leverage observations") +
  theme(text = element_text(size = 16))



### PART 2
###   - Multiple logistic regression and model selection
###   (a) Create the Background Model

## Create null model
(null.model <- glm(lowplasma ~ 1, family = "binomial", data = plasma))

## Create largest model allowed = all background variables
(full.background <- glm(lowplasma ~ age + sex + smokstat + quetelet, 
                       family = "binomial", data = plasma))

## Forward selection using AIC
(background.model <- step(null.model, 
     scope = list(upper = full.background), 
     direction = "forward"))

# AIC: Start with null model. In each step, add the variable outside the model 
# whose inclusion would cause the largest decrease in AIC. If such a variable 
# exists, add it and continue selecting, otherwise STOP

# - The order in which the variables entered and how AIC changed
# Start: lowplasma  ~ 1. AIC=358.99
# 1. quetelet added. AIC = 347.79
# 2. smokstat added. AIC = 336.92
# 3. age added. AIC = 330.69

## beta-estimates and CI
background.model.sum <- summary(background.model)    # summary of model
background.model.sum$coefficients           # extract parameter estimates
confint(background.model)                   # get confidence interval

## exp(beta)-estimates and CI
exp(background.model.sum$coefficients)      # extract parameter estimates
exp(confint(background.model))              # get confidence interval

## Divide age into 3 categories
plasma$agecat <- cut(plasma$age, breaks = c(0, 40, 55, 100))


## Predict probabilities 
## - plot lowplasma against age together with predicted probabilities and 95% CI

## predict using Age model
background.model.pred <- cbind(                          # predict for plotting
  plasma,                                         # phat = estimated probabilities p
  phat = predict(background.model, type = "response"))  

background.model.pred <- cbind(                          # logit = logodds with s.e. 
  background.model.pred,                                 # for constructing C.I.
  logit = predict(background.model, se.fit = TRUE))

(background.model.pred$logit.residual.scale <- NULL)     # remove unnecessary variable

(lambda <- qnorm(1 - 0.05/2))                     # standard normal quantile

background.model.pred$logit.lwr <- background.model.pred$logit.fit - 
  lambda*background.model.pred$logit.se.fit              # CI for log odds
background.model.pred$logit.upr <- background.model.pred$logit.fit + 
  lambda*background.model.pred$logit.se.fit

background.model.pred$odds.lwr <-                        # transform log-odds intervals 
  exp(background.model.pred$logit.lwr)                   # into C.I. for odds
background.model.pred$odds.upr <- exp(background.model.pred$logit.upr)

background.model.pred$p.lwr <- background.model.pred$odds.lwr / # transform the odds intervals
  (1 + background.model.pred$odds.lwr)                   # into C.I. for p
background.model.pred$p.upr <- background.model.pred$odds.upr /
  (1 + background.model.pred$odds.upr)
head(background.model.pred)




## plot 1
(background.plot <- ggplot(background.model.pred, aes(age, lowplasma)) +     
    geom_point() +  
    geom_point(aes(y = phat, color = as.factor(bmicat)), size = 1) +   # plot predicted p
    geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +        # plot CI
    xlab("age") +
    facet_wrap(~ smokstat) +
    ylab("lowplasma") +
    labs(title = "Plasma beta-carotene levels vs age", 
         subtitle = "Low levels (=1), High levels (=0)",
         caption = "subplots for each smokstat category", 
         color = "bmicat") +
    theme(text = element_text(size = 16)))


## plot 2
(background.plot.2 <- ggplot(background.model.pred, aes(x = quetelet, y = lowplasma)) +     
    geom_point() +  
    geom_point(aes(y = phat, color = agecat), size = 1) +        # plot predicted p
    geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +  # plot CI
    xlab("BMI") +
    facet_wrap(~ smokstat) +
    ylab("lowplasma") +
    labs(title = "Plasma beta-carotene levels vs BMI", 
         subtitle = "Low levels (=1), High levels (=0)",
         caption = "subplots for each smokstat category") +
    theme(text = element_text(size = 16)))

# - Probability of having low levels increases when age increases, when bmicat
#   increases and smoking increases

### PART 2
###   (b) Calculate leverage, deviance residuals and Cook's distance for the 
###       Background model


## leverage values for Background model
background.model.pred <- cbind(plasma,   
                         v = influence(background.model)$hat)

## standardized deviance residuals
background.model.pred$devres <- influence(background.model)$dev.res
background.model.pred$devstd <- background.model.pred$devres/sqrt(1 - background.model.pred$v)

## Cook's distance
background.model.pred$Dcook <- cooks.distance(background.model)


## plot leverage against age
ggplot(background.model.pred, aes(x = age, y = v, color = as.factor(bmicat))) +    # plot against age
  geom_point() +
  geom_hline(yintercept = 2*length(background.model$coefficients)/nrow(plasma), 
             color = "red", size = 1) +      # suitable line
  labs(title = "leverage vs age",
       caption = "2(p+1)/n in red", 
       color = "bmicat") +
  facet_wrap(~ smokstat) +
  xlab("age") +
  expand_limits(y = c(0,0.03)) +
  ylab("v") +
  theme(text = element_text(size = 16))


## plot leverage against quetelet
ggplot(background.model.pred, aes(x = quetelet, y = v, color = agecat)) +    # plot against age
  geom_point() +
  geom_hline(yintercept = 2*length(background.model$coefficients)/nrow(plasma), 
             color = "red", size = 1) +      # suitable line
  labs(title = "leverage vs BMI",
       caption = "2(p+1)/n in red") +
  facet_wrap(~ smokstat) +
  xlab("BMI") +
  expand_limits(y = c(0,0.03)) +
  ylab("v") +
  theme(text = element_text(size = 16))

# - smoking + high age gives largest leverage

## plot standardized deviance residuals vs age
ggplot(background.model.pred, aes(x = age, y = devstd, color = as.factor(bmicat))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  facet_wrap(~ smokstat) +
  xlab("age") +
  labs(title = "standardized deviance residuals vs age",
       subtitle = "low levels (=1), high levels (=0)", 
       color = "bmicat") +
  theme(text = element_text(size = 16))


## plot standardized deviance residuals vs quetelet
ggplot(background.model.pred, aes(x = quetelet, y = devstd, color = agecat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  facet_wrap(~ smokstat) +
  xlab("BMI") +
  labs(title = "standardized deviance residuals vs BMI",
       subtitle = "low levels (=1), high levels (=0)") +
  theme(text = element_text(size = 16))

## identify all observations where residual is larger than +-2

larg.res <- which(abs(background.model.pred$devstd) > 2)
background.model.pred[larg.res,]


## plot cooks D vs age
ggplot(background.model.pred, aes(x = age, y = Dcook, color = as.factor(bmicat))) +
  geom_point() +
  geom_hline(yintercept = 4/nrow(plasma), linetype = "dotted",
             size = 1) +
  geom_point(data = background.model.pred[larg.res,],   # Mark highest leverage
              color = "red", size = 4, shape = 2) +
  facet_wrap(~ smokstat) +
  xlab("age") +
  ylab("Cook's D") +
  labs(title = "Cook's distance vs age",
       color = "bmicat",
       subtitle = "low levels (=1), high levels (=0)",
       caption = "4/n in black
       triangle = large residual observations") +
  theme(text = element_text(size = 16))


## plot cooks D vs quetelet
ggplot(background.model.pred, aes(x = quetelet, y = Dcook, color = agecat)) +
  geom_point() +
  geom_hline(yintercept = 4/nrow(plasma), linetype = "dotted",
             size = 1) +
  geom_point(data = background.model.pred[larg.res,],   # Mark highest leverage
             color = "red", size = 4, shape = 2) +
  facet_wrap(~ smokstat) +
  xlab("BMI") +
  ylab("Cook's D") +
  labs(title = "Cook's distance vs age",
       color = "agecat",
       subtitle = "low levels (=1), high levels (=0)",
       caption = "4/n in black
       triangle = large residual observations") +
  theme(text = element_text(size = 16))


# highlight the observations with large residuals in the plots in 2a

ggplot(background.model.pred, aes(age, lowplasma)) +     
  geom_point() +  
  geom_point(aes(y = phat, color = as.factor(bmicat)), size = 1) +   # plot predicted p
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +        # plot CI
  geom_point(data = background.model.pred[larg.res,],   # Mark highest leverage
             color = "red", size = 4, shape = 2) +
  xlab("age") +
  facet_wrap(~ smokstat) +
  ylab("lowplasma") +
  labs(title = "Plasma beta-carotene levels vs age", 
       subtitle = "Low levels (=1), High levels (=0)",
       caption = "subplots for each smokstat category", 
       color = "bmicat") +
  theme(text = element_text(size = 16))


## plot 2
ggplot(background.model.pred, aes(x = quetelet, y = lowplasma)) +     
  geom_point() +  
  geom_point(aes(y = phat, color = agecat), size = 1) +        # plot predicted p
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +  # plot CI
  geom_point(data = background.model.pred[larg.res,],   # Mark highest leverage
             color = "red", size = 4, shape = 2) +
  xlab("BMI") +
  facet_wrap(~ smokstat) +
  ylab("lowplasma") +
  labs(title = "Plasma beta-carotene levels vs BMI", 
       subtitle = "Low levels (=1), High levels (=0)",
       caption = "subplots for each smokstat category") +
  theme(text = element_text(size = 16))


# - The highlighted observations are observations with high levels even though it "should have" low levels
#   Good idea to remove these influential points from the data and refit the model?
#   Remove it from the data would cause bias


### PART 2
###   (c) Create the Diet model

## Full diet model
## Create largest model allowed = all background variables
(full.diet <- glm(lowplasma ~ vituse + calories + fat + fiber + alcohol + cholesterol + betadiet, 
                        family = "binomial", data = plasma))

# Set vituse: 1 to reference
plasma$vituse <-
  factor(plasma$vituse,
         levels = c("1", "2", "3"))


## Trying and looking at some different models

(diet.model <- step(null.model,                              # Stepwise selection
                          scope = list(upper = full.diet)))  # Criterion: AIC


(diet.model.2 <- step(null.model,                            # Stepwise selection
                      scope = list(upper = full.diet,        # Criterion: BIC
                      k = log(nrow(plasma)))))                

(step(null.model,                                            # Forward selection 
     scope = list(upper = full.diet),                        # Criterion: AIC
     direction = "forward"))

(step(null.model,                                            # Forward selection 
     scope = list(upper = full.diet),                        # Criterion: BIC
     direction = "forward", 
     k = log(nrow(plasma))))

(step(full.diet))                                            # Backward selection 
                                                             # Criterion: AIC

(step(full.diet,                                             # Backward selection
      k = log(nrow(plasma))))                                # Criterion: BIC


# - Choosing AIC as criterion since it should be used for prediction. All variables
#   that give reasonable predictions of future observations are ok in the model
#   Also choosing stepwise selection (diet.model above) since it seems to give a 
#   reasonable model (comparing to project 1)


## beta-estimates and CI
diet.model.sum <- summary(diet.model)  # summary of Age model
diet.model.sum$coefficients            # extract parameter estimates
confint(diet.model)                    # get confidence interval

## exp(beta)-estimates and CI
exp(diet.model.sum$coefficients)      # extract parameter estimates
exp(confint(diet.model))              # get confidence interval


## Calculate the McFadden pseudo R^2 for Age, Background and Diet model

logLik(null.model)                             # null model
(lnL0 <- logLik(null.model)[1])                # log likelihood L(b0)


aic <- AIC(age.model, background.model, diet.model) 
collect.AIC <- data.frame(aic)                 # Save all models' AIC in dataframe

collect.AIC$loglik <-                          # log likelihoods L(betahat)
  c(logLik(age.model)[1],
    logLik(background.model)[1],
    logLik(diet.model)[1])


collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0      # R2_McF

collect.AIC$R2McF.adj <- 1 -                          # R2_McF_adj
  (collect.AIC$loglik - (collect.AIC$df - 1)/2)/lnL0  

collect.AIC$p <- collect.AIC$df - 1                   # covariates p = df - 1 

collect.AIC

# - R2McF and R2McF.adj agrees that only the diet model is better than only the 
#   background model (higher R2McF = better model)


### PART 2
###   (d) Create the Final model

# All possible variables
full.model <- glm(lowplasma ~ age + sex + smokstat + quetelet + vituse + 
                   calories + fat + fiber + alcohol + cholesterol + betadiet, 
                   family = "binomial", data = plasma)


## Looking at some different models

(model.1 <- step(null.model,                        # Stepwise selection (all possible variables)
     scope = list(upper = full.model)))             # Criterion: stepwise AIC


(model.2 <- glm(lowplasma ~ age + sex + smokstat + quetelet 
               + vituse + calories + fiber,        # Project 1 final model variables
               family = "binomial", data = plasma))


(model.3 <- glm(lowplasma ~ age + smokstat + quetelet  # All variables in background 
               + vituse + calories + fiber + betadiet, # and diet model
               family = "binomial", data = plasma))


(model.4 <- step(null.model,                        # Stepwise selection (diet and background model)
                scope = list(upper = model.3)))     # Criterion: AIC


(model.5 <- step(diet.model,                        # Starting from diet. Stepwise selection
                 scope = list(lower = null.model, upper = model.3),
                 direction = "both"))


(model.6 <- step(null.model,                        # Starting from diet. Stepwise selection
                 scope = list(lower = null.model, upper = model.3),
                 direction = "both"))

## Calculate the McFadden pseudo R^2 for models

aic <- AIC(full.model, model.1, model.2, model.3, model.4, model.5, model.6) 
collect.AIC <- data.frame(aic)                 # Save all models' AIC in dataframe

collect.AIC$loglik <-                          # log likelihoods L(betahat)
  c(logLik(full.model)[1],
    logLik(model.1)[1],
    logLik(model.2)[1], 
    logLik(model.3)[1], 
    logLik(model.4)[1], 
    logLik(model.5)[1], 
    logLik(model.6)[1])


collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0      # R2_McF

collect.AIC$R2McF.adj <- 1 -                          # R2_McF_adj
  (collect.AIC$loglik - (collect.AIC$df - 1)/2)/lnL0  

collect.AIC$p <- collect.AIC$df - 1                   # covariates p = df - 1 

collect.AIC

# - The best model seem to be model 3 (and 5, same model). This is the final model
(final.model <- model.3)

## beta-estimates and CI
final.model.sum <- summary(final.model)    # summary of model
final.model.sum$coefficients               # extract parameter estimates
confint(final.model)                       # get confidence interval

## exp(beta)-estimates and CI
exp(final.model.sum$coefficients)          # extract parameter estimates
exp(confint(final.model))                  # get confidence interval


## Calculate the McFadden pseudo R^2 for Final model

logLik(null.model)                             # null model
(lnL0 <- logLik(null.model)[1])                # log likelihood L(b0)


aic <- AIC(age.model, background.model, diet.model, final.model) 
collect.AIC <- data.frame(aic)                 # Save all models' AIC in dataframe

collect.AIC$loglik <-                          # log likelihoods L(betahat)
  c(logLik(age.model)[1],
    logLik(background.model)[1],
    logLik(diet.model)[1],
    logLik(final.model)[1])


collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0      # R2_McF

collect.AIC$R2McF.adj <- 1 -                          # R2_McF_adj
  (collect.AIC$loglik - (collect.AIC$df - 1)/2)/lnL0  

collect.AIC$p <- collect.AIC$df - 1                   # covariates p = df - 1 

collect.AIC

# - R2McF and R2McF.adj agrees that only the diet model is better than only the 
#   background model (higher R2McF = better model)






### PART 3
###   - Goodness-of-fit
###     Comparing Age, Background, Diet and Final model
###
###   (a) Calculate confusion matrices

## estimate probability phat for all models

pred.phat <- cbind(                             
  plasma,
  p.age = predict(age.model, type = "response"),
  p.back = predict(background.model, type = "response"),
  p.diet = predict(diet.model, type = "response"),
  p.final = predict(final.model, type = "response"))
head(pred.phat)


## phat

(pred.phat$yhat.age <- as.numeric(pred.phat$p.age > 0.5))
(pred.phat$yhat.back <- as.numeric(pred.phat$p.back > 0.5))

(pred.phat$yhat.diet <- as.numeric(pred.phat$p.diet > 0.5))
(pred.phat$yhat.final <- as.numeric(pred.phat$p.final > 0.65))


(row.01 <- table(plasma$lowplasma))

(col.01.age <- table(pred.phat$yhat.age))
(col.01.back <- table(pred.phat$yhat.back))
(col.01.diet <- table(pred.phat$yhat.diet))
(col.01.final <- table(pred.phat$yhat.final))


## confusion matrices

(confusion.age = table(pred.phat$lowplasma, pred.phat$yhat.age))
(confusion.back = table(pred.phat$lowplasma, pred.phat$yhat.back))
(confusion.diet = table(pred.phat$lowplasma, pred.phat$yhat.diet))
(confusion.final = table(pred.phat$lowplasma, pred.phat$yhat.final))


## specificity

(spec.age = 0)
(spec.back = confusion.back[1, 1] / row.01[1])
(spec.diet = confusion.diet[1, 1] / row.01[1])
(spec.final = confusion.final[1, 1] / row.01[1])  

## sensitivity

(sens.age = confusion.age[2, 1] / row.01[2])
(sens.back = confusion.back[2, 2] / row.01[2])
(sens.diet = confusion.diet[2, 2] / row.01[2])
(sens.final = confusion.final[2, 2] / row.01[2])

## accuracy

(accu.age = sum(diag(confusion.age)) / sum(confusion.age))
(accu.back = sum(diag(confusion.back)) / sum(confusion.back))
(accu.diet = sum(diag(confusion.diet)) / sum(confusion.diet))
(accu.final = sum(diag(confusion.final)) / sum(confusion.final))

## precision

(prec.age = confusion.age[2, 1] / col.01.age[1])
(prec.back = confusion.back[2, 2] / col.01.back[2])
(prec.diet = confusion.diet[2, 2] / col.01.diet[2])
(prec.final = confusion.final[2, 2] / col.01.final[2])


### PART 3
###   (b) Plot the ROC-curves and calculate AUC

# ROC-curves####
# Calculate for model 0 and 3.
(roc.age <- roc(lowplasma ~ p.age, data = pred.phat))
(roc.back <- roc(lowplasma ~ p.back, data = pred.phat))
(roc.diet <- roc(lowplasma ~ p.diet, data = pred.phat))
(roc.final <- roc(lowplasma ~ p.final, data = pred.phat))

# save the coordinates in a data frame for plotting.
roc.df.age <- coords(roc.age, transpose = FALSE)
roc.df.back <- coords(roc.back, transpose = FALSE)
roc.df.diet <- coords(roc.diet, transpose = FALSE)
roc.df.final <- coords(roc.final, transpose = FALSE)

roc.df.age$model <- "age"
roc.df.back$model <- "background"
roc.df.diet$model <- "diet"
roc.df.final$model <- "final"

roc.df.age

# Create the data for the ideal model by hand:
roc.df.ideal <- data.frame(sensitivity = c(0, 1, 1),
                           specificity = c(1, 1, 0),
                           threshold = c(NA, NA, NA))
roc.df.ideal$model <- "ideal"

# bind all roc dataframes together
roc.df <- rbind(roc.df.age, roc.df.back, roc.df.diet, roc.df.final)

## plot the ROC curves for all models
ggplot(roc.df, aes(specificity, sensitivity,
                   color = model)) +
  geom_path(size = 1) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curves for all models") +
  theme(text = element_text(size = 16))


## AUC (Area under the curve) value for all curves 
##  - measure of how close we are to the ideal curve

(aucs <- 
    data.frame(
      model = c("age", "background", "diet", "final"),
      auc = c(auc(roc.age), auc(roc.back), auc(roc.diet), auc(roc.final)),
      lwr = c(ci(roc.age)[1], ci(roc.back)[1],
              ci(roc.diet)[1], ci(roc.final)[1]),
      upr = c(ci(auc(roc.age))[3], ci(auc(roc.back))[3],
              ci(auc(roc.diet))[3], ci(auc(roc.final))[3])))

# Compare the AUC for the models:
roc.test(roc.age, roc.back)
roc.test(roc.diet, roc.back)
roc.test(roc.age, roc.diet)

roc.test(roc.age, roc.final)
roc.test(roc.diet, roc.final)
roc.test(roc.back, roc.final)

# - P-values show that all models are significantly different from final model.
# - Difference between diet and background is small. Final model performs best


### PART 3
###   (c) Find optimal threshold (cut off value for phat)

## age model 

ggroc(roc.age) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for age model")

cutoff.p <- 0.55
roc.df.age[roc.df.age$sensitivity > cutoff.p & 
               roc.df.age$specificity > cutoff.p, ]
##
(I_max.age <- which(roc.df.age$sensitivity > cutoff.p & 
                        roc.df.age$specificity > cutoff.p))


ggplot(roc.df.age, aes(specificity, sensitivity)) +
  geom_path(aes(color = threshold), size = 2) +
  geom_path(data = roc.df.ideal, color = "black", size = 1) +
  geom_point(data = roc.df.age[I_max.age, ], color = "black", size = 3) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  scale_color_gradientn(colours = rainbow(5)) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curve for Age model",
       caption = "Black dot = optimal threshold") +
  theme(text = element_text(size = 16))


## background model

ggroc(roc.back) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for background model")

cutoff.p <- 0.655
roc.df.back[roc.df.back$sensitivity > cutoff.p & 
               roc.df.back$specificity > cutoff.p, ]
##
(I_max.back <- which(roc.df.back$sensitivity > cutoff.p & 
                        roc.df.back$specificity > cutoff.p))


ggplot(roc.df.back, aes(specificity, sensitivity)) +
  geom_path(aes(color = threshold), size = 2) +
  geom_path(data = roc.df.ideal, color = "black", size = 1) +
  geom_point(data = roc.df.back[I_max.back, ], color = "black", size = 3) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  scale_color_gradientn(colours = rainbow(5)) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curve for Background model",
       caption = "Black dot = optimal threshold") +
  theme(text = element_text(size = 16))


## Diet model

ggroc(roc.diet) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for Diet model")


cutoff.p <- 0.68
roc.df.diet[roc.df.diet$sensitivity > cutoff.p & 
               roc.df.diet$specificity > cutoff.p, ]
##
(I_max.diet <- which(roc.df.diet$sensitivity > cutoff.p & 
                        roc.df.diet$specificity > cutoff.p))


ggplot(roc.df.diet, aes(specificity, sensitivity)) +
  geom_path(aes(color = threshold), size = 2) +
  geom_path(data = roc.df.ideal, color = "black", size = 1) +
  geom_point(data = roc.df.diet[I_max.diet, ], color = "black", size = 3) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  scale_color_gradientn(colours = rainbow(5)) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curve for Diet model",
       caption = "Black dot = optimal threshold") +
  theme(text = element_text(size = 16))

## Final model
ggroc(roc.final) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for final model")

cutoff.p <- 0.69
roc.df.final[roc.df.final$sensitivity > cutoff.p & 
           roc.df.final$specificity > cutoff.p, ]
##
(I_max.final <- which(roc.df.final$sensitivity > cutoff.p & 
                    roc.df.final$specificity > cutoff.p))


ggplot(roc.df.final, aes(specificity, sensitivity)) +
  geom_path(aes(color = threshold), size = 2) +
  geom_path(data = roc.df.ideal, color = "black", size = 1) +
  geom_point(data = roc.df.final[I_max.final, ], color = "black", size = 3) +
    geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  scale_color_gradientn(colours = rainbow(5)) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curve for final model",
       caption = "Black dot = optimal threshold") +
  theme(text = element_text(size = 16))


## threshold values
# Age model: 0.55
# Background model: 0.65
# Diet model: 0.68
# Final model: 0.69

# estimate probability phat for all models


pred.phat <- cbind(                             
  plasma,
  p.age = predict(age.model, type = "response"),
  p.back = predict(background.model, type = "response"),
  p.diet = predict(diet.model, type = "response"),
  p.final = predict(final.model, type = "response"))
head(pred.phat)


## phat 

(pred.phat$yhat.age <- as.numeric(pred.phat$p.age > 0.762))
(pred.phat$yhat.back <- as.numeric(pred.phat$p.back > 0.718))
(pred.phat$yhat.diet <- as.numeric(pred.phat$p.diet > 0.753))
(pred.phat$yhat.final <- as.numeric(pred.phat$p.final > 0.750))


(row.01 <- table(plasma$lowplasma))

(col.01.age <- table(pred.phat$yhat.age))
(col.01.back <- table(pred.phat$yhat.back))
(col.01.diet <- table(pred.phat$yhat.diet))
(col.01.final <- table(pred.phat$yhat.final))


## confusion matrices

(confusion.age <- table(pred.phat$lowplasma, pred.phat$yhat.age))
(confusion.back <- table(pred.phat$lowplasma, pred.phat$yhat.back))
(confusion.diet <- table(pred.phat$lowplasma, pred.phat$yhat.diet))
(confusion.final <- table(pred.phat$lowplasma, pred.phat$yhat.final))


## specificity

(spec.age = confusion.age[1, 1] / row.01[1])
(spec.back = confusion.back[1, 1] / row.01[1])
(spec.diet = confusion.diet[1, 1] / row.01[1])
(spec.final = confusion.final[1, 1] / row.01[1])  

## sensitivity

(sens.age = confusion.age[2, 2] / row.01[2])
(sens.back = confusion.back[2, 2] / row.01[2])
(sens.diet = confusion.diet[2, 2] / row.01[2])
(sens.final = confusion.final[2, 2] / row.01[2])

## accuracy

(accu.age = sum(diag(confusion.age)) / sum(confusion.age))
(accu.back = sum(diag(confusion.back)) / sum(confusion.back))
(accu.diet = sum(diag(confusion.diet)) / sum(confusion.diet))
(accu.final = sum(diag(confusion.final)) / sum(confusion.final))

## precision

(prec.age = confusion.age[2, 2] / col.01.age[2])
(prec.back = confusion.back[2, 2] / col.01.back[2])
(prec.diet = confusion.diet[2, 2] / col.01.diet[2])
(prec.final = confusion.final[2, 2] / col.01.final[2])


### PART 3
###   (d) Hosmer-Lemeshow goodness of fit test 

## Age
# p+1:
length(age.model$coefficients)
# so we need g > 2

pred.sort <- pred.phat[order(pred.phat$age), ]
pred.sort$rank <- seq(1, nrow(pred.sort))
head(pred.sort)

(HL.age <- hoslem.test(pred.sort$lowplasma, pred.sort$p.age, 
                       g = 8))
(HL.df.age <- data.frame(group = seq(1, 8),
                         Obs0 = HL.age$observed[, 1],
                         Obs1 = HL.age$observed[, 2],
                         Exp0 = HL.age$expected[, 1],
                         Exp1 = HL.age$expected[, 2]))

## CHOICE: g=7

# g = 3
(HL.age <- hoslem.test(pred.sort$lowplasma, pred.sort$p.age, 
                       g = 3))
(HL.df.age <- data.frame(group = seq(1, 3),
                         Obs0 = HL.age$observed[, 1],
                         Obs1 = HL.age$observed[, 2],
                         Exp0 = HL.age$expected[, 1],
                         Exp1 = HL.age$expected[, 2]))

ggplot(HL.df.age, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Age model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 3)) +
  theme(text = element_text(size = 16))

## g = 7
(HL.age <- hoslem.test(pred.sort$lowplasma, pred.sort$p.age, 
                       g = 8))
(HL.df.age <- data.frame(group = seq(1, 8),
                         Obs0 = HL.age$observed[, 1],
                         Obs1 = HL.age$observed[, 2],
                         Exp0 = HL.age$expected[, 1],
                         Exp1 = HL.age$expected[, 2]))

ggplot(HL.df.age, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Age model",
       subtitle = "Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 8)) +
  theme(text = element_text(size = 16))

## g = 10 
(HL.age <- hoslem.test(pred.sort$lowplasma, pred.sort$p.age, 
                       g = 10))
(HL.df.age <- data.frame(group = seq(1, 10),
                         Obs0 = HL.age$observed[, 1],
                         Obs1 = HL.age$observed[, 2],
                         Exp0 = HL.age$expected[, 1],
                         Exp1 = HL.age$expected[, 2]))

ggplot(HL.df.age, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Age model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 10)) +
  theme(text = element_text(size = 16))

## g = 12
(HL.age <- hoslem.test(pred.sort$lowplasma, pred.sort$p.age, 
                     g = 12))

HL.age$expected
(HL.df.age <- data.frame(group = seq(1, 12),
                       Obs0 = HL.age$observed[, 1],
                       Obs1 = HL.age$observed[, 2],
                       Exp0 = HL.age$expected[, 1],
                       Exp1 = HL.age$expected[, 2]))

ggplot(HL.df.age, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Age model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 12)) +
  theme(text = element_text(size = 16))



## Background
## CHOICE: g=8
# p+1:
length(background.model$coefficients)
# so we need g > 5

pred.sort.2 <- pred.phat[order(pred.phat$p.back), ]
pred.sort.2$rank <- seq(1, nrow(pred.sort.2))
head(pred.sort.2)

## g = 8
(HL.back <- hoslem.test(pred.sort.2$lowplasma, pred.sort.2$p.back, 
                       g = 9))
(HL.df.back <- data.frame(group = seq(1, 9),
                         Obs0 = HL.back$observed[, 1],
                         Obs1 = HL.back$observed[, 2],
                         Exp0 = HL.back$expected[, 1],
                         Exp1 = HL.back$expected[, 2]))

ggplot(HL.df.back, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Background model",
       y = "number of observations", 
       subtitle = "Observed and expected in each group") +
  scale_x_continuous(breaks = seq(1, 9)) +
  theme(text = element_text(size = 16))

## g = 10
(HL.back <- hoslem.test(pred.sort.2$lowplasma, pred.sort.2$p.back, 
                        g = 10))
(HL.df.back <- data.frame(group = seq(1, 10),
                          Obs0 = HL.back$observed[, 1],
                          Obs1 = HL.back$observed[, 2],
                          Exp0 = HL.back$expected[, 1],
                          Exp1 = HL.back$expected[, 2]))

ggplot(HL.df.back, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Background model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 10)) +
  theme(text = element_text(size = 16))


## g = 10
(HL.back <- hoslem.test(pred.sort.2$lowplasma, pred.sort.2$p.back, 
                        g = 8))
(HL.df.back <- data.frame(group = seq(1, 12),
                          Obs0 = HL.back$observed[, 1],
                          Obs1 = HL.back$observed[, 2],
                          Exp0 = HL.back$expected[, 1],
                          Exp1 = HL.back$expected[, 2]))

ggplot(HL.df.back, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Background model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 12)) +
  theme(text = element_text(size = 16))


## Diet
# p+1:
length(diet.model$coefficients)
# so we need g > 6

pred.sort.3 <- pred.phat[order(pred.phat$p.diet), ]
pred.sort.3$rank <- seq(1, nrow(pred.sort.3))

##CHOICE: g=
## g = 8
(HL.diet <- hoslem.test(pred.sort.3$lowplasma, pred.sort.3$p.diet, 
                        g = 8))
(HL.df.diet <- data.frame(group = seq(1, 8),
                          Obs0 = HL.diet$observed[, 1],
                          Obs1 = HL.diet$observed[, 2],
                          Exp0 = HL.diet$expected[, 1],
                          Exp1 = HL.diet$expected[, 2]))

ggplot(HL.df.diet, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Diet model",
       subtitle = "Observed and expected in each group", 
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 8)) +
  theme(text = element_text(size = 16))


## g = 12
(HL.diet <- hoslem.test(pred.sort.3$lowplasma, pred.sort.3$p.diet, 
                        g = 12))
(HL.df.diet <- data.frame(group = seq(1, 12),
                          Obs0 = HL.diet$observed[, 1],
                          Obs1 = HL.diet$observed[, 2],
                          Exp0 = HL.diet$expected[, 1],
                          Exp1 = HL.diet$expected[, 2]))

ggplot(HL.df.diet, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Diet model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 12)) +
  theme(text = element_text(size = 16))


## Final
# p+1:
length(final.model$coefficients)
# so we need g > 10

pred.sort.4 <- pred.phat[order(pred.phat$p.final), ]
pred.sort.4$rank <- seq(1, nrow(pred.sort.4))

## g = 12
(HL.final <- hoslem.test(pred.sort.4$lowplasma, pred.sort.4$p.final, 
                        g = 11))
(HL.df.final <- data.frame(group = seq(1, 11),
                          Obs0 = HL.final$observed[, 1],
                          Obs1 = HL.final$observed[, 2],
                          Exp0 = HL.final$expected[, 1],
                          Exp1 = HL.final$expected[, 2]))

ggplot(HL.df.final, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Final model",
       subtitle = "Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 11)) +
  theme(text = element_text(size = 16))


## g = 18
(HL.final <- hoslem.test(pred.sort.4$lowplasma, pred.sort.4$p.final, 
                         g = 18))
(HL.df.final <- data.frame(group = seq(1, 18),
                           Obs0 = HL.final$observed[, 1],
                           Obs1 = HL.final$observed[, 2],
                           Exp0 = HL.final$expected[, 1],
                           Exp1 = HL.final$expected[, 2]))

ggplot(HL.df.final, aes(group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Final model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 18)) +
  theme(text = element_text(size = 16))




