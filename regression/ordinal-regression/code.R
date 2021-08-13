# Project 3
# Ordinal logistic regression

# Install packages
install.packages('caret')
install.packages('e1071', dependencies=TRUE)

# Import libraries
library(caret)
library(ggplot2)
library(pROC)
library(ResourceSelection)
library(MASS)

# Load in data
plasma <- read.delim("Data/plasma.txt")
head(plasma)                                
summary(plasma)


# Calculate cut-off value a [ng/ml] between low and high
(a <- 0.42 * 536.888)

# Get all low observations
low.group <- which(plasma$betaplasma < a) 

# Get median of low group
low.group.median <- median(plasma[low.group,]$betaplasma)

# Get min and max values of all observed betaplasma values
min <- min(plasma$betaplasma)   # min =-1to get rid of NA's
max <- max(plasma$betaplasma)

# Divide betaplasma in 3 categories low/medium/high in variable plasmacat
plasma$plasmacat <- cut(plasma$betaplasma, breaks = c(-1, low.group.median, a, max), 
                        labels = c("low", "medium", "high"))
summary(plasma)

# Age model: create model
age.model <- polr(plasmacat ~ age, data = plasma)
(sum.age.model <- summary(age.model))

# Age model: beta-estimates
cbind(beta = age.model$coefficients, 
      expbeta = exp(age.model$coefficients),
      exp(confint(age.model)))

# Age model: zeta-estimates
cbind(zeta = age.model$zeta, 
      expzeta = exp(age.model$zeta))

# Age model: beta and zeta with s.e.
sum.age.model$coefficients

# Create null model
null.model <- polr(plasmacat ~ 1, data = plasma)
(sum.null <- summary(null.model))

# Change smoking status to categorical variable 
plasma$smokstat <- factor(plasma$smokstat,
                          levels = c(1, 2, 3),
                          labels = c("1", "2", "3"))

# Change vituse to categorical variable 
plasma$vituse <- factor(plasma$vituse,
                          levels = c(1, 2, 3),
                          labels = c("1", "2", "3"))

# Change sex to categorical variable 
plasma$sex <- factor(plasma$sex,
                        levels = c(1, 2),
                        labels = c("m", "f"))


# Background model: create full model (all background variables)
back.model.full <- polr(plasmacat ~ age + smokstat + sex + quetelet, data = plasma)
(sum.back.model.full <- summary(back.model.full))

# Background model: Create model using stepwise
(background.model <- step(back.model.full))
(sum.background.model <- summary(background.model))

# Background model: beta-estimates
cbind(beta = background.model$coefficients, 
      expbeta = exp(background.model$coefficients),
      exp(confint(background.model)))

# Background model: zeta-estimates
cbind(zeta = background.model$zeta, 
      expzeta = exp(background.model$zeta))

# Background model: beta and zeta with s.e.
sum.background.model$coefficients


# Diet model: create full model (all background variables)
diet.model.full <- polr(plasmacat ~ vituse + calories + fat + fiber + alcohol 
                        + cholesterol + betadiet, data = plasma)
(sum.diet.model.full <- summary(diet.model.full))

# Diet model: Create using stepwise
(diet.model <- step(diet.model.full))
(sum.diet.model <- summary(diet.model))

# Diet model: beta-estimates
cbind(beta = diet.model$coefficients, 
      expbeta = exp(diet.model$coefficients),
      exp(confint(diet.model)))

# Diet model: zeta-estimates
cbind(zeta = diet.model$zeta, 
      expzeta = exp(diet.model$zeta))

## Diet model: beta and zeta with s.e.
sum.diet.model$coefficients


# All variables in background and diet model
model.1 <- polr(plasmacat ~ age + sex + smokstat + quetelet + vituse + calories + fiber 
                        + cholesterol + betadiet, data = plasma)
(sum.model.1 <- summary(model.1))

# All variables (except bmicat- quetelet used instead)
model.2 <- polr(plasmacat ~ age + sex + smokstat + quetelet + vituse + calories
                + fat + fiber + alcohol + cholesterol + betadiet, data = plasma)

# Creating different models using stepwise
(model.3 <- step(model.1))
(sum.model.3 <- summary(model.3))

(model.4 <- step(model.2))
(sum.model.4 <- summary(model.4))

# Project 1 final model variables 
(model.5 <- polr(plasmacat ~ age + sex + smokstat + quetelet 
                + vituse + calories + fiber, data = plasma))
(sum.model.5 <- summary(model.5))

# Project 2 final model variables
(model.6 <- polr(plasmacat ~ age + smokstat + quetelet + vituse + calories
                 + fiber + betadiet, data = plasma))
(sum.model.6 <- summary(model.6))

aic = AIC(model.1, model.2, model.3, model.4)

# Compare models
info <- cbind(aic = AIC(model.1, model.2, model.3, model.4, model.5, model.6),
              bic = BIC(model.1, model.2, model.3, model.4, model.5, model.6),
              R2D = 100*c(1 - model.1$deviance/null.model$deviance, 
                          1 - model.2$deviance/null.model$deviance,
                          1 - model.3$deviance/null.model$deviance, 
                          1 - model.4$deviance/null.model$deviance, 
                          1 - model.5$deviance/null.model$deviance,
                          1 - model.6$deviance/null.model$deviance),
              R2D.adj = 100*c(1 - (model.1$deviance + model.1$edf - null.model$edf)/
                                null.model$deviance, 
                              1 - (model.2$deviance + model.2$edf - null.model$edf)/
                                null.model$deviance, 
                              1 - (model.3$deviance + model.3$edf - null.model$edf)/
                                null.model$deviance, 
                              1 - (model.4$deviance + model.4$edf - null.model$edf)/
                                null.model$deviance, 
                              1 - (model.5$deviance + model.5$edf - null.model$edf)/
                                null.model$deviance, 
                              1 - (model.6$deviance + model.6$edf - null.model$edf)/
                                null.model$deviance))
round(info, digits = 1)

# Model with lowest AIC and BIC chosen as Final model
final.model <- model.3
(sum.final.model <- summary(final.model))

# Final model: beta and exp beta-estimates
final.model.beta <- cbind(beta = final.model$coefficients, 
      expbeta = exp(final.model$coefficients),
      exp(confint(final.model)))

# Final model: zeta-estimates
final.model.zeta <- cbind(zeta = final.model$zeta, 
      expzeta = exp(final.model$zeta))

# Final model: beta and zeta with s.e.
sum.final.model$coefficients

# Compare Null, Age, Background, Diet, and Final model
info.2 <- cbind(aic = AIC(null.model, age.model, background.model, diet.model, final.model),
              bic = BIC(null.model, age.model, background.model, diet.model, final.model),
              R2D = 100*c(1 - null.model$deviance/null.model$deviance,
                          1 - age.model$deviance/null.model$deviance, 
                          1 - background.model$deviance/null.model$deviance,
                          1 - diet.model$deviance/null.model$deviance, 
                          1 - final.model$deviance/null.model$deviance),
              R2D.adj = 100*c(1 - (null.model$deviance + null.model$edf - null.model$edf)/
                                null.model$deviance,                  
                              1 - (age.model$deviance + age.model$edf - null.model$edf)/
                                null.model$deviance, 
                              1 - (background.model$deviance + background.model$edf - null.model$edf)/
                                null.model$deviance, 
                              1 - (diet.model$deviance + diet.model$edf - null.model$edf)/
                                null.model$deviance, 
                              1 - (final.model$deviance + final.model$edf - null.model$edf)/
                                null.model$deviance))
round(info.2, digits = 1)

# LR-test comparing nested models 
anova(null.model, age.model)
anova(diet.model, final.model)

# Estimate plasmacat (yhat) using all models
pred.yhat <- cbind(                             
   plasma,
   y.age = predict(age.model),
   y.back = predict(background.model),
   y.diet = predict(diet.model),
   y.final = predict(final.model))
head(pred.yhat)

# Create vector with plasmacat estimates
age.pred <- factor(pred.yhat$y.age)
back.pred <- factor(pred.yhat$y.back)
diet.pred <- factor(pred.yhat$y.diet)
final.pred <- factor(pred.yhat$y.final)

# Create vector with expected values
expected <- factor(plasma$plasmacat)

# Create confusion matrices
conf.age <- confusionMatrix(data = age.pred, reference = expected)
conf.back <- confusionMatrix(data = back.pred, reference = expected)
conf.diet <- confusionMatrix(data = diet.pred, reference = expected)
conf.final <- confusionMatrix(data = final.pred, reference = expected)

# Display goodness-of-fit metrics
conf.age
conf.back
conf.diet
conf.final

# Final model: Beta, zeta with se and Wald test P-value
final.model.beta
final.model.zeta
sum.final.model$coefficients
(P.value <- pnorm(abs(final.model$zeta), lower.tail = FALSE))

# Odds for plasma category "low", all x=0
exp(final.model$zeta[1])

# Odds for plasma category "medium", all x=0
exp(final.model$zeta[2])


# Estimate probabilities using Final model
(pred.final <- cbind(
   plasma,
   predict(final.model, type = "prob"),
   yhat = predict(final.model)))

# Visualize some estimations
ggplot(pred.final, aes(x = quetelet)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   xlab("BMI") +
   ylab("probability") +
   labs(color = "plasmacat", title = "estimated probability vs BMI", 
        subtitle = "- smoking status") +
   facet_wrap(~ smokstat, labeller = "label_both") +
   theme(text = element_text(size = 14))

ggplot(pred.final, aes(x = calories)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   xlab("calories") +
   ylab("probability") +
   labs(color = "plasmacat", title = "estimated probability vs calories", 
        subtitle = "- vitamine use") +
   facet_wrap(~ vituse, labeller = "label_both") +
   theme(text = element_text(size = 14))


# Create vector with different bmi and sex
x0 <- data.frame(quetelet = rep(seq(18, 42, 0.1), 2), 
                 sex = c(rep("m", length(seq(18, 42, 0.1))),
                         rep("f", length(seq(18, 42, 0.1)))))
x0

# Add same continuous variables
x0$age <- mean(plasma$age)
x0$fiber <- mean(plasma$fiber)
x0$vituse <- "1"
x0$calories <- mean(plasma$calories)
x0$betadiet <- mean(plasma$betadiet)

# Predict probabilities
pred.x0 <- cbind(
   x0,
   predict(final.model, newdata = x0, type = "probs"))
head(pred.x0)

# Visualize BMI and concentration relationship - sex
ggplot(pred.x0, aes(x = quetelet)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs BMI",
        y = "probability",
        fill = "plasmacat") +
   facet_wrap(~ sex, labeller = "label_both") +
   xlab("BMI") +
   theme(text = element_text(size = 14))

ggplot(pred.x0, aes(x = quetelet)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs BMI") +
   xlab("BMI") +
   facet_wrap(~ sex, labeller = "label_both") +
   theme(text = element_text(size = 14))


# Create vector with different bmi and vituse
x0 <- data.frame(quetelet = rep(seq(18, 42, 0.2), 3), 
                 vituse = c(rep("1", length(seq(18, 42, 0.2))),
                         rep("2", length(seq(18, 42, 0.2))), 
                         rep("3", length(seq(18, 42, 0.2)))))
x0

# Add same continuous variables
x0$age <- mean(plasma$age)
x0$fiber <- mean(plasma$fiber)
x0$sex <- "f"
x0$calories <- mean(plasma$calories)
x0$betadiet <- mean(plasma$betadiet)

# Predict probabilities
pred.x0 <- cbind(
   x0,
   predict(final.model, newdata = x0, type = "probs"))
head(pred.x0)

ggplot(pred.x0, aes(x = quetelet)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs BMI",
        y = "probability",
        fill = "plasmacat") +
   facet_wrap(~ vituse, labeller = "label_both") +
   xlab("BMI") +
   theme(text = element_text(size = 14))

ggplot(pred.x0, aes(x = quetelet)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs BMI") +
   xlab("BMI") +
   facet_wrap(~ vituse, labeller = "label_both") +
   theme(text = element_text(size = 14))


# Find suitable calorie intake values
summary(plasma$calories)


# Create vector with different calorie intake and sex
x0.cal <- data.frame(calories = rep(seq(45, 6600, 30), 2), 
                     sex = c(rep("m", length(seq(45, 6600, 30))),
                             rep("f", length(seq(45, 6600, 30)))))
x0.cal

# Add same continuous variable values
x0.cal$age <- mean(plasma$age)
x0.cal$vituse <- "1"
x0.cal$quetelet <- mean(plasma$quetelet)
x0.cal$fiber <- mean(plasma$fiber)
x0.cal$betadiet <- mean(plasma$betadiet)

# Predict probabilities
pred.x0.cal <- cbind(
   x0.cal,
   predict(final.model, newdata = x0.cal, type = "probs"))
head(pred.x0.cal)

# Visualize calorie intake and concentration relationship
ggplot(pred.x0.cal, aes(x = calories)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs calories",
        y = "probability",
        fill = "plasmacat") +
   xlab("calories") +
   facet_wrap(~ sex, labeller = "label_both") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.cal, aes(x = calories)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs calories") +
   facet_wrap(~ sex, labeller = "label_both") +
   xlab("calories") +
   theme(text = element_text(size = 14))


# Create vector with different calorie intake and sex
x0.cal <- data.frame(calories = rep(seq(45, 6600, 50), 3), 
                     vituse = c(rep("1", length(seq(45, 6600, 50))),
                             rep("2", length(seq(45, 6600, 50))),
                             rep("3", length(seq(45, 6600, 50)))))
x0.cal

# Add same continuous variable values
x0.cal$age <- mean(plasma$age)
x0.cal$sex <- "f"
x0.cal$quetelet <- mean(plasma$quetelet)
x0.cal$fiber <- mean(plasma$fiber)
x0.cal$betadiet <- mean(plasma$betadiet)

# Predict probabilities
pred.x0.cal <- cbind(
   x0.cal,
   predict(final.model, newdata = x0.cal, type = "probs"))
head(pred.x0.cal)

ggplot(pred.x0.cal, aes(x = calories)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs calories",
        y = "probability",
        fill = "plasmacat") +
   xlab("calories") +
   facet_wrap(~ vituse, labeller = "label_both") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.cal, aes(x = calories)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs calories") +
   facet_wrap(~ vituse, labeller = "label_both") +
   xlab("calories") +
   theme(text = element_text(size = 14))


# Create vector with different age and sex
x0.age <- data.frame(age = rep(seq(20, 80, 0.3), 2), 
                 sex = c(rep("m", length(seq(20, 80, 0.3))),
                            rep("f", length(seq(20, 80, 0.3)))))
x0.age

# Add same continous variable values
x0.age$quetelet <- mean(plasma$quetelet)
x0.age$fiber <- mean(plasma$fiber)
x0.age$vituse <- "1"
x0.age$calories <- mean(plasma$calories)
x0.age$betadiet <- mean(plasma$betadiet)
x0.age

# Predict probabilities
pred.x0.age <- cbind(
   x0.age,
   predict(final.model, newdata = x0.age, type = "probs"))
head(pred.x0.age)

# Visualize age and concentration relationship
ggplot(pred.x0.age, aes(x = age)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs age",
        y = "probability",
        fill = "plasmacat") +
   facet_wrap(~ sex, labeller = "label_both") +
   xlab("age") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.age, aes(x = age)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs BMI") +
   xlab("age") +
   facet_wrap(~ sex, labeller = "label_both") +
   theme(text = element_text(size = 14))

# Create vector with different age and vituse
x0.age <- data.frame(age = rep(seq(20, 80, 0.5), 3), 
                     vituse = c(rep("1", length(seq(20, 80, 0.5))),
                                rep("2", length(seq(20, 80, 0.5))),
                                rep("3", length(seq(20, 80, 0.5)))))
x0.age

# Add same continous variable values
x0.age$quetelet <- mean(plasma$quetelet)
x0.age$fiber <- mean(plasma$fiber)
x0.age$sex <- "f"
x0.age$calories <- mean(plasma$calories)
x0.age$betadiet <- mean(plasma$betadiet)
x0.age

# Predict probabilities
pred.x0.age <- cbind(
   x0.age,
   predict(final.model, newdata = x0.age, type = "probs"))
head(pred.x0.age)

# Visualize 
ggplot(pred.x0.age, aes(x = age)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs age",
        y = "probability",
        fill = "plasmacat") +
   facet_wrap(~ vituse, labeller = "label_both") +
   xlab("age") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.age, aes(x = age)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs age") +
   xlab("age") +
   facet_wrap(~ vituse, labeller = "label_both") +
   theme(text = element_text(size = 14))


# Find suitable fiber values
summary(plasma$fiber)

# Create vector with different fiber and categorical
x0.fiber <- data.frame(fiber = rep(seq(3, 36, 0.2), 2), 
                     sex = c(rep("m", length(seq(3, 36, 0.2))),
                             rep("f", length(seq(3, 36, 0.2)))))
x0.fiber

# Add same continous variable values
x0.fiber$quetelet <- mean(plasma$quetelet)
x0.fiber$age <- mean(plasma$age)
x0.fiber$vituse <- "1"
x0.fiber$calories <- mean(plasma$calories)
x0.fiber$betadiet <- mean(plasma$betadiet)
x0.fiber

# Predict probabilities
pred.x0.fiber <- cbind(
   x0.fiber,
   predict(final.model, newdata = x0.fiber, type = "probs"))
head(pred.x0.fiber)

# Visualize fiber and concentration relationship -sex
ggplot(pred.x0.fiber, aes(x = fiber)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs fiber",
        y = "probability",
        fill = "plasmacat") +
   facet_wrap(~ sex, labeller = "label_both") +
   xlab("fiber") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.fiber, aes(x = fiber)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs fiber") +
   xlab("fiber") +
   facet_wrap(~ sex, labeller = "label_both") +
   theme(text = element_text(size = 14))

# Create vector with different fiber and categorical
x0.fiber <- data.frame(fiber = rep(seq(3, 36, 0.3), 3), 
                       vituse = c(rep("1", length(seq(3, 36, 0.3))),
                                  rep("2", length(seq(3, 36, 0.3))), 
                                  rep("3", length(seq(3, 36, 0.3)))))
x0.fiber

# Add same continous variable values
x0.fiber$quetelet <- mean(plasma$quetelet)
x0.fiber$age <- mean(plasma$age)
x0.fiber$sex <- "f"
x0.fiber$calories <- mean(plasma$calories)
x0.fiber$betadiet <- mean(plasma$betadiet)
x0.fiber

# Predict probabilities
pred.x0.fiber <- cbind(
   x0.fiber,
   predict(final.model, newdata = x0.fiber, type = "probs"))
head(pred.x0.fiber)

ggplot(pred.x0.fiber, aes(x = fiber)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs fiber",
        y = "probability",
        fill = "plasmacat") +
   facet_wrap(~ vituse, labeller = "label_both") +
   xlab("fiber") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.fiber, aes(x = fiber)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs fiber") +
   xlab("fiber") +
   facet_wrap(~ vituse, labeller = "label_both") +
   theme(text = element_text(size = 14))


# Find suitable betadiet values
summary(plasma$betadiet)

# Create vector with different betadiet and categorical
x0.beta <- data.frame(betadiet = rep(seq(200, 9600, 60), 2),
                       sex = c(rep("m", length(seq(200, 9600, 60))),
                               rep("f", length(seq(200, 9600, 60)))))


# Add same continuous variable values
x0.beta$quetelet <- mean(plasma$quetelet)
x0.beta$age <- mean(plasma$age)
x0.beta$vituse <- "1"
x0.beta$calories <- mean(plasma$calories)
x0.beta$fiber <- mean(plasma$fiber)

# Predict probabilities
pred.x0.beta <- cbind(
   x0.beta,
   predict(final.model, newdata = x0.beta, type = "probs"))
head(pred.x0.beta)

# Visualize betadiet and concentration relationship
ggplot(pred.x0.beta, aes(x = betadiet)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs betadiet",
        y = "probability",
        fill = "plasmacat") +
   facet_wrap(~ sex, labeller = "label_both") +
   xlab("betadiet") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.beta, aes(x = betadiet)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs betadiet") +
   xlab("betadiet") +
   facet_wrap(~ sex, labeller = "label_both") +
   theme(text = element_text(size = 14))

# Create vector with different betadiet and categorical
x0.beta <- data.frame(betadiet = rep(seq(200, 9600, 80), 3), 
                      vituse = c(rep("1", length(seq(200, 9600, 80))),
                                 rep("2", length(seq(200, 9600, 80))),
                                 rep("3", length(seq(200, 9600, 80)))))


# Add same continuous variable values
x0.beta$quetelet <- mean(plasma$quetelet)
x0.beta$age <- mean(plasma$age)
x0.beta$sex <- "f"
x0.beta$calories <- mean(plasma$calories)
x0.beta$fiber <- mean(plasma$fiber)

# Predict probabilities
pred.x0.beta <- cbind(
   x0.beta,
   predict(final.model, newdata = x0.beta, type = "probs"))
head(pred.x0.beta)

# Visualize
ggplot(pred.x0.beta, aes(x = betadiet)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs betadiet",
        y = "probability",
        fill = "plasmacat") +
   facet_wrap(~ vituse, labeller = "label_both") +
   xlab("betadiet") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.beta, aes(x = betadiet)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs betadiet") +
   xlab("betadiet") +
   facet_wrap(~ vituse, labeller = "label_both") +
   theme(text = element_text(size = 14))


   
# Create vector with different bmi
x0 <- data.frame(quetelet = rep(seq(18, 42, 0.1), 2))
x0

# Add same continuous variables
x0$age <- mean(plasma$age)
x0$sex <- "f"
x0$fiber <- mean(plasma$fiber)
x0$vituse <- "1"
x0$calories <- mean(plasma$calories)
x0$betadiet <- mean(plasma$betadiet)

# Predict probabilities
pred.x0 <- cbind(
   x0,
   predict(final.model, newdata = x0, type = "probs"))
head(pred.x0)

# Visualize BMI and concentration relationship - sex
ggplot(pred.x0, aes(x = quetelet)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs BMI",
        y = "probability",
        fill = "plasmacat") +
   xlab("BMI") +
   theme(text = element_text(size = 14))

ggplot(pred.x0, aes(x = quetelet)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs BMI") +
   xlab("BMI") +
   theme(text = element_text(size = 14))


# Create vector with different age
x0.age <- data.frame(age = rep(seq(20, 80, 0.3), 2))
x0.age

# Add same continous variable values
x0.age$quetelet <- mean(plasma$quetelet)
x0.age$fiber <- mean(plasma$fiber)
x0.age$vituse <- "1"
x0.age$sex <- "f"
x0.age$calories <- mean(plasma$calories)
x0.age$betadiet <- mean(plasma$betadiet)
x0.age

# Predict probabilities
pred.x0.age <- cbind(
   x0.age,
   predict(final.model, newdata = x0.age, type = "probs"))
head(pred.x0.age)

# Visualize age and concentration relationship
ggplot(pred.x0.age, aes(x = age)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs age",
        y = "probability",
        fill = "plasmacat") +
   xlab("age") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.age, aes(x = age)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs age") +
   xlab("age") +
   theme(text = element_text(size = 14))
   

# Create vector with different fiber and categorical
x0.fiber <- data.frame(fiber = rep(seq(3, 36, 0.2), 2))
x0.fiber

# Add same continous variable values
mean(plasma$quetelet)

x0.fiber$quetelet <- mean(plasma$quetelet)
x0.fiber$age <- mean(plasma$age)
x0.fiber$vituse <- "3"
x0.fiber$sex <- "f"
x0.fiber$calories <- 5000
   
   mean(plasma$calories)
summary(plasma$betadiet)
x0.fiber$betadiet <- 220

   mean(plasma$betadiet)
x0.fiber

# Predict probabilities
pred.x0.fiber <- cbind(
   x0.fiber,
   predict(final.model, newdata = x0.fiber, type = "probs"))
head(pred.x0.fiber)

# Visualize fiber and concentration relationship -sex
ggplot(pred.x0.fiber, aes(x = fiber)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs fiber",
        y = "probability",
        fill = "plasmacat") +
   xlab("fiber") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.fiber, aes(x = fiber)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs fiber",
        subtitle = "        vitamine use: no
        calories: 5000 (high)
        dietary beta-carotene: 220 (low)") +
   xlab("fiber") +
   theme(text = element_text(size = 14))


# Create vector with different calorie intake and sex
x0.cal <- data.frame(calories = rep(seq(45, 6600, 30), 2))
x0.cal

# Add same continuous variable values
x0.cal$age <- mean(plasma$age)
x0.cal$sex <- "f"
x0.cal$vituse <- "1"
x0.cal$quetelet <- mean(plasma$quetelet)
x0.cal$fiber <- mean(plasma$fiber)
x0.cal$betadiet <- mean(plasma$betadiet)

# Predict probabilities
pred.x0.cal <- cbind(
   x0.cal,
   predict(final.model, newdata = x0.cal, type = "probs"))
head(pred.x0.cal)

# Visualize calorie intake and concentration relationship
ggplot(pred.x0.cal, aes(x = calories)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs calories",
        y = "probability",
        fill = "plasmacat") +
   xlab("calories") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.cal, aes(x = calories)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs calories") +
   xlab("calories") +
   theme(text = element_text(size = 14))


# Create vector with different betadiet 
x0.beta <- data.frame(betadiet = rep(seq(200, 9600, 60), 2))


# Add same continuous variable values
x0.beta$quetelet <- mean(plasma$quetelet)
x0.beta$sex <- "f"
x0.beta$age <- mean(plasma$age)
x0.beta$vituse <- "1"
x0.beta$calories <- mean(plasma$calories)
x0.beta$fiber <- mean(plasma$fiber)

# Predict probabilities
pred.x0.beta <- cbind(
   x0.beta,
   predict(final.model, newdata = x0.beta, type = "probs"))
head(pred.x0.beta)

# Visualize betadiet and concentration relationship
ggplot(pred.x0.beta, aes(x = betadiet)) +
   geom_line(aes(y = low, color = "low"), size = 2) +
   geom_line(aes(y = medium, color = "medium"), size = 2) +
   geom_line(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs betadiet",
        y = "probability",
        fill = "plasmacat") +
   xlab("betadiet") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.beta, aes(x = betadiet)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs betadiet") +
   xlab("betadiet") +
   theme(text = element_text(size = 14))


# Create vector with different sex
x0.sex <- data.frame(sex = "f")
x0.sex
# Add same continuous variable values
x0.sex$quetelet <- mean(plasma$quetelet)
x0.sex$betadiet <- mean(plasma$betadiet)
x0.sex$age <- mean(plasma$age)
x0.sex$vituse <- "1"
x0.sex$calories <- mean(plasma$calories)
x0.sex$fiber <- mean(plasma$fiber)

x0.sex

# Predict probabilities
pred.x0.sex <- cbind(
   x0.sex,
   predict(final.model, newdata = x0.sex, type = "probs"))
head(pred.x0.sex)


# Visualize betadiet and concentration relationship
ggplot(pred.x0.sex, aes(x = quetelet)) +
   geom_point(aes(y = low, color = "low"), size = 2) +
   geom_point(aes(y = medium, color = "medium"), size = 2) +
   geom_point(aes(y = high, color = "high"), size = 2) +
   expand_limits(y = c(0, 1)) +
   labs(title = "concentration probability vs betadiet",
        y = "probability",
        fill = "plasmacat") +
   xlab("betadiet") +
   theme(text = element_text(size = 14))

ggplot(pred.x0.sex, aes(x = betadiet)) +
   geom_ribbon(aes(ymin = 0, ymax = low, fill = "low")) +
   geom_ribbon(aes(ymin = low, 
                   ymax = low + medium, 
                   fill = "medium")) +
   geom_ribbon(aes(ymin = low + medium, ymax = 1,
                   fill = "high")) +
   labs(fill = "concentration", title = "concentration probability vs betadiet") +
   xlab("betadiet") +
   theme(text = element_text(size = 14))

