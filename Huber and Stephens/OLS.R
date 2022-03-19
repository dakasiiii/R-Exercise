# OLS in R

# We need the following packages:
library(tidyverse)
# the politicalds package contains our dataset
library(politicalds)
library(ggcorrplot)
library(ggplot2)
library(texreg)
library(prediction)
library(car)
library(lmtest)
library(sandwich)
library(miceadds)
library(ggpubr)

# The dataset we will be using are works by Evelyn Huber
# and John Stephens about welfare in Latin America
data("welfare")
skimr::skim(welfare)

# we first need to know how each of the variables are
# related to each other
corr_welfare <- welfare %>%
  select(gini, education_budget, sector_dualism, foreign_inv, gdp, 
         ethnic_diversity, regime_type, health_budget, socialsec_budget,
         legislative_bal, population) %>%
  cor(use = "pairwise") %>%
  round(1)

ggcorrplot(corr_welfare, type = "lower", lab = T, show.legend = F)

# The dependent variable in the study is gini while Education expenditure as the 
# independent variable. We need to first visualize both varibales

ggplot(welfare, aes(x = gini, na.rm = T)) +
  geom_histogram(binwidth = 1) +
  labs(x = "GINI Index", y = "Frequency",
       caption = "Source: Huber et al (2012)")

ggplot(welfare, aes(x = education_budget, na.rm = T)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Education Expenditure", y = "Frequency",
       caption = "Source: Huber et al (2012)")
  
# after seeing how the data are distributed for both variables, we can now graph
# the correlation between the two varibales

ggplot(welfare, aes(education_budget, gini)) +
  geom_point() +
  labs(x = "Education Expenditure (% of GDP)", y = "GINI",
       caption = "Source: Huber and Stephens (2012)")

# Exercise: Analyze the distribution of foreign_inv and make a graph
# to evaluate the relation between it and GINI. 

ggplot(welfare, aes(x = foreign_inv, na.rm = T)) +
  geom_histogram() +
  labs(x = "Foreign Investment", y = "Frequency",
       caption = "Source: Huber et. al (2012)")

#There are not much variation in the Foreign Direct Investment

ggplot(welfare, aes(foreign_inv, gini)) +
  geom_point() +
  labs(x = "Foreign Investment", y = "GINI",
       cpation = "Source: Huber and Stephens (2012)")

# There appears to be no sign of relationship between the two variables

# ESTIMATING A LINEAR MODEL
model_1 <- lm(gini ~ 1 + education_budget, data = welfare)

screenreg(model_1,
          custom.model.names = "Model 1",
          custom.coef.names = c("Constant", "Education Expenditure"))

# Implications:
# When the Education Expenditure is 0, GINI is 44.81
# A one unit increase in Education Expenditure is associated with a 1.23 increase
# in GINI
# The results are highly significant

# Exercise: estimate a model where the independent variable is Foreign Direct Investment
# Export it into a .doc file
model_2 <- lm(gini ~ 1 + foreign_inv, data = welfare)

screenreg(model_2,
          custom.model.names = "Model 2",
          custom.coef.names = c("Constant", "FDI"))

# The effects are statistically significant. GINI is more significant that of FDI

htmlreg(list(model_2), file = "model_2.doc",
        custom.model.names = "Model 2",
        custom.coef.names = c("Constant", "FDI"),
        inline.css = FALSE, doctype = T, html.tag = T,
        head.tag = T, body.tag = T)

#Graphic Representation

ggplot(data = welfare, aes(x = education_budget, y = gini)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, color = "black") +
  labs( x = "Education Expenditure", y = "Inequality")

# linear fit for 95% level of confidence

# Exercise: produce a graphical representation of the relationship between
# GINI and FDI

ggplot(data = welfare, aes(x = foreign_inv, y = gini)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, color = "black") +
  labs(x = "FDI", y = "Inequality")

# MULTIPLE REGRESSION ANALYSIS

# We first remove the NA values

welfare_no_na <- welfare %>%
  drop_na(gini, education_budget, foreign_inv, health_budget,
          socialsec_budget, population, sector_dualism, ethnic_diversity,
          gdp, regime_type, legislative_bal)

model_3 <- lm(gini ~ 1 + education_budget + foreign_inv + health_budget +
              socialsec_budget + population + sector_dualism + ethnic_diversity +
              gdp + factor(regime_type) + legislative_bal, data = welfare_no_na)

screenreg(model_3,
           custom.model.names = "Model 3",
          custom.coef.names = c(
            "Constant" , "Education expenditure" , "FDI",
            "Health expenditure" , "Social sec. expenditure" ,
            "Young population" , "Dualism in economy" ,
            "Ethnic division" , "pc GDP" , "Democratic.reg" , "Mixed.reg" ,
            "Authoritarian.reg" , "Balance between powers")
          )

# Comparing models

models_1 <- list(model_1, model_2, model_3)

screenreg(models_1,
          custom.model.names = c("Model 1", "Model 2", "Model 3"),
          custom.coef.names = c(
          "Constant" , "Education expenditure" , "FDI",
          "Health expenditure" , "Social sec. expenditure" ,
          "Young population" , "Dualism in economy" ,
          "Ethnic division" , "pc GDP" , "Democratic.reg" , "Mixed.reg" ,
          "Authoritarian.reg" , "Balance between powers")
          )

# PREDICTING VALUES OF A MODEL

# Let's look at the GINI, education expenditure, and ethnic division:
model_2_restricted <- lm(gini ~ 1 + education_budget + ethnic_diversity, 
                         data = welfare_no_na)

screenreg(model_2_restricted, 
          custom.model.names = "Model 1",
          custom.coef.names = c("Constant", "Education Expenditure", "Ethnic Division"))

pred_model_2_restricted <- as_tibble(prediction(model_2_restricted))

# We can predict the values when Ethnic Division is present with our model
ggplot(data = pred_model_2_restricted) +
  geom_point(mapping = aes(x = education_budget, y = gini, 
                           color = factor(ethnic_diversity))) +
  geom_line(mapping = aes(x = education_budget, y = fitted,
                          color = factor(ethnic_diversity),
                          group = factor(ethnic_diversity))) +
  labs(x = "Education Expenditure", y = "Inequality", color = "Ethnic division")

# GINI is higher in places with Ethnic Division

# MODEL ADJUSTMENTS
# Adjusted R squared measures how much variation in the dependent variable
# can be explained by the model

# Exercise: Suppose someone said that balance between powers, regime type, and 
# ethnic diversity should be excluded from the model, compare the explanatory capacity
# of each model

model_3_restricted <- lm(gini ~ 1 + education_budget + foreign_inv + health_budget
                         + socialsec_budget + population + sector_dualism + gdp,
                         data = welfare_no_na)

anova(model_3, model_3_restricted)

# the null hypothesis is rejected therefore, these variables should
# not be included

# Exercise: Estimate a model where you exclude health expenditure and
# social security expenditure, and compare its explanatory capacity with the
# complete model. Should we exclude these variables from the model?

model_4_restricted <- lm(gini ~ 1 + education_budget + foreign_inv +
                           population + sector_dualism + ethnic_diversity +
                           gdp + factor(regime_type) + legislative_bal,
                         data = welfare_no_na)

anova(model_3, model_4_restricted)

# These variables should not be excluded as the p-value is below the 0.05 threshold

# OLS AsSUMPTIONS

# Evaluating linearity
ggplot(mapping = aes(x = model_1$fitted.values, y = model_1$residuals)) +
  labs(x = "Predicted Values", y = "Residuals") +
  geom_point() +
  geom_hline(mapping = aes(yintercept = 0))

# If the average residuals tend to show a pattern then the functional form
# of one variable is non-linear. Thus model should be adjusted

model_1_log <- lm(log(gini) ~ 1 + education_budget, data = welfare)   

screenreg(model_1_log)  
# Holding all variables constant, If we increase education expenditure by one unit, 
# we would expect GINI to increase by 3%

# Linearity test with car package

crPlots(model_1)
# The models seem to be quadratic

welfare_no_na_2 <- welfare_no_na %>%
  mutate(cseduc2 = education_budget * education_budget)

model_1_quadratic <- lm(gini ~ 1 + cseduc2 + education_budget, 
                        data = welfare_no_na_2)

crPlots(model_1_quadratic)

# We use Ramsey's RESET Test to assess whether there is an error of specification
# in the regression equation

resettest(model_1, power = 2, type = "fitted", data = welfare_no_na)

# Adding a quadratic term to education expenditure improves the fit of out
# estimation

# Multicollinearity issues

# Refer to the correlation matrix before
# to detect if milticollinearity is a problem, we need to perform a 
# variance inflation factors test

vif(model_3)
sqrt(vif(model_3)) > 2

# If the score is more than the sqrt of 2, then the variance and high and
# there indeed is a multicollinearity issue

# Homoscedasticity

# Visual diagnostics

ggplot(welfare_no_na, aes(education_budget, gini)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black")

# at low levels of education expenditure, the variability of inequality levels
# is significantly higher than at higher levels of education expenditure

# we can make a better visual diagnosis if we use the estimated model
# and we graph the residuals

# graphing the bivariate model
residualPlot(model_1)

# graphing the multivariate model
residualPlot(model_3)

# Since the residuals are variable, we have a case of heteroscedasticity
# we can evaluate each variable in the model to identify in which specific variable
# heteroscedasticity is present

residualPlots(model_3, layout = c(3, 4), tests = F, fitted = F)

# Statistical Diagnosis
# We can use the Breusch-Pagan test in determining heteroscedasticity

bptest(model_3, studentize = T)

# since the p value is less than 0.05, we have a scenario of heteroscedasticity

# Solutions:

# Robust Standard Errors

model_3_robust_3 <- coeftest(model_3, vcov = vcovHC(model_3, "HC3"))
model_3_robust_1 <- coeftest(model_3, vcov = vcovHC(model_3, "HC1"))
model_3_robust_0 <- coeftest(model_3, vcov = vcovHC(model_3, "HC0"))

models_robust <- list(model_3, model_3_robust_0, model_3_robust_1, 
                      model_3_robust_3)

screenreg(models_robust,
           custom.model.names = c("w/o robust SE", "robust HC0", 
                                  "robust HC1", "Robust HC3"))

# error variance associated to clusters

ggplot(welfare_no_na, aes(education_budget, gini)) +
  geom_point() +
  facet_wrap(~country_id)

# Visual inspection suggests that there is some clustering by country
# RSE by clusters can increase or decrease the standard errors

model_3_cluster <- miceadds::lm.cluster(
  data = welfare_no_na,
  formula = gini ~ 1 + education_budget + sector_dualism + foreign_inv +
    gdp + ethnic_diversity + regime_type + health_budget +
    socialsec_budget + legislative_bal,
  cluster = "country_id"
)

summary(model_3_cluster)

# Normality in the error distribution

# We can check if the residuals of the estimated model
# through OLS follow a T-student distribution

qqPlot(model_3$residuals, col.lines = "black")

ggdensity(model_3$residuals, main = "Density plot of the residuals")

# Exercise: Make a scatter plot of the relationship between variable
# gini and foreign_inv. Add the code of the country to each observation

ggplot(data = welfare_no_na, aes(foreign_inv, gini)) +
  geom_point() +
  facet_wrap(~country_id)

# Exercise: to model 1, add the foreign_inv as a control variable and interpret
# its coefficient

model_1_plus_control <- lm(gini ~ 1 + education_budget + foreign_inv, data = welfare_no_na)

screenreg(model_1_plus_control,
          custom.model.names = "Model 1 adjusted",
          custom.coef.names = c("Constant", "Educational Expenditure", "FDI"))
# A one unit increase in FDI corresponds to a 0,70 increase in inequality
# it is significant at 99% confidence interval

# Exercise: Make the corresponding tests to check that OLS assumptions are not violated

# Linearity in Parameters
crPlots(model_1_plus_control)
# It appears to follow a quadratic trend
welfare_no_na_3 <- welfare_no_na %>%
  mutate(csfdi = foreign_inv ** 2)

model_1_quadratic_2 <- lm(gini ~ 1 + csfdi + foreign_inv,
                          data = welfare_no_na_3)

crPlots(model_1_quadratic_2)

resettest(model_1_plus_control, power = 2, type = "fitted",
          data = welfare_no_na_3)
# looking at the P-value, the null hypothesis is accepted and adjusting the model
# into a quadratic model does not improve fit

# How about a Log transformation
model_1_plus_control_log <- lm(log(gini) ~ 1 + education_budget + foreign_inv, 
                               data = welfare_no_na)

screenreg(model_1_plus_control_log)

crPlots(model_1_plus_control_log)

resettest(model_1_plus_control_log, power = 2, type = "fitted",
          data = welfare_no_na_3)
# Adjusting the model into a Log model does not improve fit

# multicollinearity
vif(model_1_plus_control)
sqrt(vif(model_1_plus_control)) > 2
# Therefore we do not have multicollinearity issues

#Homoscedasticity
residualPlot(model_1_plus_control)
# Visually, it appears to exhibit heteroscedasticity
bptest(model_1_plus_control, studentize = T)
# Null hypothesis is rejected, we have a scenario of heteroscedasticity
model_3_robust_3_2 <- coeftest(model_1_plus_control, vcov = vcovHC(model_1_plus_control, "HC3"))
model_3_robust_1_2 <- coeftest(model_1_plus_control, vcov = vcovHC(model_1_plus_control, "HC1"))
model_3_robust_0_2 <- coeftest(model_1_plus_control, vcov = vcovHC(model_1_plus_control, "HC0"))

models_robust_2 <- list(model_1_plus_control, model_3_robust_0_2, model_3_robust_1_2, 
                        model_3_robust_3_2)

screenreg(models_robust_2,
          custom.model.names = c("w/o robust SE", "robust HC0", 
                                 "robust HC1", "Robust HC3"))
# Clustering plays a role

# Normality in the error distribution
qqPlot(model_1_plus_control$residuals, col.lines = "Black")
ggdensity(model_1_plus_control$residuals, main = "Density plot of the residuals")
# Both graphs viusally show that there is no normality in error distribution

# Exercise: Export the regression table to word
htmlreg(list(model_1_plus_control), file = "model_1_plus_control.doc",
        custom.model.names = "Model 1 adjusted",
        custom.coef.names = c("Constant", "Education Expenditure", "FDI"),
        inline.css = F, doctype = T, html.tag = T,
        head.tag = T, body.tag = T)

# END