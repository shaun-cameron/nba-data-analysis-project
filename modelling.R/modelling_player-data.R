## Multiple Regression to Predict Wins Above Replacement



## Load Relevant Packages

library(tidyverse)
library(broom)
library(car)


## Reading in the Data

players <- read_csv("data/processed/player_stats_final.csv")




players <- players %>%
  mutate(eFGp = eFGp * 100,
         TSp = TSp * 100)

## The above code changes the percentage values to be out of 100, making the regression model more logical.


## Filtering By Position

## Because each position demands different attributes, a regression model will be fitted for each position.


centre <- players %>%
  filter(Pos == "C" | Pos == "C-PF")

pf <- players %>%
  filter(Pos == "PF" | Pos == "c-PF" | Pos == "PF-SF" | Pos == "SG-PF") 

pg <- players %>%
  filter(Pos == "PG")

sf <- players %>%
  filter(Pos == "SF" | Pos == "PF-SF" | Pos == "SF-SG" | Pos == "SG-SF")

sg <- players %>%
  filter(Pos == "SG" | Pos == "SF-SG" | Pos == "SG-PF" | Pos == "SG-SF")




## Creating the Regression Model - Centre

## The following code runs a regression model of WAR against all other variables. It then prints a tidy version of
## equation, which follows the form y = mx + b, where m is the slope coefficient for all x variables & b is the
## y intercept

fit_c <- lm(WAR ~ eFGp + PPG + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = centre)

tidy(fit_c, conf.int = TRUE)


## Independence of Observations

car::durbinWatsonTest(fit_c)

## The value D-W stat value of approximately 2 meets the assumption that there is independence of observations.


## Linearity

car::avPlots(fit_c)

## This plot demonstrates that there is a linear relationship between the response variable and each explanatory
## variable.



## Detecting Outliers

std_res <- rstandard(fit_c)
points <- 1 : length(std_res)

ggplot(NULL, aes(points, std_res)) +
  geom_point() +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

## The above code considers the residuals of single data points and comapares against the residuals of other
## data points. Looking at the plot it is evident that there might be one outlier present.



## Detecting High Leverage Points

hats <- hatvalues(fit_c)


ggplot(NULL, aes(points, hats)) +
  geom_point()

## Some of the points have the potential for leverage, but none significantly stand out against the others.



## Detecting Influential Points


cook <- cooks.distance(fit_c)


ggplot(NULL, aes(points, cook)) +
  geom_point()

## A couple of points coulkd be considered highly influential, but as they are not considered an outlier
## they will not be removed from the regression model.


## Homoscedasticity

res <- residuals(fit_c)
fitted <- predict(fit_c)

ggplot(NULL, aes(fitted, res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "magenta")


## The above code plots residuals against fitted values of the equation, andshows that there is
## constant variance across all values of the x variable. Therefore the data shows homoscedasticity.



## Normality of Residuals

ggplot(NULL, aes(sample= res)) +
  stat_qq() +
  stat_qq_line()

## The strong adherence to the line of best fit implies that the residuals display normality.



## Multicollinearity


sqrt(car::vif(fit_c))

## Variance Inflation Factor of less than 5 shows that there is no multicollinearity in the model.


## Model Testing

centre <- centre %>%
  mutate(XPWAR = predict(fit_c))

centre %>%
  ggplot(aes(XPWAR, WAR)) +
  geom_point(colour = "dodgerblue") +
  geom_abline(linetype = "dashed", colour = "magenta")


## Selecting Centres by Expected WAR

centre_war <- centre %>%
  mutate(sal_mil = salary / 1000000) %>%
  select(player_name, Tm, Age, sal_mil, XPWAR) %>%
  arrange(desc(XPWAR), sal_mil) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  top_n(10)




## Creating the Regression Model - Power Forward

## The following code runs a regression model of WAR against all other variables. It then prints a tidy version of
## equation, which follows the form y = mx + b, where m is the slope coefficient for all x variables & b is the
## y intercept

fit_pf <- lm(WAR ~ eFGp + PPG + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = pf)

tidy(fit_pf, conf.int = TRUE)


## Independence of Observations

car::durbinWatsonTest(fit_pf)

## The value D-W stat value of approximately 2 meets the assumption that there is independence of observations.


## Linearity

car::avPlots(fit_pf)

## This plot demonstrates that there is a linear relationship between the response variable and each explanatory
## variable.



## Detecting Outliers

std_res <- rstandard(fit_pf)
points <- 1 : length(std_res)

ggplot(NULL, aes(points, std_res)) +
  geom_point() +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

## The above code considers the residuals of single data points and comapares against the residuals of other
## data points. Looking at the plot it is evident that there are no outliers present.



## Detecting High Leverage Points

hats <- hatvalues(fit_pf)

hat_labels <- if_else(hats >= 0.45, paste(points), "")

ggplot(NULL, aes(points, hats)) +
  geom_point() +
  geom_text(aes(label = hat_labels), nudge_y = 0.01)

## Some of the points have the potential for leverage, and one significantly stands out against the others.



## Detecting Influential Points


cook <- cooks.distance(fit_pf)

cook_labels <- if_else(cook >= 0.5, paste(points), "")


ggplot(NULL, aes(points, cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_y = 0.03)

## A couple of points could be considered highly influential, including point 15 again. So this will be removed
## to see how much of a difference it makes to the regression model.



## Re-Running the Regression Model 

pf_new <- pf %>%
  filter(player_name != "Draymond Green")

fit_pf_new <- lm(WAR ~ eFGp + PPG + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = pf_new)

summary(fit_pf_new)

summary(fit_pf)

tidy(fit_pf_new, conf.int = TRUE)

## The R-squared value of pf_new is higher than the original, so it will be used for the rest of the analysis.



## Homoscedasticity

res <- residuals(fit_pf_new)
fitted <- predict(fit_pf_new)

ggplot(NULL, aes(fitted, res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "magenta")


## The above code plots residuals against fitted values of the equation, andshows that there is
## constant variance across all values of the x variable. Therefore the data shows homoscedasticity.



## Normality of Residuals

ggplot(NULL, aes(sample= res)) +
  stat_qq() +
  stat_qq_line()

## The strong adherence to the line of best fit implies that the residuals display normality.



## Multicollinearity


sqrt(car::vif(fit_pf_new))

## Variance Inflation Factor of less than 5 shows that there is no multicollinearity in the model.


## Model Testing

pf_new <- pf_new %>%
  mutate(XPWAR = predict(fit_pf_new))

pf_new %>%
  ggplot(aes(XPWAR, WAR)) +
  geom_point(colour = "dodgerblue") +
  geom_abline(linetype = "dashed", colour = "magenta")


## Selecting Power Forwards by Expected WAR

pf_war <- pf_new %>%
  mutate(sal_mil = salary / 1000000) %>%
  select(player_name, Tm, Age, sal_mil, XPWAR) %>%
  arrange(desc(XPWAR), sal_mil) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  top_n(10)



## Creating the Regression Model - Small Forward

## The following code runs a regression model of WAR against all other variables. It then prints a tidy version of
## equation, which follows the form y = mx + b, where m is the slope coefficient for all x variables & b is the
## y intercept

fit_sf <- lm(WAR ~ eFGp + PPG + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = sf)

tidy(fit_sf, conf.int = TRUE)


## Independence of Observations

car::durbinWatsonTest(fit_sf)

## The value D-W stat value of approximately 2 meets the assumption that there is independence of observations.


## Linearity

car::avPlots(fit_sf)

## This plot demonstrates that there is a linear relationship between the response variable and each explanatory
## variable.



## Detecting Outliers

std_res <- rstandard(fit_sf)
points <- 1 : length(std_res)

ggplot(NULL, aes(points, std_res)) +
  geom_point() +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

## The above code considers the residuals of single data points and comapares against the residuals of other
## data points. Looking at the plot it is evident that there are no outliers present.



## Detecting High Leverage Points

hats <- hatvalues(fit_sf)

hat_labels <- if_else(hats >= 0.45, paste(points), "")

ggplot(NULL, aes(points, hats)) +
  geom_point() +
  geom_text(aes(label = hat_labels), nudge_y = 0.01)

## Some of the points have the potential for leverage, and one significantly stands out against the others.



## Detecting Influential Points


cook <- cooks.distance(fit_sf)

cook_labels <- if_else(cook >= 0.4, paste(points), "")


ggplot(NULL, aes(points, cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_y = 0.03)

## A couple of points could be considered highly influential, including points 31 & 42. So these will 
## be removed to see how much of a difference it makes to the regression model.



## Re-Running the Regression Model 

sf_new <- sf %>%
  filter(player_name != "Kevin Durant",
         player_name != "Paul George")

fit_sf_new <- lm(WAR ~ eFGp + PPG + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = sf_new)

summary(fit_sf_new)

summary(fit_sf)

tidy(fit_sf_new, conf.int = TRUE)

## The R-squared value of sf_new is higher than the original, so it will be used for the rest of the analysis.



## Homoscedasticity

res <- residuals(fit_sf_new)
fitted <- predict(fit_sf_new)

ggplot(NULL, aes(fitted, res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "magenta")


## The above code plots residuals against fitted values of the equation, andshows that there is
## constant variance across all values of the x variable. Therefore the data shows homoscedasticity.



## Normality of Residuals

ggplot(NULL, aes(sample= res)) +
  stat_qq() +
  stat_qq_line()

## The strong adherence to the line of best fit implies that the residuals display normality.



## Multicollinearity


sqrt(car::vif(fit_sf_new))

## Variance Inflation Factor of less than 5 shows that there is no multicollinearity in the model.


## Model Testing

sf_new <- sf_new %>%
  mutate(XPWAR = predict(fit_sf_new))

sf_new %>%
  ggplot(aes(XPWAR, WAR)) +
  geom_point(colour = "dodgerblue") +
  geom_abline(linetype = "dashed", colour = "magenta")



## Selecting Small Forwards by Expected WAR

sf_war <- sf_new %>%
  mutate(sal_mil = salary / 1000000) %>%
  select(player_name, Tm, Age, sal_mil, XPWAR) %>%
  arrange(desc(XPWAR), sal_mil) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  top_n(10)



## Creating the Regression Model - Point Guard

## The following code runs a regression model of WAR against all other variables. It then prints a tidy version of
## equation, which follows the form y = mx + b, where m is the slope coefficient for all x variables & b is the
## y intercept

fit_pg <- lm(WAR ~ eFGp + PPG + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = pg)

tidy(fit_pg, conf.int = TRUE)

## The offensive rebound statistic is classed as a negative in this formula, when by all basketball accounts it is
## a positive. SO it will be removed and the formula will be looked at again.

fit_pg <- lm(WAR ~ eFGp + PPG + APG + DPG + SPG + BPG + TPG + FPG, data = pg)

tidy(fit_pg, conf.int = TRUE)


## Everything that should be positive and negative is accounted for, so this is the model that will be used
## for point guards.


## Independence of Observations

car::durbinWatsonTest(fit_pg)

## The value D-W stat value of approximately 2 meets the assumption that there is independence of observations.


## Linearity

car::avPlots(fit_pg)

## There doesn't appear to be a linear relationship with APG, so that will be removed and the model retried.


fit_pg <- lm(WAR ~ eFGp + PPG + DPG + SPG + BPG + TPG + FPG, data = pg)

tidy(fit_pg, conf.int = TRUE)

car::avPlots(fit_pg)

## There is a linear relationship between each exploratory available, so this assumption is now met.


## Detecting Outliers

std_res <- rstandard(fit_pg)
points <- 1 : length(std_res)

ggplot(NULL, aes(points, std_res)) +
  geom_point() +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

## The above code considers the residuals of single data points and comapares against the residuals of other
## data points. Looking at the plot it is evident that there are no outliers present.



## Detecting High Leverage Points

hats <- hatvalues(fit_pg)

hat_labels <- if_else(hats >= 0.45, paste(points), "")

ggplot(NULL, aes(points, hats)) +
  geom_point() +
  geom_text(aes(label = hat_labels), nudge_y = 0.01)

## Some of the points have the potential for leverage, and one significantly stands out against the others.



## Detecting Influential Points


cook <- cooks.distance(fit_pg)

cook_labels <- if_else(cook >= 0.4, paste(points), "")


ggplot(NULL, aes(points, cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_y = 0.03)

## A couple of points could be considered highly influential, including point 35. So ths will 
## be removed to see how much of a difference it makes to the regression model.



## Re-Running the Regression Model 

pg_new <- pg %>%
  filter(player_name != "James Harden")

fit_pg_new <- lm(WAR ~ eFGp + PPG + DPG + SPG + BPG + TPG + FPG, data = pg_new)

summary(fit_pg_new)

summary(fit_pg)

tidy(fit_pg_new, conf.int = TRUE)

## The R-squared value of pg_new is lower than the original, so it will not be used for the rest of the analysis.



## Homoscedasticity

res <- residuals(fit_pg)
fitted <- predict(fit_pg)

ggplot(NULL, aes(fitted, res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "magenta")


## The above code plots residuals against fitted values of the equation, andshows that there is
## constant variance across all values of the x variable. Therefore the data shows homoscedasticity.



## Normality of Residuals

ggplot(NULL, aes(sample= res)) +
  stat_qq() +
  stat_qq_line()

## The strong adherence to the line of best fit implies that the residuals display normality.



## Multicollinearity


sqrt(car::vif(fit_pg))

## Variance Inflation Factor of less than 5 shows that there is no multicollinearity in the model.


## Model Testing

pg <- pg %>%
  mutate(XPWAR = predict(fit_pg))

pg %>%
  ggplot(aes(XPWAR, WAR)) +
  geom_point(colour = "dodgerblue") +
  geom_abline(linetype = "dashed", colour = "magenta")


## Selecting Point Guards by Expected WAR

pg_war <- pg %>%
  mutate(sal_mil = salary / 1000000) %>%
  select(player_name, Tm, Age, sal_mil, XPWAR) %>%
  arrange(desc(XPWAR), sal_mil) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  top_n(10)




## Creating the Regression Model - Shooting Guard

## The following code runs a regression model of WAR against all other variables. It then prints a tidy version of
## equation, which follows the form y = mx + b, where m is the slope coefficient for all x variables & b is the
## y intercept

fit_sg <- lm(WAR ~ eFGp + PPG + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = sg)

tidy(fit_sg, conf.int = TRUE)


## Independence of Observations

car::durbinWatsonTest(fit_sg)

## The value D-W stat value of approximately 2 meets the assumption that there is independence of observations.


## Linearity

car::avPlots(fit_sg)

## This plot demonstrates that there is a linear relationship between the response variable and each explanatory
## variable.



## Detecting Outliers

std_res <- rstandard(fit_sg)
points <- 1 : length(std_res)

ggplot(NULL, aes(points, std_res)) +
  geom_point() +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

## The above code considers the residuals of single data points and comapares against the residuals of other
## data points. Looking at the plot it is evident that there are no outliers present.



## Detecting High Leverage Points

hats <- hatvalues(fit_sg)

hat_labels <- if_else(hats >= 0.45, paste(points), "")

ggplot(NULL, aes(points, hats)) +
  geom_point() +
  geom_text(aes(label = hat_labels), nudge_y = 0.01)

## Some of the points have the potential for leverage, but nothing really stands out.



## Detecting Influential Points


cook <- cooks.distance(fit_sg)

cook_labels <- if_else(cook >= 0.3, paste(points), "")


ggplot(NULL, aes(points, cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_y = 0.03)

## One point could be considered highly influential, point 61. So this will 
## be removed to see how much of a difference it makes to the regression model.



## Re-Running the Regression Model 

sg_new <- sg %>%
  filter(player_name != "Luka Doncic")

fit_sg_new <- lm(WAR ~ eFGp + PPG + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = sg_new)

summary(fit_sg_new)

summary(fit_sg)

tidy(fit_sg_new, conf.int = TRUE)

## The R-squared value of sg_new is not much different from the original, so it will not be used for the rest of 
## the analysis.



## Homoscedasticity

res <- residuals(fit_sg)
fitted <- predict(fit_sg)

ggplot(NULL, aes(fitted, res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "magenta")


## The above code plots residuals against fitted values of the equation, andshows that there is
## constant variance across all values of the x variable. Therefore the data shows homoscedasticity.



## Normality of Residuals

ggplot(NULL, aes(sample= res)) +
  stat_qq() +
  stat_qq_line()

## The strong adherence to the line of best fit implies that the residuals display normality.



## Multicollinearity


sqrt(car::vif(fit_sg))

## Variance Inflation Factor of less than 5 shows that there is no multicollinearity in the model.


## Model Testing

sg <- sg %>%
  mutate(XPWAR = predict(fit_sg))

sg %>%
  ggplot(aes(XPWAR, WAR)) +
  geom_point(colour = "dodgerblue") +
  geom_abline(linetype = "dashed", colour = "magenta")


## Selecting Shooting Guards by Expected WAR

sg_war <- sg %>%
  mutate(sal_mil = salary / 1000000) %>%
  select(player_name, Tm, Age, sal_mil, XPWAR) %>%
  arrange(desc(XPWAR), sal_mil) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  top_n(10)