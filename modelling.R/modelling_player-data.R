## Multiple Regression to Predict Wins



## Load Relevant Packages

library(tidyverse)
library(broom)
library(car)


## Reading in the Data

teams <- read_csv("data/processed/team_stats_final.csv")


## Finding the Most Logical Regression Model


teams <- teams %>%
  mutate(eFGp = eFGp * 100,
         TSp = TSp * 100)

## The above code mutates the percentage values to be out of 100, making the regression model more logical.

## The following code runs a regression model of Wins against all other variables. It then prints a tidy version of
## equation, which follows the form y = mx + b, where m is the slope coefficient for the x variable & b is the
## y intercept.


  
fit_teams <- lm(W ~ eFGp + PPG + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = teams)

tidy(fit_teams, conf.int = TRUE)

## It seems from this that more points equals less chance of a win, along with assists, blocks & turnovers.
## As turnovers are considered negative this is fine, but the others are concerning. PPG, deemed the most negative
## influence on wins, will be removed and the model will be re-run.


fit_teams <- lm(W ~ eFGp + APG + OPG + DPG + SPG + BPG + TPG + FPG, data = teams)

tidy(fit_teams, conf.int = TRUE)

## This model fits better, but assists per game is still deemed a negative influence. Will removing it change 
## things?


fit_teams <- lm(W ~ eFGp + OPG + DPG + SPG + BPG + TPG + FPG, data = teams)

tidy(fit_teams, conf.int = TRUE)

## This model seems adequate. What this points out is that it wins are not really about getting more points or
## assists, but about being efficient when you have the ball. What's interesting to note is that the more fouls
## committed the more likely a team increases their win chance. Maybe this is due to the chance that a fouled
## player will miss one or more of their free throws, decreasing opposition points.



## Independence of Observations

car::durbinWatsonTest(fit_teams)

## The value D-W stat valie of approximately 2 meets the assumption that there is independence of observations.


## Linearity

car::avPlots(fit_teams)

## This plot demonstrates that there is a linear relationship between the response variable and each explanatory
## variable.



## Detecting Outliers

std_res <- rstandard(fit_teams)
points <- 1 : length(std_res)

ggplot(NULL, aes(points, std_res)) +
  geom_point() +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

## The above code considers the residuals of single data points and comapares against the residuals of other
## data points. Looking at the plot it is evident that there are no outliers present.



## Detecting High Leverage Points

hats <- hatvalues(fit_teams)


ggplot(NULL, aes(points, hats)) +
  geom_point()
  
## Most of the points have the potential for leverage, but none significantly stand out against the others.



## Detecting Influential Points


cook <- cooks.distance(fit_teams)


ggplot(NULL, aes(points, cook)) +
  geom_point()

## Point 10 (Golden State Warriors) is considered highly influential, but as it is not considered an outlier
## it will not be removed from the regression model.


## Homoscedasticity

res <- residuals(fit_teams)
fitted <- predict(fit_teams)

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

pairs(formula = ~ eFGp + OPG + DPG + SPG + BPG + TPG + FPG, data = teams)


## This plot shows that there is no linear relationship between any explanatory variable.


sqrt(car::vif(fit_teams))

## Variance Inflation Factor of approximately 1 also shows that there is no multicollinearity in the model.