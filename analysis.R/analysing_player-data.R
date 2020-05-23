## Exploratory Data Analysis - Players


## Loading Relevant Packages

library(tidyverse)



## Reading in the Data

players <- read_csv("data/processed/player_stats_final.csv")


## Exploratory Analysis of the Data

## Each cycle of code creates a plot, prints the plot & shows the correlation coefficient. The correlation is 
## important as it will contribute to the modelling process.


## Salary vs WAR

salary_war <- players %>%
  ggplot(aes(salary / 1000000, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

salary_war

cor(players$salary, players$WAR, method = "pearson") # How much a player is paid has a moderate correlation with
# Wins ABove Replacement



## PPG vs WAR


ppg_war <- players %>%
  ggplot(aes(PPG, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

ppg_war

cor(players$PPG, players$WAR, method = "pearson") # More points per game has a strong correlation with Wins Above
# Replacement.


## PPM vs WAR

ppm_war <- players %>%
  ggplot(aes(PPM, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

ppm_war

cor(players$PPM, players$WAR, method = "pearson") # More points per minute has a moderate correlation with Wins Above
# Replacement.Interesting to note that per game relates better than per minute.


## APG vs WAR

apg_war <- players %>%
  ggplot(aes(APG, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

apg_war

cor(players$APG, players$WAR, method = "pearson") # Assists per game has a moderate correlation with Wins Above
# Replacement.


## APM vs WAR

apm_war <- players %>%
  ggplot(aes(APM, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

apm_war

cor(players$APM, players$WAR, method = "pearson") # Assists per minute has a weak correlation with Wins Above
# Replacement.


## RPG vs WAR

rpg_war <- players %>%
  ggplot(aes(RPG, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

rpg_war

cor(players$RPG, players$WAR, method = "pearson") # Rebounds per game has a moderate correlation with Wins Above
# Replacement.


## RPM vs WAR

rpm_war <- players %>%
  ggplot(aes(RPM, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

rpm_war

cor(players$RPM, players$WAR, method = "pearson") # Rebounds per minute has a weak correlation with Wins Above
# Replacement.


## OPG vs WAR

opg_war <- players %>%
  ggplot(aes(OPG, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

opg_war

cor(players$OPG, players$WAR, method = "pearson") # Offensive rebounds per game has a moderate correlation with
# Wins Above Replacement.


## DPG vs WAR

dpg_war <- players %>%
  ggplot(aes(DPG, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

dpg_war

cor(players$DPG, players$WAR, method = "pearson") # Defensive rebounds per game has a moderate correlation with
# Wins Above Replacement. 


## SPG vs WAR

spg_war <- players %>%
  ggplot(aes(SPG, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

spg_war

cor(players$SPG, players$WAR, method = "pearson") # Steals per game has a moderate correlation with Wins 
# Above Replacement.


## BPG vs WAR

bpg_war <- players %>%
  ggplot(aes(BPG, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

bpg_war

cor(players$BPG, players$WAR, method = "pearson") # Blocks per game has a moderate correlation with Wins Above
# Replacement.



## TPG vs WAR

tpg_war <- players %>%
  ggplot(aes(TPG, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

tpg_war

cor(players$TPG, players$WAR, method = "pearson") # Turnovers per game has a moderate correlation with
# Wins Above Replacement. However this will not be used in the regression model as turnovers are generally
# condiered detrimental.


## FPG vs WAR

fpg_war <- players %>%
  ggplot(aes(FPG, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

fpg_war

cor(players$FPG, players$WAR, method = "pearson") # Fouls per game has a weak correlation with
# Wins Above Replacement.


## eFGp vs WAR

efgp_war <- players %>%
  ggplot(aes(eFGp, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

efgp_war

cor(players$eFGp, players$WAR, method = "pearson") # Effective field goal percentage has a moderate correlation with
# Wins Above Replacement.



## TSp vs WAR

tsp_war <- players %>%
  ggplot(aes(TSp, WAR)) +
  geom_point() +
  geom_smooth(method = "lm") 

tsp_war

cor(players$TSp, players$WAR, method = "pearson") # True Shooting Percentage has a moderate correlation with
# Wins Above Replacement.



## All of the per game metrics will be used in the regression modelling to predict expected WAR, excpet total
## rebounds (RPG)and fouls (FPG). Each position may have different influences on each metric, and that will 
## be explored in the regression analysis.




