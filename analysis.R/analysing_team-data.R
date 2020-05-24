## Exploratory Data Analysis - Teams


## Loading Relevant Packages

library(tidyverse)



## Reading in the Data

teams <- read_csv("data/processed/team_stats_final.csv")


## Exploratory Analysis of the Data

## Each cycle of code creates a plot, prints the plot & shows the correlation coefficient. The correlation is 
## important as it will contribute to the modelling process.


## Salary vs Wins


salary_wins <- teams %>%
  ggplot(aes(Salary / 1000000, W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 

salary_wins

cor(teams$Salary, teams$W, method = "pearson") # More money to spend does not equal more wins.


## PPG vs Wins


ppg_wins <- teams %>%
  ggplot(aes(PPG, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

ppg_wins

cor(teams$PPG, teams$W, method = "pearson") # More points per game generally signifies more wins.


## TSp vs Wins

tsp_wins <- teams %>%
  ggplot(aes(TSp, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

tsp_wins

cor(teams$TSp, teams$W, method = "pearson") # Higher true shooting percentage strongly contributes to more wins.



## eFGp vs Wins

efgp_wins <- teams %>%
  ggplot(aes(eFGp, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

efgp_wins

cor(teams$eFGp, teams$W, method = "pearson") # Better field goal efficiency strongly contributes to more wins.


## APG vs Wins

apg_wins <- teams %>%
  ggplot(aes(APG, W)) +
  geom_point() +
  geom_smooth(method = "lm")

apg_wins

cor(teams$APG, teams$W, method = "pearson") # More assists per game moderately affects wins.



## RPG vs Wins


rpg_wins <- teams %>%
  ggplot(aes(RPG, W)) +
  geom_point() +
  geom_smooth(method = "lm")

rpg_wins

cor(teams$RPG, teams$W, method = "pearson") # More rebounds per game moderately affects wins.


## OPG vs Wins

opg_wins <- teams %>%
  ggplot(aes(OPG, W)) +
  geom_point() +
  geom_smooth(method = "lm")

opg_wins

cor(teams$OPG, teams$W, method = "pearson") # Offensive rebounds do not affect wins.


## DPG vs Wins

dpg_wins <- teams %>%
  ggplot(aes(DPG, W)) +
  geom_point() +
  geom_smooth(method = "lm")

dpg_wins

cor(teams$DPG, teams$W, method = "pearson") # More defensive rebounds per game moderately affects wins.


## SPG vs Wins

spg_wins <- teams %>%
  ggplot(aes(SPG, W)) +
  geom_point() +
  geom_smooth(method = "lm")

spg_wins

cor(teams$SPG, teams$W, method = "pearson") # More steals per weakly influences wins.


## BPG vs Wins

bpg_wins <- teams %>%
  ggplot(aes(BPG, W)) +
  geom_point() +
  geom_smooth(method = "lm")

bpg_wins

cor(teams$BPG, teams$W, method = "pearson") # Blocks per game moderately affects wins.


## TPG vs WIns

tpg_wins <- teams %>%
  ggplot(aes(TPG, W)) +
  geom_point() +
  geom_smooth(method = "lm")

tpg_wins

cor(teams$TPG, teams$W, method = "pearson") # Turnovers per game have a weak influence on losses.


## FPG vs Wins

fpg_wins <- teams %>%
  ggplot(aes(FPG, W)) +
  geom_point() +
  geom_smooth(method = "lm")

fpg_wins

cor(teams$FPG, teams$W, method = "pearson") # Fouls have a weak influence on losses.



  