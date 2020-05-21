## Reading, Tidying & Transforming Raw Team Data


## Load Relevant Packages

library(tidyverse)


## Reading In Data

team_stats_1 <- read_csv("data/raw/2018-19_nba_team_statistics_1.csv")

team_stats_2 <- read_csv("data/raw/2018-19_nba_team_statistics_2.csv")

team_payroll <- read_csv("data/raw/2019-20_nba_team-payroll.csv")


## The above code reads in the relevant data sets and saves them to the objects preceding the <-.


## Tidying & Transforming Data - team_stats_1

team_tidy_1 <- team_stats_1 %>%
  rename(x3PAr = '3PAr',
         TSp = 'TS%',
         eFGp = 'eFG%',
         TOVp = 'TOV%',
         ORBp = 'ORB%',
         FTpFGA = 'FT/FGA',
         DRBp = 'DRB%') %>%
  arrange(Team) %>%
  select(Team : W, Rk, ORtg : DRtg, TSp : eFGp)


## The above code does the following:

## 1. Creates new object, team_tidy_1, consisting of data from team_stats_1.

## 2. Renames variables with illegal characters in them.

## 3. Arranges the data so the teams are in alphabetical order.

## 4. Selects variables in the preferred order, leaving out ones deemed irrelavant for the analysis.



## Tidying & Transforming Data - team_stats_2

team_tidy_2 <-team_stats_2 %>%
  rename(FGp = 'FG%',
         x3P = '3P',
         x3PA = '3PA',
         x3Pp = '3P%',
         x2P = '2P',
         x2PA = '2PA',
         x2Pp = '2P%',
         FTp = 'FT%') %>%
  arrange(Team) %>%
  mutate(PPG = PTS / G,
         APG = AST / G,
         RPG = TRB / G,
         OPG = ORB / G,
         DPG = DRB / G,
         SPG = STL / G,
         BPG = BLK / G,
         TPG = TOV / G,
         FPG = PF / G,
         PPM = PTS / MP,
         APM = AST / MP,
         RPM = TRB / MP) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(Team : G, PPG : RPM)

## The above code does the following: 

## 1. Creates a new object, team_tidy_2, using data from team_stats_2.

## 2. Renames variables with illegal characters in them.

## 3. Arranges the teams so they are in alphabetical order.

## 4. Creates new variables based on existing variables, paying particular attention to per game
## and per minute metrics.

## 5. Rounds the variables categorised as numeric to 3 decimal places.

## 6. Selects variables in the preferred order, leaving out ones deemed unnecessary for the analysis.



## Tidying & Transforming Data - team_payroll

team_payroll$salary <- team_payroll$salary %>%
  str_replace_all(pattern = "\\$", replacement = "") %>%
  str_replace_all(pattern = "\\,", replacement = "") %>%
  as.numeric()

## The above code does the following:

## 1. Saves changes made into salary variable of the team_payroll data set.

## 2. Removes the $ and , from the salary value to make them resemble numbers.

## 3. Changes the variable class to numeric.



team_payroll$team_id <- team_payroll$team_id %>%
  as.factor()

## The above code changes the team_id variable in the team_payroll data from a character vector to a factor vector.
## This is important for the next step.


levels(team_payroll$team_id) <- c("Miami Heat", "Golden State Warriors", "Oklahoma City Thunder", "Toronto Raptors",
                               "Milwaukee Bucks", "Portland Trail Blazers", "Detroit Pistons", "Houston Rockets",
                               "Memphis Grizzlies", "Boston Celtics", "Washington Wizards", "New York Knicks",
                               "Cleveland Cavaliers", "Minnesota Timberwolves", "San Antonio Spurs",
                               "Charlotte Hornets", "Brooklyn Nets", "Denver Nuggets", "Los Angeles Clippers",
                               "New Orleans Pelicans", "Philadelphia 76ers", "Orlando Magic", "Utah Jazz",
                               "Chicago Bulls", "Indiana Pacers", "Phoenix Suns", "Los Angeles Lakers",
                               "Sacramento Kings", "Dallas Mavericks", "Atlanta Hawks") 

team_payroll$team_id <- as.character(team_payroll$team_id)

## The above code renames all the team_id levels to their respective city and nickname. It is then converted
## to a character vector so it is easier to combine the data later on.



team_payroll_tidy <- team_payroll %>%
  rename(Team = 'team_id',
         Salary = 'salary') %>%
  arrange(Team) %>%
  select(Team, Salary)
  
  

## The above code does the following:

## 1. Creates a new object, team_payroll_tidy, using data from team_payroll.

## 2. Renames the team_id variable to "Team" to facilitate easier combining of objects later on.
## The salary variable is also capitalised.

## 3. Arranges the Team variable so the teams are in alphabetical order.

## 4. Selects the variables in the preferred order, leaving out ones deemed unnecessary for the analysis.



## Combining the Transformed Data

team_joined <- left_join(team_tidy_1, team_tidy_2,
                         "Team")

## The above code joins team_trans_1 to team_trans_2, matching by Team, and is then saved into a new 
## object called team_joined.


team_stats_final <- team_joined %>%
  left_join(x = team_joined, y = team_payroll_tidy,
            by = "Team") %>%
  select(Team, G, W, Rk, Salary, ORtg : eFGp, PPG : RPM)


## The above code does the following:

## 1. Creates a new object, team_stats_final, using data from team_joined.

## 2. team_payroll_tidy is joined to team_joined, matching by Team.

## 3. The variables are reordered in a more ideal way for analysis.



## Writing the Processed Data to a New File

write_csv(x = team_stats_final, path = "data/processed/team_stats_final.csv")