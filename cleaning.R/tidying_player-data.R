## Reading, Tidying & Transforming Raw Player Data


library(tidyverse) # loads relevant packages for reading, tidying and transforming data

## Reading In Data

player_stats <- read_csv("data/raw/2018-19_nba_player_statistics.csv")

player_stats_adv <- read_csv("data/raw/2018-19_nba_player_stats_advanced.csv")

player_salaries <- read_csv("data/raw/2018-19_nba_player-salaries.csv")


## the above code reads in the relevant data sets and saves them to the objects preceding the <-.


## Tidying & Transforming Data - player_stats


player_stats$player_name <- player_stats$player_name %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  stringr::str_replace_all(pattern = "\\.", replacement = "")


## The above code removes all accents from the player_names variable. It then removes all fullstops, with a blank
## character replacing it.


player_stats_tidy <- player_stats %>%
  arrange(player_name, desc(G)) %>%
  distinct(player_name, .keep_all = TRUE) %>% 
  rename(FGp = 'FG%',
         x3P = '3P',
         x3PA = '3PA',
         x3Pp = '3P%',
         x2P = '2P',
         x2PA = '2PA',
         x2Pp = '2P%',
         eFGp = 'eFG%',
         FTp = 'FT%') %>%
  replace_na(list(FGp = 0, 
                  x3Pp = 0, 
                  x2Pp = 0, 
                  eFGp = 0, 
                  FTp = 0)) %>%
  filter(G >= 29) %>%
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
  select(player_name : G, eFGp, PPG, 
         PPM, APG, APM, OPG, DPG, SPG,
         BPG, TPG, FPG)

## The above code does the following things: 

## 1. Arranges dataset by player name, by games played in descending order.

## 2. Removes duplicates of player_names based on games played. This will keep the row of duplicate data with
## the highest G, representing their season totals.

## 3. Renames variables to get rid of "illegal" variable names.

## 4. Fills in missing percentage values with a known value (0).

## 5. Filters the data so only players who have played more than 29 games are included.
## This is the minimum number of games required for a player (rookies included) 
## to be eligible for leaderboards, per NBA.com.

## 6. Creates new variables based on existing variables. The metrics created pay particular attention
## to per game and per minute totals.

## 7. Rounds the variables categorised as numeric to 3 decimal places.

## 8. Selects variables in the preferred order, leaving out the ones deemed unimportant for my analysis.

## 9. Assigns the changes into an object "player_stats_tidy".




## Tidying & Transforming Data - player_stats_adv


player_stats_adv$player_name <- player_stats_adv$player_name %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  stringr::str_replace_all(pattern = "\\.", replacement = "")


## The above code removes all accents from the player_names variable. It then removes all fullstops, with a blank
## character replacing it.

player_stats_adv__tidy <- player_stats_adv %>%
  arrange(player_name, desc(G)) %>%
  distinct(player_name, .keep_all = TRUE) %>% 
  rename(TSp = 'TS%',
         x3PAr = '3PAr',
         ORBp = 'ORB%',
         DRBp = 'DRB%',
         TRBp = 'TRB%',
         ASTp = 'AST%',
         STLp = 'STL%',
         BLKp = 'BLK%',
         TOVp = 'TOV%',
         USGp = 'USG%',
         WSpG = 'WS/48') %>%
  replace_na(list(TSp = 0, 
                  x3PAr = 0, 
                  FTr = 0, 
                  TOVp = 0)) %>%
  filter(G >= 29) %>%
  mutate(WAR = VORP * 2.70) %>%
  select(player_name, PER, TSp, OWS : WSpG, WAR)


## The above code does the following things:

## 1. Creates a new object, player_stats_adv_tidy, using data from player_stats_adv.

## 2. Arranges dataset by player name, by games played in descending order.

## 3. Removes duplicates of player_names based on games played. This will keep the row of duplicate data with
## the highest G, representing their season totals.

## 4. Renames variables to get rid of "illegal" variable names.

## 5. Fills in missing percentage values with known value (0).

## 6. Filters the data so only players who have played more than 29 games are included.
## This is the minimum number of games required for a player (rookies included) 
## to be eligible for leaderboards, per NBA.com.

## 7. Creates new variable WAR, or Wins Above Replacement. WAR is a metric that estimates the number of 
## additional wins a player contributes to a team than a player deemed replacement level. Basketball Reference
## calculates WAR as 2.7 * VORP (Value Over Replacement Player). Replacement level players have a WAR of 0.7
## according to this metric.

## 8. Selects variables in the preferred order, leaving out the ones deemed unimportant for the analysis.



## Tidying & Transforming Data - player_salaries

player_salaries$player_name <- player_salaries$player_name %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  stringr::str_replace_all(pattern = "\\.", replacement = "")


## The above code removes all accents from the player_names variable. It then removes all fullstops, with a blank
## character replacing it.

player_salaries_tidy <- player_salaries %>%
  select(player_name : salary)
  
## The above code selects the necessary variables for the analysis.



## Combining the Data - player_x_transformed

stats_joined <- left_join(x = player_stats_tidy, y = player_stats_adv__tidy, 
            by = "player_name")

## The above code combines the transformed data of the player_stats & player_stats_adv files.


player_stats_final <- stats_joined %>%
  left_join(x = stats_joined, y = player_salaries_tidy, 
            by = "player_name") %>%
  select(player_name : G, salary, WAR, PPG : FPG, eFGp, PER : WSpG)

## The above code does the following:

## 1. Creates a new object, player_stats_final, using data from stats_joined.

## 2. Joins player_salaries_transformed to stats_joined by player_name to make one set of data.

## 3. Orders the variables into an ideal format.


## Writing the Processed Data to a New File

write_csv(x = player_stats_final, path = "data/processed/player_stats_final.csv")

