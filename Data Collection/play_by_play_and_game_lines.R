## Liam Jennings
## Data Science Capstone


# DATA COLLECTION ---------------------------------------------------------------------

## libraries
library(nflreadr)
library(tidyverse)
library(nflplotR)

## read in data
### NFL 2024 play-by-play
pbp_24 <- nflreadr::load_pbp()

### NFL 2023 play-by-play
pbp_23 <- nflreadr::load_pbp(seasons = 2023)

### NFL 2022 play-by-play
pbp_22 <- nflreadr::load_pbp(seasons = 2022)

### NFL 2021 play-by-play
pbp_21 <- nflreadr::load_pbp(seasons = 2021)

## glimpse
glimpse(pbp_24)


## get total lines for each game
game_totals_24 <- pbp_24 |> 
  # group by game, teams, and weeks
  group_by(game_id, season, home_team, away_team, week, season_type) |> 
  # home and away team scores and total line
  summarize(
    total_home_score = last(total_home_score),
    total_away_score = last(total_away_score),
    total_line = last(total_line)
  ) |> 
  # calculate total final score and whether the over hits or not
  mutate(
    total_score = total_home_score + total_away_score,
    over = ifelse(total_score > total_line, 1, 0)
  ) |> 
  # ungroup
  ungroup()

## get total lines for each game for 2023
game_totals_23 <- pbp_23 |> 
  # group by game, teams, and weeks
  group_by(game_id, season, home_team, away_team, week, season_type) |> 
  # home and away team scores and total line
  summarize(
    total_home_score = last(total_home_score),
    total_away_score = last(total_away_score),
    total_line = last(total_line)
  ) |> 
  # calculate total final score and whether the over hits or not
  mutate(
    total_score = total_home_score + total_away_score,
    over = ifelse(total_score > total_line, 1, 0)
  ) |> 
  # ungroup
  ungroup()


## get total lines for each game for 2022
game_totals_22 <- pbp_22 |> 
  # group by game, teams, and weeks
  group_by(game_id, season, home_team, away_team, week, season_type) |> 
  # home and away team scores and total line
  summarize(
    total_home_score = last(total_home_score),
    total_away_score = last(total_away_score),
    total_line = last(total_line)
  ) |> 
  # calculate total final score and whether the over hits or not
  mutate(
    total_score = total_home_score + total_away_score,
    over = ifelse(total_score > total_line, 1, 0)
  ) |> 
  # ungroup
  ungroup()

## get total lines for each game for 2021
game_totals_21 <- pbp_21 |> 
  # group by game, teams, and weeks
  group_by(game_id, season, home_team, away_team, week, season_type) |> 
  # home and away team scores and total line
  summarize(
    total_home_score = last(total_home_score),
    total_away_score = last(total_away_score),
    total_line = last(total_line)
  ) |> 
  # calculate total final score and whether the over hits or not
  mutate(
    total_score = total_home_score + total_away_score,
    over = ifelse(total_score > total_line, 1, 0)
  ) |> 
  # ungroup
  ungroup()

## combine into one dataframe
game_total_lines <- rbind(game_totals_21, game_totals_22, game_totals_23, game_totals_24)

