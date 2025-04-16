## Liam Jennings
## Data Science Capstone


# Libraries ---------------------------------------------------------------------

## libraries
library(nflreadr)
library(tidyverse)


# Game Lines --------------------------------------------------------------

## read in data
### NFL 2024 play-by-play
pbp_24 <- nflreadr::load_pbp()

### NFL 2023 play-by-play
pbp_23 <- nflreadr::load_pbp(seasons = 2023)

### NFL 2022 play-by-play
pbp_22 <- nflreadr::load_pbp(seasons = 2022)

### NFL 2021 play-by-play
pbp_21 <- nflreadr::load_pbp(seasons = 2021)

### combine play-by-play into one dataset
play_by_play <- rbind(pbp_21, pbp_22, pbp_23, pbp_24)

## glimpse
glimpse(play_by_play)


## get total lines for each game
game_total_lines <- {
  play_by_play |> 
    # filter
    filter(
      # regular season
      season_type == "REG",
      
      # remove international games
      location == "Home"
    ) |> 
    # group by game, teams, and weeks
    group_by(game_id, season, home_team, away_team, week) |> 
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
}

