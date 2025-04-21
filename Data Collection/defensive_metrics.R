## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)


# Possession --------------------------------------------------------------

## calculate average time of possession per game
t_o_p_defense <- {
  play_by_play |> 
    filter(
      # regular season
      season_type == "REG",
      
      # remove plays that were blown dead
      play_type != "no_play",
      
      # plays that are not two point attempts
      two_point_attempt == 0,
      
      # remove extra points
      special == 0
    ) |>
    # mutate
    mutate(
      # create artificial number of possessions variable
      possession = paste(drive, "-", fixed_drive_result),
      
      # call posteam by team (for plotting purposes in nflplotr)
      team = defteam,
      
      # time of possession in seconds
      time_of_possession_sec = as.numeric(
        ms(
          drive_time_of_possession
        )
      )
    ) |> 
    # group by team, week, and possession
    group_by(
      game_id,
      season,
      team, 
      week,
      possession
    ) |> 
    # summarize
    summarize(
      # average time of possession in seconds
      avg_drive_time_of_possession_sec = mean(time_of_possession_sec)
    ) |> 
    # ungroup
    ungroup() |> 
    # group by team and week
    group_by(
      game_id,
      season,
      team, 
      week
    ) |> 
    # summarize
    summarize(
      # average time of possession for each game and week in seconds
      avg_time_of_possession_sec = sum(avg_drive_time_of_possession_sec),
      
      # average drive time of possession for each game and week in seconds
      avg_drive_time = mean(avg_drive_time_of_possession_sec)
    ) |> 
    # ungroup
    ungroup() |> 
    # group by team
    group_by(team, season) |> 
    mutate(
      # sequence along to get cumulative average
      avg_time_of_possession_sec_allowed = cumsum(avg_time_of_possession_sec) / seq_along(avg_time_of_possession_sec),
      
      # convert average time of possession back to minutes and seconds for visual
      avg_time_of_possession_allowed = seconds_to_period(avg_time_of_possession_sec_allowed),
      
      # sequence along to get cumulative average
      avg_drive_time_sec_allowed = cumsum(avg_drive_time) / seq_along(avg_drive_time),
      
      # convert average time of possession back to minutes and seconds for visual
      avg_drive_time_allowed = seconds_to_period(avg_drive_time_sec_allowed)
    ) |> 
    # ungroup
    ungroup() |> 
    # select
    select(
      game_id,
      team,
      week,
      avg_drive_time_sec_allowed, 
      avg_drive_time_allowed,
      avg_time_of_possession_sec_allowed,
      avg_time_of_possession_allowed
    )
}



# Defensive Metrics -------------------------------------------------------

## get offensive statistics
defensive_metrics <- {
  play_by_play |> 
    filter(
      # regular season
      season_type == "REG",
      
      # real plays
      play == 1 |
      # QB kneels
      qb_kneel == 1,
      
      # remove plays that were blown dead
      play_type != "no_play",
      
      # plays that are not two point attempts
      two_point_attempt == 0
    ) |>
    mutate(
      # create artificial number of possessions variable
      possession = paste(drive, "-", fixed_drive_result),
      
      # call posteam by team (for plotting purposes in nflplotr)
      team = defteam
    ) |> 
    # group by week
    group_by(game_id, season, team, week) |> 
    # calculate offensive statistics
    summarize(
      # total yards allowed
      total_yards_allowed = sum(yards_gained),
      
      # total passing yards allowed - does not include loss of yards from sacks
      total_pass_yards_allowed = sum(passing_yards, na.rm = TRUE),
      
      # total rushing yards allowed
      total_rush_yards_allowed = sum(rushing_yards, na.rm = TRUE),
      
      # points per game allowed
      points_allowed = last(posteam_score),
      
      # pass rate
      opposing_pass_rate = mean(pass),
      
      # rush rate
      opposing_rush_rate = sum(rush + qb_kneel) / n(),
      
      # pass rate over expected (PROE)
      opposing_proe = mean(pass) - mean(xpass, na.rm = TRUE),
      
      # expected points added (EPA)
      epa_allowed = mean(epa, na.rm = TRUE),
      
      # giveaways
      takeaways = sum(interception) + sum(fumble_lost),
      
      # number of possessions
      possession_allowed = n_distinct(possession)
    ) |> 
    # ungroup
    ungroup() |> 
    # group by team and season
    group_by(team, season) |> 
    # average statistics going into the match up
    mutate(
      # average total yards
      avg_total_yards_allowed = cumsum(total_yards_allowed) / seq_along(total_yards_allowed),
      
      # average total passing yards
      avg_total_pass_yards_allowed = cumsum(total_pass_yards_allowed) / seq_along(total_pass_yards_allowed),
      
      # average total rushing yards
      avg_total_rush_yards_allowed = cumsum(total_rush_yards_allowed) / seq_along(total_rush_yards_allowed),
      
      # points per game
      ppg_allowed = cumsum(points_allowed) / seq_along(points_allowed),
      
      # series per game (figure out how to calculate this and time of possession)
      possessions_per_game_allowed = cumsum(possession_allowed) / seq_along(possession_allowed),
      
      # pass rate
      opposing_pass_rate = cumsum(opposing_pass_rate) / seq_along(opposing_pass_rate),
      
      # rush rate
      opposing_rush_rate = cumsum(opposing_rush_rate) / seq_along(opposing_rush_rate),
      
      # pass rate over expected (PROE)
      opposing_proe = cumsum(opposing_proe) / seq_along(opposing_proe),
      
      # expected points added (EPA)
      epa_allowed = cumsum(epa_allowed) / seq_along(epa_allowed),
      
      # giveaways per game
      takeaways_per_game = cumsum(takeaways) / seq_along(takeaways),
      
      # points per possession
      points_allowed_per_poss = ppg_allowed / possessions_per_game_allowed
    ) |> 
    # ungroup
    ungroup() |> 
    # join time of possession to dataset
    inner_join(
      t_o_p_defense
    )
}



# Lagged Metrics ------------------------------------------------------------

defensive_metrics_lagged <- defensive_metrics |> 
  # order dataset
  arrange(team, season, week) |> 
  # group by team and season
  group_by(team, season) |> 
  mutate(
    # apply lag function across certain columns
    across(
      # columns
      c(opposing_pass_rate:epa_allowed, avg_total_yards_allowed:avg_time_of_possession_allowed),
      # lag function
      lag
    )
  ) |> 
  # ungroup
  ungroup() |> 
  # remove week 1
  drop_na(opposing_pass_rate)
