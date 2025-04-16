## Liam Jennings
## Data Science Capstone


# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)


# Time of Possession ------------------------------------------------------

## calculate average time of possession per game
t_o_p <- {
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
      team = posteam,
      
      # time of possession in seconds
      time_of_possession_sec = as.numeric(
        ms(
          drive_time_of_possession
        )
      )
    ) |> 
    # group by game ID, team, week, and possession
    group_by(
      game_id,
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
    group_by(team) |> 
    mutate(
      # sequence along to get cumulative average
      avg_time_of_possession_sec = cumsum(avg_time_of_possession_sec) / seq_along(avg_time_of_possession_sec),
      
      # convert average time of possession back to minutes and seconds for visual
      avg_time_of_possession = seconds_to_period(avg_time_of_possession_sec),
      
      # sequence along to get cumulative average
      avg_drive_time_sec = cumsum(avg_drive_time) / seq_along(avg_drive_time),
      
      # convert average time of possession back to minutes and seconds for visual
      avg_drive_time = seconds_to_period(avg_drive_time_sec)
    ) |> 
    # ungroup
    ungroup() |> 
    # select
    select(
      game_id,
      team,
      week,
      avg_drive_time_sec, 
      avg_drive_time,
      avg_time_of_possession_sec,
      avg_time_of_possession
    )
}



# Offensive Metrics -------------------------------------------------------

## get offensive statistics
offensive_metrics <- {
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
      team = posteam
    ) |> 
    # group by week
    group_by(game_id, team, week) |> 
    # calculate offensive statistics
    summarize(
      # total yards
      total_yards = sum(yards_gained),
      
      # total passing yards - does not include loss of yards from sacks
      total_pass_yards = sum(passing_yards, na.rm = TRUE),
      
      # total rushing yards
      total_rush_yards = sum(rushing_yards, na.rm = TRUE),
      
      # points per game
      points = last(posteam_score),
      
      # pass rate
      pass_rate = mean(pass),
      
      # rush rate
      rush_rate = sum(rush + qb_kneel) / n(),
      
      # pass rate over expected (PROE)
      proe = mean(pass) - mean(xpass, na.rm = TRUE),
      
      # expected points added (EPA)
      epa = mean(epa, na.rm = TRUE),
      
      # giveaways
      giveaways = sum(interception) + sum(fumble_lost),
      
      # number of possessions
      possession = n_distinct(possession),
      
      # divisional game
      div_game = last(div_game)
    ) |> 
    # ungroup
    ungroup() |> 
    # group by team
    group_by(team) |> 
    # average statistics going into the match up
    mutate(
      # average total yards
      avg_total_yards = cumsum(total_yards) / seq_along(total_yards),
      
      # average total passing yards
      avg_total_pass_yards = cumsum(total_pass_yards) / seq_along(total_pass_yards),
      
      # average total rushing yards
      avg_total_rush_yards = cumsum(total_rush_yards) / seq_along(total_rush_yards),
      
      # points per game
      ppg = cumsum(points) / row_number(),
      
      # series per game (figure out how to calculate this and time of possession)
      possessions_per_game = cumsum(possession) / seq_along(possession),
      
      # pass rate
      pass_rate = cumsum(pass_rate) / seq_along(pass_rate),
      
      # rush rate
      rush_rate = cumsum(rush_rate) / seq_along(rush_rate),
      
      # pass rate over expected (PROE)
      proe = cumsum(proe) / seq_along(proe),
      
      # expected points added (EPA)
      epa = cumsum(epa) / seq_along(epa),
      
      # giveaways per game
      giveaways_per_game = cumsum(giveaways) / seq_along(giveaways),
      
      # points per possession
      points_per_poss = ppg / possessions_per_game
    ) |> 
    # ungroup
    ungroup() |> 
    # join time of possession to dataset
    inner_join(
      t_o_p
    )
}

