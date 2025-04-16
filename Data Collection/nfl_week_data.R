## Liam Jennings
## Data Science Capstone


# Libraries ---------------------------------------------------------------------

## libraries
library(nflreadr)
library(tidyverse)


# Combining Datasets --------------------------------------------------------------


# Reshape Offensive Metrics -----------------------------------------------

offensive_metrics_reshape <- {
  offensive_metrics |> 
    # group by game_id
    group_by(game_id) |> 
    mutate(
      # create home team based on game_id variable
      home_team = sub(".*_", "", game_id),
      
      # create a team role variable to use in pivot wider
      team_role = if_else(
        # condition
        team == home_team,
        
        # if home team
        "home_team",
        
        # if not home team
        "away_team"
      )
    ) |> 
    # reshape the data
    pivot_wider(
      id_cols = game_id,
      
      names_from = team_role,
      
      values_from = -c(game_id, team_role)
    ) |> 
    # select columns
    select(
      game_id,
      home_team = team_home_team,
      away_team = team_away_team,
      week = week_home_team,
      div_game = div_game_away_team,
      pass_rate_away_team:epa_home_team,
      avg_total_yards_home_team:avg_time_of_possession_home_team
    )
}


# Reshape Defensive Metrics -----------------------------------------------

defensive_metrics_reshape <- {
  defensive_metrics |> 
    # group by game_id
    group_by(game_id) |> 
    mutate(
      # create home team based on game_id variable
      home_team = sub(".*_", "", game_id),
      
      # create a team role variable to use in pivot wider
      team_role = if_else(
        # condition
        team == home_team,
        
        # if home team
        "home_team",
        
        # if not home team
        "away_team"
      )
    ) |> 
    # reshape the data
    pivot_wider(
      id_cols = game_id,
      
      names_from = team_role,
      
      values_from = -c(game_id, team_role)
    ) |> 
    # select columns
    select(
      game_id,
      home_team = team_home_team,
      away_team = team_away_team,
      week = week_home_team,
      opposing_pass_rate_away_team:epa_allowed_home_team,
      avg_drive_time_sec_allowed_away_team:avg_time_of_possession_allowed_home_team
    )
}



# Combine All Datasets Together -------------------------------------------

nfl_team_week_data <- {
  game_total_lines |> 
    # join offensive metrics
    inner_join(offensive_metrics_reshape) |> 
    
    # join defensive metrics
    inner_join(defensive_metrics_reshape) |> 
    
    # join independent factors
    inner_join(independent_metrics)
}
