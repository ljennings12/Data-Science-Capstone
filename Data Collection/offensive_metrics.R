## Liam Jennings
## Data Science Capstone


# Libraries and Functions -------------------------------------------------

# Offensive Metrics ---------------------------------------------------------

## calculate average time of possession per game
t_o_p <- pbp_24 |> 
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
    team,
    week,
    avg_drive_time_sec, 
    avg_drive_time,
    avg_time_of_possession_sec,
    avg_time_of_possession
  )


## get offensive statistics
offensive_metrics <- pbp_24 |> 
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
    # initial win probability (could look at vegas wp)
    win_prob = first(wp),
    # giveaways
    giveaways = sum(interception) + sum(fumble_lost),
    # number of possessions
    possession = n_distinct(possession),
    # divisional game
    div_game = last(div_game)
  ) |> 
  # average statistics going into the match up
  mutate(
    # average total yards
    avg_total_yards = cumsum(total_yards) / seq_along(total_yards),
    # average total passing yards
    avg_total_pass_yards = cumsum(total_pass_yards) / seq_along(total_pass_yards),
    # average total rushing yards
    avg_total_rush_yards = cumsum(total_rush_yards) / seq_along(total_rush_yards),
    # points per game
    ppg = cumsum(points) / seq_along(points),
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
  # join time of possession to dataset
  inner_join(
    t_o_p
  )




# Defensive Metrics -------------------------------------------------------

## calculate average time of possession per game
t_o_p_defense <- pbp_24 |> 
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
    team,
    week,
    avg_drive_time_sec, 
    avg_drive_time,
    avg_time_of_possession_sec,
    avg_time_of_possession
  )


## get offensive statistics
defensive_metrics <- pbp_24 |> 
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
  group_by(game_id, team, week) |> 
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
  # join time of possession to dataset
  inner_join(
    t_o_p_defense
  )




# Independent Metrics -------------------------------------------------------

independent_metrics <- {
  pbp_24 |> 
    # group by game
    group_by(game_id, home_team, away_team, week) |> 
    # summarize
    summarize(
      roof = first(roof),
      temperature = first(temp),
      wind = first(wind),
      weather = last(weather),
      time_of_kickoff = first(time_of_day, na_rm = TRUE)
    ) |> 
    # mutate
    mutate(
      # parse the ISO 8601 string properly to strip milliseconds and handle the time zone
      time_of_kickoff = ymd_hms(sub("\\.\\d+Z$", "", time_of_kickoff), tz = "UTC"),
      
      # add time zone to the dataset
      home_timezone = case_when(
        # Eastern Time Zone
        home_team %in% c(
          "ATL", "BAL", "BUF", "CAR", "CIN", "CLE", 
          "DET", "IND", "JAX", "MIA", "NE", "NYG",
          "NYJ", "PHI", "PIT", "TB", "WAS"
        ) ~ "US/Eastern",
        
        # Central Time Zone
        home_team %in% c(
          "CHI", "DAL", "GB", "HOU", "KC", "MIN", "NO", "TEN"
        ) ~ "US/Central",
        
        # Mountain Time Zone
        home_team %in% c(
          "DEN"
        ) ~ "US/Mountain",
        
        # Arizona
        home_team %in% c(
          "ARI"
        ) ~ "US/Arizona",
        
        # Pacific
        home_team %in% c(
          "LA", "LAC", "LV", "SEA", "SF"
        ) ~ "US/Pacific"
      ),
      
      # add time zone to the dataset
      away_timezone = case_when(
        # Eastern Time Zone
        away_team %in% c(
          "ATL", "BAL", "BUF", "CAR", "CIN", "CLE", 
          "DET", "IND", "JAX", "MIA", "NE", "NYG",
          "NYJ", "PHI", "PIT", "TB", "WAS"
        ) ~ "US/Eastern",
        
        # Central Time Zone
        away_team %in% c(
          "CHI", "DAL", "GB", "HOU", "KC", "MIN", "NO", "TEN"
        ) ~ "US/Central",
        
        # Mountain Time Zone
        away_team %in% c(
          "DEN"
        ) ~ "US/Mountain",
        
        # Arizona
        away_team %in% c(
          "ARI"
        ) ~ "US/Arizona",
        
        # Pacific
        away_team %in% c(
          "LA", "LAC", "LV", "SEA", "SF"
        ) ~ "US/Pacific"
      ),
      
      # eastern kickoff time with specified time zone
      eastern_kickoff_time = format(
        with_tz(time_of_kickoff, tzone = "US/Eastern"),
        "%I:%M:%S %p"
      ),
      
      # local kickoff time with specified time zone
      local_kickoff_time = with_tz(time_of_kickoff, tzone = home_timezone),  
      
      # away team kickoff time with specified time zone
      away_team_kickoff_time = with_tz(time_of_kickoff, tzone = away_timezone)  
      
      # extract the time of day
      #time_of_kickoff = hms::as_hms(time_of_kickoff)
    ) |> 
    # ungroup
    ungroup()
}
