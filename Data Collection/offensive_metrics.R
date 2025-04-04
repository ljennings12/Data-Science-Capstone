## Liam Jennings
## Data Science Capstone

# OFFENSIVE STATISTICS ---------------------------------------------------------


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
  # group by week
  group_by(posteam, week) |> 
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
    giveaways = sum(interception) + sum(fumble),
    # number of possessions
    drives = last(drive),
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
    possessions_per_game = cumsum(drives) / seq_along(drives),
    # pass rate
    pass_rate = cumsum(pass_rate) / seq_along(pass_rate),
    # rush rate
    rush_rate = cumsum(rush_rate) / seq_along(rush_rate),
    # pass rate over expected (PROE)
    proe = cumsum(proe) / seq_along(proe),
    # expected points added (EPA)
    epa = cumsum(epa) / seq_along(epa),
    # giveaways per game
    giveaways_per_game = cumsum(giveaways) / seq_along(giveaways)
  )


## test out
offensive_totals <- pbp_24 |>
  # only get pass and rush plays in the regular season
  filter(play == 1, season_type == "REG") |> 
  # group by possessing team
  group_by(game_id, posteam) |> 
  # total offensive yards
  summarize(
    total_offensive_yards = sum(yards_gained, na.rm = TRUE)
  ) |>
  # group by team
  group_by(posteam) |> 
  # total offensive yards per game
  summarize(
    total_offensive_yards = mean(total_offensive_yards)
  )