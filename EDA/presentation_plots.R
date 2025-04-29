## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(nflplotR)
library(patchwork)
library(gt)
library(gtExtras)


## create theme
nfl_bar_theme <- function(){
  # theme
  theme(
    # adjust plot title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    # adjust plot subtitle
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
    # adjust plot caption
    plot.caption = element_text(size = 10),
    # # adjust x axis title
    # axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5),
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = element_blank(),
    # adjust y axis title
    axis.title.y = element_text(size = 14, face = "bold", hjust = 0.5),
    # # adjust x axis text
    # axis.text.x = element_text(size = 12, hjust = 0.5),
    # adjust y axis text
    axis.text.y = element_text(size = 12, hjust = 0.5),
    # adjust legend position
    legend.position = "bottom",
    # adjust legend title text
    legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust legend text
    legend.text = element_text(size = 12, hjust = 0.5),
    # replacement of team abbreviations with logos
    axis.text.x = nflplotR::element_nfl_logo(size = 1),
    # adjust the strip text size
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    # adjust the strip text background color
    strip.background = element_rect(fill = "#013369", color = "black", linewidth = 1),
  )
}


## create theme
nfl_plot_theme <- function(){
  # theme
  theme(
    # adjust plot title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    # adjust plot subtitle
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
    # adjust plot caption
    plot.caption = element_text(size = 10),
    # adjust x axis title
    axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust y axis title
    axis.title.y = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust x axis text
    axis.text.x = element_text(size = 12, hjust = 0.5),
    # adjust y axis text
    axis.text.y = element_text(size = 12, hjust = 0.5),
    # adjust legend position
    legend.position = "bottom",
    # adjust legend title text
    legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust legend text
    legend.text = element_text(size = 12, hjust = 0.5),
    # adjust the strip text size
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    # adjust the strip text background color
    strip.background = element_rect(fill = "#013369", color = "black", linewidth = 1),
  )
}

## set theme
theme_set(theme_bw())


## data
## create difference variables
nfl_model_data <- {
  nfl_team_week_data |> 
    # mutate
    mutate(
      # have team as a factor
      home_team = factor(home_team),
      away_team = factor(away_team),
      
      # have divisional game as a factor
      div_game = factor(div_game),
      
      # have hour time difference as a factor
      hour_time_difference = factor(hour_time_difference),
      
      # difference in points scored
      net_ppg = ppg_home_team - ppg_allowed_away_team,
      
      # difference in points allowed
      net_ppg_allowed = ppg_allowed_home_team - ppg_away_team,
      
      # difference in offensive yards
      net_offensive_yards = avg_total_yards_home_team - avg_total_yards_allowed_away_team,
      
      # difference in defensive yards
      net_defensive_yards = avg_total_yards_allowed_home_team - avg_total_yards_away_team,
      
      # difference in offensive possessions
      net_offensive_possessions = possessions_per_game_home_team - possessions_per_game_allowed_away_team,
      
      # difference in defensive possessions
      net_defensive_possessions = possessions_per_game_allowed_home_team - possessions_per_game_away_team,
      
      # difference in giveaways
      net_giveaways = giveaways_per_game_home_team - takeaways_per_game_away_team,
      
      # difference in takeaways
      net_takeaways = giveaways_per_game_away_team - takeaways_per_game_home_team,
      
      # difference in offensive EPA
      net_epa = epa_home_team - epa_allowed_away_team,
      
      # difference in defensive EPA
      net_epa_allowed = epa_allowed_home_team - epa_away_team,
      
      # difference in offensive pass rate
      net_pass_rate = pass_rate_home_team - opposing_pass_rate_away_team,
      
      # difference in opposing pass rate
      net_opposing_pass_rate = pass_rate_away_team - opposing_pass_rate_home_team,
      
      # difference in pass rate over expected
      net_proe = proe_home_team - opposing_proe_away_team,
      
      # difference in opposing pass rate over expected
      net_opposing_proe = opposing_proe_home_team - proe_away_team,
      
      # difference in points scored
      net_ppg_ma4 = ppg_ma4_home_team - ppg_allowed_ma4_away_team,
      
      # difference in points allowed
      net_ppg_allowed_ma4 = ppg_allowed_ma4_home_team - ppg_ma4_away_team,
      
      # difference in offensive yards
      net_offensive_yards_ma4 = avg_total_yards_ma4_home_team - avg_total_yards_allowed_ma4_away_team,
      
      # difference in defensive yards
      net_defensive_yards_ma4 = avg_total_yards_allowed_ma4_home_team - avg_total_yards_ma4_away_team,
      
      # difference in offensive possessions
      net_offensive_possessions_ma4 = possessions_per_game_ma4_home_team - possessions_per_game_allowed_ma4_away_team,
      
      # difference in defensive possessions
      net_defensive_possessions_ma4 = possessions_per_game_allowed_ma4_home_team - possessions_per_game_ma4_away_team,
      
      # difference in giveaways
      net_giveaways_ma4 = giveaways_per_game_ma4_home_team - takeaways_per_game_ma4_away_team,
      
      # difference in takeaways
      net_takeaways_ma4 = takeaways_per_game_ma4_home_team - giveaways_per_game_ma4_away_team,
      
      # difference in offensive EPA
      net_epa_ma4 = epa_ma4_home_team - epa_allowed_ma4_away_team,
      
      # difference in defensive EPA
      net_epa_allowed_ma4 = epa_allowed_ma4_home_team - epa_ma4_away_team,
      
      # difference in offensive pass rate
      net_pass_rate_ma4 = pass_rate_ma4_home_team - opposing_pass_rate_ma4_away_team,
      
      # difference in opposing pass rate
      net_opposing_pass_rate_ma4 = opposing_pass_rate_ma4_home_team - pass_rate_ma4_away_team,
      
      # difference in pass rate over expected
      net_proe_ma4 = proe_ma4_home_team - opposing_proe_ma4_away_team,
      
      # difference in opposing pass rate over expected
      net_opposing_proe_ma4 = opposing_proe_ma4_home_team - proe_ma4_away_team,
      
      # temperature as a factor variable
      temperature = factor(
        # categories
        case_when(
          temperature < 32 ~ "Freezing",
          temperature >= 32 & temperature <= 50 ~ "Cold",
          temperature >= 51 & temperature <= 65 ~ "Cool",
          temperature >= 66 & temperature <= 75 ~ "Mild",
          temperature >= 76 & temperature <= 85 ~ "Warm",
          temperature > 85                      ~ "Hot",
          is.na(temperature)                    ~ "Inside"
        ),
        
        # factor levels
        levels = c(
          "Freezing",
          "Cold",
          "Cool",
          "Mild",
          "Warm",
          "Hot",
          "Inside"
        )
      ),
      
      # convert wind to a factor
      wind = factor(
        # categories
        case_when(
          wind < 1 ~ "Calm",
          wind >= 1 & wind <= 3   ~ "Light Air",
          wind >= 4 & wind <= 7   ~ "Light Breeze",
          wind >= 8 & wind <= 12  ~ "Gentle Breeze",
          wind >= 13 & wind <= 18 ~ "Moderate Breeze",
          wind >= 19 & wind <= 24 ~ "Fresh Breeze",
          wind >= 25 & wind <= 31 ~ "Strong Breeze",
          wind >= 32 & wind <= 38 ~ "Near Gale",
          wind >= 39 & wind <= 46 ~ "Gale",
          is.na(wind)             ~ "Inside"
        ),
        
        # factor levels
        levels = c(
          "Calm",
          "Light Air",
          "Light Breeze",
          "Gentle Breeze",
          "Moderate Breeze",
          "Fresh Breeze",
          "Strong Breeze",
          "Near Gale",
          "Gale",
          "Inside"
        )
      ),
      
      # convert roof to a factor
      roof = factor(
        recode(
          roof,
          "closed" = "dome",
          "open" = "dome"
        )
      )
    ) |> 
    # select columns to use in modeling
    select(
      game_id,
      season,
      week,
      home_team,
      away_team,
      over:avg_drive_time_sec_away_team,
      opposing_pass_rate_home_team:avg_drive_time_sec_allowed_away_team,
      avg_total_yards_allowed_ma4_home_team,
      avg_total_yards_allowed_ma4_away_team,
      avg_total_yards_ma4_home_team,
      avg_total_yards_ma4_away_team,
      starts_with("net_"),
      hour_time_difference,
      temperature,
      wind,
      roof
    ) |> 
    # drop NAs (remove weeks 2-4)
    drop_na(
      starts_with("net")
    )
}


# Team Plots --------------------------------------------------------------

## over by home team
home_team_over <- nfl_team_week_data |> 
  # group by home team
  group_by(home_team) |> 
  summarize(
    over_prop = sum(over) / n()
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = home_team,
      # y axis
      y = over_prop
    )
  ) +
  # column
  geom_col(
    aes(color = home_team, fill = home_team)
  ) +
  # scale fill value
  scale_color_nfl(type = "secondary") +
  # scale fill opacity
  scale_fill_nfl(alpha = 0.4) +
  # line at 50%
  geom_hline(
    # line at 50%
    yintercept = 0.50,
    # color
    color = "#D50A0A",
    # dashed line
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale y axis
  scale_y_continuous(
    # breaks
    breaks = seq(0, 0.60, 0.10),
    # limits
    limits = c(0, 0.65),
    # convert to a percentage
    labels = scales::percent_format(accuracy = 1)
  ) +
  # labels
  labs(
    y = "Pecentage of Games the Over Hit",
    title = "At Home"
  ) +
  # custom theme
  nfl_bar_theme() +
  # remove x axis
  theme(
    axis.text.x = element_blank()
  )


## over by away team
away_team_over <- nfl_team_week_data |> 
  # group by home team
  group_by(away_team) |> 
  summarize(
    over_prop = sum(over) / n()
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = away_team,
      # y axis
      y = over_prop
    )
  ) +
  # column
  geom_col(
    aes(color = away_team, fill = away_team)
  ) +
  # scale fill value
  scale_color_nfl(type = "secondary") +
  # scale fill opacity
  scale_fill_nfl(alpha = 0.4) +
  # line at 50%
  geom_hline(
    # line at 50%
    yintercept = 0.50,
    # color
    color = "#D50A0A",
    # dashed line
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale y axis
  scale_y_continuous(
    # breaks
    breaks = seq(0, 0.60, 0.10),
    # limits
    limits = c(0, 0.65),
    # convert to a percentage
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  # labels
  labs(
    y = "Pecentage of Games the Over Hit",
    title = "On the Road"
  ) +
  # custom theme
  nfl_bar_theme()



## combine the two plots
home_team_over / away_team_over + plot_layout(axes = "collect")







# Data Table --------------------------------------------------------------

nfl_model_data |>
  # select columns
  select(
    season, 
    home_team,
    away_team,
    week,
    over,
    avg_total_yards_home_team,
    avg_total_yards_allowed_ma4_away_team,
    net_ppg,
    net_offensive_yards_ma4
  ) |>
  # gt table
  gt() |> 
  # align columns
  cols_align(
    align = "center"
  ) |> 
  # label columns
  cols_label(
    season = "Season",
    home_team = "Home Team",
    away_team = "Away Team",
    week = "Week",
    over = "Over",
    avg_total_yards_home_team = md("Home Team:<br>Avg Total Yards"),
    avg_total_yards_allowed_ma4_away_team = md("Away Team:<br>MA of Avg Total Yards Allowed"),
    net_ppg = "Matchup Strength Points Per Game",
    net_offensive_yards_ma4 = "MA of Matchup Strength Offensive Yards"
  ) |> 
  # format numerical columns
  fmt_number(
    columns = c(
      avg_total_yards_home_team,
      avg_total_yards_allowed_ma4_away_team,
      net_ppg,
      net_offensive_yards_ma4
    ),
    decimals = 2
  ) |> 
  # add color
  data_color(
    # columns
    columns = c(
      avg_total_yards_home_team,
      net_ppg,
      net_offensive_yards_ma4
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"), 
      domain = NULL
    )
  ) |> 
  # this needs to be in reverse since it is a defensive statistic (lower = better)
  data_color(
    # columns
    columns = c(
      avg_total_yards_allowed_ma4_away_team
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("goldenrod", "white", "dodgerblue4"), 
      domain = NULL
    )
  ) |> 
  # title and subtitle
  tab_header(
    title = md("**NFL Game-by-Game Stats**"),
    subtitle = md("*Data: nflreadr*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*Note: Over = 1, Under = 0*")
  ) |> 
  # theme
  gt_theme_espn()
  



# How Often Did the Over Hit ----------------------------------------------


## over/under by year
nfl_team_week_data |> 
  # group by season
  group_by(season) |> 
  # summarize
  summarize(
    over_count = sum(over) / n()
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = season,
      # y axis
      y = over_count
    )
  ) +
  # geom col
  geom_col(
    # color
    color = "black",
    # fill
    fill = "#013369"
  ) +
  # line at 50%
  geom_hline(
    # line at 50%
    yintercept = 0.50,
    # color
    color = "#D50A0A",
    # dashed line
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale y axis
  scale_y_continuous(
    breaks = seq(0.30, 0.60, 0.10),
    labels = scales::percent_format(accuracy = 1)
  ) +
  # change limits to better show plots
  coord_cartesian(
    ylim = c(0.30, 0.65)
  ) +
  # labels
  labs(
    x = "Season",
    y = "Pecentage of Games the Over Hit",
    title = "The Over Hit over 50% of the Time in 2024"
  ) +
  # custom theme
  nfl_plot_theme()



# Motivation --------------------------------------------------------------

game_total_lines |> 
  filter(
    season == 2024,
    home_team == "NO" |
    away_team == "NO"
  ) |> 
  head(n = 3) |> 
  # remove game ID
  select(
    -game_id
  ) |> 
  # gt table
  gt() |> 
  # align columns
  cols_align(
    align = "center"
  ) |> 
  # label columns
  cols_label(
    season = "Season",
    home_team = "Home Team",
    away_team = "Away Team",
    week = "Week",
    total_home_score = "Home Score",
    total_away_score = "Away Score",
    total_score = "Total Score",
    total_line = "O/U Line",
    over = "Over",
  ) |> 
  # format numerical columns
  fmt_number(
    columns = c(
      total_home_score,
      total_away_score,
      over
    ),
    decimals = 0
  ) |> 
  fmt_number(
    columns = c(
      total_line
    ),
    decimals = 1
  ) |> 
  # title and subtitle
  tab_header(
    title = md("**New Orleans Saints Hot Start**"),
    subtitle = md("*Data: nflreadr*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*Note: Over = 1, Under = 0*")
  ) |> 
  # theme
  gt_theme_espn()
  

offensive_metrics |> 
  filter(
    season == 2024,
    week == 18,
    team == "NO"
  ) |> 
  select(
    avg_total_yards,
    ppg,
    giveaways_per_game
  )


## over by home team
offensive_metrics |> 
  filter(
    week == 3,
    season == 2024
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = fct_reorder(team, points),
      # y axis
      y = points
    )
  ) +
  # column
  geom_col(
    aes(color = team, fill = team)
  ) +
  # scale fill value
  scale_color_nfl(type = "secondary") +
  # scale fill opacity
  scale_fill_nfl(alpha = 0.4) +
  # mean points scored
  # geom_hline(
  #   # line at 50%
  #   yintercept = 0.50,
  #   # color
  #   color = "#D50A0A",
  #   # dashed line
  #   linetype = "dashed",
  #   # size
  #   linewidth = 1.25
  # ) +
  # scale y axis
  # scale_y_continuous(
  #   # breaks
  #   breaks = seq(0, 0.60, 0.10),
  #   # limits
  #   limits = c(0, 0.65),
  #   # convert to a percentage
  #   labels = scales::percent_format(accuracy = 1)
  # ) +
  # labels
  labs(
    y = "Points Scored",
    title = "The Saints Were Last in Points Scored in Week"
  ) +
  # custom theme
  nfl_bar_theme()


## over by home team
offensive_metrics |> 
  filter(
    week >= 3,
    season == 2024
  ) |> 
  # group by team
  group_by(
    team
  ) |> 
  # points per game from weeks 3 - 18
  summarize(
    ppg = mean(points)
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = fct_reorder(team, ppg),
      # y axis
      y = ppg
    )
  ) +
  # column
  geom_col(
    aes(color = team, fill = team)
  ) +
  # scale fill value
  scale_color_nfl(type = "secondary") +
  # scale fill opacity
  scale_fill_nfl(alpha = 0.4) +
  # mean points scored
  geom_hline(
    # line at 50%
    yintercept = offensive_metrics |> 
      filter(
        week >= 3,
        season == 2024
      ) |> 
      # group by team
      group_by(
        team
      ) |> 
      # points per game from weeks 3 - 18
      summarize(
        ppg = mean(points)
      ) |> 
      pull(ppg) |> 
      mean(),
    # color
    color = "black",
    # dashed line
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale y axis
  # scale_y_continuous(
  #   # breaks
  #   breaks = seq(0, 0.60, 0.10),
  #   # limits
  #   limits = c(0, 0.65),
  #   # convert to a percentage
  #   labels = scales::percent_format(accuracy = 1)
  # ) +
  # labels
  labs(
    y = "Points Per Game",
    title = "The Saints Only Scored More Points Than the Browns After Week 2"
  ) +
  # custom theme
  nfl_bar_theme()




# Model Plot --------------------------------------------------------------

tibble(
  model = c(
    "Logit",
    "RF",
    "XGB",
    "GAM",
    "SVM",
    "Ensemble"
  ),
  accuracy = c(
    accuracy(nfl_model_data$over, all_model_preds[,1]),
    accuracy(nfl_model_data$over, all_model_preds[,2]),
    accuracy(nfl_model_data$over, all_model_preds[,3]),
    accuracy(nfl_model_data$over, all_model_preds[,4]),
    accuracy(nfl_model_data$over, all_model_preds[,5]),
    0.5159
  )
) |> 
  ggplot(
    aes(
      x = fct_reorder(model, accuracy),
      y = accuracy
    )
  ) +
  # columns
  geom_col(
    fill = "#013369"
  ) +
  # line at 53%
  geom_hline(
    # line at 53%
    yintercept = 0.53,
    # color
    color = "#D50A0A",
    # dashed line
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale y axis
  scale_y_continuous(
    # convert to a percentage
    labels = scales::percent_format(accuracy = 1)
  ) +
  # change plot zoom
  coord_cartesian(ylim = c(0.40, 0.60)) +
  # labels
  labs(
    x = "Models",
    y = "Test Accuracy",
    title = "Logistic Regression Performs the Best"
  ) +
  # custom theme
  nfl_plot_theme()




# Model Formula Table -----------------------------------------------------

tibble(
  model = c(
    "Logistic",
    "Random Forest",
    "XGBoost",
    "GAM",
    "SVM"
  ),
  pred = c(
    "Home Team Avg Total Yards MA * Away Team Avg Total Yards Allowed MA +
    Away Team Avg Total Yards MA * Home Team Avg Total Yards Allowed MA",
    
    "Matchup Strength EPA MA + 
    Matchup Strength EPA Allowed MA +
    Matchup Strength PROE MA +
    Matchup Strength Opposing PROE MA +
    Matchup Strength Offensive Possessions +
    Matchup Strength Giveaways MA +
    Matchup Strength Takeaways MA",
    
    "Matchup Strength EPA MA + 
    Matchup Strength PROE MA +
    Matchup Strength Opposing PROE MA +
    Matchup Strength Offensive Possessions +
    Matchup Strength Giveaways MA +
    Matchup Strength Takeaways MA",
    
    "Matchup Strength EPA MA + 
    Matchup Strength EPA Allowed MA +
    Matchup Strength PROE MA +
    Matchup Strength Opposing PROE MA",
    
    "Matchup Strength PPG +
    Matchup Strength PPG Allowed +
    Matchup Strength Giveaways MA +
    Matchup Strength Takeaways MA +
    Matchup Strength PROE MA +
    Matchup Strength Opposing PROE MA
    "
  )
) |> 
  # gt table
  gt() |> 
  # align columns
  cols_align(
    columns = c(pred),
    align = "left"
  ) |> 
  # label columns
  cols_label(
    model = "Model",
    pred = "Predictors"
  ) |> 
  # title and subtitle
  tab_header(
    title = md("**Model Predictors**"),
    subtitle = md("*Best Model: Logistic Regression*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*Note: MA = Moving Average (4 weeks)*")
  ) |> 
  # theme
  gt_theme_espn()



tibble(
  model = c(
    "Logistic",
    "Random Forest",
    "XGBoost",
    "GAM",
    "SVM"
  ),
  pred = c(
    "Home Team Avg Total Yards MA * Away Team Avg Total Yards Allowed MA<br>
    Away Team Avg Total Yards MA * Home Team Avg Total Yards Allowed MA",
    
    "Matchup Strength EPA MA<br> 
    Matchup Strength EPA Allowed MA<br>
    Matchup Strength PROE MA<br>
    Matchup Strength Opposing PROE MA<br>
    Matchup Strength Offensive Possessions<br>
    Matchup Strength Giveaways MA<br>
    Matchup Strength Takeaways MA",
    
    "Matchup Strength EPA MA<br> 
    Matchup Strength PROE MA<br>
    Matchup Strength Opposing PROE MA<br>
    Matchup Strength Offensive Possessions<br>
    Matchup Strength Giveaways MA<br>
    Matchup Strength Takeaways MA",
    
    "Matchup Strength EPA MA<br> 
    Matchup Strength EPA Allowed MA<br>
    Matchup Strength PROE MA<br>
    Matchup Strength Opposing PROE MA",
    
    "Matchup Strength PPG<br>
    Matchup Strength PPG Allowed<br>
    Matchup Strength Giveaways MA<br>
    Matchup Strength Takeaways MA<br>
    Matchup Strength PROE MA<br>
    Matchup Strength Opposing PROE MA
    "
  )
) |> 
  # gt table
  gt() |> 
  # align columns
  cols_align(
    columns = c(pred),
    align = "left"
  ) |> 
  # label columns
  cols_label(
    model = "Model",
    pred = "Predictors"
  ) |> 
  # give column markdown 
  fmt_markdown(
    columns = c(pred)
  ) |> 
  # title and subtitle
  tab_header(
    title = md("**Model Predictors**"),
    subtitle = md("*Best Model: Logistic Regression*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*Note: MA = Moving Average (4 weeks)*")
  ) |> 
  # theme
  gt_theme_espn()




# Random Forest Plot ------------------------------------------------------

## random forest
rf_nfl_model <- ranger(
  over ~ net_epa_ma4 + net_proe_ma4 + net_offensive_possessions_ma4 + net_opposing_proe_ma4 + net_epa_allowed_ma4 + 
    net_giveaways_ma4 + net_takeaways_ma4,
  num.trees = 1000,
  importance = "impurity",
  data = nfl_model_data
)

tibble(
  variable = c(
    "Matchup Strength EPA MA",
    "Matchup Strength PROE MA",
    "Matchup Strength Offensive Possessions",
    "Matchup Strength Opposing PROE MA",
    "Matchup Strength EPA Allowed MA",
    "Matchup Strength Giveaways MA",
    "Matchup Strength Takeaways MA"
  ),
  var_imp = rf_nfl_model$variable.importance
) |> 
  arrange(-var_imp) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = var_imp,
      # y axis
      y = fct_reorder(variable, var_imp)
    )
  ) +
  # geom col
  geom_col(
    # color
    color = "black",
    # fill
    fill = "#013369"
  ) +
  # labels
  labs(
    x = "Variable Importance",
    y = "Predictor",
    title = "Random Forest Variable Importance",
    caption = "Note: MA = Moving Average (4 weeks)"
  ) +
  # custom theme
  nfl_plot_theme()




# Logistic Regression Plot ------------------------------------------------

## logistic model
logit_nfl_model <- glm(
  # formula
  over ~ 
    avg_total_yards_ma4_home_team:avg_total_yards_allowed_ma4_away_team +
    avg_total_yards_ma4_away_team:avg_total_yards_allowed_ma4_home_team,
  
  # data 
  data = nfl_model_data,
  
  # family
  family = "binomial"
)


## variables table
logit_nfl_model |> 
  broom::tidy() |> 
  # mutate
  mutate(
    pred = c(
      "Intercept",
      "Home Team Avg Total Yards MA * Away Team Avg Total Yards Allowed MA",
      "Away Team Avg Total Yards MA * Home Team Avg Total Yards Allowed MA"
    )
  ) |> 
  select(
    pred,
    estimate,
    std.error,
    statistic,
    p.value
  ) |> 
  # gt table
  gt() |> 
  # align columns
  cols_align(
    columns = c(pred),
    align = "left"
  ) |> 
  # label columns
  cols_label(
    pred = "Predictors",
    estimate = "Slope",
    std.error = "Std Error",
    statistic = "t-value",
    p.value = "p-value"
  ) |> 
  # round for scientific notation
  fmt_scientific(
    columns = c(
      estimate,
      std.error
    )
  ) |> 
  # round 3 decimals for t and p values
  fmt_number(
    columns = c(
      statistic,
      p.value
    ),
    decimals = 3
  ) |> 
  # title and subtitle
  tab_header(
    title = md("**Logistic Regression Model Predictors**")#,
    #subtitle = md("*Best Model: Logistic Regression*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*Note: MA = Moving Average (4 weeks)*")
  ) |> 
  # theme
  gt_theme_espn()
