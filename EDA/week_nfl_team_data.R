## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(nflplotR)
library(patchwork)


## create theme
nfl_scatterplot_theme <- function(){
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



# EDA ---------------------------------------------------------------------


# General Trends -------------------------------------------------------


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
    # size
    linewidth = 1.25
  ) +
  # scale y axis
  scale_y_continuous(
    breaks = seq(0.10, 0.50, 0.10),
    labels = scales::percent_format(accuracy = 1)
  ) +
  # labels
  labs(
    x = "Season",
    y = "Pecentage of Games the Over Hit",
    title = "How Often Did the Over Hit in Each Season"
  ) +
  # custom theme
  nfl_scatterplot_theme()



## over/under by week
nfl_team_week_data |> 
  # group by season
  group_by(week) |> 
  # summarize
  summarize(
    over_count = sum(over) / n()
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = week,
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
    # breaks
    breaks = seq(0.10, 0.50, 0.10),
    # convert to a percentage
    labels = scales::percent_format(accuracy = 1)
  ) +
  # scale x axis
  scale_x_continuous(
    # breaks 
    breaks = seq(1, 18, 1)
  ) +
  # labels
  labs(
    x = "Week",
    y = "Pecentage of Games the Over Hit",
    title = "How Often Did the Over Hit in Each Week"
  ) +
  # custom theme
  nfl_scatterplot_theme()



## over/under line by week
nfl_team_week_data |> 
  # group by season
  group_by(week) |> 
  # summarize
  summarize(
    mean_ou_line = mean(total_line)
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = week,
      # y axis
      y = mean_ou_line
    )
  ) +
  # geom col
  geom_col(
    # color
    color = "black",
    # fill
    fill = "#013369"
  ) +
  # # line at 50%
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
  # # scale y axis
  # scale_y_continuous(
  #   # breaks
  #   breaks = seq(0.10, 0.50, 0.10),
  #   # convert to a percentage
  #   labels = scales::percent_format(accuracy = 1)
  # ) +
  # scale x axis
  scale_x_continuous(
    # breaks 
    breaks = seq(1, 18, 1)
  ) +
  # labels
  labs(
    x = "Week",
    y = "Mean O/U Line",
    title = "Mean O/U Line by Week"
  ) +
  # custom theme
  nfl_scatterplot_theme()



## over/under by divisional game
nfl_team_week_data |> 
  # group by season
  group_by(div_game) |> 
  # summarize
  summarize(
    over_count = sum(over) / n()
  ) |> 
  # mutate
  mutate(
    div_game = factor(
      case_when(
        div_game == 1 ~ "Divisional Game",
        div_game == 0 ~  "Non-divisional Game"
      )
    )
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = div_game,
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
    # breaks
    breaks = seq(0.10, 0.50, 0.10),
    # convert to a percentage
    labels = scales::percent_format(accuracy = 1)
  ) +
  # labels
  labs(
    x = "Week",
    y = "Pecentage of Games the Over Hit",
    title = "How Often Did the Over Hit in Each Week"
  ) +
  # custom theme
  nfl_scatterplot_theme()





# Independent ------------------------------------------------------------

## temperature
nfl_team_week_data |>
  # mutate
  mutate(
    # turn over/under into a factor
    over = factor(
      over,
      # levels
      levels = c(0, 1),
      # labels
      labels = c("Under", "Over")
    )
  ) |>
  # plot
  ggplot(
    aes(
      # x axis
      temperature,
      # y axis
      over,
      # fill
      fill = over
    )
  ) +
  # boxplot
  geom_boxplot() +
  # line at 0
  geom_vline(
    xintercept = mean(nfl_team_week_data$temperature, na.rm = TRUE),
    # color
    color = "goldenrod",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale color
  scale_fill_manual(
    values = c("#013369", "#D50A0A")
  ) +
  # labels
  labs(
    x = "Temperature (in Fahrenheit)",
    y = "Over/Under",
    title = "Over/Under by Game Temperature"
  ) +
  # custom theme
  nfl_scatterplot_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )



## wind
nfl_team_week_data |>
  # mutate
  mutate(
    # turn over/under into a factor
    over = factor(
      over,
      # levels
      levels = c(0, 1),
      # labels
      labels = c("Under", "Over")
    )
  ) |>
  # plot
  ggplot(
    aes(
      # x axis
      wind,
      # y axis
      over,
      # fill
      fill = over
    )
  ) +
  # boxplot
  geom_boxplot() +
  # line at 0
  geom_vline(
    xintercept = mean(nfl_team_week_data$wind, na.rm = TRUE),
    # color
    color = "goldenrod",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale color
  scale_fill_manual(
    values = c("#013369", "#D50A0A")
  ) +
  # labels
  labs(
    x = "Wind (in mpg)",
    y = "Over/Under",
    title = "Over/Under by Wind (mph)"
  ) +
  # custom theme
  nfl_scatterplot_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )



## temperature
nfl_team_week_data |>
  # mutate
  mutate(
    # turn over/under into a factor
    over = factor(
      over,
      # levels
      levels = c(0, 1),
      # labels
      labels = c("Under", "Over")
    )
  ) |>
  # plot
  ggplot(
    aes(
      # x axis
      temperature,
      # y axis
      total_line
    )
  ) +
  # boxplot
  geom_point(
    # color
    color = "#013369"
  ) +
  # line at mean temperature
  geom_vline(
    xintercept = mean(nfl_team_week_data$temperature, na.rm = TRUE),
    # color
    color = "#D50A0A",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # line at mean total line
  geom_hline(
    yintercept = mean(nfl_team_week_data$total_line, na.rm = TRUE),
    # color
    color = "#D50A0A",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale color
  # scale_fill_manual(
  #   values = c("#013369", "#D50A0A")
  # ) +
  # labels
  labs(
    x = "Temperature (in Fahrenheit)",
    y = "Total Line",
    title = "Over/Under Line by Game Temperature"
  ) +
  # custom theme
  nfl_scatterplot_theme()
  # # remove legend
  # theme(
  #   legend.position = "none"
  # )




## wind
nfl_team_week_data |>
  # mutate
  mutate(
    # turn over/under into a factor
    over = factor(
      over,
      # levels
      levels = c(0, 1),
      # labels
      labels = c("Under", "Over")
    )
  ) |>
  # plot
  ggplot(
    aes(
      # x axis
      wind,
      # y axis
      total_line
    )
  ) +
  # boxplot
  geom_point(
    # color
    color = "#013369"
  ) +
  # line at mean temperature
  geom_vline(
    xintercept = mean(nfl_team_week_data$wind, na.rm = TRUE),
    # color
    color = "#D50A0A",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # line at mean total line
  geom_hline(
    yintercept = mean(nfl_team_week_data$total_line, na.rm = TRUE),
    # color
    color = "#D50A0A",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale color
  # scale_fill_manual(
  #   values = c("#013369", "#D50A0A")
  # ) +
  # labels
  labs(
    x = "Wind (in mph)",
    y = "Total Line",
    title = "Over/Under Line by Wind"
  ) +
  # custom theme
  nfl_scatterplot_theme()
  # # remove legend
  # theme(
  #   legend.position = "none"
  # )



## temperature
nfl_team_week_data |>
  # mutate
  mutate(
    # turn over/under into a factor
    over = factor(
      over,
      # levels
      levels = c(0, 1),
      # labels
      labels = c("Under", "Over")
    )
  ) |>
  # plot
  ggplot(
    aes(
      # x axis
      temperature,
      # y axis
      total_line,
      # color
      color = over
    )
  ) +
  # boxplot
  geom_point() +
  # line at mean temperature
  geom_vline(
    xintercept = mean(nfl_team_week_data$temperature, na.rm = TRUE),
    # color
    color = "goldenrod",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # line at mean total line
  geom_hline(
    yintercept = mean(nfl_team_week_data$total_line, na.rm = TRUE),
    # color
    color = "goldenrod",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale color
  scale_color_manual(
    values = c("#013369", "#D50A0A")
  ) +
  # scale x axis
  scale_x_continuous(
    breaks = seq(10, 100, 10)
  ) +
  # labels
  labs(
    x = "Temperature (in Fahrenheit)",
    y = "Total Line",
    color = "Over/Under",
    title = "Over/Under Line by Game Temperature"
  ) +
  # custom theme
  nfl_scatterplot_theme()
# # remove legend
# theme(
#   legend.position = "none"
# )



## hour time difference
nfl_team_week_data |>
  # mutate
  mutate(
    # turn over/under into a factor
    over = factor(
      over,
      # levels
      levels = c(0, 1),
      # labels
      labels = c("Under", "Over")
    ),
    
    # turn hour time difference into a variable
    hour_time_difference = factor(hour_time_difference)
  ) |>
  # plot
  ggplot(
    aes(
      # x axis
      total_line,
      # y axis
      hour_time_difference,
      # fill
      fill = hour_time_difference
    )
  ) +
  # boxplot
  geom_boxplot() +
  # # line at 0
  # geom_vline(
  #   xintercept = mean(nfl_team_week_data$total_line, na.rm = TRUE),
  #   # color
  #   color = "goldenrod",
  #   # dashed
  #   linetype = "dashed",
  #   # size
  #   linewidth = 1.25
  # ) +
  # scale color
  scale_fill_manual(
    values = c("#013369", "#D50A0A", "goldenrod", "#39275b")
  ) +
  # labels
  labs(
    x = "Total O/U Line",
    y = "Time Zone Difference for Away Team",
    title = "Time Zone Difference for Away Team by Total O/U Line"
  ) +
  # custom theme
  nfl_scatterplot_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )



## hour time difference
nfl_team_week_data |>
  group_by(hour_time_difference) |> 
  summarize(
    over_prop = sum(over) / n()
  ) |> 
  # mutate
  mutate(
    # turn hour time difference into a variable
    hour_time_difference = factor(hour_time_difference)
  ) |>
  # plot
  ggplot(
    aes(
      # x axis
      hour_time_difference,
      # y axis
      over_prop,
      # fill
      fill = hour_time_difference
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
      breaks = seq(0.10, 0.50, 0.10),
      labels = scales::percent_format(accuracy = 1)
    ) +
    # labels
    labs(
      x = "Time Zone Difference for Away Team",
      y = "Pecentage of Games the Over Hit",
      title = "How Often Did the Over Hit in Each Season"
    ) +
    # custom theme
    nfl_scatterplot_theme()



# Facet by Year -----------------------------------------------------------

## over/under by week
nfl_team_week_data |> 
  # group by season
  group_by(season, week) |> 
  # summarize
  summarize(
    over_count = sum(over) / n()
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = week,
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
    # breaks
    breaks = seq(0, 0.80, 0.10),
    # convert to a percentage
    labels = scales::percent_format(accuracy = 1)
  ) +
  # scale x axis
  scale_x_continuous(
    # breaks 
    breaks = seq(2, 18, 1)
  ) +
  # labels
  labs(
    x = "Week",
    y = "Pecentage of Games the Over Hit",
    title = "How Often Did the Over Hit in Each Week"
  ) +
  # facet by season
  facet_wrap(
    ~ season
  ) +
  # custom theme
  nfl_scatterplot_theme()




# Interactions ------------------------------------------------------------

## average total yards home vs. average total yards allowed away
nfl_team_week_data |>
  # mutate
  mutate(
    # turn over/under into a factor
    over = factor(
      over,
      # levels
      levels = c(0, 1),
      # labels
      labels = c("Under", "Over")
    ),
    
    # difference in home offense vs. away defense
    home_team_net_yardage_advantage = avg_total_yards_home_team - avg_total_yards_allowed_away_team,
    
    # interaction term
    net_total_yards = avg_total_yards_home_team * avg_total_yards_allowed_away_team
  ) |>
  # plot
  ggplot(
    aes(
      # x axis
      avg_total_yards_allowed_away_team,
      # y axis
      avg_total_yards_home_team
    )
  ) +
  # geom point
  geom_point(
    alpha = 0.5
  ) +
  # scatterplot
  # geom_nfl_logos(
  #   # home team logos
  #   aes(team_abbr = home_team),
  #   # adjust width
  #   width = 0.07, 
  #   # adjust opacity
  #   alpha = 0.7
  # ) +
  # line at mean
  geom_vline(
    xintercept = mean(nfl_team_week_data$avg_total_yards_allowed_away_team, na.rm = TRUE),
    # color
    color = "goldenrod",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # line at mean
  geom_hline(
    yintercept = mean(nfl_team_week_data$avg_total_yards_home_team, na.rm = TRUE),
    # color
    color = "goldenrod",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # labels
  labs(
    x = "Away Team Average Total Yards Allowed",
    y = "Home Team Average Total Yards",
    title = "Home Offense vs. Away Defense"
  ) +
  # facet by season
  facet_wrap(
    ~ season
  ) +
  # custom theme
  nfl_scatterplot_theme()



## average total yards home vs. average total yards allowed away
nfl_team_week_data |>
  # mutate
  mutate(
    # turn over/under into a factor
    over = factor(
      over,
      # levels
      levels = c(0, 1),
      # labels
      labels = c("Under", "Over")
    ),
    
    # interaction term
    net_total_yards = avg_total_yards_home_team * avg_total_yards_allowed_away_team
  ) |>
  # plot
  ggplot(
    aes(
      # x axis
      net_total_yards,
      # y axis
      over,
      # fill
      fill = over
    )
  ) +
  # boxplot
  geom_boxplot() +
  # line at 0
  geom_vline(
    xintercept = 0,
    # color
    color = "goldenrod",
    # dashed
    linetype = "dashed",
    # size
    linewidth = 1.25
  ) +
  # scale color
  scale_fill_manual(
    values = c("#013369", "#D50A0A")
  ) +
  # labels
  labs(
    x = "Difference between Home Offense and Away Defense",
    y = "Over/Under",
    title = "Over/Under by Net Yardage"
  ) +
  # custom theme
  nfl_scatterplot_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )


## heatmap
library(reshape2)

nfl_team_week_data |> 
  dcast(
    # formula
    home_team ~ away_team,
    # value
    value.var = "over",
    # function
    fun.aggregate = mean
  ) |> 
  # melt
  melt(
    id.vars = "home_team",
    variable.name = "away_team",
    value.name = "mean_over",
    na.rm = TRUE
  ) |> 
  ggplot(
    # aes
    aes(
      # x axis
      home_team,
      # y axis
      away_team,
      # fill
      fill = mean_over
    )
  ) +
  # tile for heatmap
  geom_tile() +
  # scale fill
  scale_fill_gradientn(
    colors = c("#013369", "black", "#D50A0A")
  ) +
  # labels
  labs(
    x = "Home Team",
    y = "Away Team",
    fill = "Mean Over",
    title = "Heatmap of Over/Under"
  ) +
  # custom theme
  nfl_scatterplot_theme()




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
  nfl_bar_theme()


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
  nfl_bar_theme() +
  # remove y axis
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )



## combine the two plots
home_team_over + away_team_over








# Combined Yards ----------------------------------------------------------


## total combined yards by combined points
nfl_team_week_data |> 
  mutate(
    avg_combined_yards = avg_total_yards_away_team + avg_total_yards_home_team,
    avg_combined_ppg = ppg_away_team + ppg_home_team
  ) |> 
  # plot
  ggplot(
    aes(
      avg_combined_yards,
      avg_combined_ppg
    )
  ) +
  # scatterplot
  geom_point(
    # change opacity
    alpha = 0.50,
    # change color
    color = "dodgerblue4",
    # change size
    size = 2
  ) +
  # # vertical line for COVID-19
  geom_smooth(
    # method
    method = "lm",
    # color
    color = "black",
    # dashed line
    linetype = "dashed",
    # change size
    linewidth = 1.15,
    # remove standard error bar
    se = FALSE
  ) +
  # labels
  labs(
    x = "Avg Combined Total Yards",
    y = "Avg Combined Points per Game",
    title = "Combined PPG by Combined Total Yards"
  ) +
  # apply custom theme
  nfl_scatterplot_theme()




## look at boxplot
nfl_team_week_data |> 
  mutate(
    avg_combined_yards = avg_total_yards_away_team + avg_total_yards_home_team,
    over = factor(over, labels = c("Under", "Over"))
  ) |> 
  # plot
  ggplot(
    aes(
      avg_combined_yards,
      factor(over),
      fill = factor(over)
    )
  ) +
  # scatterplot
  geom_boxplot() +
  # scale colors
  scale_fill_manual(
    values = c("dodgerblue4", "firebrick")
  ) +
  # labels
  labs(
    x = "Avg Combined Total Yards",
    y = "Over/Under",
    title = "Combined Total Yards by Over/Under",
    fill = "Over/Under"
  ) +
  # apply custom theme
  nfl_scatterplot_theme()


## look at boxplot
nfl_team_week_data |> 
  mutate(
    avg_combined_yards = avg_total_yards_away_team + avg_total_yards_home_team,
    over = factor(over, labels = c("Under", "Over"))
  ) |> 
  # plot
  ggplot(
    aes(
      avg_combined_yards,
      factor(over),
      fill = factor(over)
    )
  ) +
  # scatterplot
  geom_boxplot() +
  # scale colors
  scale_fill_manual(
    values = c("dodgerblue4", "firebrick")
  ) +
  # labels
  labs(
    x = "Avg Combined Total Yards",
    y = "Over/Under",
    title = "Combined Total Yards by Over/Under",
    fill = "Over/Under"
  ) +
  # apply custom theme
  nfl_scatterplot_theme()




# Time of Possession ------------------------------------------------------


## points per game by average drive time (in sec) for home team
home_team_drive_ppg <- nfl_team_week_data |> 
  # plot
  ggplot(
    aes(
      avg_drive_time_sec_home_team,
      ppg_home_team
    )
  ) +
  # scatterplot
  geom_point(
    # change opacity
    alpha = 0.50,
    # change color
    color = "dodgerblue4",
    # change size
    size = 2
  ) +
  # labels
  labs(
    x = "Avg Drive Time (in sec)",
    y = "Avg PPG",
    title = "Avg PPG by Avg Drive Time (in sec) for Home Teams"
  ) +
  # apply custom theme
  nfl_scatterplot_theme()


## points per game by average drive time (in sec) for away team
away_team_drive_ppg <- nfl_team_week_data |> 
  # plot
  ggplot(
    aes(
      avg_drive_time_sec_away_team,
      ppg_away_team
    )
  ) +
  # scatterplot
  geom_point(
    # change opacity
    alpha = 0.50,
    # change color
    color = "firebrick",
    # change size
    size = 2
  ) +
  # labels
  labs(
    x = "Avg Drive Time (in sec)",
    y = "Avg PPG",
    title = "Avg PPG by Avg Drive Time (in sec) for Home Teams"
  ) +
  # apply custom theme
  nfl_scatterplot_theme()


## combine the two plots
home_team_drive_ppg + away_team_drive_ppg
