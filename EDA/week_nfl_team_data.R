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



# EDA ---------------------------------------------------------------------



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
