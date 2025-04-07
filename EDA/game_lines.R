## Liam Jennings
## Data Science Capstone


# Game Totals EDA ---------------------------------------------------------

## set theme
theme_set(theme_bw())

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

## summary statistics
total_summary_statistics <- game_total_lines |> 
  # group by season
  group_by(season) |> 
  summarize(
    # calculate the average total line for each season
    mean_total_line = mean(total_line), 
    # calculate the average total score for each season
    mean_score = mean(total_score)
  )

## print out table
total_summary_statistics


## bar chart of NFL teams and point totals
### dataset
team_totals <- game_total_lines |> 
  # convert data to just teams and the total line and total score
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = "team_type",
    values_to = "team"
  ) |> 
  # select certain variables
  select(season, team, total_line, total_score)


### plot
team_totals |> 
  # group by team
  group_by(team, season) |> 
  # summarize stats for teams
  summarize(
    mean_total_line = mean(total_line),
    mean_total_score = mean(total_score)
  ) |> 
  # plot
  # variables
  ggplot(
    aes(team, mean_total_score)
  ) + 
  # column
  geom_col(
    aes(color = team, fill = team)
  ) +
  # scale fill value
  scale_color_nfl(type = "secondary") +
  # scale fill opacity
  scale_fill_nfl(alpha = 0.4) +
  # labels
  labs(
    title = "Total Combined Score for Each Team",
    y = "Total Combined Score"
  ) +
  # facet wrap
  facet_wrap(~ season) +
  # average total score for each season
  geom_hline(
    aes(
      yintercept = case_when(
        season == 2021 ~ 46.1,
        season == 2022 ~ 44.0,
        season == 2023 ~ 43.8,
        season == 2024 ~ 46.0
      )
    ),
    color = "black",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # apply custom theme
  nfl_bar_theme()


### plot
team_totals |> 
  # group by team
  group_by(team, season) |> 
  # summarize stats for teams
  summarize(
    mean_total_line = mean(total_line),
    mean_total_score = mean(total_score)
  ) |> 
  # plot
  # variables
  ggplot(
    aes(team, mean_total_line)
  ) + 
  # column
  geom_col(
    aes(color = team, fill = team)
  ) +
  # scale fill value
  scale_color_nfl(type = "secondary") +
  # scale fill opacity
  scale_fill_nfl(alpha = 0.4) +
  # labels
  labs(
    title = "Average O/U for Each Team",
    y = "Average O/U"
  ) +
  # facet wrap
  facet_wrap(~ season) +
  # average total score for each season
  geom_hline(
    aes(
      yintercept = case_when(
        season == 2021 ~ 46.5,
        season == 2022 ~ 44.2,
        season == 2023 ~ 43.1,
        season == 2024 ~ 44.5
      )
    ),
    color = "black",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # apply custom theme
  nfl_bar_theme()


## difference between O/U and total score for each season
team_totals |> 
  # group by team
  group_by(team, season) |> 
  # summarize stats for teams
  summarize(
    mean_total_line = mean(total_line),
    mean_total_score = mean(total_score)
  ) |> 
  # calculate the difference
  mutate(
    difference = mean_total_score - mean_total_line
  ) |> 
  # plot
  # variables
  ggplot(
    aes(team, difference)
  ) + 
  # column
  geom_col(
    aes(color = team, fill = team)
  ) +
  # scale fill value
  scale_color_nfl(type = "secondary") +
  # scale fill opacity
  scale_fill_nfl(alpha = 0.4) +
  # labels
  labs(
    title = "Difference Between Score and O/U for Each Team",
    y = "Difference Between Score and O/U"
  ) +
  # hline at 0
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # facet wrap
  facet_wrap(~ season) +
  # apply custom theme
  nfl_bar_theme()


## average difference between O/U and total score for each team
team_totals |> 
  # group by team
  group_by(team) |> 
  # summarize stats for teams
  summarize(
    mean_total_line = mean(total_line),
    mean_total_score = mean(total_score)
  ) |> 
  # calculate the difference
  mutate(
    difference = mean_total_score - mean_total_line
  ) |> 
  # plot
  # variables
  ggplot(
    aes(team, difference)
  ) + 
  # column
  geom_col(
    aes(color = team, fill = team)
  ) +
  # scale fill value
  scale_color_nfl(type = "secondary") +
  # scale fill opacity
  scale_fill_nfl(alpha = 0.4) +
  # labels
  labs(
    title = "Difference Between Score and O/U for Each Team",
    y = "Difference Between Score and O/U"
  ) +
  # hline at 0
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # apply custom theme
  nfl_bar_theme()
