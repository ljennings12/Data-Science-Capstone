## Liam Jennings
## Data Science Capstone


# Offensive Metrics EDA -------------------------------------------------------

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

# Points per Possession ---------------------------------------------------

## points per possession for each team
offensive_metrics |> 
  # group by team
  group_by(team) |> 
  # summarize stats for teams
  summarize(avg_points_per_poss = mean(points_per_poss)) |> 
  # plot
  # variables
  ggplot(
    aes(team, avg_points_per_poss)
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
    title = "Average Points per Possession for Each Team",
    y = "Average Points per Possession"
  ) +
  # hline at 0
  geom_hline(
    yintercept = mean(offensive_metrics$points_per_poss),
    color = "black",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # apply custom theme
  nfl_bar_theme()


# Giveaways ---------------------------------------------------

## points per possession for each team
offensive_metrics |> 
  # group by team
  group_by(team) |> 
  # summarize stats for teams
  summarize(avg_giveaways = mean(giveaways)) |> 
  # plot
  # variables
  ggplot(
    aes(team, avg_giveaways)
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
    title = "Average Number of Giveaways for Each Team",
    y = "Average Giveaways"
  ) +
  # hline at 0
  geom_hline(
    yintercept = mean(offensive_metrics$giveaways),
    color = "black",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # apply custom theme
  nfl_bar_theme()

