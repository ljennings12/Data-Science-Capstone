## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(glmnet)
library(broom)


# Original Logistic Regression -----------------------------------------------------

## set a seed
set.seed(8029)

## training data
train <- nfl_team_week_data |>
  # 70% training set
  slice_sample(prop = 0.70)


## testing set
test <- nfl_team_week_data |> 
  anti_join(train)

## initial model
model <- glm(
  # formula
  over ~ div_game + 
    pass_rate_home_team:opposing_pass_rate_away_team + 
    pass_rate_away_team:opposing_pass_rate_home_team + 
    epa_home_team:epa_allowed_away_team +
    epa_away_team:epa_allowed_home_team +
    avg_total_yards_home_team:avg_total_yards_allowed_away_team +
    avg_total_yards_away_team:avg_total_yards_allowed_home_team +
    ppg_home_team:ppg_allowed_away_team +
    ppg_away_team:ppg_allowed_home_team +
    possessions_per_game_home_team:possessions_per_game_allowed_away_team +
    possessions_per_game_away_team:possessions_per_game_allowed_home_team +
    hour_time_difference,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(model)


## variables
model |> 
  tidy()


## exponentiate coefficient estimates to get odds
model |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(model, newdata = data.frame(test), type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob > 0.5, "Over", "Under")


## model assessment
### confusion matrix
table("Predicted" = model_pred_class, "Observed" = test$over)

### Accuracy ~ 47%




# Second Logistic Regression ----------------------------------------------

## create difference variables
nfl_logit_data <- {
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
      net_offensive_possessions = possessions_per_game_allowed_home_team - possessions_per_game_away_team,
      
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
      net_opposing = opposing_proe_home_team - proe_away_team
    ) |> 
    # select columns to use in modeling
    select(
      game_id,
      season,
      week,
      home_team,
      away_team,
      over,
      div_game,
      hour_time_difference,
      starts_with("net_")
    )
}  


## set a seed
set.seed(8029)

## training data
train <- nfl_logit_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |> 
  # remove season and game ID
  select(
    -c(game_id:away_team)
  )


## testing set
test <- nfl_logit_data |> 
  anti_join(train)

## initial model
nfl_logit_model <- glm(
  # formula
  over ~ .,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(nfl_logit_model)


## variables
nfl_logit_model |> 
  tidy()


## exponentiate coefficient estimates to get odds
nfl_logit_model |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_logit_model, newdata = data.frame(test), type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )

### Accuracy ~ 46.4%
