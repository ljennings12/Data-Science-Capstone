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



## improve model
nfl_logit_model_2 <- glm(
  # formula
  over ~ net_offensive_yards + net_defensive_yards + net_offensive_possessions + net_takeaways + net_epa,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(nfl_logit_model_2)


## variables
nfl_logit_model_2 |> 
  tidy()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_logit_model_2, newdata = data.frame(test), type = "response")

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


## Accuracy: 46.8%



# Points Logistic Regression ----------------------------------------------

## improve model
nfl_logit_model_points <- glm(
  # formula
  over ~ net_ppg + net_ppg_allowed,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(nfl_logit_model_points)


## variables
nfl_logit_model_points |> 
  tidy()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_logit_model_points, newdata = data.frame(test), type = "response")

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


## Accuracy: 47.2%


# Moving Average Logistic Regression ----------------------------------------------

## create difference variables
nfl_logit_ma4_data <- {
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
      net_offensive_possessions_ma4 = possessions_per_game_allowed_ma4_home_team - possessions_per_game_ma4_away_team,
      
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
      net_opposing_ma4 = opposing_proe_ma4_home_team - proe_ma4_away_team
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
      ends_with("_ma4")
    )
}  


## set a seed
set.seed(8029)

## training data
train <- nfl_logit_ma4_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |> 
  # remove NAs
  drop_na(
    ends_with("ma4")
  ) |> 
  # remove season and game ID
  select(
    -c(game_id:away_team)
  )


## testing set
test <- nfl_logit_ma4_data |> 
  anti_join(train) |> 
  # remove NAs
  drop_na(
    ends_with("ma4")
  )

## initial model
nfl_logit_ma4_model <- glm(
  # formula
  over ~ .,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(nfl_logit_ma4_model)


## variables
nfl_logit_model |> 
  tidy()


## exponentiate coefficient estimates to get odds
nfl_logit_model |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_logit_ma4_model, newdata = data.frame(test), type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted, na.rm = TRUE)
  )

### Accuracy ~ 46.1%




# Points Logistic Regression ----------------------------------------------

## set a seed
set.seed(8029)

## training data
train <- nfl_logit_ma4_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |> 
  # remove NAs
  drop_na(
    ends_with("ma4")
  ) |> 
  # remove season and game ID
  select(
    -c(game_id:away_team)
  )


## testing set
test <- nfl_logit_ma4_data |> 
  anti_join(train) |> 
  # remove NAs
  drop_na(
    ends_with("ma4")
  )

## initial model
nfl_logit_ma4_model <- glm(
  # formula
  over ~ net_ppg_ma4 + net_ppg_allowed_ma4,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(nfl_logit_ma4_model)


## variables
nfl_logit_model |> 
  tidy()


## exponentiate coefficient estimates to get odds
nfl_logit_model |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_logit_ma4_model, newdata = data.frame(test), type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted, na.rm = TRUE)
  )

### Accuracy ~ 46.1%
