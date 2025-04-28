## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(xgboost)
library(vip)
library(caret)


# Full XGBoost Model -----------------------------------------

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
      starts_with("net_"),
      hour_time_difference,
      temperature,
      wind,
      roof
    )
}  


## training data
train <- nfl_model_data |>
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
test <- nfl_model_data |> 
  anti_join(train) |> 
  # remove NAs
  drop_na(
    ends_with("ma4")
  ) |> 
  # remove season and game ID
  select(
    -c(game_id:away_team)
  )


## model data (must be a matrix)

### training variable
x_train <- train |> 
  select(-over) |> 
  data.matrix()


### testing variable
x_test <- test |> 
  select(-over) |> 
  data.matrix()


## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(20, 150, 10),
  eta = c(0.01, 0.05, 0.1), 
  gamma = 0,
  max_depth = 2:4, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

## tuning

### set seed
set.seed(8029)

### tuning
xg_full_model_tune <- train(
  x = x_train,
  y = train$over,
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "binary:logistic", 
  method = "xgbTree"
)


## model evaluation
### fit model to training data
xg_full_model <- xgboost(
  data = x_train, 
  label = train$over,
  objective = "binary:logistic",
  nrounds = xg_full_model_tune$bestTune$nrounds,
  params = as.list(select(xg_full_model_tune$bestTune, -nrounds)),
  verbose = 0
)

### training error
train |> 
  mutate(pred = round(predict(xg_full_model, newdata = x_train))) |> 
  summarize(correct = mean(over == pred))

### testing error
test |> 
  mutate(pred = round(predict(xg_full_model, newdata = x_test))) |> 
  summarize(correct = mean(over == pred))

### variable importance
xg_full_model |> 
  vip()


## accuracy: 48.0%




# Improve the Model -------------------------------------------------------

## model data (must be a matrix)

### training variable
x_train <- train |> 
  select(
    net_offensive_yards,
    net_defensive_yards,
    net_defensive_yards_ma4,
    net_defensive_yards_ma4,
    net_offensive_possessions,
    net_defensive_possessions,
    net_epa,
    net_opposing_pass_rate,
    net_epa,
    net_epa_allowed,
    net_ppg_ma4,
    net_ppg_allowed_ma4,
    net_offensive_possessions_ma4, 
    net_takeaways_ma4,
    net_epa_ma4,
    net_epa_allowed_ma4,
    net_proe_ma4,
    net_opposing_proe_ma4
  ) |> 
  data.matrix()


### testing variable
x_test <- test |> 
  select(
    net_offensive_yards,
    net_defensive_yards,
    net_defensive_yards_ma4,
    net_defensive_yards_ma4,
    net_offensive_possessions,
    net_defensive_possessions,
    net_epa,
    net_opposing_pass_rate,
    net_epa,
    net_epa_allowed,
    net_ppg_ma4,
    net_ppg_allowed_ma4,
    net_offensive_possessions_ma4, 
    net_takeaways_ma4,
    net_epa_ma4,
    net_epa_allowed_ma4,
    net_proe_ma4,
    net_opposing_proe_ma4
  ) |> 
  data.matrix()


## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(20, 150, 10),
  eta = c(0.01, 0.05, 0.1), 
  gamma = 0,
  max_depth = 2:4, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

## tuning

### set seed
set.seed(8029)

### tuning
xg_full_model_tune_2 <- train(
  x = x_train,
  y = train$over,
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "binary:logistic", 
  method = "xgbTree"
)


## model evaluation
### fit model to training data
xg_full_model_2 <- xgboost(
  data = x_train, 
  label = train$over,
  objective = "binary:logistic",
  nrounds = xg_full_model_tune_2$bestTune$nrounds,
  params = as.list(select(xg_full_model_tune_2$bestTune, -nrounds)),
  verbose = 0
)

### training error
train |> 
  mutate(pred = round(predict(xg_full_model_2, newdata = x_train))) |> 
  summarize(correct = mean(over == pred))

### testing error
test |> 
  mutate(pred = round(predict(xg_full_model_2, newdata = x_test))) |> 
  summarize(correct = mean(over == pred))

### variable importance
xg_full_model_2 |> 
  vip()


## accuracy: 46.1%



# Improve the Model -------------------------------------------------------

## model data (must be a matrix)

### training variable
x_train <- train |> 
  select(
    net_epa_ma4,
    net_proe_ma4,
    net_offensive_possessions_ma4,
    net_opposing_proe_ma4,
    net_giveaways_ma4,
    net_takeaways_ma4
  ) |> 
  data.matrix()


### testing variable
x_test <- test |> 
  select(
    net_epa_ma4,
    net_proe_ma4,
    net_offensive_possessions_ma4,
    net_opposing_proe_ma4,
    net_giveaways_ma4,
    net_takeaways_ma4
  ) |>
  data.matrix()


## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(30, 150, 20),
  eta = c(0.01, 0.05, 0.1), 
  gamma = c(0, 0.10, 0.20),
  max_depth = 2:4, 
  colsample_bytree = c(0.6, 0.8, 1), 
  min_child_weight = c(1, 3, 5), 
  subsample = c(0.6, 0.8, 1)
)

## tuning

### set seed
set.seed(8029)

### tuning
xg_full_model_tune_3 <- train(
  x = x_train,
  y = train$over,
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "binary:logistic", 
  method = "xgbTree"
)


## model evaluation
### fit model to training data
xg_full_model_3 <- xgboost(
  data = x_train, 
  label = train$over,
  objective = "binary:logistic",
  nrounds = 60,
  params = list(
    max_depth = 3,
    eta = 0.10,
    gamma = 0.1,
    colsample_bytree = 0.6,
    min_child_weight = 1,
    subsample = 0.6
  ),
  verbose = 0
)

### training error
train |> 
  mutate(pred = round(predict(xg_full_model_3, newdata = x_train))) |> 
  summarize(correct = mean(over == pred))

### testing error
test |> 
  mutate(pred = round(predict(xg_full_model_3, newdata = x_test))) |> 
  summarize(correct = mean(over == pred))

### variable importance
xg_full_model_3 |> 
  vip()


## accuracy: 48.0%


# Improve the Model -------------------------------------------------------

## training data
train <- nfl_full_model_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |>
  # remove NAs
  drop_na(
    ends_with("ma4")
  ) |> 
  # keep these for modeling
  select(
    over, 
    net_takeaways_ma4, 
    net_defensive_possessions, 
    net_opposing_proe_ma4, 
    net_offensive_yards
  )


## testing set
test <- nfl_full_model_data |> 
  anti_join(train) |> 
  # remove NAs
  drop_na(
    ends_with("ma4")
  ) |> 
  # keep these for modeling
  select(
    over, 
    net_takeaways_ma4, 
    net_defensive_possessions, 
    net_opposing_proe_ma4, 
    net_offensive_yards
  )


## model data (must be a matrix)

### training variable
x_train <- train |> 
  select(-over) |> 
  as.matrix()


### testing variable
x_test <- test |> 
  select(-over) |> 
  as.matrix()


## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(20, 150, 10),
  eta = c(0.01, 0.05, 0.1), 
  gamma = 0,
  max_depth = 2:4, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

## tuning

### set seed
set.seed(8029)

### tuning
xg_tune <- train(
  x = x_train,
  y = train$over, 
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "binary:logistic", 
  method = "xgbTree"
)


## model evaluation
### fit model to training data
xg_fit <- xgboost(
  data = x_train, 
  label = train$over,
  objective = "binary:logistic",
  nrounds = xg_tune$bestTune$nrounds,
  params = as.list(select(xg_tune$bestTune, -nrounds)),
  verbose = 0
)

### training error
train |> 
  mutate(pred = round(predict(xg_fit, newdata = x_train))) |> 
  summarize(correct = mean(over == pred))

### testing error
test |> 
  mutate(pred = round(predict(xg_fit, newdata = x_test))) |> 
  summarize(correct = mean(over == pred))

### variable importance
xg_fit |> 
  vip()


## Accuracy: 49.0%



# Points XGBoost -------------------------------------------------------

## training data
train <- nfl_full_model_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |>
  # keep these for modeling
  select(
    over, 
    net_ppg,
    net_ppg_allowed
  )


## testing set
test <- nfl_full_model_data |> 
  anti_join(train) |> 
  # keep these for modeling
  select(
    over, 
    net_ppg,
    net_ppg_allowed
  )


## model data (must be a matrix)

### training variable
x_train <- train |> 
  select(-over) |> 
  as.matrix()


### testing variable
x_test <- test |> 
  select(-over) |> 
  as.matrix()


## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(20, 150, 10),
  eta = c(0.01, 0.05, 0.1), 
  gamma = 0,
  max_depth = 2:4, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

## tuning

### set seed
set.seed(8029)

### tuning
xg_tune <- train(
  x = x_train,
  y = train$over, 
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "binary:logistic", 
  method = "xgbTree"
)


## model evaluation
### fit model to training data
xg_fit <- xgboost(
  data = x_train, 
  label = train$over,
  objective = "binary:logistic",
  nrounds = xg_tune$bestTune$nrounds,
  params = as.list(select(xg_tune$bestTune, -nrounds)),
  verbose = 0
)

### training error
train |> 
  mutate(pred = round(predict(xg_fit, newdata = x_train))) |> 
  summarize(correct = mean(over == pred))

### testing error
test |> 
  mutate(pred = round(predict(xg_fit, newdata = x_test))) |> 
  summarize(correct = mean(over == pred))

### variable importance
xg_fit |> 
  vip()


## Accuracy: 53.0%


# Improve the Model -------------------------------------------------------

## training data
train <- nfl_full_model_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |>
  # keep these for modeling
  select(
    over, 
    net_epa_ma4,
    net_proe_ma4,
    net_offensive_possessions_ma4,
    net_opposing_proe_ma4,
    net_epa_allowed_ma4
  )


## testing set
test <- nfl_full_model_data |> 
  anti_join(train) |> 
  # keep these for modeling
  select(
    over, 
    net_epa_ma4,
    net_proe_ma4,
    net_offensive_possessions_ma4,
    net_opposing_proe_ma4,
    net_epa_allowed_ma4
  )


## model data (must be a matrix)

### training variable
x_train <- train |> 
  select(-over) |> 
  as.matrix()


### testing variable
x_test <- test |> 
  select(-over) |> 
  as.matrix()


## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(20, 150, 10),
  eta = c(0.01, 0.05, 0.1), 
  gamma = 0,
  max_depth = 2:4, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

## tuning

### set seed
set.seed(8029)

### tuning
xg_tune <- train(
  x = x_train,
  y = train$over, 
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "binary:logistic", 
  method = "xgbTree"
)


## model evaluation
### fit model to training data
xg_fit <- xgboost(
  data = x_train, 
  label = train$over,
  objective = "binary:logistic",
  nrounds = xg_tune$bestTune$nrounds,
  params = as.list(select(xg_tune$bestTune, -nrounds)),
  verbose = 0
)

### training error
train |> 
  mutate(pred = round(predict(xg_fit, newdata = x_train))) |> 
  summarize(correct = mean(over == pred))

### testing error
test |> 
  mutate(pred = round(predict(xg_fit, newdata = x_test))) |> 
  summarize(correct = mean(over == pred))

### variable importance
xg_fit |> 
  vip()
