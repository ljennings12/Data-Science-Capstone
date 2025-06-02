## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(ranger)
library(vip)
library(pdp)



# Full Random Forest Model ------------------------------------------------

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
      hour_time_difference
    )
}  


## set a seed
set.seed(8029)


## training data
train <- nfl_model_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |> 
  # remove season and game ID
  select(
    -c(game_id:away_team)
  )


## testing set
test <- nfl_model_data |> 
  anti_join(train)


## model
nfl_rf_full_model <- ranger(
  over ~ .,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_full_model


## variable importance
nfl_rf_full_model |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_full_model, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 43.7%



# Improve Full Model ------------------------------------------------------

## set a seed
set.seed(8029)


## model
nfl_rf_full_model_2 <- ranger(
  over ~ 
    net_opposing_proe_ma4 +
    net_epa_allowed_ma4 +
    net_epa_ma4,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_full_model_2


## variable importance
nfl_rf_full_model_2 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_full_model_2, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 54.4%
## but predicts under for every game in weeks 2-4 because all predictors are NA



# Improve Full Model ------------------------------------------------------

## set a seed
set.seed(8029)


## model
nfl_rf_full_model_3 <- ranger(
  over ~ 
    net_opposing_proe_ma4 +
    net_epa_allowed_ma4 +
    net_epa_ma4 +
    net_giveaways_ma4,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_full_model_3


## variable importance
nfl_rf_full_model_3 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_full_model_3, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 52.4%
## but predicts under for every game in weeks 2-4 because all predictors are NA


# Improve Full Model ------------------------------------------------------

## set a seed
set.seed(8029)


## model
nfl_rf_full_model_4 <- ranger(
  over ~ 
    net_opposing_proe_ma4 +
    net_epa_ma4 +
    net_epa_allowed_ma4 +
    net_offensive_possessions_ma4 +
    net_giveaways_ma4,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_full_model_4


## variable importance
nfl_rf_full_model_4 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_full_model_4, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 53.6%
## but predicts under for every game in weeks 2-4 because all predictors are NA


# Improve Full Model and Remove Weeks 2-4 ------------------------------------------------------

## set a seed
set.seed(8029)

## training data
train <- nfl_model_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |> 
  # remove season and game ID
  select(
    -c(game_id:away_team)
  ) |> 
  # remove NAs (weeks 2:4)
  drop_na(starts_with("net"))


## testing set
test <- nfl_model_data |> 
  anti_join(train) |> 
  # remove NAs (weeks 2:4)
  drop_na(starts_with("net"))


## model
nfl_rf_model_ma4 <- ranger(
  over ~ 
    net_ppg_ma4 +
    net_ppg_allowed_ma4 +
    net_offensive_yards_ma4 +
    net_defensive_yards_ma4 +
    net_takeaways_ma4 + 
    net_epa_ma4 +
    net_epa_allowed_ma4 +
    net_pass_rate_ma4 +
    net_opposing_pass_rate_ma4 +
    net_proe_ma4 +
    net_opposing_proe_ma4
    ,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_ma4


## variable importance
nfl_rf_model_ma4 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_ma4, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 47.1%



# Improve Full Model and Remove Weeks 2-4 ------------------------------------------------------

## set a seed
set.seed(8029)


## model
nfl_rf_model_ma4_1 <- ranger(
  over ~ 
    net_ppg_ma4 +
    net_offensive_yards_ma4 +
    net_takeaways_ma4 + 
    net_epa_ma4 +
    net_epa_allowed_ma4 +
    net_pass_rate_ma4 +
    net_opposing_pass_rate_ma4 +
    net_proe_ma4 +
    net_opposing_proe_ma4
  ,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_ma4_1


## variable importance
nfl_rf_model_ma4_1 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_ma4_1, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 49.5%


# Improve Full Model and Remove Weeks 2-4 ------------------------------------------------------

## set a seed
set.seed(8029)


## model
nfl_rf_model_ma4_2 <- ranger(
  over ~ 
    net_ppg_ma4 +
    net_offensive_yards_ma4 +
    net_takeaways_ma4 + 
    net_epa_ma4 +
    net_epa_allowed_ma4
  ,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_ma4_2


## variable importance
nfl_rf_model_ma4_2 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_ma4_2, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 50.5%



# Improve Full Model and Remove Weeks 2-4 ------------------------------------------------------

## set a seed
set.seed(8029)


## model
nfl_rf_model_ma4_3 <- ranger(
  over ~ 
    net_ppg_ma4 +
    net_offensive_yards_ma4 +
    net_takeaways_ma4
  ,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_ma4_3


## variable importance
nfl_rf_model_ma4_3 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_ma4_3, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 50.5%





# Improve Moving Average Model --------------------------------------------

## set a seed
set.seed(8029)


## model
nfl_rf_model_ma4_4 <- ranger(
  over ~ 
    net_ppg_ma4 +
    net_ppg_allowed_ma4
  ,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_ma4_4


## variable importance
nfl_rf_model_ma4_4 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_ma4_4, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 46.4%







# Random Forest Differences Model -----------------------------------------

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
      net_opposing_proe = opposing_proe_home_team - proe_away_team
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
      hour_time_difference,
      temperature,
      wind
    )
}  


## set a seed
set.seed(8029)


## training data
train <- nfl_model_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |> 
  # remove season and game ID
  select(
    -c(game_id:away_team)
  )


## testing set
test <- nfl_model_data |> 
  anti_join(train)


## model
nfl_rf_full_model <- ranger(
  over ~ .,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_full_model


## variable importance
nfl_rf_full_model |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_full_model, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 46.0%


# Random Forest Differences Model -----------------------------------------

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
      net_opposing_proe = opposing_proe_home_team - proe_away_team
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
      starts_with("net_"),
      temperature,
      wind
    )
}  


## set a seed
set.seed(8029)


## training data
train <- nfl_model_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |> 
  # remove season and game ID
  select(
    -c(game_id:away_team)
  )


## testing set
test <- nfl_model_data |> 
  anti_join(train)


## model
nfl_rf_model <- ranger(
  over ~ .,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model


## variable importance
nfl_rf_model |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 49.6%




# Points Random Forest -----------------------------------------------

## set a seed
set.seed(8029)

## model
nfl_rf_model_points <- ranger(
  over ~ net_ppg + net_ppg_allowed,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_points


## variable importance
nfl_rf_model_points |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_points, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 54.4%



# Improve Points Random Forest -----------------------------------------------

## set a seed
set.seed(8029)

## model
nfl_rf_model_points_2 <- ranger(
  over ~ net_ppg + net_ppg_allowed,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_points_2


## variable importance
nfl_rf_model_points_2 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_points_2, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 54.4%




# Moving Average Random Forest Model --------------------------------------

## create difference variables
nfl_model_ma4_data <- {
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
train <- nfl_model_ma4_data |>
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
test <- nfl_model_ma4_data |> 
  anti_join(train) |> 
  # remove NAs
  drop_na(
    ends_with("ma4")
  )


## model
nfl_rf_model_ma4 <- ranger(
  over ~ .,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_ma4


## variable importance
nfl_rf_model_ma4 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_ma4, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 46.1%



# Improve Moving Average Random Forest ------------------------------------

## model
nfl_rf_model_ma4_points <- ranger(
  over ~ net_ppg_ma4 + net_ppg_allowed_ma4,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_ma4_points


## variable importance
nfl_rf_model_ma4_points |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_ma4_points, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 48.5%



# Account for Everything Random Forest ------------------------------------

## create difference variables
nfl_full_model_data <- {
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
      net_opposing_proe_ma4 = opposing_proe_ma4_home_team - proe_ma4_away_team
    ) |> 
    # select columns to use in modeling
    select(
      game_id,
      season,
      week,
      home_team,
      away_team,
      over,
      starts_with("net_"),
      hour_time_difference,
      # temperature,
      # wind
    )
}  


## training data
train <- nfl_full_model_data |>
  # 2021 - 2023
  filter(
    season != 2024
  ) |>
  # remove season and game ID
  select(
    -c(game_id:away_team)
  )


## testing set
test <- nfl_full_model_data |> 
  anti_join(train)


## model
nfl_full_rf_model <- ranger(
  over ~ .,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_full_rf_model


## variable importance
nfl_full_rf_model |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_full_rf_model, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 48.8%


# Improve Everything Random Forest ------------------------------------

# set seed
set.seed(8029)

## model
nfl_full_rf_model_2 <- ranger(
  over ~ net_epa_ma4 + net_proe_ma4 + net_offensive_possessions_ma4 + net_opposing_proe_ma4 + net_epa_allowed_ma4,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_full_rf_model_2


## variable importance
nfl_full_rf_model_2 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_full_rf_model_2, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 52.0%



# Improve Everything Random Forest ------------------------------------

# set seed
set.seed(8029)

## model
nfl_full_rf_model_3 <- ranger(
  over ~ net_epa_ma4 + net_proe_ma4 + net_offensive_possessions_ma4 + net_opposing_proe_ma4 + net_epa_allowed_ma4 + 
    net_giveaways_ma4 + net_takeaways_ma4,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_full_rf_model_3


## variable importance
nfl_full_rf_model_3 |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_full_rf_model_3, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 52.4%





# New Random Forest -------------------------------------------------------

## create difference variables
nfl_model_data_sandbox <- {
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
      net_ppg = ppg_home_team + ppg_allowed_away_team,
      
      # difference in points allowed
      matchup_ppg_allowed = ppg_allowed_home_team + ppg_away_team,
      
      # difference in offensive yards
      matchup_offensive_yards = avg_total_yards_home_team + avg_total_yards_allowed_away_team,
      
      # difference in defensive yards
      matchup_defensive_yards = avg_total_yards_allowed_home_team + avg_total_yards_away_team,
      
      # difference in offensive possessions
      matchup_offensive_possessions = possessions_per_game_home_team + possessions_per_game_allowed_away_team,
      
      # difference in defensive possessions
      matchup_defensive_possessions = possessions_per_game_allowed_home_team + possessions_per_game_away_team,
      
      # difference in giveaways
      matchup_giveaways = giveaways_per_game_home_team + takeaways_per_game_away_team,
      
      # difference in takeaways
      matchup_takeaways = giveaways_per_game_away_team + takeaways_per_game_home_team,
      
      # difference in offensive EPA
      matchup_epa = epa_home_team + epa_allowed_away_team,
      
      # difference in defensive EPA
      matchup_epa_allowed = epa_allowed_home_team + epa_away_team,
      
      # difference in offensive pass rate
      matchup_pass_rate = pass_rate_home_team + opposing_pass_rate_away_team,
      
      # difference in opposing pass rate
      matchup_opposing_pass_rate = pass_rate_away_team + opposing_pass_rate_home_team,
      
      # difference in pass rate over expected
      matchup_proe = proe_home_team + opposing_proe_away_team,
      
      # difference in opposing pass rate over expected
      matchup_opposing_proe = opposing_proe_home_team + proe_away_team,
      
      # difference in points scored
      matchup_ppg_ma4 = ppg_ma4_home_team + ppg_allowed_ma4_away_team,
      
      # difference in points allowed
      matchup_ppg_allowed_ma4 = ppg_allowed_ma4_home_team + ppg_ma4_away_team,
      
      # difference in offensive yards
      matchup_offensive_yards_ma4 = avg_total_yards_ma4_home_team + avg_total_yards_allowed_ma4_away_team,
      
      # difference in defensive yards
      matchup_defensive_yards_ma4 = avg_total_yards_allowed_ma4_home_team + avg_total_yards_ma4_away_team,
      
      # difference in offensive possessions
      matchup_offensive_possessions_ma4 = possessions_per_game_ma4_home_team + possessions_per_game_allowed_ma4_away_team,
      
      # difference in defensive possessions
      matchup_defensive_possessions_ma4 = possessions_per_game_allowed_ma4_home_team + possessions_per_game_ma4_away_team,
      
      # difference in giveaways
      matchup_giveaways_ma4 = giveaways_per_game_ma4_home_team + takeaways_per_game_ma4_away_team,
      
      # difference in takeaways
      matchup_takeaways_ma4 = takeaways_per_game_ma4_home_team + giveaways_per_game_ma4_away_team,
      
      # difference in offensive EPA
      matchup_epa_ma4 = epa_ma4_home_team + epa_allowed_ma4_away_team,
      
      # difference in defensive EPA
      matchup_epa_allowed_ma4 = epa_allowed_ma4_home_team + epa_ma4_away_team,
      
      # difference in offensive pass rate
      matchup_pass_rate_ma4 = pass_rate_ma4_home_team + opposing_pass_rate_ma4_away_team,
      
      # difference in opposing pass rate
      matchup_opposing_pass_rate_ma4 = opposing_pass_rate_ma4_home_team + pass_rate_ma4_away_team,
      
      # difference in pass rate over expected
      matchup_proe_ma4 = proe_ma4_home_team + opposing_proe_ma4_away_team,
      
      # difference in opposing pass rate over expected
      matchup_opposing_proe_ma4 = opposing_proe_ma4_home_team + proe_ma4_away_team,
      
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
      starts_with("matchup_"),
      hour_time_difference,
      temperature,
      wind
    )
}  



## set a seed
set.seed(8029)


## training data
train <- nfl_model_data_sandbox |>
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
test <- nfl_model_data_sandbox |> 
  anti_join(train) |> 
  # remove NAs
  drop_na(
    ends_with("ma4")
  )


## model
nfl_rf_model_sandbox <- ranger(
  over ~ . - 
    div_game - 
    hour_time_difference,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_sandbox


## variable importance
nfl_rf_model_sandbox |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_sandbox, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 50.5%




# Sandbox Model 2 ---------------------------------------------------------

## set a seed
set.seed(8029)


## model
nfl_rf_model_sandbox <- ranger(
  over ~ 
    matchup_ppg_ma4 +
    matchup_ppg_allowed_ma4 +
    matchup_offensive_yards_ma4 +
    matchup_defensive_yards_ma4 +
    matchup_offensive_possessions_ma4 +
    matchup_defensive_possessions_ma4 +
    matchup_giveaways_ma4 +
    matchup_takeaways_ma4 +
    matchup_epa_ma4 +
    matchup_epa_allowed_ma4 +
    matchup_proe_ma4 +
    matchup_opposing_proe_ma4
    ,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
nfl_rf_model_sandbox


## variable importance
nfl_rf_model_sandbox |> 
  vip()


## predict probabilities of test set
### predictions
model_pred_prob <- predict(nfl_rf_model_sandbox, data = test, type = "response")

### predict the class
model_pred_class <- ifelse(model_pred_prob$predictions > 0.5, 1, 0)


## model assessment
### confusion matrix
tibble(
  actual = test$over,
  predicted = model_pred_class
) |> 
  summarize(
    accuracy = mean(actual == predicted)
  )


## accuracy: 50.5%

