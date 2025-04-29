## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(e1071)


# Initial Model -----------------------------------------------------------

## k
k <- 10

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
      ),
      
      # create a column of test fold assignments
      test_fold = sample(
        rep(
          1:k, 
          length.out = n()
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
      roof,
      test_fold
    )
}  


## get cross validation predictions using model formula
### can change to specify which dataset it is
get_cv_pred_naive_bayes <- function(model_formula, k = 10){
  get_test_pred <- function(fold_num) {
    
    ## training data
    train <- nfl_model_data |>
      # k fold cross validation
      filter(test_fold != fold_num) |> 
      # remove NAs
      drop_na(
        ends_with("ma4")
      )
    
    ## testing set
    test <- nfl_model_data |> 
      # k fold cross validation
      filter(test_fold == fold_num) |> 
      # remove NAs
      drop_na(
        ends_with("ma4")
      )
    
    
    ## fit the model
    train_fit <- svm(
      # formula
      as.formula(model_formula), 
      
      # kernel
      kernel = "radial",
      
      # cost
      cost = 1,
      
      # type
      type = "C-classification",
      
      # data
      data = train
    )
    
    
    ## test the model using test data
    test_res <- tibble(
      test_pred = predict(train_fit, newdata = test),
      test_actual = test$over,
      fold = fold_num
    )
  }
  
  ## do 10 fold cross validation
  output <- map(1:k, get_test_pred) |> 
    bind_rows()
  
  return(output)
}



## function enables easy generation of holdout analysis

## all net predictors
all_pred <- get_cv_pred_naive_bayes(
  "over ~ 
  net_ppg + 
  net_ppg_allowed +
  net_offensive_yards +
  net_defensive_yards +
  net_offensive_possessions +
  net_defensive_possessions +
  net_giveaways +
  net_takeaways +
  net_epa + 
  net_epa_allowed +
  net_proe +
  net_opposing_proe +
  net_ppg_ma4 + 
  net_ppg_allowed_ma4 +
  net_offensive_yards_ma4 +
  net_defensive_yards_ma4 +
  net_offensive_possessions_ma4 +
  net_defensive_possessions_ma4 +
  net_giveaways_ma4 +
  net_takeaways_ma4 +
  net_epa_ma4 + 
  net_epa_allowed_ma4 +
  net_proe_ma4 +
  net_opposing_proe_ma4"
)

## all net predictors
all_net_pred <- get_cv_pred_naive_bayes(
  "over ~ 
  net_ppg + 
  net_ppg_allowed +
  net_offensive_yards +
  net_defensive_yards +
  net_offensive_possessions +
  net_defensive_possessions +
  net_giveaways +
  net_takeaways +
  net_epa + 
  net_epa_allowed +
  net_proe +
  net_opposing_proe"
)

## all moving average predictors
all_ma4 <- get_cv_pred_naive_bayes(
  "over ~
  net_ppg_ma4 + 
  net_ppg_allowed_ma4 +
  net_offensive_yards_ma4 +
  net_defensive_yards_ma4 +
  net_offensive_possessions_ma4 +
  net_defensive_possessions_ma4 +
  net_giveaways_ma4 +
  net_takeaways_ma4 +
  net_epa_ma4 + 
  net_epa_allowed_ma4 +
  net_proe_ma4 +
  net_opposing_proe_ma4"
)

## points only
points_only_pred <- get_cv_pred_naive_bayes(
  "over ~ net_ppg + net_ppg_allowed"
)

## points moving avergae only
points_ma4_only_pred <- get_cv_pred_naive_bayes(
  "over ~ net_ppg_ma4 + net_ppg_allowed_ma4"
)

## advanced analytics predictors
analytics_pred <- get_cv_pred_naive_bayes(
  "over ~ 
  net_proe_ma4 +
  net_opposing_proe_ma4 +
  net_epa_ma4 +
  net_epa_allowed_ma4 +
  net_offensive_possessions_ma4 +
  net_defensive_possessions_ma4"
)

## intercept only
# int_only_pred <- get_cv_pred_naive_bayes("over ~ 1")



## can summarize together for a single plot
### shows average test error
bind_rows(
  mutate(all_pred, mod = "All"),
  mutate(all_net_pred, mod = "All Net Predictors"),
  mutate(all_ma4, mod = "All Moving Average Predictors"),
  mutate(points_only_pred, mod = "PPG and Opposing PPG Only"),
  mutate(points_ma4_only_pred, mod = "Moving Average PPG and Opposing PPG Only"),
  mutate(analytics_pred, mod = "Advanced Analytics")
) |>
  group_by(mod) |>
  # convert to a number
  mutate(
    test_pred = case_when(
      test_pred == 1 ~ 1,
      test_pred == 0 ~ 0
    )
  ) |> 
  summarize(
    rmse = sqrt(mean((test_actual - test_pred)^2))
  ) |>
  mutate(mod = fct_reorder(mod, rmse)) |>
  ggplot(
    aes(
      x = rmse, 
      y = mod
    )
  ) +
  geom_point()


# Train and Test Model --------------------------------------------------------------

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

## fit model
nfl_svm_tune <- tune(
  # model
  svm,
  
  # formula
  over ~ 
    net_ppg + 
    net_ppg_allowed +
    net_offensive_yards +
    net_defensive_yards +
    net_offensive_possessions +
    net_defensive_possessions +
    net_giveaways +
    net_takeaways +
    net_epa + 
    net_epa_allowed +
    net_proe +
    net_opposing_proe +
    net_ppg_ma4 + 
    net_ppg_allowed_ma4 +
    net_offensive_yards_ma4 +
    net_defensive_yards_ma4 +
    net_offensive_possessions_ma4 +
    net_defensive_possessions_ma4 +
    net_giveaways_ma4 +
    net_takeaways_ma4 +
    net_epa_ma4 + 
    net_epa_allowed_ma4 +
    net_proe_ma4 +
    net_opposing_proe_ma4,
  
  # kernel 
  kernel = "radial",
  
  # ranges
  
  ranges = list(cost = c(0.01, 0.1, 1, 5, 10,50, 100)),
  
  # data
  data = train
)

## best model
best_model <- nfl_svm_tune$best.model


## training accuracy
tibble(
  actual = train$over,
  train_pred = predict(best_model, newdata = train),
  pred = case_when(
    train_pred == 1 ~ 1,
    train_pred == 0 ~ 0
  )
) |> 
  # summarize
  summarize(
    train_accuracy = mean(actual == pred, na.rm = TRUE)
  )


## test accuracy
tibble(
  actual = test$over,
  train_pred = predict(nfl_NB, newdata = test),
  pred = case_when(
    train_pred == 1 ~ 1,
    train_pred == 0 ~ 0
  )
) |> 
  # summarize
  summarize(
    train_accuracy = mean(actual == pred)
  )


## Accuracy 45.6%



# Improve Model -----------------------------------------------------------

nfl_svm <- svm(
  # formula
  # formula
  over ~ 
    avg_total_yards_ma4_home_team +
    avg_total_yards_allowed_ma4_away_team +
    avg_total_yards_ma4_away_team +
    avg_total_yards_allowed_ma4_home_team +
    ppg_ma4_home_team +
    ppg_ma4_away_team +
    ppg_allowed_ma4_home_team +
    ppg_allowed_ma4_away_team
  
    ,
  
  # kernel 
  kernel = "radial",
  
  # cost
  cost = 1,
  
  data = train,
  
  probability = TRUE
)

## training accuracy
tibble(
  actual = train$over,
  train_pred = predict(nfl_svm, newdata = train, probability = TRUE),
  pred = if_else(
    train_pred >= 0.5,
    1, 
    0
  )
) |> 
  # summarize
  summarize(
    train_accuracy = mean(actual == pred, na.rm = TRUE)
  )


## test accuracy
tibble(
  actual = test$over,
  train_pred = predict(nfl_svm, newdata = test, probability = TRUE),
  pred = if_else(
    train_pred >= 0.5,
    1, 
    0
  )
) |> 
  # summarize
  summarize(
    train_accuracy = mean(actual == pred)
  )
