## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(glmnet)
library(ranger)
library(xgboost)
library(mgcv)
library(e1071)
library(caret)

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
    ) |> 
    # drop NAs (remove weeks 2-4)
    drop_na(
      starts_with("net")
    )
}



# Soft Voting Classifier -------------------------------------------------------

## set seed
set.seed(9723)


## create folds
folds <- createFolds(
  # target
  nfl_model_data$over,
  
  # number of folds
  k = 10,
  
  # list
  list = TRUE,
  
  # return training set
  returnTrain = FALSE
)


## store results
all_predictions <- rep(NA, nrow(nfl_model_data))


## loop over folds
for(i in seq_along(folds)){
  # training and testing sets
  ## test
  test_index <- folds[[i]]
  test <- nfl_model_data[test_index, ]
  
  ## training
  train_index <- setdiff(
    seq_len(nrow(nfl_model_data)), 
    test_index
  )
  train <- nfl_model_data[train_index, ]
  
  ## data matrices (for XGBoost)
  ### training
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
  
  
  ### testing
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
  
  
  # fit models on the training data
  ## logistic model
  logit_nfl_model <- glm(
    # formula
    over ~ 
      avg_total_yards_ma4_home_team:avg_total_yards_allowed_ma4_away_team +
      avg_total_yards_ma4_away_team:avg_total_yards_allowed_ma4_home_team,
    
    # data 
    data = train,
    
    # family
    family = "binomial"
  )
  
  ## random forest
  rf_nfl_model <- ranger(
    over ~ net_epa_ma4 + net_proe_ma4 + net_offensive_possessions_ma4 + net_opposing_proe_ma4 + net_epa_allowed_ma4 + 
      net_giveaways_ma4 + net_takeaways_ma4,
    num.trees = 1000,
    importance = "impurity",
    data = train
  )
  
  ## xgboost
  xgboost_nfl_model <- xgboost(
    data = x_train, 
    label = as.vector(train$over),
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
  
  ## GAM## Gas.vector.data.frame()AM
  gam_nfl_model <- gam(
    # formula
    over ~
      s(net_proe_ma4) +
      s(net_opposing_proe_ma4) +
      s(net_epa_ma4) +
      s(net_epa_allowed_ma4),
    # family
    family = "binomial",
    # method
    method = "REML",
    # data
    data = train
  )
  
  ## Naive Bayes
  NB_nfl_model <- naiveBayes(
    # formula
    over ~ 
      net_ppg + 
      net_ppg_allowed +
      net_epa + 
      net_epa_allowed +
      net_ppg_ma4 + 
      net_ppg_allowed_ma4 +
      net_offensive_yards_ma4 +
      net_defensive_yards_ma4 +
      net_giveaways_ma4 +
      net_takeaways_ma4,
    
    # data
    data = train
  )
  
  ## get predicted probabilities
  prob1 <- predict(logit_nfl_model, test, type = "response")
  prob2 <- predict(rf_nfl_model, test, type = "response")
  prob3 <- predict(xgboost_nfl_model, x_test, type = "response")
  prob4 <- predict(gam_nfl_model, test, type = "response")
  prob5 <- predict(NB_nfl_model, test, type = "raw")[, "1"]
  
  
  ## soft voting
  avg_prob <- (prob1 + prob2$predictions + prob3 + prob4 + prob5) / 5
  
  
  ## final prediction
  all_predictions[test_index] <- ifelse(avg_prob > 0.5, 1, 0)
}


## evaluate performance
confusionMatrix(factor(all_predictions), factor(nfl_model_data$over))




# Hard Voting Classifier -------------------------------------------------------

## set seed
set.seed(9723)


## create folds
folds <- createFolds(
  # target
  nfl_model_data$over,
  
  # number of folds
  k = 10,
  
  # list
  list = TRUE,
  
  # return training set
  returnTrain = FALSE
)


## store results
all_predictions <- rep(NA, nrow(nfl_model_data))


## loop over folds
for(i in seq_along(folds)){
  # training and testing sets
  ## test
  test_index <- folds[[i]]
  test <- nfl_model_data[test_index, ]
  
  ## training
  train_index <- setdiff(
    seq_len(nrow(nfl_model_data)), 
    test_index
  )
  train <- nfl_model_data[train_index, ]
  
  ## data matrices (for XGBoost)
  ### training
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
  
  
  ### testing
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
  
  
  # fit models on the training data
  ## logistic model
  logit_nfl_model <- glm(
    # formula
    over ~ 
      avg_total_yards_ma4_home_team:avg_total_yards_allowed_ma4_away_team +
      avg_total_yards_ma4_away_team:avg_total_yards_allowed_ma4_home_team,
    
    # data 
    data = train,
    
    # family
    family = "binomial"
  )
  
  ## random forest
  rf_nfl_model <- ranger(
    over ~ net_epa_ma4 + net_proe_ma4 + net_offensive_possessions_ma4 + net_opposing_proe_ma4 + net_epa_allowed_ma4 + 
      net_giveaways_ma4 + net_takeaways_ma4,
    num.trees = 1000,
    importance = "impurity",
    data = train
  )
  
  ## xgboost
  xgboost_nfl_model <- xgboost(
    data = x_train, 
    label = as.vector(train$over),
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
  
  ## GAM## Gas.vector.data.frame()AM
  gam_nfl_model <- gam(
    # formula
    over ~
      s(net_proe_ma4) +
      s(net_opposing_proe_ma4) +
      s(net_epa_ma4) +
      s(net_epa_allowed_ma4),
    # family
    family = "binomial",
    # method
    method = "REML",
    # data
    data = train
  )
  
  ## Naive Bayes
  NB_nfl_model <- naiveBayes(
    # formula
    over ~ 
      net_ppg + 
      net_ppg_allowed +
      net_epa + 
      net_epa_allowed +
      net_ppg_ma4 + 
      net_ppg_allowed_ma4 +
      net_offensive_yards_ma4 +
      net_defensive_yards_ma4 +
      net_giveaways_ma4 +
      net_takeaways_ma4,
    
    # data
    data = train
  )
  
  ## get predicted probabilities
  pred1 <- ifelse(
    predict(logit_nfl_model, test, type = "response") > 0.5,
    1,
    0
  )
  pred2 <- ifelse(
    predict(rf_nfl_model, test)$predictions > 0.5,
    1,
    0
  )
  pred3 <- ifelse(
    predict(xgboost_nfl_model, x_test) > 0.5,
    1,
    0
  )
  pred4 <- predict(gam_nfl_model, test, type = "response")
  pred5 <- predict(NB_nfl_model, test)
  
  
  ## combine into a matrix
  prediction_matrix <- cbind(
    pred1, pred2, pred3, pred4, pred5
  )
  
  
  ## hard voting
  vote_prediction <- apply(prediction_matrix, 1, function(x) {
    # Find the most common class label (majority vote)
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  })
  
  
  ## final prediction
  all_predictions[test_index] <- vote_prediction
}


## evaluate performance
confusionMatrix(factor(all_predictions), factor(nfl_model_data$over))
