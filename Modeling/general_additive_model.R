## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(mgcv)
library(broom)



# Initial Model -----------------------------------------------------------

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
      starts_with("net_"),
      hour_time_difference,
      temperature,
      wind,
      roof,
      test_fold
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


## get cross validation predictions using model formula
### can change to specify which dataset it is
get_cv_pred <- function(model_formula, k = 10){
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
    train_fit <- gam(
      # formula
      as.formula(model_formula), 
      # family
      family = "binomial",
      # method
      method = "REML",
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
all_pred <- get_cv_pred(
  "over ~ 
  s(net_ppg) + 
  s(net_ppg_allowed) +
  s(net_offensive_yards) +
  s(net_defensive_yards) +
  s(net_offensive_possessions) +
  s(net_defensive_possessions) +
  s(net_giveaways) +
  s(net_takeaways) +
  s(net_epa) + 
  s(net_epa_allowed) +
  s(net_proe) +
  s(net_opposing_proe) +
  s(net_ppg_ma4) + 
  s(net_ppg_allowed_ma4) +
  s(net_offensive_yards_ma4) +
  s(net_defensive_yards_ma4) +
  s(net_offensive_possessions_ma4) +
  s(net_defensive_possessions_ma4) +
  s(net_giveaways_ma4) +
  s(net_takeaways_ma4) +
  s(net_epa_ma4) + 
  s(net_epa_allowed_ma4) +
  s(net_proe_ma4) +
  s(net_opposing_proe_ma4)"
)

## all net predictors
all_net_pred <- get_cv_pred(
  "over ~ 
  s(net_ppg) + 
  s(net_ppg_allowed) +
  s(net_offensive_yards) +
  s(net_defensive_yards) +
  s(net_offensive_possessions) +
  s(net_defensive_possessions) +
  s(net_giveaways) +
  s(net_takeaways) +
  s(net_epa) + 
  s(net_epa_allowed) +
  s(net_proe) +
  s(net_opposing_proe)"
)

## all moving average predictors
all_ma4 <- get_cv_pred(
  "over ~
  s(net_ppg_ma4) + 
  s(net_ppg_allowed_ma4) +
  s(net_offensive_yards_ma4) +
  s(net_defensive_yards_ma4) +
  s(net_offensive_possessions_ma4) +
  s(net_defensive_possessions_ma4) +
  s(net_giveaways_ma4) +
  s(net_takeaways_ma4) +
  s(net_epa_ma4) + 
  s(net_epa_allowed_ma4) +
  s(net_proe_ma4) +
  s(net_opposing_proe_ma4)"
)

## points only
points_only_pred <- get_cv_pred(
  "over ~ s(net_ppg) + s(net_ppg_allowed)"
)

## points moving avergae only
points_ma4_only_pred <- get_cv_pred(
  "over ~ s(net_ppg_ma4) + s(net_ppg_allowed_ma4)"
)

## advanced analytics predictors
analytics_pred <- get_cv_pred(
  "over ~ 
  s(net_proe_ma4) +
  s(net_opposing_proe_ma4) +
  s(net_epa_ma4) +
  s(net_epa_allowed_ma4) +
  s(net_offensive_possessions_ma4) +
  s(net_defensive_possessions_ma4)"
)

## intercept only
int_only_pred <- get_cv_pred("over ~ 1")



## can summarize together for a single plot
### shows average test error
bind_rows(
  mutate(all_pred, mod = "All"),
  mutate(all_net_pred, mod = "All Net Predictors"),
  mutate(all_ma4, mod = "All Moving Average Predictors"),
  mutate(points_only_pred, mod = "PPG and Opposing PPG Only"),
  mutate(points_ma4_only_pred, mod = "Moving Average PPG and Opposing PPG Only"),
  mutate(analytics_pred, mod = "Advanced Analytics"),
  mutate(int_only_pred, mod = "Intercept only")
) |>
  group_by(mod) |>
  summarize(
    rmse = sqrt(mean((test_actual - test_pred)^2))
  ) |>
  mutate(mod = fct_reorder(mod, rmse)) |>
  ggplot(aes(x = rmse, y = mod)) +
  geom_point()




# Train and Test Model --------------------------------------------------------------

## fit model
nfl_gam <- gam(
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


## look at variables
nfl_gam |> 
  tidy()


## evaluate prediction accuracy
### in-sample performance (training set)
nfl_gam |> 
  augment(
    newdata = train, 
    type.predict = "response"
  ) |> 
  mutate(pred_class = round(.fitted)) |> 
  # prediction accuracy - count how many "1's" there are
  summarize(correct = mean(over == pred_class))

### out-of-sample performance (test set)
nfl_gam |> 
  augment(
    newdata = test, 
    type.predict = "response"
  ) |> 
  mutate(pred_class = round(.fitted)) |> 
  # prediction accuracy - count how many "1's" there are
  summarize(correct = mean(over == pred_class))


## Accuracy: 48.5%



# Train and Test Model --------------------------------------------------------------

## fit model
nfl_gam <- gam(
  # formula
  over ~
    s(net_ppg) + s(net_ppg_allowed),
  # family
  family = "binomial",
  # method
  method = "REML",
  # data
  data = train
)


## look at variables
nfl_gam |> 
  tidy()


## evaluate prediction accuracy
### in-sample performance (training set)
nfl_gam |> 
  augment(
    newdata = train, 
    type.predict = "response"
  ) |> 
  mutate(pred_class = round(.fitted)) |> 
  # prediction accuracy - count how many "1's" there are
  summarize(correct = mean(over == pred_class))

### out-of-sample performance (test set)
nfl_gam |> 
  augment(
    newdata = test, 
    type.predict = "response"
  ) |> 
  mutate(pred_class = round(.fitted)) |> 
  # prediction accuracy - count how many "1's" there are
  summarize(correct = mean(over == pred_class))


## accuracy: 45.1%




# Improve the Model -------------------------------------------------------

## fit model
nfl_gam <- gam(
  # formula
  over ~
    s(net_ppg_ma4) + 
    s(net_offensive_yards_ma4) +
    s(net_offensive_possessions_ma4) +
    s(net_giveaways_ma4) +
    s(net_takeaways_ma4) +
    s(net_epa_ma4),
  
  # family
  family = "binomial",
  # method
  method = "REML",
  # data
  data = train
)


## look at variables
nfl_gam |> 
  tidy() |> 
  arrange(-p.value)


## evaluate prediction accuracy
### in-sample performance (training set)
nfl_gam |> 
  augment(
    newdata = train, 
    type.predict = "response"
  ) |> 
  mutate(pred_class = round(.fitted)) |> 
  # prediction accuracy - count how many "1's" there are
  summarize(correct = mean(over == pred_class))

### out-of-sample performance (test set)
nfl_gam |> 
  augment(
    newdata = test, 
    type.predict = "response"
  ) |> 
  mutate(pred_class = round(.fitted)) |> 
  # prediction accuracy - count how many "1's" there are
  summarize(correct = mean(over == pred_class))


## Accuracy: 46.6%
