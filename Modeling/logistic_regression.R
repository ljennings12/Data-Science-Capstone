## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(glmnet)
library(broom)


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
      contains("ma4"),
      starts_with("net_"),
      hour_time_difference
    )
}  


# Original Logistic Regression -----------------------------------------------------

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


## initial model
full_model <- glm(
  # formula
  over ~ .,
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(full_model)


## variables
full_model |> 
  tidy() |> 
  arrange(p.value)


## exponentiate coefficient estimates to get odds
full_model |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(full_model, newdata = data.frame(test), type = "response")

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

### Accuracy ~ 49.5%



# Improve Full Model ------------------------------------------------------

## initial model
full_model_1 <- glm(
  # formula
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
    net_opposing_proe_ma4,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(full_model_1)


## variables
full_model_1 |> 
  tidy() |> 
  arrange(-p.value)


# ## exponentiate coefficient estimates to get odds
# full_model_1 |> 
#   tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(full_model_1, newdata = data.frame(test), type = "response")

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

### Accuracy ~ 55.3%




# Improve Full Model ------------------------------------------------------

## initial model
full_model_2 <- glm(
  # formula
  over ~ 
    possessions_per_game_ma4_home_team +
    possessions_per_game_allowed_ma4_home_team +
    opposing_proe_ma4_home_team,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(full_model_2)


## variables
full_model_2 |> 
  tidy()


## exponentiate coefficient estimates to get odds
full_model_2 |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(full_model_2, newdata = data.frame(test), type = "response")

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


# Interaction Model -------------------------------------------------------

## initial model
interaction_model <- glm(
  # formula
  over ~ 
    proe_home_team:opposing_proe_away_team + 
    proe_away_team:opposing_proe_home_team + 
    epa_home_team:epa_allowed_away_team +
    avg_total_yards_home_team:avg_total_yards_allowed_away_team +
    ppg_home_team:ppg_allowed_away_team +
    ppg_away_team:ppg_allowed_home_team,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(interaction_model)


## variables
interaction_model |> 
  tidy()


## exponentiate coefficient estimates to get odds
interaction_model |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(interaction_model, newdata = data.frame(test), type = "response")

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

### Accuracy ~ 51.5%



# Home Offense vs. Away Defense ------------------------------------------------------

## initial model
yards_model <- glm(
  # formula
  over ~ 
    avg_total_yards_ma4_home_team:avg_total_yards_allowed_ma4_away_team +
    avg_total_yards_ma4_away_team:avg_total_yards_allowed_ma4_home_team,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(yards_model)


## variables
yards_model |> 
  tidy()


## exponentiate coefficient estimates to get odds
yards_model |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(yards_model, newdata = data.frame(test), type = "response")

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

### Accuracy ~ 52.5%




# Differences Model Logistic Regression ----------------------------------------------

## difference model
differences_full_model <- glm(
  # formula
  over ~ .,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(differences_full_model)


## variables
differences_full_model |> 
  tidy() |> 
  arrange(p.value)


## exponentiate coefficient estimates to get odds
differences_full_model |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(differences_full_model, newdata = data.frame(test), type = "response")

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

### Accuracy ~ 45.6%



# Improve Differences Model -----------------------------------------------

## difference model
differences_model <- glm(
  # formula
  over ~ 
    net_takeaways +
    net_epa_allowed +
    net_offensive_possessions
    ,
  
  # data 
  data = train,
  
  # family
  family = "binomial"
)


## summary
summary(differences_model)


## variables
differences_model |> 
  tidy()


## exponentiate coefficient estimates to get odds
differences_model |> 
  tidy(conf.int = TRUE, exponentiate = TRUE)



## predict probabilities of test set
### predictions
model_pred_prob <- predict(differences_model, newdata = data.frame(test), type = "response")

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

### Accuracy ~ 46.1%



# Points Logistic Regression ----------------------------------------------

## improve model
nfl_logit_model_points <- glm(
  # formula
  over ~ 
    ppg_home_team:ppg_allowed_away_team +
    ppg_away_team:ppg_allowed_home_team,
  
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


## Accuracy: 48.5%

