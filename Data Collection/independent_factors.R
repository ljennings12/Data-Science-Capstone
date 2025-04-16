## Liam Jennings
## Data Science Capstone

# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)

# Independent Factors -------------------------------------------------------

independent_metrics <- {
  play_by_play |> 
    # filter
    filter(
      # regular season
      season_type == "REG",
      # remove international games
      location == "Home"
    ) |> 
    # group by game
    group_by(game_id, home_team, away_team, week) |> 
    # summarize
    summarize(
      # inside or outside
      roof = first(roof),
      
      # turf type
      surface = first(surface),
      
      # temperature (in fahrenheit)
      temperature = first(temp),
      
      # wind (in mph)
      wind = first(wind),
      
      # weather description
      weather = last(weather),
      
      # kickoff time (game start)
      time_of_kickoff = first(time_of_day, na_rm = TRUE)
    ) |> 
    # mutate
    mutate(
      # parse the ISO 8601 string properly to strip milliseconds and handle the time zone
      time_of_kickoff = ymd_hms(sub("\\.\\d+Z$", "", time_of_kickoff), tz = "UTC"),
      
      # add time zone to the dataset
      home_timezone = case_when(
        # Eastern Time Zone
        home_team %in% c(
          "ATL", "BAL", "BUF", "CAR", "CIN", "CLE", 
          "DET", "IND", "JAX", "MIA", "NE", "NYG",
          "NYJ", "PHI", "PIT", "TB", "WAS"
        ) ~ "US/Eastern",
        
        # Central Time Zone
        home_team %in% c(
          "CHI", "DAL", "GB", "HOU", "KC", "MIN", "NO", "TEN"
        ) ~ "US/Central",
        
        # Mountain Time Zone
        home_team %in% c(
          "DEN"
        ) ~ "US/Mountain",
        
        # Arizona
        home_team %in% c(
          "ARI"
        ) ~ "US/Arizona",
        
        # Pacific
        home_team %in% c(
          "LA", "LAC", "LV", "SEA", "SF"
        ) ~ "US/Pacific"
      ),
      
      # add time zone to the dataset
      away_timezone = case_when(
        # Eastern Time Zone
        away_team %in% c(
          "ATL", "BAL", "BUF", "CAR", "CIN", "CLE", 
          "DET", "IND", "JAX", "MIA", "NE", "NYG",
          "NYJ", "PHI", "PIT", "TB", "WAS"
        ) ~ "US/Eastern",
        
        # Central Time Zone
        away_team %in% c(
          "CHI", "DAL", "GB", "HOU", "KC", "MIN", "NO", "TEN"
        ) ~ "US/Central",
        
        # Mountain Time Zone
        away_team %in% c(
          "DEN"
        ) ~ "US/Mountain",
        
        # Arizona
        away_team %in% c(
          "ARI"
        ) ~ "US/Arizona",
        
        # Pacific
        away_team %in% c(
          "LA", "LAC", "LV", "SEA", "SF"
        ) ~ "US/Pacific"
      ),
      
      # home team kickoff time with specified time zone
      home_team_kickoff_time = map2(
        time_of_kickoff,
        home_timezone,
        ~with_tz(.x, tzone = .y)
      ),
      
      # unwrap list and convert into a datetime object
      home_team_kickoff_time = as_datetime(
        unlist(
          home_team_kickoff_time
        )
      ),
      
      # visual home time zone
      home_team_kickoff_time_visual = map2_chr(
        time_of_kickoff, 
        home_timezone, 
        ~format(
          with_tz(.x, tzone = .y), 
          "%Y-%m-%d %H:%M:%S %Z"
        )
      ),
      
      # home time hour:minute
      home_team_kickoff_time_hm = format(
        ymd_hms(home_team_kickoff_time_visual),
        "%I:%M:%S %p"
      ),
      
      # away team kickoff time with specified time zone
      away_team_kickoff_time = map2(
        time_of_kickoff,
        away_timezone,
        ~with_tz(.x, tzone = .y)
      ),
      
      # unwrap list column
      away_team_kickoff_time = as_datetime(unlist(home_team_kickoff_time)),
      
      # visual home time zone
      away_team_kickoff_time_visual = map2_chr(
        time_of_kickoff, 
        away_timezone, 
        ~format(
          with_tz(.x, tzone = .y), 
          "%Y-%m-%d %H:%M:%S %Z"
        )
      ),
      
      # home time hour:minute
      away_team_kickoff_time_hm = format(
        ymd_hms(away_team_kickoff_time_visual), 
        "%I:%M:%S %p"
      ),
      
      # calculate time difference between home and away team
      hour_time_difference = abs(
        as.numeric(
          difftime(
            ymd_hms(away_team_kickoff_time, tz = away_timezone), 
            ymd_hms(home_team_kickoff_time, tz = home_timezone), 
            units = "hours"
          )
        )
      )
    ) |> 
    # ungroup
    ungroup() |> 
    # select variables you want to consider
    select(
      -c(
        week,
        weather:away_team_kickoff_time_hm
      )
    )
}
