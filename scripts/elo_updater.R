library(sabRmetrics)
library(tidyverse)
library(dplyr)
library(readr)


#check current year as function
#if it fails, i.e. doesn't return the most recent year yet, then default back to last year until it does
get_schedule_for_current_season = function() {
  current_year = as.numeric(format(Sys.Date(), "%Y"))
  current_start = as.Date(paste0(current_year, "-01-01"))
  current_end = as.Date(paste0(current_year, "-12-31"))
  
  schedule = tryCatch({
    sched = extract_schedule(start_date = current_start, end_date = current_end)
    
    #has the season actually started? (i.e., any games played yet?)
    #making sure we're not accidently including anything before march
    if (nrow(sched) == 0 || max(sched$date, na.rm = TRUE) < current_start + 60) {
      #still before real games so treat as not ready
      stop("No games played this year yet")
    }
    
    return(sched)
  }, error = function(e) {
    message("No usable schedule for ", current_year, ": ", e$message) #for testing
    return(NULL)
  })
  
  return(schedule)
}

seasonal_schedule = get_schedule_for_current_season()

#pull elo rating csv. for checking inital csv
elo_rating = read_csv("data/elo_ratings.csv")
elo_rating

#hfa: home field advantage, k-factor initally 20, but we're adjusting based on how far we are in season.
update_elo_csv = function(latest_games, elo_csv_path = "data/elo_ratings.csv", k = 20, hfa = 30) {
  
  #making sure latest games is non 0
  if (nrow(latest_games) == 0) {
    message("No games provided — skipping Elo update.")
    #this is pretty neat i didn't know you could remove NULL using invisible from console
    return(invisible(NULL))
  }
  
  #load the current Elo ratings
  elo_df = read_csv(elo_csv_path, show_col_types = FALSE)
  
  #determine the date of the last Elo update
  #check this later, i think logically something is off, but not sure
  if (!"date" %in% names(elo_df)) {
    last_elo_date = min(latest_games$date, na.rm = TRUE) - 1
    latest_elos = elo_df[nrow(elo_df), ]
  } else {
    last_elo_date = max(elo_df$date, na.rm = TRUE)
    latest_elos = elo_df %>% filter(date == last_elo_date)
  }
  
  #set latest_game date
  latest_game_date = max(latest_games$date, na.rm = TRUE)
  
  #skip update if we're already up to date
  #in the scenario where there is no games played by any team on a certain date.
  if (latest_game_date <= last_elo_date) {
    message("Elo already up to date for ", latest_game_date, ". Skipping update.")
    return(invisible(NULL))
  }
  
  #initialize new_elos (reminder to self: new as in latest elo not 1500) based on the latest game date. 
  new_elos = as.list(latest_elos[1, ])
  new_elos$date = as.Date(latest_game_date)
  
  #update Elo ratings
  for (i in seq_len(nrow(latest_games))) {
    game = latest_games[i, ]
    
    #setting home, away ... you get the idea
    home = game$team_name_home
    away = game$team_name_away
    winner = game$winner
    loser = game$loser
    
    #pulling current ratings
    elo_home = as.numeric(new_elos[[home]])
    elo_away = as.numeric(new_elos[[away]])
    
    #adjust for HFA
    elo_home_adj = elo_home + hfa
    elo_away_adj = elo_away
    
    #expected scores
    expected_home = 1 / (1 + 10 ^ ((elo_away_adj - elo_home_adj) / 400))
    expected_away = 1 - expected_home
    
    #actual results
    score_home = ifelse(home == winner, 1, 0)
    score_away = 1 - score_home
    
    #update ratings
    new_elo_home = elo_home + k * (score_home - expected_home)  # Use dynamic k here
    new_elo_away = elo_away + k * (score_away - expected_away)
    
    #save ratings
    new_elos[[home]] = round(new_elo_home, 2)
    new_elos[[away]] = round(new_elo_away, 2)
  }
  
  #add new row and write csv 
  updated_elo_df = bind_rows(elo_df, new_elos)
  write_csv(updated_elo_df, elo_csv_path)
  
  #for testing purposes if the csv IS updated this will show otherwise other messages above
  message("Elo updated for ", nrow(latest_games), " games on ", latest_game_date)
}



#calculating how far we are in season for k-factor 
calculate_cumulative_games_played = function(latest_games) {
  total_games_played = nrow(latest_games)
  return(total_games_played)
}


adjust_k = function(latest_games, base_k = 20) {
  #count the total number of games played so far
  total_games_played = calculate_cumulative_games_played(latest_games)
  
  #calculate total possible games (i.e. if 30 teams played 162 games each)
  total_possible_games = 30 * 162 #reconsider this, maybe hard coding this isn't a good idea?
  
  #calculate the percentage of the season played
  season_progress = total_games_played / total_possible_games
  
  #adjust K based on the percentage of games played in 3 categories similar to how chess USCF does it 
  #note to self: apparently this is outdated for them.. reconsider if you have time? 
  if (season_progress < 0.3) {
    dynamic_k = base_k * 1.5  #K early in the season
  } else if (season_progress < 0.7) {
    dynamic_k = base_k       #K during mid-season
  } else {
    dynamic_k = base_k * 0.5  #K near the end of the season
  }
  return(dynamic_k)
}


if (is.null(seasonal_schedule)) {
  #should trigger if there are no seasonal data (we don't want the csv to update)
  message("No current season data available yet. Elo not updated.") #for testing 
  
  #if everything goes right, new data for games played, no repeat days, etc. then we update csv.
} else {
  wl_schedule = seasonal_schedule %>%
    #create winner and loser column, remove unnecessary columns
    mutate(winner = ifelse(score_away > score_home, team_name_away, team_name_home)) %>%
    mutate(loser = ifelse(winner == team_name_away, team_name_home, team_name_away)) %>%
    mutate(date = as.Date(date)) %>%
    select(date, team_id_away, team_name_away, team_id_home, team_name_home, winner, loser)
  
  latest_date = max(wl_schedule$date)
  latest_games = wl_schedule %>% filter(date == latest_date)
  
  dynamic_k = adjust_k(latest_games)
  update_elo_csv(latest_games, k = dynamic_k)
}
