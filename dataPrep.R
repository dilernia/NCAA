#################
# Title: Prepping data for NCAA Tournament
# Author: Andrew DiLernia
# Date: 12/17/2021
# Purpose: Prep data to help find optimal model for predicting NCAA Tournament games
#################

library(tidyverse)
library(dtplyr)
library(furrr)
library(progress)
library(mice)
library(ranger)
library(glmnet)
library(patchwork)
library(RColorBrewer)

t1 <- Sys.time()

# Script parameters: 
# womens: whether to create modelling data for Men's (womens = FALSE) or Womens Tourney (womens = TRUE)
# tourneyYear: year of tournament to forecast
# finalFit: creating design matrix for final tournament predictions (TRUE) or not (FALSE)
womens <- TRUE
tourneyYear <- 2024
finalFit <- FALSE

# Setting file paths
stageDir <- paste0(tourneyYear, "/march-machine-learning-mania-", tourneyYear, "/")

# Importing & Cleaning Box Score Data -------------------------------------

# Importing raw detailed box score data
regRaw <- read_csv(paste0(stageDir, paste0(ifelse(womens, "W", "M"), 
                                           "RegularSeasonDetailedResults.csv"))) |> 
  dplyr::mutate(tournament_game = FALSE)
tourneyRaw <- read_csv(paste0(stageDir, paste0(ifelse(womens, "W", "M"), "NCAATourneyDetailedResults.csv"))) |> 
  dplyr::mutate(tournament_game = TRUE)

# Importing conference data
confInfo <- read_csv(paste0(stageDir, paste0(ifelse(womens, "W", "M"), "TeamConferences.csv"))) |> 
  left_join(read_csv(paste0(stageDir, "Conferences.csv"))) |> 
  dplyr::rename(Conference = Description) |> 
  dplyr::select(Season, TeamID, Conference)

# Merging tournament and regular season data 
# and adding conference info, possession, and efficiency columns
# POSS calculation from https://thepowerrank.com/cbb-analytics/ and https://kenpom.com/blog/ratings-glossary/
# Rate statistics from Dean Oliver: https://www.basketball-reference.com/about/factors.html
# TADOO: Ideally a binary indicator for a game being a tourney game or not should
#        be included, along with an interaction of this with home-court status.
fullRaw <- bind_rows(regRaw, tourneyRaw) |> 
  dplyr::mutate(LLoc = case_when(WLoc == "H" ~ "A",
                          WLoc == "A" ~ "H",
                          WLoc == "N" ~ "N",)) |> 
  dplyr::select(Season:WTeamID, LTeamID, WScore, LScore, WLoc, LLoc, WFGM:LPF, tournament_game) |> 
  left_join(confInfo, by = c("Season" = "Season",
                             "WTeamID" = "TeamID")) |> 
  left_join(confInfo, suffix = c("W", "L"),
            by = c("Season" = "Season", "LTeamID" = "TeamID")) |> 
  dplyr::rename(WConference = ConferenceW, 
                LConference = ConferenceL) |>
  dplyr::arrange(Season, DayNum, WTeamID) |> 
  dplyr::mutate(WPoss = ((WFGA - WOR + WTO + (0.475 * WFTA)) + 
                         (LFGA - LOR + LTO + (0.475 * LFTA))) / 2,
         LPoss = WPoss, 
         GameID = row_number(), 
         Afix_mov = WScore - LScore,
         WScore_Eff = WScore / WPoss,
         LScore_Eff = LScore / LPoss,
         WMargin_Eff = Afix_mov / WPoss,
         LMargin_Eff = Afix_mov / LPoss,
         WPyth = (WScore^11.5) / (WScore^11.5 + LScore^11.5),
         LPyth = (LScore^11.5) / (WScore^11.5 + LScore^11.5),
         WeFG_perc = (WFGM + 0.5 * WFGM3) / WFGA,
         LeFG_perc = (LFGM + 0.5 * LFGM3) / LFGA,
         WAst_rate = WAst / WFGM,
         LAst_rate = LAst / LFGM,
         WTO_rate = WTO / (WTO + WFGA + (0.475 * WFTA)), # turnover rate = turnovers / (possessions)
         LTO_rate = LTO / (LTO + LFGA + (0.475 * LFTA)),
         WOR_rate = WOR / (WOR + LDR), # OR rate = OR / (own shots missed)
         LOR_rate = LOR / (LOR + WDR),
         Wwin_rate = Afix_mov > 0,
         Lwin_rate = Afix_mov < 0,
         WFT_factor = WFTM / WFGA,
         LFT_factor = LFTM / LFGA,
         WLuck = Wwin_rate - WPyth,
         LLuck = Lwin_rate - LPyth)

# Adding game location information -------------------------------------
if(FALSE) {
# Importing spatial information for cities from Google: 
# https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_basketball_arenas
# https://www.google.com/maps/d/u/0/viewer?mid=1JaZgEuzxZ5Sn2UjtA1vTIstU8Fjfl8MM&ll=39.374932225470644%2C-95.83631723748385&z=5
cities_info <- sf::st_read("NCAA D1 Basketball Map 2023-24.kml") |> 
  dplyr::mutate(school = str_squish((str_to_lower(Name)))) |> 
  full_join(read_csv(paste0(stageDir, paste0(ifelse(womens, "W", "M"), 
                                             "TeamSpellings.csv"))), by = c("school" = "TeamNameSpelling")) |> 
  dplyr::mutate(longitude = sf::st_coordinates(geometry)[, 1],
                latitude = sf::st_coordinates(geometry)[, 2]) |> 
  distinct(TeamID, .keep_all = TRUE) |> 
  sf::st_drop_geometry() |> 
  as_tibble() |> 
  dplyr::select(TeamID:latitude)

# Determining home cities for each team for each season
home_cities <- fullRaw |> 
  left_join(game_cities) |> 
  dplyr::select(Season, WTeamID, LTeamID, WLoc, LLoc, city_id) |> 
  pivot_longer(cols = c(WTeamID, LTeamID), names_to = "winner", values_to = "team_id") |> 
  dplyr::mutate(winner = str_detect(winner, pattern = "W"),
                location = case_when(winner == TRUE ~ WLoc,
                                     winner == FALSE ~ LLoc,
                                     TRUE ~ NA)) |> 
  dplyr::select(-WLoc, -LLoc) |> 
  dplyr::filter(location == "H") |> 
  group_by(Season, team_id) |> 
  dplyr::count(city_id) |> 
  slice_max(n, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  dplyr::select(-n) |> 
  dplyr::rename(home_city_id = city_id) |> 
  left_join(cities_info, by = c("team_id" = "TeamID"))

# Importing city ID info
game_cities <- read_csv(paste0(stageDir, paste0(ifelse(womens, "W", "M"), 
                                                "GameCities.csv"))) |> 
  dplyr::select(-CRType) |> 
  dplyr::rename(city_id = CityID) |> 
  left_join(home_cities, by = c("Season" = "Season", "city_id" = "home_city_id"))
}

# Creating duplicate rows to be agnostic to team winning or not
# and allow calculating rolling statistics
fullRaw1 <- fullRaw |> 
  rename_with(.fn =  ~ gsub("_L", "B_", paste0("_", .x),
                            fixed = TRUE), .cols = starts_with("L")) |> 
  rename_with(.fn =  ~ gsub("_W", "A_", paste0("_", .x),
                            fixed = TRUE), .cols = starts_with("W")) |>
  dplyr::mutate(Afix_win = TRUE) |> 
  rename_with(.fn =  ~ gsub("A_", "Afix_", .x,
                            fixed = TRUE), .cols = c(A_TeamID, A_Loc, A_Conference)) |> 
  rename_with(.fn =  ~ gsub("B_", "Bfix_", .x,
                            fixed = TRUE), .cols = c(B_TeamID, B_Loc, B_Conference)) |> 
  dplyr::arrange(GameID)

fullRaw2 <- fullRaw |> 
  rename_with(.fn =  ~ gsub("_W", "B_", paste0("_", .x),
                            fixed = TRUE), .cols = starts_with("W")) |> 
  rename_with(.fn =  ~ gsub("_L", "A_", paste0("_", .x),
                            fixed = TRUE), .cols = starts_with("L")) |>
  dplyr::mutate(Afix_win = FALSE, Afix_mov = -Afix_mov) |> 
  rename_with(.fn =  ~ gsub("A_", "Afix_", .x,
                            fixed = TRUE), .cols = c(A_TeamID, A_Loc, A_Conference)) |> 
  rename_with(.fn =  ~ gsub("B_", "Bfix_", .x,
                            fixed = TRUE), .cols = c(B_TeamID, B_Loc, B_Conference)) |> 
  dplyr::arrange(GameID)

# Adding in games played for each team to facilitate decay in ELO & AP preseason ranks
game_counts <- bind_rows(fullRaw1, fullRaw2) |> 
  group_by(Season, Afix_TeamID) |> 
  dplyr::arrange(DayNum) |> 
  dplyr::mutate(Afix_count = row_number()) |> 
  dplyr::select(Season, DayNum, GameID, Afix_TeamID, Afix_count) |> 
  ungroup() |> 
  full_join(bind_rows(fullRaw1, fullRaw2) |> 
              group_by(Season, Bfix_TeamID) |> 
              dplyr::arrange(DayNum) |> 
              dplyr::mutate(Bfix_count = row_number()) |> 
              dplyr::select(Season, DayNum, GameID, Bfix_TeamID, Bfix_count) |> 
              ungroup())

# Elo Ratings -------------------------------------------------------------

# Calculating ELO ratings: https://www.kaggle.com/lpkirwin/fivethirtyeight-elo-ratings
# Wikipedia elo: https://en.wikipedia.org/wiki/Elo_rating_system

# Parameters to be optimized: k, homeAdv
# Initial values from 538's values for NBA elo ratings:
# https://fivethirtyeight.com/features/how-we-calculate-nba-elo-ratings/
# I found k = 45 to be good, this Python user found 43 for college BBall:
# https://github.com/grdavis/college-basketball-elo
# They also found home advantage parameter optimally to be 81.

# Function for calculating game prediction from elo values home: -1 for away, 0 for neutral, 1 for home
elo_pred <- function(elo1, elo2, homeAdv = 81, home = 0) {
  return(1 / (1 + 10^((elo2 - (elo1 + home*homeAdv)) / 400)))
}

# Function for calculating updated elo values
elo_update <- function(elo, pred, actual, k = 45) {
  return(elo + k*(actual - pred))
}

# Function to reset team elos to average of previous season and conference average
elo_reset <- function(oldElos, teamIDs, center = mean, 
                        confer = confInfo) {
  
  # Conference average elos
  conf <- confer |> 
    dplyr::filter(Season == oldElos$Season[1])
  suppressMessages(confAvg <- oldElos |> 
                     left_join(conf, by = c("team" = "TeamID")) |> 
                     group_by(Conference) |> 
                     dplyr::summarize(eloAvg = center(elo)))
  
  # End of season elos
  endElos <- oldElos |> 
    dplyr::rename(TeamID = team, eloEnd = elo)
  
  # Averaging end of season and conference average elos
  suppressMessages(newElos <- endElos |> left_join(confAvg |> 
                                                      left_join(confer |> dplyr::filter(Season == oldElos$Season[1]))) |> 
                     dplyr::mutate(elo =  (eloEnd + eloAvg) / 2) |> 
                     dplyr::select(TeamID, elo) |> 
                     dplyr::filter(TeamID %in% teamIDs) |> 
                     right_join(data.frame(TeamID = teamIDs)) |> 
                     dplyr::mutate(elo = ifelse(is.na(elo), 1500, elo)))
  
  return(newElos)
}

# Function for adding elo values to data frame
# scores: A data frame with columns Season, Afix_TeamID,
# Bfix_TeamID, Afix_Loc, Afix_mov, Afix_win
# k_value: Smoothing parameter for elo calculation
# method: One of "NBA" or "NFL". Specifies smoothing method in elo update.
# elo_starts: Optional. Scalar or vector of starting elo values for unique TeamID's
# tau: Second smoothing parameter for elo calculation
add_elo <- function(scores, method = "NFL", 
                   k_value = 45, elo_starts = 1500, tau = 0.006, home_advantage = 81,
                   center_fun = mean, returnRecent = FALSE) {
  
  # Sorting rows to start
  scores <- scores |> dplyr::arrange(Season, DayNum)
  
  seasons <- unique(scores$Season)
  n_seasons <- length(seasons)
  output <- vector("list", n_seasons)
  
  pb <- progress_bar$new(total = n_seasons)
  
  for(s in 1:n_seasons) {
    seasonData <- scores |> dplyr::filter(Season == seasons[s])
    teamIDs <- unique(c(seasonData$Afix_TeamID, 
                        seasonData$Bfix_TeamID))
    
    homes <- ifelse(seasonData$Afix_Loc == "N", 0,
                    ifelse(seasonData$Afix_Loc == "A", -1,
                           ifelse(seasonData$Afix_Loc == "H", 1, NA)))
    
    mmNumerator <- sqrt(seasonData$Afix_mov)
    Afix_win10 <- ifelse(seasonData$Afix_win, 1, 0)
    
    elos <- data.frame(team = teamIDs, elo = 1500)
    
    # Initialize elo columns
    seasonData <- seasonData |> 
      mutate(Afix_elo = 1500, Bfix_elo = 1500)
    elos <- data.frame(team = teamIDs, elo = elo_starts)
    
    if(method == "NBA") {
      for(i in 1:nrow(seasonData)) {
        # Storing current elo values
        Ainds <- elos$team == seasonData$Afix_TeamID[i]
        Binds <- elos$team == seasonData$Bfix_TeamID[i]
        seasonData$Afix_elo[i] <- elos$elo[Ainds]
        seasonData$Bfix_elo[i] <- elos$elo[Binds]
        
        # Elo prediction
        pred <- elo_pred(elo1 = seasonData$Afix_elo[i],
                        elo2 = seasonData$Bfix_elo[i], 
                        home = homes[i], homeAdv = home_advantage)
        
        # Margin of victory multipliers
        movMultiA <- mmNumerator[i] /
          (7.5 + tau*(seasonData$Afix_elo[i] - seasonData$Bfix_elo[i]))
        
        # Calculating new elo values
        newA <- elo_update(elo = seasonData$Afix_elo[i], pred = pred,
                          actual = Afix_win10[i], k = movMultiA*k_value)
        newB <- seasonData$Bfix_elo[i] - (newA - seasonData$Afix_elo[i])
        
        # Updating elo values
        elos$elo[Ainds] <- newA
        elos$elo[Binds] <- newB
      }
    } else if(method == "NFL") {
      
      ks <- k_value*log(1 + abs(seasonData$Afix_mov))
      
      for(i in 1:nrow(seasonData)) {
        # Storing current elo values
        Ainds <- elos$team == seasonData$Afix_TeamID[i]
        Binds <- elos$team == seasonData$Bfix_TeamID[i]
        seasonData$Afix_elo[i] <- elos$elo[Ainds]
        seasonData$Bfix_elo[i] <- elos$elo[Binds]
        
        # Elo prediction
        pred <- elo_pred(elo1 = seasonData$Afix_elo[i],
                        elo2 = seasonData$Bfix_elo[i], 
                        home = homes[i], homeAdv = home_advantage)
        
        # Calculating new elo values
        newA <- elo_update(elo = seasonData$Afix_elo[i], pred = pred,
                          actual = Afix_win10[i], k = ks[i])
        newB <- seasonData$Bfix_elo[i] - (newA - seasonData$Afix_elo[i])
        
        # Updating elo values
        elos$elo[Ainds] <- newA
        elos$elo[Binds] <- newB
      }
    }
    
    output[[s]] <- seasonData
    
    if(s < n_seasons) {
      newSeason <- scores |> 
        dplyr::filter(Season == seasons[s+1])
      newTeams <- unique(c(newSeason$Afix_TeamID, newSeason$Bfix_TeamID))
      
      newElos <- elo_reset(oldElos = elos |> mutate(Season = seasonData$Season[1]),
                             teamIDs = newTeams, center = center_fun)
      
      elo_starts <- newElos |> pull(elo)
    }
    
    # Progress bar update
    pb$tick()
  }
  if(returnRecent == FALSE) {
    return(bind_rows(output))
  } else {
    return(list(eloData = bind_rows(output), newElos = elos))
  }
}

runSim <- FALSE

# Function for searching for optimal Elo parameters
elo_sim <- function(method, k_value = 45, tau, center_fun) {
  
  if(center_fun == "mean") {
    center_fun <- mean
  } else if(center_fun == "median") {
    center_fun <- median
  }
  
  # Calculating elo values across all seasons
  fullElo1 <- add_elo(scores = fullRaw1, 
                     elo_starts = 1500, 
                     method = method, 
                     k_value = k_value, 
                     tau = tau,
                     center_fun = center_fun)
  
  # Full performance of calculated elo scores for MOV
  eloMod1 <- lm(Afix_mov ~ 0 + Afix_elo + Bfix_elo, 
                data = fullElo1)
  r2Val <- summary(eloMod1)$r.squared
  
  # Later in season performance of calculated elo scores for MOV
  eloModNew <- lm(Afix_mov ~ 0 + Afix_elo + Bfix_elo, 
                  data = fullElo1 |> 
                    dplyr::filter(DayNum >= 50))
  r2ValNew <- summary(eloModNew)$r.squared
  
  # Binary win / loss response
  eloShuffled <- fullElo1 |> 
    mutate(Afix_win2 = ifelse(Afix_mov %% 2 == 0, FALSE, TRUE),
           Afix_elo2 = ifelse(Afix_mov %% 2 == 0, Bfix_elo, Afix_elo),
           Bfix_elo2 = ifelse(Afix_mov %% 2 == 0, Afix_elo, Bfix_elo)) |> 
    dplyr::select(DayNum, Afix_win2, Afix_elo2, Bfix_elo2)
  
  # Full AIC
  eloMod2 <- glm(Afix_win2 ~ 0 + Afix_elo2 + Bfix_elo2, 
                 data = eloShuffled, family = "binomial")
  logisticAIC <- summary(eloMod2)$aic
  
  # Later in season AIC
  eloMod2New <- glm(Afix_win2 ~ 0 + Afix_elo2 + Bfix_elo2, 
                    data = eloShuffled |> 
                      dplyr::filter(DayNum >= 50), family = "binomial")
  logisticAICNew <- summary(eloMod2New)$aic
  
  return(data.frame(method = method, k_value = k_value, tau = tau,
                    center_fun = center_fun, r2 = r2Val, r2ValNew = r2ValNew,
                    AIC = logisticAIC, logisticAICNew = logisticAICNew))
}

if(runSim == TRUE) {
  
  # Grid of tuning arameters
  params <- expand.grid(k_value = seq(35, 60, by = 5),
                        tau = 0.002,
                        method = c("NFL"),
                        center_fun = "mean")
  
  # Finding optimal tuning parameters
  plan(multisession, workers = 3)
  eloSimRes <- future_pmap_dfr(.l = params, .f = elo_sim, .progress = TRUE)
  
  # Saving results
  saveRDS(eloSimRes, paste0(tourneyYear, ifelse(womens, "-Womens/", "/"), "eloSimRes.rds"))
  eloSimRes <- readRDS(paste0(tourneyYear, ifelse(womens, "-Womens/", "/"), "eloSimRes.rds"))
  
  # Plotting results
  eloSimRes |> pivot_longer(cols = r2:logisticAICNew, values_to = "Value",
                             names_to = "Metric") |> 
    dplyr::filter(center_fun == "mean") |> 
    ggplot(aes(x = k_value, y = Value, color = tau)) + 
    geom_point() + facet_grid(Metric ~ method, scales = "free_y")
  
  # Table of optimal values
  eloSimRes |> pivot_longer(cols = r2:logisticAICNew, values_to = "Value",
                             names_to = "Metric") |> 
    mutate(Value = ifelse(Metric %in% c("r2", "r2ValNew"), Value, -Value)) |> 
    arrange(k_value) |> group_by(center_fun, method, Metric) |> slice_max(order_by = Value, n = 1, with_ties = FALSE)
}

# scores = fullRaw1; method = "NFL"; k_value = 45;
# elo_starts = 1500; center_fun = mean;
# home_advantage = 81; returnRecent = TRUE

# Calculating elo values across all seasons
eloRes <- add_elo(scores = fullRaw1, method = "NFL", k_value = 45,
                 elo_starts = 1500, center_fun = mean,
                 home_advantage = 81, returnRecent = TRUE)

fullElo1 <- eloRes$eloData

# Performance of calculated elo scores
eloMod1 <- lm(Afix_mov ~ 0 + Afix_elo + Bfix_elo, 
              data = fullElo1)
r2Val <- summary(eloMod1)$r.squared

# Binary win / loss response
eloShuffled <- fullElo1 |> 
  mutate(Afix_win2 = ifelse(Afix_mov %% 2 == 0, FALSE, TRUE),
         Afix_elo2 = ifelse(Afix_mov %% 2 == 0, Bfix_elo, Afix_elo),
         Bfix_elo2 = ifelse(Afix_mov %% 2 == 0, Afix_elo, Bfix_elo)) |> 
  dplyr::select(Afix_win2, Afix_elo2, Bfix_elo2)

eloMod2 <- glm(Afix_win2 ~ 0 + Afix_elo2 + Bfix_elo2, 
               data = eloShuffled, family = "binomial")

logisticAIC <- summary(eloMod2)$aic

# Fixed elos drifting higher using Nate Silver's 
# advice of reducing MOV multiplier for heavier favorites: https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/
fullElo1 |> group_by(Season) |> 
  summarize(Avg = mean(Afix_elo), Min = min(Afix_elo), Max = max(Afix_elo)) |> 
  pivot_longer(cols = Avg:Max, names_to = "Metric", values_to = "Value") |> 
  ggplot(aes(x = Season, y = Value, color = Metric)) + 
  geom_line(aes(x = Season)) + labs(title = "Elo Across Seasons") + 
  theme_bw()

# Adding in team name
teamNames <- read_csv(paste0(stageDir, ifelse(womens, "W", "M"), "Teams.csv")) |> 
  dplyr::select(TeamID, TeamName)

# Visualizing elo across season
if(FALSE) {
  eloSpaghetti <- fullElo1 |> 
    dplyr::filter(Season > 2015) |> 
    left_join(teamNames, by = c("Afix_TeamID" = "TeamID")) |> 
    dplyr::mutate(Afix_elo = round(Afix_elo)) |> 
    dplyr::rename(Team = TeamName, Day = DayNum, ELO = Afix_elo) |> 
    ggplot(aes(y = ELO, x = Day, color = Team)) + 
    geom_line() + facet_grid(rows = vars(Season)) +
    labs(title = "Team Elos by Season", y = "Elo", x = "Day of Season") + theme_bw() +
    theme(legend.position = "none") 
  
  eloSpaghetti
  
  plotly::ggplotly(eloSpaghetti)
}

# Rolling Team Statistics -------------------------------------------------

fullElo2 <- fullRaw2 |> 
  dplyr::mutate(Afix_elo = fullElo1$Bfix_elo,
                Bfix_elo = fullElo1$Afix_elo)

# Combine into single data set with 2 rows for each game; 
# 1 where loser is Team A and 1 where loser is Team B
longElo <- bind_rows(fullElo1, fullElo2) |> 
  group_by(Season, Afix_TeamID) |> 
  arrange(DayNum) |> 
  dplyr::mutate(A_elo_against = Bfix_elo,
                B_elo_against = Afix_elo) |> 
  dplyr::mutate(Afix_count = row_number(), 
                across(.cols = starts_with(c("A_", "B_")),
                       .fns = ~ cummean(.x))) |> 
  dplyr::select(Season, DayNum, GameID, Afix_count, tournament_game, starts_with(c("Afix_", "Bfix_")),
                starts_with(c("A_", "B_")), -Bfix_Loc)

# Storing data without lagging to use for final predictions.
# Only last row of each team for current season is actually needed,
# but all rows are kept to use the same code / functions as for the training data
long_ELO_final <- longElo |>
  ungroup() |> 
  group_by(Season, Bfix_TeamID) |> 
  dplyr::mutate(Bfix_count = row_number()) |> 
  ungroup()

# Number of games by season for each team & last_day_* variables
n_games <- long_ELO_final |> 
  group_by(Season, Afix_TeamID) |> 
  summarize(last_day = max(DayNum)) |> 
  ungroup() |> 
  dplyr::rename(team_id = Afix_TeamID)

# Make last game ELO values equal to most recent ELO value for final season if final fit
long_ELO_final <- long_ELO_final |> 
    dplyr::filter(Season == tourneyYear) |> 
    group_by(Afix_TeamID) |> 
    slice_max(DayNum, n = 1) |> 
    ungroup() |> 
    dplyr::select(-Afix_elo, -Bfix_elo) |> 
    left_join(n_games, by = c("Season" = "Season",
                              "Afix_TeamID" = "team_id")) |> 
    left_join(n_games, by = c("Season" = "Season",
                              "Bfix_TeamID" = "team_id"),
              suffix = c("_A", "_B")) |> 
    left_join(eloRes$newElos, by = c("Afix_TeamID" = "team")) |> 
    left_join(eloRes$newElos, by = c("Bfix_TeamID" = "team")) |> 
    dplyr::rename(Afix_elo_final = `elo.x`, Bfix_elo_final = `elo.y`) |> 
    dplyr::select(Season:GameID, Afix_TeamID, Bfix_TeamID, Afix_elo_final, Bfix_elo_final, last_day_A, last_day_B) |> 
  right_join(long_ELO_final, 
             by = c('Season', 'DayNum', 'GameID', 'Afix_TeamID', 'Bfix_TeamID')) |> 
  dplyr::mutate(Afix_elo = case_when(Season == tourneyYear & DayNum == last_day_A ~ Afix_elo_final,
                                     TRUE ~ Afix_elo),
                Bfix_elo = case_when(Season == tourneyYear & DayNum == last_day_B ~ Bfix_elo_final,
                                     TRUE ~ Bfix_elo)) |> 
  dplyr::select(-Afix_elo_final, -Bfix_elo_final, -last_day_A, -last_day_B)

# Lag information by 1 game to prevent "data leakage" if not final fit and 
# adding game count for team B
# Should the lagging be done at end right before model fitting??
long_ELO <- longElo |> 
    dplyr::mutate(dplyr::across(starts_with(c("A_", "B_")), ~ dplyr::lag(.x, n = 1))) |> 
  ungroup() |> 
  group_by(Season, Bfix_TeamID) |> 
  dplyr::mutate(Bfix_count = row_number()) |> 
  ungroup()

# Function to include both 'for' and 'against' info for each game, rather than just 'for'
# perspective: should the reference team be considered the winner (A_mov > 0) or the loser (A_mov < 0)?
make_for_and_against <- function(myData, perspective = "winner") {
  ret <- myData |> 
    rename_with(.fn = ~ gsub("A_", "A_for_", .x,
                             fixed = TRUE), .cols = starts_with("A_")) |> 
    rename_with(.fn = ~ gsub("B_", "A_against_", .x,
                             fixed = TRUE), .cols = starts_with("B_")) |> 
    dplyr::arrange(GameID) |> 
    dplyr::mutate(Ref = ifelse(sign(Afix_mov) == ifelse(perspective == "winner", 1, -1), "-A", "-B")) |> 
    pivot_wider(id_cols = c(Season, DayNum, GameID), 
                values_from = starts_with(c("A_", "B_")), 
                names_from = Ref) |> 
    rename_with(.fn =  ~ gsub("_-A", "", .x, fixed = TRUE), .cols = ends_with("_-A")) |> 
    rename_with(.fn =  ~ gsub("A_", "B_", gsub("_-B", "", .x, fixed = TRUE), fixed = TRUE),
                .cols = ends_with("_-B")) |> 
    left_join(long_ELO |> 
                dplyr::filter(sign(Afix_mov) == ifelse(perspective == "winner", 1, -1)) |> 
                dplyr::select(-starts_with(c("A_", "B_")))) |> 
    dplyr::select(Season, DayNum, GameID, starts_with(c("Afix_", "Bfix_")), everything())
  
  return(ret)
}

# Including both 'for' and 'against' for modeling data from winner's perspective
wide_ELO <- make_for_and_against(long_ELO, perspective = "winner")

# Including both 'for' and 'against' for final predictions data
wide_ELO_final <- bind_rows(make_for_and_against(long_ELO_final, perspective = "winner"),
                            make_for_and_against(long_ELO_final, perspective = "loser"))

t2 <- Sys.time()

t2 - t1

# Add Pythagorean Winning Percentage --------------------------------------

# Function to add Pythagorean Winning Percentage and Luck
# Formula from Ken Pom: https://kenpom.com/blog/ratings-glossary/
# Ken Pom uses 11.5 for the exponent, but there is not one correct value as others use 13.91.
# Found when using scoring *efficiency* that exp_value of 1.5 seems best
# Also adding in Luck: difference between actual winning % & Pythagorean expected winning %
add_pyth_win_perc <- function(myData, exp_value = 1.5) {
  ret <- myData |> 
    dplyr::mutate(Afix_Pyth = (A_for_Score_Eff^exp_value) / (A_for_Score_Eff^exp_value + A_against_Score_Eff^exp_value),
                  Bfix_Pyth = (B_for_Score_Eff^exp_value) / (B_for_Score_Eff^exp_value + B_against_Score_Eff^exp_value),
                  Afix_Luck = A_for_win_rate - Afix_Pyth,
                  Bfix_Luck = B_for_win_rate - Bfix_Pyth)
  
  return(ret)
}

wide_ELO <- add_pyth_win_perc(wide_ELO)
wide_ELO_final <- add_pyth_win_perc(wide_ELO_final)

# Determining optimal exp_value
if(FALSE) {
  
pyth_exp_finder <- function(value) {
  pyth_elo <- wide_ELO |> 
    dplyr::mutate(Afix_Pyth = (A_for_Score^exp_value) / (A_for_Score^exp_value + A_against_Score^exp_value),
                  Bfix_Pyth = (B_for_Score^exp_value) / (B_for_Score^exp_value + B_against_Score^exp_value),
                  Afix_Luck = A_for_win_rate - Afix_Pyth,
                  Bfix_Luck = B_for_win_rate - Bfix_Pyth)
  
  ret_r2 <- summary(lm(Afix_mov ~ 0 + Afix_Pyth + Bfix_Pyth + Afix_Luck + Bfix_Luck, 
                data = pyth_elo))$r.squared
  return(ret_r2)
}

library(tidymodels)

pyth_exp_finder_new <- function(value) {
  pyth_elo <- wide_ELO |> 
    dplyr::mutate(Afix_Pyth = (A_for_Score^exp_value) / (A_for_Score^exp_value + A_against_Score^exp_value),
                  Bfix_Pyth = (B_for_Score^exp_value) / (B_for_Score^exp_value + B_against_Score^exp_value),
                  Afix_Luck = A_for_win_rate - Afix_Pyth,
                  Bfix_Luck = B_for_win_rate - Bfix_Pyth)
  
  # Define a linear regression model
  lm_model <- linear_reg() |>
    set_engine("lm")
  
  # Create a recipe
  rec <- recipe(Afix_mov ~ 0 + Afix_Pyth + Bfix_Pyth + Afix_Luck + Bfix_Luck, data = pyth_elo)
  
  # Set up the workflow
  wf <- workflow() |>
    add_recipe(rec) |>
    add_model(lm_model)
  
  # Perform LOOCV
  cv_results <- fit_resamples(wf, resamples = vfold_cv(pyth_elo, v = 20)) |> 
    collect_metrics() |> 
    dplyr::filter(.metric == "rsq") |> 
    dplyr::pull(mean)
  
  return(cv_results)
}

cand_exps <- seq(2, 30, by = 2)
r2s <- map_dbl(.x = cand_exps, .f = pyth_exp_finder)
r2_cvs <- map_dbl(.x = cand_exps, .f = pyth_exp_finder_new)

# Plotting candidate exponential values performances
tibble(Value = rep(cand_exps, times = 2), 
       R2 = c(r2s, r2_cvs),
       Method = rep(c("Standard", "CV"), each = length(cand_exps))) |> 
  ggplot(aes(x = Value, y = R2, color = Method)) +
  geom_line() +
  geom_point(color = "black") +
  facet_wrap(~ Method, ncol = 2, scales = "free_y") + 
  labs(title = expression("R"^2*" by exponential values for Pythagorean Winning Percentage"),
       y = expression("R"^2*""),
       x = "Exponential value") +
  ggthemes::theme_few() +
  theme(legend.position = "none")
}

# Offensive & Defensive Efficiency ----------------------------------------

# Future idea: Implement Jeff Sagarin’s WIN50 method for strength of schedule ratings

# Efficiency discussion from Ken Pom: https://kenpom.com/blog/ratings-methodology-update/
# Explore adjusted efficiencies on offense and defense using principles from Dean Oliver: http://www.rawbw.com/~deano/articles/kalman.html

# Another explanation of Ken Pom metrics: https://kenpom.com/blog/ratings-explanation/

# KenPom 101: What the college basketball metric system is and how it ranks Michigan: https://www.maizenbrew.com/2019/10/23/20928669/kenpom-explained-what-it-means-michigan-basketball-ranking

if(FALSE) {
  
  # Function for calculating game prediction from metric values home: -1 for away, 0 for neutral, 1 for home
  metricPred <- function(mOff, mDef, homeAdv = 0.1, home = 0) {
    return((mDef + mOff) / 2 + home*homeAdv)
  }
  
  # Function for calculating updated metric values
  metricUpdate <- function(metric, pred, actual, k = 0.5) {
    return(metric + k*(actual - pred))
  }
  
  # Logistic-based function for smoothing adjusted metric change across season
  # steepness: Steepness of inverse logit function for weights
  # kfloor: controls the lower asymptote of the inverse logit function
  # ngames: number of games to 'standardize' average of Team A and Team B's game number by
  smoothFun <- function(x, steepness = 2, kfloor = 0.10, ngames = 30) {
    x <- (x - mean(1:ngames)) / sd(1:ngames)
    (exp(-steepness*(x - 0.50)) / (1 + exp(-steepness*(x - 0.50))) + kfloor) / (1 + kfloor)
  }
  
  # Function for calculating game prediction from metric values home: -1 for away, 0 for neutral, 1 for home
  metricPred_old <- function(mOff, mDef, homeAdv = 0.1, home = 0) {
    return((mDef + mOff) / 2 + home*homeAdv)
  }
  
  # Function for calculating updated metric values
  metricUpdate_old <- function(metric, pred, actual, k = 0.5) {
    return(metric + k*(actual - pred))
  }
  
  # Predicted probability of winning
  metricPredProb <- function(m1, m2, homeAdv = 0.03, home = 0, steep = 12) {
    eVal <- exp(steep*((aOff - bDef) - (bOff - aDef) + home*homeAdv))
    return(eVal / (1 + eVal))
  }
  
  # Function to reset team metrics to average of previous season and conference average
  metricSeasonReset <- function(oldMets, teamIDs, center = mean, 
                                confer = confInfo, 
                                mStart = data.frame(adjMetricOff = 1, adjMetricDef = 1)) {
    
    # Conference average metrics
    conf <- confer |> 
      dplyr::filter(Season == oldMets$Season[1])
    suppressMessages(confAvg <- oldMets |> 
                       left_join(conf, by = c("team" = "TeamID",
                                              "Season" = "Season")) |> 
                       group_by(Conference) |> 
                       summarize(adjMetricOffAvg = center(adjMetricOff),
                                 adjMetricDefAvg = center(adjMetricDef)))
    
    # End of season metrics
    endMets <- oldMets |> rename(TeamID = team, 
                                  adjMetricOffEnd = adjMetricOff,
                                  adjMetricDefEnd = adjMetricDef)
    
    # Averaging end of season and conference average metrics
    suppressMessages(newMets <- endMets |> left_join(confAvg |> 
                                                        left_join(confer |> 
                                                                    dplyr::filter(Season == oldMets$Season[1]))) |> 
                       dplyr::mutate(adjMetricOff = (adjMetricOffEnd + adjMetricOffAvg) / 2,
                              adjMetricDef = (adjMetricDefEnd + adjMetricDefAvg) / 2) |> 
                       dplyr::select(TeamID, adjMetricOff, adjMetricDef) |>
                       dplyr::filter(TeamID %in% teamIDs) |> 
                       right_join(data.frame(TeamID = teamIDs)) |> 
                       dplyr::mutate(adjMetricOff = ifelse(is.na(adjMetricOff), mStart$adjMetricOff, adjMetricOff),
                              adjMetricDef = ifelse(is.na(adjMetricDef), mStart$adjMetricDef, adjMetricDef)))
    
    return(newMets)
  }
  
  # Function for adding opponent-adjusted metric values to data frame
  # boxscores: A data frame with columns Season, DayNum, Afix_TeamID,
  # Bfix_TeamID, Afix_Loc, A_metric, B_metric
  # steep: Smoothing parameter for metric calculation
  # k_value: Second smoothing parameter for metric calculation
  # mStarts: Optional. Data frame with columns 
  # offensive (adjMetricOff) and defensive (adjMetricDef) initial adjusted metric values for unique TeamID's
  # tau: Second smoothing parameter for metric calculation
  addMetric <- function(boxscores, 
                        steepness = 2, 
                        kfloor = 0.20, 
                        startOff = 1, 
                        startDef = 1, 
                        home_advantage = 0.03,
                        center_fun = mean, returnRecent = FALSE) {
    
    # Sorting rows to start
    boxscores <- boxscores |> dplyr::arrange(Season, DayNum)
    
    seasons <- unique(boxscores$Season)
    n_seasons <- length(seasons)
    output <- vector("list", n_seasons)
    
    newTeams <- unique(c(dplyr::pull(dplyr::filter(boxscores, Season == seasons[1]), Afix_TeamID), 
                         dplyr::pull(dplyr::filter(boxscores, Season == seasons[1]), Bfix_TeamID)))
    
    # Starting values
    mStarts <- data.frame(TeamID = newTeams,
                          adjMetricOff = startOff, adjMetricDef = startDef)
    
    pb <- progress::progress_bar$new(total = n_seasons)
    
    for(s in 1:n_seasons) {
      seasonData <- boxscores |> dplyr::filter(Season == seasons[s])
      
      homes <- ifelse(seasonData$Afix_Loc == "N", 0,
                      ifelse(seasonData$Afix_Loc == "A", -1,
                             ifelse(seasonData$Afix_Loc == "H", 1, NA)))
      
      mets <- data.frame(team = newTeams, adjMetricOff = mStarts$adjMetricOff, 
                         adjMetricDef = mStarts$adjMetricDef)
      
      # Initialize metric columns
      seasonData <- seasonData |> 
        dplyr::left_join(mStarts, by = c("Afix_TeamID" = "TeamID")) |> 
        dplyr::rename(A_adjMetricOff = adjMetricOff,
                      A_adjMetricDef = adjMetricDef) |> 
        dplyr::left_join(mStarts, by = c("Bfix_TeamID" = "TeamID")) |> 
        dplyr::rename(B_adjMetricOff = adjMetricOff,
                      B_adjMetricDef = adjMetricDef) 
      
      seasonData <- seasonData |> 
        dplyr::mutate(k = smoothFun(x = (Afix_count + Bfix_count) / 2, 
                                    steepness = steepness, kfloor = kfloor))
      
      for(i in 1:nrow(seasonData)) {
        # Storing current metric values
        Ainds <- mets$team == seasonData$Afix_TeamID[i]
        Binds <- mets$team == seasonData$Bfix_TeamID[i]
        seasonData$A_adjMetricOff[i] <- mets$adjMetricOff[Ainds]
        seasonData$B_adjMetricDef[i] <- mets$adjMetricDef[Binds]
        
        # Metric prediction
        predA <- metricPred(mOff = seasonData$A_adjMetricOff[i],
                            mDef = seasonData$B_adjMetricDef[i], 
                            home = homes[i], 
                            homeAdv = home_advantage)
        
        predB <- metricPred(mOff = seasonData$B_adjMetricOff[i],
                            mDef = seasonData$A_adjMetricDef[i], 
                            home = homes[i]*(-1), 
                            homeAdv = home_advantage)
        
        # Calculating new metric values
        newAOff <- metricUpdate(metric = seasonData$A_adjMetricOff[i], pred = predA,
                                actual = seasonData$A_metric[i], k = seasonData$k[i])
        newBDef <- seasonData$B_adjMetricDef[i] - (seasonData$A_adjMetricOff[i] - newAOff)
        
        newBOff <- metricUpdate(metric = seasonData$B_adjMetricOff[i], pred = predB,
                                actual = seasonData$B_metric[i], k = seasonData$k[i])
        newADef <- seasonData$A_adjMetricDef[i] - (seasonData$B_adjMetricOff[i] - newBOff)
        
        # Updating metric values
        mets$adjMetricOff[Ainds] <- newAOff
        mets$adjMetricDef[Binds] <- newBDef
        
        mets$adjMetricOff[Binds] <- newBOff
        mets$adjMetricDef[Ainds] <- newADef
      }
      
      output[[s]] <- seasonData
      
      if(s < n_seasons) {
        newSeason <- boxscores |> dplyr::filter(Season == seasons[s+1])
        newTeams <- unique(c(newSeason$Afix_TeamID, newSeason$Bfix_TeamID))
        
        newMets <- metricSeasonReset(oldMets = mets |> mutate(Season = seasonData$Season[1]),
                                     teamIDs = newTeams, center = center_fun)
        
        mStarts <- newMets
      }
      
      # Progress bar update
      pb$tick()
    }
    if(returnRecent == FALSE) {
      return(bind_rows(output))
    } else {
      return(list(metData = bind_rows(output), newMets = mets))
    }
  }
  
  # Effect of being home / away
  fullRaw1 |> 
    dplyr::mutate(A_metric = A_Score / A_Poss,
           B_metric = B_Score / B_Poss) |> 
    group_by(Afix_Loc) |> 
    summarize(A_metric = mean(A_metric), B_metric = mean(B_metric))
  
  # Calculating adjusted metric values across all seasons
  bs <- fullRaw1 |> 
    dplyr::mutate(A_metric = A_Score / A_Poss,
           B_metric = B_Score / B_Poss) |> 
    dplyr::select(Season, DayNum, Afix_TeamID, Bfix_TeamID, Afix_Loc, Afix_mov, Afix_win, 
           A_metric, B_metric) |> 
    group_by(Season, Afix_TeamID) |> 
    dplyr::arrange(DayNum) |> 
    dplyr::mutate(Afix_count = seq(n())) |> 
    ungroup() |> 
    group_by(Season, Bfix_TeamID) |> 
    dplyr::arrange(DayNum) |> 
    dplyr::mutate(Bfix_count = seq(n())) |> 
    dplyr::filter(Season >= 2010)
  
  # Detrending Scoring Efficiencies -----------------------------------------
  
  # Looking at rolling average SE across season (is there upward drift?)
  winLength <- 3
  roll_mean_na_rm <- tibbletime::rollify(~ mean(.x, na.rm = TRUE), window = winLength)
  
  # Offensive rolling avg SE
  rollMeansOff <- bs |> group_by(Season, Afix_TeamID) |> 
    dplyr::summarize(nGames = n(), A_metric = A_metric) |> 
    dplyr::filter(nGames >= winLength) |> 
    dplyr::summarize(SErollingOff = roll_mean_na_rm(A_metric),
              Afix_count = 1:n()) |> ungroup() |> 
    group_by(Season, Afix_count) |> 
    dplyr::summarize(SEoff = mean(SErollingOff, na.rm = TRUE))
  
  # Defensive rolling avg SE
  rollMeansDef <- bs |> 
    group_by(Season, Bfix_TeamID) |> 
    dplyr::summarize(nGames = n(), B_metric = B_metric) |> 
    dplyr::filter(nGames >= winLength) |> 
    dplyr::summarize(SErollingDef = roll_mean_na_rm(B_metric),
              Bfix_count = 1:n()) |> ungroup() |> 
    group_by(Season, Bfix_count) |> 
    dplyr::summarize(SEdef = mean(SErollingDef, na.rm = TRUE))
  
  # Plotting average of rolling average of offensive scoring efficiency
  offGG <- rollMeansOff |>  
    ggplot(aes(x = Afix_count, y = SEoff, color = factor(Season))) + 
    geom_line() +
    labs(title = "Rolling Average Offensive Scoring Efficiency",
         y = "Rolling average points per possession",
         x = "Game number",
         caption = "Each line is for a different season. \n
       Higher volatility occurs later in season when fewer teams are incorporated in the calculated average.") +
    ggthemes::theme_few() +
    theme(legend.position = "none")
  
  offGG + 
    geom_smooth(aes(x = Afix_count, y = SEoff), color = "black", method = "lm", se = FALSE)
  
  offGG + 
    stat_smooth(aes(x = Afix_count, y = SEoff),
                color = "black", se = FALSE,
                method = "lm", formula = y ~ x + I(x^2))
  
  # Plotting with quadratic fit
  
  # Plotting average of rolling average of defensive scoring efficiency
  defGG <- rollMeansDef |>  
    ggplot(aes(x = Bfix_count, y = SEdef, color = factor(Season))) + 
    geom_line() +
    labs(title = "Rolling Average Defensive Scoring Efficiency",
         y = "Rolling average points per possession",
         x = "Game number",
         caption = "Each line is for a different season. \n
       Higher volatility occurs later in season when fewer teams are incorporated in the calculated average.") +
    ggthemes::theme_few() +
    theme(legend.position = "none")
  
  defGG + 
    geom_smooth(aes(x = Bfix_count, y = SEdef), 
                color = "black", method = "lm", se = FALSE)
  
  defGG + 
    stat_smooth(aes(x = Bfix_count, y = SEdef),
                color = "black", se = FALSE,
                method = "lm", formula = y ~ x + I(x^2))
  
  # Interpolating to plot contours: https://stackoverflow.com/questions/65873211/empty-contour-plot-in-ggplot
  plot3D <- function(data3D, dupes = "mean") {
    
    data3Dclean <- data3D |> tidyr::drop_na() |> 
      dplyr::rename("x" = 1, "y" = 2, "z" = 3)
    
    suppressWarnings(grid <- akima::interp(dplyr::pull(data3Dclean, 1), 
                                           dplyr::pull(data3Dclean, 2),
                                           dplyr::pull(data3Dclean, 3),
                                           duplicate = dupes))
    
    griddf <- data.frame(x = rep(grid$x, ncol(grid$z)), 
                         y = rep(grid$y, each = nrow(grid$z)), 
                         z = as.numeric(grid$z))
    
    myVars <- colnames(data3D)
    
    griddf |>
      ggplot2::ggplot(ggplot2::aes(x = x, y = y, z = z)) +
      ggplot2::geom_contour_filled(aes(x = x, 
                                       y = y, 
                                       z = z)) + 
      ggplot2::geom_point(data = data3Dclean, colour="white",pch=21, 
                          fill = "black", size = 1.5) +
      ggplot2::labs(x = myVars[1], y = myVars[2],
                    fill = myVars[3],
                    title = "Three-dimensional distribution") +
      ggplot2::theme_bw() + 
      ggplot2::theme(text = element_text(face = "bold"), 
                     panel.grid = element_blank(),
                     legend.position = "bottom")
  }
  
  plot3D(rollMeansOff)
  plot3D(rollMeansDef)
  
  # Estimating trend in each season
  seModOff <- lm(SEoff ~ 1 + Afix_count + I(Afix_count^2) + factor(Season), 
                 data = rollMeansOff |> 
                   dplyr::filter(Afix_count <= 30, Season >= 2010))
  
  seModDef <- lm(SEdef ~ 1 + Bfix_count + I(Bfix_count^2) + factor(Season), 
                 data = rollMeansDef |> 
                   dplyr::filter(Bfix_count <= 30, Season >= 2010))
  
  coeffResOff <- broom::tidy(seModOff)
  coeffResDef <- broom::tidy(seModDef)
  
  # Based on p-values, the trend is significant, and the average
  # SE varies by season, but the trend does not vary by season
  
  # Detrending scoring efficiencies
  vanillaGames <- expand_grid(Afix_count = 1:40,
                              Season = min(seModOff$xlevels$`factor(Season)`):max(seModOff$xlevels$`factor(Season)`)) |> 
    mutate(adj_met_off = Afix_count*(coeffResOff$estimate[2] + coeffResOff$estimate[3]),
           adj_met_def = Afix_count*(coeffResDef$estimate[2] + coeffResDef$estimate[3]))
  
  bsDetrended <- bs |> left_join(vanillaGames, by = c("Season", "Afix_count", "Bfix_count")) |> 
    mutate(A_metric = A_metric - adj_met_off,
           B_metric = B_metric - expected_met_def) |> 
    dplyr::select(-expected_met_off, -expected_met_def)
  
  rollMeansDetrendedOff <- bsDetrended |> 
    group_by(Season, Afix_TeamID) |> 
    dplyr::summarize(nGames = n(), A_metric = A_metric) |> 
    dplyr::filter(nGames >= winLength) |> 
    dplyr::summarize(SErolling = roll_mean_na_rm(A_metric),
              Afix_count = 1:n()) |> ungroup() |> 
    group_by(Season, Afix_count) |> 
    dplyr::summarize(SE = mean(SErolling, na.rm = TRUE))
  
  # Plotting average of rolling average of offensive scoring efficiency
  rollMeansDetrendedOff |> 
    ggplot(aes(x = Afix_count, y = SE, color = factor(Season))) + 
    geom_line() +
    geom_smooth(aes(x = Afix_count, y = SE), color = "black", method = "lm", se = FALSE) +
    labs(title = "Rolling Average Offensive Scoring Efficiency",
         y = "Rolling average points per possession",
         x = "Game number",
         caption = "Each line is for a different season. \n
       Higher volatility occurs later in season when fewer teams are incorporated in the calculated average.") +
    ggthemes::theme_few() +
    theme(legend.position = "none")
  
  # Detrending scoring efficiency values within each season
  plot3D(rollMeansDetrendedOff)
  
  if(FALSE) {
    boxscores = bs 
    k_value = 0.10
    startOff = 1
    startDef = 1
    center_fun = mean
    home_advantage = 0.05 
    returnRecent = FALSE
  }
  
  # Trying using points per possessoion (offensive efficiency)
  metRes <- addMetric(boxscores = bs, 
                      k_value = 1,
                      startOff = 1.02,
                      startDef = 1.02, 
                      center_fun = mean,
                      home_advantage = 0.03, 
                      returnRecent = FALSE)
  
  # Plotting as sanity check
  metRes |> pivot_longer(cols = c(A_adjMetricOff, A_adjMetricDef, B_adjMetricOff, B_adjMetricDef),
                          names_to = "Category", values_to = "ScoringEfficiency") |> 
    mutate(Category = ifelse(str_detect(Category, pattern = "Off"), "Offensive", "Defensive")) |> 
    group_by(Season, Category) |> 
    summarize(Avg = mean(ScoringEfficiency), Min = min(ScoringEfficiency), Max = max(ScoringEfficiency)) |> 
    pivot_longer(cols = Avg:Max, names_to = "Metric", values_to = "Value") |> 
    ggplot(aes(x = Season, y = Value, color = Metric, linetype = Category)) + 
    geom_line(aes(x = Season)) + labs(title = "Scoring Efficiency Across Seasons") + 
    theme_bw()
}

# Massey Ordinal Rankings -------------------------------------------------

if(womens == FALSE) {
  # Adding in Massey ordinal ranking data
  # Descriptions / info for systems: https://masseyratings.com/cb/compare.htm
  massey <- data.table::fread(paste0(stageDir, "MMasseyOrdinals.csv")) |> 
    as.data.frame() |> 
    dplyr::rename(DayNum = RankingDayNum) 
  
  # Exploring which systems have collective most complete data
  # The `values_fn` option keeps first ranking when multiple for single day provided.
  # Filling down so ratings carry forward if provided day before game or so
  # Names from https://masseyratings.com/cb/compare.htm
  wideMassey <- massey |> 
    mutate(SystemName = case_when(SystemName == "MOR" ~ "Moore",
                                  SystemName == "SAG" ~ "Sagarin",
                                  SystemName == "MAS" ~ "Massey",
                                  SystemName == "POM" ~ "Pomeroy",
                                  TRUE ~ SystemName)) |> 
    pivot_wider(id_cols = c(Season, DayNum, TeamID), 
                names_from = SystemName, values_from = OrdinalRank, values_fill = NA, 
                values_fn = function(x){x[1]}) |> 
    dplyr::arrange(Season, DayNum) |> 
    group_by(Season, TeamID) |> 
    tidyr::fill(SEL:REI, .direction = "down") |> 
    ungroup()
  
  # Exploring pattern of missing ratings.
  # Imputation for ratings helps retain large amount of data.
  # In preliminary modelling Massey ratings not important, so drop here to retain ~ 3000 more games
  
  # Using mice package
  wideMassey |> 
    dplyr::select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey) |> 
    md.pattern()
  
  # Using naniar package
  wideMassey |> 
    dplyr::select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey) |> 
  naniar::gg_miss_upset(nsets = 7)
  
  # Imputing for ordinal rankings when 1 of 4 are missing. 
  # Can use rowSums since Season, DayNum, & TeamID are all complete
  masseyMiss <- wideMassey |> 
    dplyr::select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin) |> 
    dplyr::filter((is.na(Pomeroy) + is.na(Moore) + is.na(Sagarin)) < 2)
  
  # Impute ordinal ratings when 1 of 4 is missing using random forest
  massey_impute <- bind_cols(masseyMiss |> dplyr::select(-c(Pomeroy:Sagarin)), 
                            masseyMiss |> dplyr::select(Pomeroy:Sagarin) |> 
                              mice::mice(defaultMethod = "rf", seed = 1994, m = 1, maxit = 1) |> 
                              mice::complete())
  
  # Function to add ordinal ratings to box scores and elo data
  # Carry last rating forward for each team in each season via the fill function
  add_ratings <- function(myData, ratings_data = massey_impute) {
    ret <- myData |> 
      full_join(ratings_data |> 
                dplyr::select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin),
              by = c("Season" = "Season", "DayNum" = "DayNum",
                     "Afix_TeamID" = "TeamID")) |> 
      dplyr::rename(Afix_Pomeroy = Pomeroy, Afix_Moore = Moore, 
                    Afix_Sagarin = Sagarin) |> 
      full_join(ratings_data |> 
                  dplyr::select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin),
                by = c("Season" = "Season", "DayNum" = "DayNum",
                       "Bfix_TeamID" = "TeamID")) |> 
      dplyr::rename(Bfix_Pomeroy = Pomeroy, Bfix_Moore = Moore, 
                    Bfix_Sagarin = Sagarin) |> 
      arrange(Season, DayNum) |> group_by(Season, Afix_TeamID) |> 
      tidyr::fill(c(Afix_Pomeroy, Afix_Moore, Afix_Sagarin), .direction = "down") |> 
      ungroup() |> group_by(Season, Bfix_TeamID) |> 
      tidyr::fill(c(Bfix_Pomeroy, Bfix_Moore, Bfix_Sagarin), .direction = "down") |> 
      ungroup() |> 
      drop_na()
    
    return(ret)
  }
  
  massey_ELO <- add_ratings(wide_ELO, ratings_data = massey_impute)
  massey_ELO_final <- add_ratings(wide_ELO_final, ratings_data = massey_impute)
    
  # Testing to see if columns seem reasonable
  
  # Randomly choosing some games to have Team A lose
  massey_ELOMix <- massey_ELO |> 
    dplyr::filter(Afix_mov %% 2 == 1) |> 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Afix_" = "Xfix_", "A_" = "X_",
                                                        "Bfix_" = "Yfix_", "B_" = "Y_")),
                .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc"))) |> 
    bind_rows(massey_ELO |> 
                dplyr::filter(Afix_mov %% 2 == 0) |> 
                dplyr::mutate(Afix_mov = -Afix_mov, Afix_win = !Afix_win,
                       Afix_Loc = case_when(Afix_Loc == "A" ~ "H",
                                            Afix_Loc == "H" ~ "A",
                                            TRUE ~ Afix_Loc)) |> 
                rename_with(.fn =  ~ str_replace_all(string = .x, c("Bfix_" = "Xfix_", "B_" = "X_",
                                                                    "Afix_" = "Yfix_", "A_" = "Y_")),
                            .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc")))) |> 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Xfix_" = "Afix_", "X_" = "A_",
                                                        "Yfix_" = "Bfix_", "Y_" = "B_")),
                .cols = everything())
  
  massey_ELOMix_final <- massey_ELO_final
  
  summary(lm(Afix_mov ~ 0 + Afix_Loc + Afix_Pomeroy + Afix_Moore + Afix_Sagarin + 
               Bfix_Pomeroy + Bfix_Moore + Bfix_Sagarin + 
               Afix_elo + Bfix_elo, 
             data = massey_ELOMix))
}

# Adding in pre-season AP rank / previous season-ending ELO  --------------------------------------------
  
  # Logistic-based function for smoothing across season
  # decay_rate: Steepness of inverse logit function for weights
  # converge_point: controls the lower asymptote of the inverse logit function
  logistic_decay <- function(x, decay_rate = 0.1, converge_point = 0.3, mid_point = 0) {
    x <- x - 20
    (exp(-decay_rate*(x - mid_point)) / (1 + exp(-decay_rate*(x - mid_point))) + converge_point) / (1 + converge_point)
  }
  
  if(womens) {
    massey_ELO <- wide_ELO
    massey_ELO_final <- wide_ELO_final
  }
  
  # Function to add ending ELO values from previous season as tapered predictor
  add_ending_elos <- function(elo_data, massey_elo_data) {
    # Previous season's ending elo values
    # TADOO: Ideally should include ELOs factoring in results of last game of previous season,
    # but currently excludes these as these 'final ELOs' are not directly returned 
    # from the ELO functions for every season (only the final season). 
    # This should be corrected when time permits. 
    elo_starts <- elo_data |> 
      group_by(Season, Afix_TeamID) |>
      slice_max(DayNum, n = 1) |> 
      ungroup() |> 
      dplyr::select(Season, DayNum, Afix_TeamID, Afix_elo) |> 
      dplyr::rename(TeamID = Afix_TeamID, elo_start = Afix_elo) |> 
      bind_rows(elo_data |> 
                  group_by(Season, Bfix_TeamID) |>
                  slice_max(DayNum, n = 1) |> 
                  ungroup() |> 
                  dplyr::select(Season, DayNum, Bfix_TeamID, Bfix_elo) |> 
                  dplyr::rename(TeamID = Bfix_TeamID, elo_start = Bfix_elo)) |> 
      group_by(Season, TeamID) |> 
      slice_max(DayNum, n = 1, with_ties = FALSE) |> 
      ungroup() |> 
      dplyr::mutate(Season = Season + 1) |> 
      dplyr::select(-DayNum)
    
    # Adding into full data: elo_data
    ret <- massey_elo_data |> 
      left_join(elo_starts, by = c("Season" = "Season",  "Afix_TeamID" = "TeamID")) |> 
      left_join(elo_starts, by = c("Season" = "Season",  "Bfix_TeamID" = "TeamID")) |> 
      dplyr::rename(Afix_eloStart = elo_start.x, 
                    Bfix_eloStart = elo_start.y) |> 
      dplyr::mutate(Afix_eloStart = logistic_decay(x = Afix_count,
                                                   decay_rate = 0.04, converge_point = 0.19, mid_point = 0) * Afix_eloStart,
                    Bfix_eloStart = logistic_decay(x = Bfix_count,
                                                   decay_rate = 0.04, converge_point = 0.19, mid_point = 0) * Bfix_eloStart)
    
    return(ret)
  }
  
  massey_ELOAP <- add_ending_elos(elo_data = wide_ELO, 
                                  massey_elo_data = massey_ELO)
  massey_ELOAP_final <- add_ending_elos(elo_data = wide_ELO_final, 
                                        massey_elo_data = massey_ELO_final)
  
  # Optimizing parameters for logistic_decay function for ELO starts
  if(FALSE) {
  # Simulation function to select optimal decay
  elo_decay_sim <- function(param) {
    return(summary(lm(Afix_mov ~ Afix_eloStartdecay + Bfix_eloStartdecay, data = massey_ELO |> 
      left_join(elo_starts, by = c("Season" = "Season",  "Afix_TeamID" = "TeamID")) |> 
      left_join(elo_starts, by = c("Season" = "Season",  "Bfix_TeamID" = "TeamID")) |> 
      dplyr::rename(Afix_eloStart = elo_start.x, 
                    Bfix_eloStart = elo_start.y) |> 
      dplyr::mutate(Afix_eloStartdecay = logistic_decay(x = Afix_count,
                                                        decay_rate = 0.04, converge_point = 0.19, mid_point = 0) * Afix_eloStart,
                    Bfix_eloStartdecay = logistic_decay(x = Bfix_count,
                                                        decay_rate = 0.04, converge_point = 0.19, mid_point = 0) * Bfix_eloStart)))$r.squared)
  }
  
  cand_params <- seq(0.02, 0.5, by = 0.02)
  decay_gg <- tibble(R2 = map_dbl(.f = elo_decay_sim, .x = cand_params),
                     Param = cand_params)
  
  # Visualizing performance of decay values
  decay_gg |> 
    ggplot(aes(x = Param, y = R2)) +
    geom_line() +
    geom_point() +
    ggthemes::theme_few()
  }
  
  if(womens == FALSE) {
    # Preseason poll data from sports-reference: https://www.sports-reference.com/cbb/seasons/2003-polls.html
    #                                          : https://www.sports-reference.com/cbb/seasons/women/2010-polls.html
    # AP rankings from Kaggle did not seem to have preseason AP rankings for teams
    apPres <- list.files(paste0("AP/", ifelse(womens, "womens/", "mens/"))) |> 
      str_subset(pattern = "csv") |> 
      map_dfr(.f = function(myFile) {
        season <- str_sub(str_split(myFile, pattern = "-")[[1]][2], start = 1, end = 4)
        ret <- read_csv(paste0("AP/", ifelse(womens, "womens/", "mens/"), myFile), 
                        skip = 2, show_col_types = FALSE) |> 
          dplyr::select(School, Pre) |> 
          drop_na() |> 
          dplyr::mutate(Season = as.numeric(season))
      }) |> 
      dplyr::mutate(TeamNameSpelling = tolower(School)) |> 
      left_join(read_csv(paste0(stageDir, ifelse(womens, "W", "M"), "TeamSpellings.csv"))) |> 
      dplyr::rename(AP = Pre) |> 
      dplyr::select(Season, TeamID, AP)
    
  # Imputing initial AP ranks using Massey ordinals
  apMiss <- massey_impute |> 
    dplyr::arrange(Season, DayNum) |> 
    group_by(Season, TeamID) |>
    slice(1) |> 
    ungroup() |> 
    full_join(apPres) |> 
    group_by(Season) |> 
    dplyr::mutate(AggRank = rank(Pomeroy + Moore + Sagarin)) |> 
    ungroup() |> 
    dplyr::mutate(AP = case_when(is.na(AP) ~ AggRank, 
                          TRUE ~ AP)) |> 
    dplyr::select(Season, TeamID, AP)
  
  # Function to add tapered AP preseason ranking into data
  add_AP <- function(massey_data, ap_data) {
    ret <- massey_data |> 
      left_join(ap_data, by = c("Season" = "Season",  "Afix_TeamID" = "TeamID")) |> 
      left_join(ap_data, by = c("Season" = "Season",  "Bfix_TeamID" = "TeamID")) |> 
      dplyr::rename(Afix_AP = AP.x, Bfix_AP = AP.y) |> 
      dplyr::mutate(Afix_AP = logistic_decay(x = Afix_count,
                                             decay_rate = 0.1, converge_point = 0.3, mid_point = 0) * Afix_AP,
                    Bfix_AP = logistic_decay(x = Bfix_count,
                                             decay_rate = 0.1, converge_point = 0.3, mid_point = 0) * Bfix_AP)
    return(ret)
  }
  
  # Adding tapered AP preseason rankings into full data
  massey_ELOAP <- add_AP(massey_data = massey_ELOAP, ap_data = apMiss)
  massey_ELOAP_final <- add_AP(massey_data = massey_ELOAP_final, ap_data = apMiss)
}
  
  # Optimizing parameters for logistic_decay function for AP preseason ranks
  if(FALSE) {
    # Simulation function to select optimal decay
    ap_decay_sim <- function(param) {
      return(summary(lm(Afix_mov ~ Afix_APdecay + Bfix_APdecay, data = massey_ELOAP |> 
                          left_join(apMiss, by = c("Season" = "Season",  "Afix_TeamID" = "TeamID")) |> 
                          left_join(apMiss, by = c("Season" = "Season",  "Bfix_TeamID" = "TeamID")) |> 
                          dplyr::rename(Afix_AP = AP.x, Bfix_AP = AP.y) |> 
                          dplyr::mutate(Afix_APdecay = logistic_decay(x = Afix_count,
                                                                      decay_rate = 0.1, converge_point = param, mid_point = 0) * Afix_AP,
                                        Bfix_APdecay = logistic_decay(x = Bfix_count,
                                                                      decay_rate = 0.1, converge_point = param, mid_point = 0) * Bfix_AP)))$r.squared)
    }
    
    cand_params <- seq(0.05, 1.5, by = 0.05)
    decay_gg <- tibble(R2 = map_dbl(.f = ap_decay_sim, .x = cand_params),
                       Parameter = cand_params)
    
    # Visualizing performance of decay values
    decay_gg |> 
      ggplot(aes(x = Parameter, y = R2)) +
      geom_line() +
      geom_point() +
      ggthemes::theme_few()
  }
  
  # Creating myopic data set from team A's perspective as winner always
  myopicData <- massey_ELOAP

# Randomly selecting some games to have Team A lose
massey_ELOAPMix <- myopicData |> 
  dplyr::filter(Afix_mov %% 2 == 1) |> 
  rename_with(.fn =  ~ str_replace_all(string = .x, c("Afix_" = "Xfix_", "A_" = "X_",
                                                      "Bfix_" = "Yfix_", "B_" = "Y_")),
              .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc"))) |> 
  bind_rows(myopicData |> 
              dplyr::filter(Afix_mov %% 2 == 0) |> 
              dplyr::mutate(Afix_mov = -Afix_mov, Afix_win = !Afix_win,
                            Afix_Loc = case_when(Afix_Loc == "A" ~ "H",
                                                 Afix_Loc == "H" ~ "A",
                                                 TRUE ~ Afix_Loc)) |> 
              rename_with(.fn = ~ str_replace_all(string = .x, c("Bfix_" = "Xfix_", "B_" = "X_",
                                                                 "Afix_" = "Yfix_", "A_" = "Y_")),
                          .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc")))) |> 
  rename_with(.fn =  ~ str_replace_all(string = .x, c("Xfix_" = "Afix_", "X_" = "A_",
                                                      "Yfix_" = "Bfix_", "Y_" = "B_")),
              .cols = everything())

# Removing first G games of the season for each team
# e.g., G=1 means exclude games where it is any teams first game of season
G <- 3
massey_ELOAPMix <- massey_ELOAPMix |> 
  dplyr::filter(Afix_count > G, Bfix_count > G)

summary(lm(Afix_mov ~ Afix_elo + Bfix_elo, 
           data = massey_ELOAPMix))

# Sanity Checks ---------------------------------------------

# Tadoo: 
## 1) Use principal components to create summary variables:
#### a) 'SOS' based on elo_against and win_rate_against
#### b) 'offensive_rating' based on all for_* offensive metrics
#### b) 'defensive_rating' based on all against_* defensive metrics

# Sanity check for data leakage or other issues using cv.glmnet

# Truncating data & dropping those with missing elo starts.
# Missing elo starts I believe are new teams and tend to be very bad teams, so
# are not likely good for training for tournament games anyways

# Function to create design matrix and response vector
make_design_response <- function(massey_data, season_cutoff = 2010, preds = "all", remove_nas = TRUE, model = "first-order") {
  
  if(remove_nas) {
    massey_data <- drop_na(massey_data) |> 
      dplyr::filter(Season >= season_cutoff)
  }
  
  if(length(preds) == 1 && all(preds == "all")) {
    massey_data <- massey_data |> 
      dplyr::select(-Afix_count, -Bfix_count,
                    -Season, -DayNum, -GameID, -Afix_TeamID,
                    -Bfix_TeamID, -Afix_Conference,
                    -Bfix_Conference, -Afix_mov)
  } else {
    massey_data <- massey_data |> 
      dplyr::select(all_of(c(preds, "Afix_win")))
  }
  
  ret <- list()
  
  if(model == "first-order") {
    model_formula <- formula(Afix_win ~ 0 + .)
  } else if(model == "second-order"){
    model_formula <- formula(Afix_win ~ 0 + .^2)
  }
  
  ret$design <- massey_data |> 
    model.matrix(object = model_formula) |> 
    as.data.frame() |> 
    dplyr::mutate(across(.cols = everything(), .fns = \(x) scale(x, center = TRUE, scale = TRUE))) |> 
    as.matrix()
  
  ret$response <- massey_data$Afix_win
  
  return(ret)
}

design_response <- make_design_response(massey_data = massey_ELOAPMix,
                                        season_cutoff = 2010, model = "first-order")

sanityCheck <- FALSE

if(sanityCheck == TRUE) {
# Fitting lasso-penalized model
library(glmnet)
set.seed(1994)
cvModel <- cv.glmnet(x = design_response$design, y = design_response$response,
                     family = "binomial", type.measure = "class",
                     nfolds = 10)

plot(cvModel)
coef(cvModel, s = "lambda.min")

# Important predictors from lasso model (kept for both A and B and for and against):
fitRes <- data.frame(Predictor = coef(cvModel, s = "lambda.min") |>
                       as.matrix() |> as.data.frame() |> rownames(),
                     Value = coef(cvModel, s = "lambda.min") |> as.matrix() |>
                       as.data.frame() |> unlist()) |> 
  dplyr::filter(abs(Value) > 0.00001)

preds <- fitRes |> 
  dplyr::pull(Predictor) |>
  str_split(pattern = "A_|B_|Afix|Bfix") |> 
  unlist() |>
  trimws(whitespace = "_") |> 
  table() |> 
  as.data.frame() |>
  dplyr::arrange(desc(Freq)) |> 
  dplyr::filter(`Var1` != "")

# Since predictors are standardized, size of coefficients indicates "importance"
predSummary <- tibble(Pred = names(coef(cvModel, s = "lambda.min")[, 1]), 
                      Coeff = coef(cvModel, s = "lambda.min")[, 1]) |> 
  dplyr::arrange(desc(abs(Coeff)))
}

design_response_file <- paste0(tourneyYear, "/design_response_", tourneyYear, "_", ifelse(womens, "W", "M"), ".rds")

if(file.exists(design_response_file) == FALSE) {
if(womens == FALSE) {
  # Important predictor variables
  pkeeps <- c('Afix_win', 'Afix_Loc', 'tournament_game',
              'Afix_Pomeroy', 'Bfix_Pomeroy',
              'Afix_AP', 'Bfix_AP', 
              'Afix_elo', 'Bfix_elo', 
              'A_for_Score_Eff', 'B_for_Score_Eff', 
              'Afix_eloStart', 'Bfix_eloStart',
              'Afix_Pyth', 'Bfix_Pyth',
              'Afix_Sagarin', 'Bfix_Sagarin', 
              'Afix_Moore', 'Bfix_Moore', 
              'A_for_Blk', 'B_for_Blk',
              'A_for_OR_rate', 'B_for_OR_rate', 
              'A_for_elo_against', 'B_for_elo_against',
              'A_for_TO_rate', 'B_for_TO_rate', 
              'A_for_Ast', 'B_for_Ast', 
              'A_for_FGA3', 'B_for_FGA3', 
              'A_for_Margin_Eff', 'B_for_Margin_Eff', 
              'A_against_OR', 'B_against_OR', 
              'A_against_TO_rate', 'B_against_TO_rate',
              'A_against_Ast', 'B_against_Ast')
  
  # Creating first-order design matrix for model fitting
  designMatFit <- make_design_response(massey_data = massey_ELOAPMix, 
                                       preds = pkeeps, model = "first-order")
  
  # Creating second-order design matrix for model fitting
  designMatFit2 <- make_design_response(massey_data = massey_ELOAPMix, 
                                        preds = pkeeps, model = "second-order")
} else {
  # Important predictor variables
  pkeeps <- c('Afix_win', 'Afix_Loc', 'tournament_game',
              'Afix_elo', 'Bfix_elo', 
              'Afix_eloStart', 'Bfix_eloStart',
              'A_for_Score_Eff', 'B_for_Score_Eff', 
              'Afix_Pyth', 'Bfix_Pyth',
              'A_for_FGA3', 'B_for_FGA3', 
              'A_for_FGM3', 'B_for_FGM3',
              'A_for_Blk', 'B_for_Blk',
              'A_for_FGM', 'B_for_FGM', 
              'A_for_Luck', 'B_for_Luck', 
              'A_for_Ast', 'B_for_Ast', 
              'A_for_Stl', 'B_for_Stl', 
              'A_for_PF', 'B_for_PF', 
              'A_for_Margin_Eff', 'B_for_Margin_Eff', 
              'A_for_TO_rate', 'B_for_TO_rate', 
              'A_for_FGA', 'B_for_FGA',
              'A_for_OR_rate', 'B_for_OR_rate',
              'A_for_elo_against', 'B_for_elo_against',
              'A_against_elo_against', 'B_against_elo_against',
              'A_against_OR', 'B_against_OR', 
              'A_against_FGM', 'B_against_FGM', 
              'A_against_FGM3', 'B_against_FGM3',
              'A_against_FTA', 'B_against_FTA', 
              'A_against_FGA', 'B_against_FGA', 
              'A_against_FGA3', 'B_against_FGA3',
              'A_against_Ast', 'B_against_Ast', 
              'A_against_Stl', 'B_against_Stl', 
              'A_against_PF', 'B_against_PF',
              'A_against_TO_rate', 'B_against_TO_rate',
              'A_against_Luck', 'B_against_Luck',
              'A_against_Margin_Eff', 'B_against_Margin_Eff',
              'A_against_Blk', 'B_against_Blk')

  # Creating first-order design matrix for model fitting
  designMatFit <- make_design_response(massey_data = massey_ELOAPMix, 
                                       preds = pkeeps, model = "first-order")
  
  # Creating second-order design matrix for model fitting
  designMatFit2 <- make_design_response(massey_data = massey_ELOAPMix, 
                                        preds = pkeeps, model = "second-order")
}

  design_response <- list(designMatFit, designMatFit2)
  
# Saving design and response data for model fitting
write_rds(design_response, file = design_response_file)
} else {
  design_response <- read_rds(design_response_file)
}

# Fitting models ----------------------------------------------------------



# Making final design matrix ----------------------------------------------

# Function for swapping role of 'Team A' and 'Team B'
swap_A_B <- function(bball_data) {
  ret <- bball_data |> 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Afix_" = "Xfix_", "A_" = "X_",
                                                        "Bfix_" = "Yfix_", "B_" = "Y_")),
                .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc"))) |> 
    bind_rows(bball_data |> 
                mutate(Afix_mov = -Afix_mov, Afix_win = !Afix_win,
                       Afix_Loc = case_when(Afix_Loc == "A" ~ "H",
                                            Afix_Loc == "H" ~ "A",
                                            TRUE ~ Afix_Loc)) |> 
                rename_with(.fn =  ~ str_replace_all(string = .x, c("Bfix_" = "Xfix_", "B_" = "X_",
                                                                    "Afix_" = "Yfix_", "A_" = "Y_")),
                            .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc")))) |> 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Xfix_" = "Afix_", "X_" = "A_",
                                                        "Yfix_" = "Bfix_", "Y_" = "B_")),
                .cols = everything())
  
  return(ret)
}

# Create designMatrix with duplicates by swapping role of Team A & Team B
massey_ELOAPMixDupe <- massey_ELOAPMix |> 
  bind_rows(swap_A_B(massey_ELOAPMix))

designMatFitDupe <- massey_ELOAPMixDupe |> 
  dplyr::select(all_of(pkeeps)) |>   
  model.matrix(object = formula(Afix_win ~ 0 + .)) |> 
  as.data.frame() |> 
  mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) |> 
  as.matrix()

# Creating first-order design matrix for model fitting
designMatFit <- make_design_response(massey_data = massey_ELOAPMix, 
                                     preds = pkeeps, model = "first-order")

designMatFit$

# Final Predictions -------------------------------------------------------

model <- "MOV"

predsDir <- paste0("/Preds/", model, "/")

# Adding empty tournament games to predict for
sampleSubmission <- read_csv(paste0(stageDir, "SampleSubmission", tourneyYear, ".csv"))

matchups <- sampleSubmission |> 
  mutate(Season = as.integer(map_chr(str_split(ID, pattern = "_"), 1)), 
         Afix_TeamID = as.integer(map_chr(str_split(ID, pattern = "_"), 2)), 
         Bfix_TeamID = as.integer(map_chr(str_split(ID, pattern = "_"), 3))) |> 
  dplyr::select(-ID, -Pred)

# Read in model fits to make predictions
myFinalMods <- readRDS(paste0(tourneyYear, "/finalFits_", ifelse(womens, "W", "M"),
                              "_", model, ".rds"))

designMat <- designRows2

# Function to convert MOV to win probability
movPreds <- predict(myFinalMods$my.nnet, newdata = designMatFitDupe, type = "raw")

convertFit <- glm(formula = outcome ~ 0 + ., 
           data = data.frame(outcome = massey_ELOAPMixDupe$Afix_win, 
  movHat = predict(myFinalMods$my.nnet, newdata = designMatFitDupe, type = "raw")), 
  family = "binomial")

mov_to_prob <- function(movs, fit = convertFit) {
  return(predict(fit, newdata = data.frame(movHat = movs), 
          type = 'response'))
}


# Final tournament preds
if(model == "Awin") {
  finalPreds <- tibble(nnet = predict(myFinalMods$my.nnet, newdata = designMat, type = "prob")[, "win"],
                       glmnet = as.numeric(predict(myFinalMods$cvglmFit, newx = designMat, 
                                                   s = "lambda.min", type = "response")))
} else {
  finalPreds <- tibble(nnet = mov_to_prob(predict(myFinalMods$my.nnet, newdata = designMat, type = "raw")),
                       glmnet = mov_to_prob(as.numeric(predict(myFinalMods$cvglmFit, newx = designMat, 
                                                   s = "lambda.min", type = "response"))))
}

# Saving just mens or just womens file
write_csv(matchups |> dplyr::filter(Afix_TeamID %in% lastObserved$TeamID |
                                     Bfix_TeamID %in% lastObserved$TeamID) |> 
            dplyr::mutate(ID = stringr::str_c(Season, "_", Afix_TeamID, "_", Bfix_TeamID),
                          Pred = finalPreds$nnet) |> dplyr::select(ID, Pred), 
          file = paste0(tourneyYear, predsDir, "preds_nnet_", 
                        ifelse(womens, "W", "M"), "_", model, "_only.csv"))

write_csv(matchups |> dplyr::filter(Afix_TeamID %in% lastObserved$TeamID |
                                       Bfix_TeamID %in% lastObserved$TeamID) |> 
            dplyr::mutate(ID = stringr::str_c(Season, "_", Afix_TeamID, "_", Bfix_TeamID),
                          Pred = finalPreds$glmnet) |> dplyr::select(ID, Pred), 
          file = paste0(tourneyYear, predsDir, "preds_glmnet_", 
                        ifelse(womens, "W", "M"), "_", model, "_only.csv"))

# Merging with sample submission
filledSubmits <- sampleSubmission |> dplyr::select(-Pred) |> 
  left_join(matchups |> dplyr::filter(Afix_TeamID %in% lastObserved$TeamID |
                                         Bfix_TeamID %in% lastObserved$TeamID) |> 
              dplyr::mutate(ID = stringr::str_c(Season, "_", Afix_TeamID, "_", Bfix_TeamID),
                            Pred = finalPreds$nnet) |> dplyr::select(ID, Pred))


# Saving files for submission
finalNnet <- sampleSubmission |> dplyr::select(-Pred) |> 
  left_join(matchups |> dplyr::filter(Afix_TeamID %in% lastObserved$TeamID |
                                         Bfix_TeamID %in% lastObserved$TeamID) |> 
              dplyr::mutate(ID = stringr::str_c(Season, "_", Afix_TeamID, "_", Bfix_TeamID),
                            Pred = finalPreds$nnet) |> dplyr::select(ID, Pred))

write_csv(finalNnet, 
          file = paste0(tourneyYear, predsDir, "preds_nnet_", 
                        ifelse(womens, "W", "M"), "_", model, "_",
                        "partial", ".csv"))

finalGlmnet <- sampleSubmission |> dplyr::select(-Pred) |> 
  left_join(matchups |> dplyr::filter(Afix_TeamID %in% lastObserved$TeamID |
                                         Bfix_TeamID %in% lastObserved$TeamID) |> 
              dplyr::mutate(ID = stringr::str_c(Season, "_", Afix_TeamID, "_", Bfix_TeamID),
                            Pred = finalPreds$glmnet) |> dplyr::select(ID, Pred))

write_csv(finalGlmnet, 
          file = paste0(tourneyYear, predsDir, "preds_glmnet_", 
                        ifelse(womens, "W", "M"), "_", model, "_",
                        "partial", ".csv"))


# Combining mens and womens predictions for final submission
mensPReds <- read_csv(paste0(tourneyYear, predsDir, "preds_nnet_", "M", "_", 
                             model, "_partial", ".csv")) |> drop_na()

womensPReds <- read_csv(paste0(tourneyYear, predsDir, "preds_nnet_", "W", "_", 
                             model, "_partial", ".csv")) |> drop_na()


write_csv(bind_rows(mensPReds, womensPReds), 
          file = paste0(tourneyYear, "/Preds/preds_nnet_", "MW", "_", 
                        model, "_final", ".csv"))

# Data for Tournament Predictions -----------------------------------------
  
  # Data for final games
  lastAGames <- massey_ELOAPMix |> 
    dplyr::filter(Season == tourneyYear) |> 
    arrange(DayNum) |> 
    group_by(Afix_TeamID) |> 
    slice_tail(n = 1) |> ungroup()
  
  lastBGames <- massey_ELOAPMix |> 
    dplyr::filter(Season == tourneyYear) |> 
    arrange(DayNum) |> 
    group_by(Bfix_TeamID) |> 
    slice_tail(n = 1) |> ungroup()
  
  lastGames <- bind_rows(lastAGames, lastBGames) |> dplyr::distinct()
  
  # Updating ELOs based on result of final games
  nteams <- length(eloRes$newElos$team)
  for(team in 1:nteams) {
    lastGames[which(lastGames$Afix_TeamID == eloRes$newElos$team[team]), "Afix_elo"] <- eloRes$newElos$elo[team]
    lastGames[which(lastGames$Bfix_TeamID == eloRes$newElos$team[team]), "Bfix_elo"] <- eloRes$newElos$elo[team]
  }
  
  # Dropping final games with unupdated ELOs and replacing with updated ELO data
  massey_ELOAPMixFinal <- massey_ELOAPMix |> 
    dplyr::filter(!(GameID %in% c(lastGames$GameID))) |> 
    bind_rows(lastGames)
  
  # Indices of final games 
  finalInds <- which(massey_ELOAPMixFinal$GameID %in% lastGames$GameID)
  
  # Creating final first-order design matrix for model fitting
  designMatFitFinal <- massey_ELOAPMixFinal |> 
    dplyr::select(all_of(pkeeps)) |>  
    model.matrix(object = formula(Afix_win ~ 0 + .)) |> 
    as.data.frame() |> 
    mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) |> 
    as.matrix()
  
  # Extracting rows for final predictions
  finalTeamGames <- map_dfr(.x = c("A", "B"), .f = function(refTeam) {
    lastGames |> 
    dplyr::select(Season:GameID, starts_with(refTeam, ignore.case = FALSE)) |> 
    dplyr::select(Season:GameID, ends_with("fix_TeamID"), any_of(pkeeps)) |> 
    rename_with(.fn =  ~ str_sub(.x, start = 2, end = -1), .cols = starts_with(refTeam, ignore.case = FALSE)) |> 
      dplyr::mutate(Reference = refTeam)
  }) |> 
    arrange(desc(DayNum)) |> 
    group_by(fix_TeamID, .drop = FALSE) |> 
    slice_head(n = 1) |> ungroup() |> 
    dplyr::select(GameID, fix_TeamID, Reference)
    
  # Indices for rows to keep from design matrix
  dIndices <- map_int(1:nrow(finalTeamGames), .f = function(g) {
    which(massey_ELOAPMixFinal$GameID == finalTeamGames$GameID[g] &
          (massey_ELOAPMixFinal$Afix_TeamID == finalTeamGames$fix_TeamID[g] |
           massey_ELOAPMixFinal$Bfix_TeamID == finalTeamGames$fix_TeamID[g]))
  })
  
  # Including design matrix indices
  finalTeamGamesFull <- finalTeamGames |> dplyr::mutate(emapRow = dIndices)
  
  # Design matrix columns for Team A and Team B
  dcols <- list(A = which(stringr::str_starts(colnames(designMatFitFinal), pattern = "A") &
                            !(colnames(designMatFitFinal) %in% c("Afix_LocA", "Afix_LocH", "Afix_LocN"))),
                B = which(stringr::str_starts(colnames(designMatFitFinal), pattern = "B")))
  
  # All relevant matchups (excluding womens for mens and vice versa)
  fullMatchups <- matchups |> 
    left_join(finalTeamGamesFull, by = c("Afix_TeamID" = "fix_TeamID")) |> 
    left_join(finalTeamGamesFull, by = c("Bfix_TeamID" = "fix_TeamID"), suffix = c("_A", "_B")) |> 
    dplyr::filter(!is.na(GameID_A))
  
  # Home, Neutral, and Away court values
  awayVal <- min(designMatFitFinal[, "Afix_LocA"])
  homeVal <- min(designMatFitFinal[, "Afix_LocH"])
  neutralVal <- max(designMatFitFinal[, "Afix_LocN"])
  
  # Design matrix without location
  fullDesign <- matrix(unlist(purrr::map(.x = 1:nrow(fullMatchups),
                         .f = function(x) {
                           c(designMatFitFinal[fullMatchups$emapRow_A[x], dcols[[fullMatchups$Reference_A[x]]]],
                             designMatFitFinal[fullMatchups$emapRow_B[x], dcols[[fullMatchups$Reference_B[x]]]])
                           })), byrow = TRUE, ncol = length(dcols[[1]])*2)
  
  # Adding location columns
  fullDesign <- cbind(matrix(rep(c(awayVal, homeVal, neutralVal), times = nrow(fullDesign)),
         byrow = TRUE, nrow = nrow(fullDesign)),
         fullDesign)
  
  colnames(fullDesign) <- c("Afix_LocA", "Afix_LocH", "Afix_LocN",
                            sort(pkeeps[-c(1:2)]))
  
  # Rearranging columns properly
  fullDesign <- fullDesign[, colnames(designMatFitFinal)]

 
