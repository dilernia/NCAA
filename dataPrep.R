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
# stage: first (stage = 1) or second stage (stage = 2) of Tourney competition
# tourneyYear: year of tournament to forecast
womens <- TRUE
stage <- 2
tourneyYear <- 2023

# Setting file paths
if(Sys.info()["sysname"] == "Darwin") {
  setwd("/Volumes/GoogleDrive/My Drive/Other/Fun/March Madness")
  stageDir <- paste0("/Volumes/GoogleDrive/My Drive/Other/Fun/March Madness/", tourneyYear, "/march-machine-learning-mania-", tourneyYear, "/")
} else {
  stageDir <- paste0(tourneyYear, "/march-machine-learning-mania-", tourneyYear, "/")
}

# Importing & Cleaning Box Score Data -------------------------------------

# Importing raw detailed box score data
regRaw <- read_csv(paste0(stageDir, paste0(ifelse(womens, "W", "M"), 
                                           "RegularSeasonDetailedResults.csv"))) %>% 
  mutate(Tourney = FALSE)
tourneyRaw <- read_csv(paste0(stageDir, paste0(ifelse(womens, "W", "M"), "NCAATourneyDetailedResults.csv"))) %>% 
  mutate(Tourney = TRUE)

# Importing conference data
confInfo <- read_csv(paste0(stageDir, paste0(ifelse(womens, "W", "M"), "TeamConferences.csv"))) %>% 
  left_join(read_csv(paste0(stageDir, "Conferences.csv"))) %>% 
  rename(Conference = Description) %>% dplyr::select(Season, TeamID, Conference)

# Merging tournament and regular season data 
# and adding conference info, possession, and efficiency columns
# POSS calculation from https://thepowerrank.com/cbb-analytics/ and https://kenpom.com/blog/ratings-glossary/
# Rate statistics from Dean Oliver: https://www.basketball-reference.com/about/factors.html
fullRaw <- bind_rows(regRaw, tourneyRaw) %>% 
  mutate(LLoc = case_when(WLoc == "H" ~ "A",
                          WLoc == "A" ~ "H",
                          WLoc == "N" ~ "N",)) %>% 
  dplyr::select(Season:WTeamID, LTeamID, WScore, LScore, WLoc, LLoc, WFGM:LPF) %>% 
  left_join(confInfo, by = c("Season" = "Season",
                             "WTeamID" = "TeamID")) %>% 
  left_join(confInfo, suffix = c("W", "L"),
            by = c("Season" = "Season", "LTeamID" = "TeamID")) %>% 
  rename(WConference = ConferenceW, LConference = ConferenceL) %>%
  arrange(Season, DayNum, WTeamID) %>% 
  mutate(WPoss = ((WFGA - WOR + WTO + (0.475 * WFTA)) + 
                  (LFGA - LOR + LTO + (0.475 * LFTA))) / 2,
         LPoss = WPoss, GameID = row_number(), 
         Afix_mov = WScore - LScore,
         WScore_Eff = WScore / WPoss,
         LScore_Eff = LScore / LPoss,
         WMargin_Eff = Afix_mov / WPoss,
         LMargin_Eff = Afix_mov / LPoss,
         WPyth = (WScore_Eff^11.5) / (WScore_Eff^11.5 + LScore_Eff^11.5),
         LPyth = (LScore_Eff^11.5) / (WScore_Eff^11.5 + LScore_Eff^11.5),
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

# Creating duplicate rows to be agnostic to team winning or not
# and allow calculating rolling statistics
fullRaw1 <- fullRaw %>% 
  rename_with(.fn =  ~ gsub("_L", "B_", paste0("_", .x),
                            fixed = TRUE), .cols = starts_with("L")) %>% 
  rename_with(.fn =  ~ gsub("_W", "A_", paste0("_", .x),
                            fixed = TRUE), .cols = starts_with("W")) %>%
  mutate(Afix_win = TRUE) %>% 
  rename_with(.fn =  ~ gsub("A_", "Afix_", .x,
                            fixed = TRUE), .cols = c(A_TeamID, A_Loc, A_Conference)) %>% 
  rename_with(.fn =  ~ gsub("B_", "Bfix_", .x,
                            fixed = TRUE), .cols = c(B_TeamID, B_Loc, B_Conference)) %>% 
  arrange(GameID)

fullRaw2 <- fullRaw %>% 
  rename_with(.fn =  ~ gsub("_W", "B_", paste0("_", .x),
                            fixed = TRUE), .cols = starts_with("W")) %>% 
  rename_with(.fn =  ~ gsub("_L", "A_", paste0("_", .x),
                            fixed = TRUE), .cols = starts_with("L")) %>%
  mutate(Afix_win = FALSE, Afix_mov = -Afix_mov) %>% 
  rename_with(.fn =  ~ gsub("A_", "Afix_", .x,
                            fixed = TRUE), .cols = c(A_TeamID, A_Loc, A_Conference)) %>% 
  rename_with(.fn =  ~ gsub("B_", "Bfix_", .x,
                            fixed = TRUE), .cols = c(B_TeamID, B_Loc, B_Conference)) %>% 
  arrange(GameID)

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
eloPred <- function(elo1, elo2, homeAdv = 81, home = 0) {
  return(1 / (1 + 10^((elo2 - (elo1 + home*homeAdv)) / 400)))
}

# Function for calculating updated elo values
eloUpdate <- function(elo, pred, actual, k = 45) {
  return(elo + k*(actual - pred))
}

# Function to reset team elos to average of previous season and conference average
seasonReset <- function(oldElos, teamIDs, center = mean, 
                        confer = confInfo) {
  
  # Conference average elos
  conf <- confer %>% filter(Season == oldElos$Season[1])
  suppressMessages(confAvg <- oldElos %>% 
                     left_join(conf, by = c("team" = "TeamID")) %>% 
                     group_by(Conference) %>% 
                     summarize(eloAvg = center(elo)))
  
  # End of season elos
  endElos <- oldElos %>% rename(TeamID = team, eloEnd = elo)
  
  # Averaging end of season and conference average elos
  suppressMessages(newElos <- endElos %>% left_join(confAvg %>% 
                                                      left_join(confer %>% filter(Season == oldElos$Season[1]))) %>% 
                     mutate(elo =  (eloEnd + eloAvg) / 2) %>% 
                     dplyr::select(TeamID, elo) %>% filter(TeamID %in% teamIDs) %>% 
                     right_join(data.frame(TeamID = teamIDs)) %>% 
                     mutate(elo = ifelse(is.na(elo), 1500, elo)))
  
  return(newElos)
}

# Function for adding elo values to data frame
# scores: A data frame with columns Season, Afix_TeamID,
# Bfix_TeamID, Afix_Loc, Afix_mov, Afix_win
# kVal: Smoothing parameter for elo calculation
# method: One of "NBA" or "NFL". Specifies smoothing method in elo update.
# eloStarts: Optional. Scalar or vector of starting elo values for unique TeamID's
# tau: Second smoothing parameter for elo calculation
addElo <- function(scores, method = "NFL", 
                   kVal = 45, eloStarts = 1500, tau = 0.006,
                   homeAdvantage = 81,
                   centerFun = mean, returnRecent = FALSE) {
  
  # Sorting rows to start
  scores <- scores %>% arrange(Season, DayNum)
  
  seasons <- unique(scores$Season)
  nSeasons <- length(seasons)
  output <- vector("list", nSeasons)
  
  pb <- progress_bar$new(total = nSeasons)
  
  for(s in 1:nSeasons) {
    seasonData <- scores %>% filter(Season == seasons[s])
    teamIDs <- unique(c(seasonData$Afix_TeamID, 
                        seasonData$Bfix_TeamID))
    
    homes <- ifelse(seasonData$Afix_Loc == "N", 0,
                    ifelse(seasonData$Afix_Loc == "A", -1,
                           ifelse(seasonData$Afix_Loc == "H", 1, NA)))
    
    mmNumerator <- sqrt(seasonData$Afix_mov)
    Afix_win10 <- ifelse(seasonData$Afix_win, 1, 0)
    
    elos <- data.frame(team = teamIDs, elo = 1500)
    
    # Initialize elo columns
    seasonData <- seasonData %>% 
      mutate(Afix_elo = 1500, Bfix_elo = 1500)
    elos <- data.frame(team = teamIDs, elo = eloStarts)
    
    if(method == "NBA") {
      for(i in 1:nrow(seasonData)) {
        # Storing current elo values
        Ainds <- elos$team == seasonData$Afix_TeamID[i]
        Binds <- elos$team == seasonData$Bfix_TeamID[i]
        seasonData$Afix_elo[i] <- elos$elo[Ainds]
        seasonData$Bfix_elo[i] <- elos$elo[Binds]
        
        # Elo prediction
        pred <- eloPred(elo1 = seasonData$Afix_elo[i],
                        elo2 = seasonData$Bfix_elo[i], 
                        home = homes[i], homeAdv = homeAdvantage)
        
        # Margin of victory multipliers
        movMultiA <- mmNumerator[i] /
          (7.5 + tau*(seasonData$Afix_elo[i] - seasonData$Bfix_elo[i]))
        
        # Calculating new elo values
        newA <- eloUpdate(elo = seasonData$Afix_elo[i], pred = pred,
                          actual = Afix_win10[i], k = movMultiA*kVal)
        newB <- seasonData$Bfix_elo[i] - (newA - seasonData$Afix_elo[i])
        
        # Updating elo values
        elos$elo[Ainds] <- newA
        elos$elo[Binds] <- newB
      }
    } else if(method == "NFL"){
      
      ks <- kVal*log(1 + abs(seasonData$Afix_mov))
      
      for(i in 1:nrow(seasonData)) {
        # Storing current elo values
        Ainds <- elos$team == seasonData$Afix_TeamID[i]
        Binds <- elos$team == seasonData$Bfix_TeamID[i]
        seasonData$Afix_elo[i] <- elos$elo[Ainds]
        seasonData$Bfix_elo[i] <- elos$elo[Binds]
        
        # Elo prediction
        pred <- eloPred(elo1 = seasonData$Afix_elo[i],
                        elo2 = seasonData$Bfix_elo[i], 
                        home = homes[i], homeAdv = homeAdvantage)
        
        # Calculating new elo values
        newA <- eloUpdate(elo = seasonData$Afix_elo[i], pred = pred,
                          actual = Afix_win10[i], k = ks[i])
        newB <- seasonData$Bfix_elo[i] - (newA - seasonData$Afix_elo[i])
        
        # Updating elo values
        elos$elo[Ainds] <- newA
        elos$elo[Binds] <- newB
      }
    }
    
    output[[s]] <- seasonData
    
    if(s < nSeasons) {
      newSeason <- scores %>% filter(Season == seasons[s+1])
      newTeams <- unique(c(newSeason$Afix_TeamID, newSeason$Bfix_TeamID))
      
      newElos <- seasonReset(oldElos = elos %>% mutate(Season = seasonData$Season[1]),
                             teamIDs = newTeams, center = centerFun)
      
      eloStarts <- newElos %>% pull(elo)
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
eloSim <- function(method, kVal = 45, tau, centerName) {
  
  if(centerName == "mean") {
    centerFun <- mean
  } else if(centerName == "median"){
    centerFun <- median
  }
  
  # Calculating elo values across all seasons
  fullElo1 <- addElo(scores = fullRaw1, 
                     eloStarts = 1500, 
                     method = method, 
                     kVal = kVal, tau = tau,
                     centerFun = centerFun)
  
  # Full performance of calculated elo scores for MOV
  eloMod1 <- lm(Afix_mov ~ 0 + Afix_elo + Bfix_elo, 
                data = fullElo1)
  r2Val <- summary(eloMod1)$r.squared
  
  # Later in season performance of calculated elo scores for MOV
  eloModNew <- lm(Afix_mov ~ 0 + Afix_elo + Bfix_elo, 
                  data = fullElo1 %>% filter(DayNum >= 50))
  r2ValNew <- summary(eloModNew)$r.squared
  
  # Binary win / loss response
  eloShuffled <- fullElo1 %>% 
    mutate(Afix_win2 = ifelse(Afix_mov %% 2 == 0, FALSE, TRUE),
           Afix_elo2 = ifelse(Afix_mov %% 2 == 0, Bfix_elo, Afix_elo),
           Bfix_elo2 = ifelse(Afix_mov %% 2 == 0, Afix_elo, Bfix_elo)) %>% 
    dplyr::select(DayNum, Afix_win2, Afix_elo2, Bfix_elo2)
  
  # Full AIC
  eloMod2 <- glm(Afix_win2 ~ 0 + Afix_elo2 + Bfix_elo2, 
                 data = eloShuffled, family = "binomial")
  logisticAIC <- summary(eloMod2)$aic
  
  # Later in season AIC
  eloMod2New <- glm(Afix_win2 ~ 0 + Afix_elo2 + Bfix_elo2, 
                    data = eloShuffled %>% filter(DayNum >= 50), family = "binomial")
  logisticAICNew <- summary(eloMod2New)$aic
  
  return(data.frame(method = method, kVal = kVal, tau = tau,
                    centerFun = centerName, r2 = r2Val, r2ValNew = r2ValNew,
                    AIC = logisticAIC, logisticAICNew = logisticAICNew))
}

if(runSim == TRUE) {
  
  # Grid of tuning arameters
  params <- expand.grid(kVal = seq(35, 60, by = 5),
                        tau = 0.002,
                        method = c("NFL"),
                        centerName = "mean")
  
  # Finding optimal tuning parameters
  plan(multisession, workers = 3)
  eloSimRes <- future_pmap_dfr(.l = params, .f = eloSim, .progress = TRUE)
  
  # Saving results
  saveRDS(eloSimRes, paste0(tourneyYear, ifelse(womens, "-Womens/", "/"), "eloSimRes.rds"))
  eloSimRes <- readRDS(paste0(tourneyYear, ifelse(womens, "-Womens/", "/"), "eloSimRes.rds"))
  
  # Plotting results
  eloSimRes %>% pivot_longer(cols = r2:logisticAICNew, values_to = "Value",
                             names_to = "Metric") %>% 
    filter(centerFun == "mean") %>% ggplot(aes(x = kVal, y = Value, color = tau)) + 
    geom_point() + facet_grid(Metric ~ method, scales = "free_y")
  
  # Table of optimal values
  eloSimRes %>% pivot_longer(cols = r2:logisticAICNew, values_to = "Value",
                             names_to = "Metric") %>% 
    mutate(Value = ifelse(Metric %in% c("r2", "r2ValNew"), Value, -Value)) %>% 
    arrange(kVal) %>% group_by(centerFun, method, Metric) %>% slice_max(order_by = Value, n = 1, with_ties = FALSE)
}

# Calculating elo values across all seasons
eloRes <- addElo(scores = fullRaw1, method = "NFL", kVal = 45,
                   eloStarts = 1500, centerFun = mean,
                   homeAdvantage = 81, returnRecent = ifelse(stage == 2, TRUE, FALSE))

if(stage == 2) {
newElos <- eloRes$newElos
fullElo1 <- eloRes$eloData
} else {
  fullElo1 <- eloRes
}

# Performance of calculated elo scores
eloMod1 <- lm(Afix_mov ~ 0 + Afix_elo + Bfix_elo, 
              data = fullElo1)
r2Val <- summary(eloMod1)$r.squared

# Binary win / loss response
eloShuffled <- fullElo1 %>% 
  mutate(Afix_win2 = ifelse(Afix_mov %% 2 == 0, FALSE, TRUE),
         Afix_elo2 = ifelse(Afix_mov %% 2 == 0, Bfix_elo, Afix_elo),
         Bfix_elo2 = ifelse(Afix_mov %% 2 == 0, Afix_elo, Bfix_elo)) %>% 
  dplyr::select(Afix_win2, Afix_elo2, Bfix_elo2)

eloMod2 <- glm(Afix_win2 ~ 0 + Afix_elo2 + Bfix_elo2, 
               data = eloShuffled, family = "binomial")

logisticAIC <- summary(eloMod2)$aic

# Fixed elos drifting higher using Nate Silver's 
# advice of reducing MOV multiplier for heavier favorites: https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/
fullElo1 %>% group_by(Season) %>% 
  summarize(Avg = mean(Afix_elo), Min = min(Afix_elo), Max = max(Afix_elo)) %>% 
  pivot_longer(cols = Avg:Max, names_to = "Metric", values_to = "Value") %>% 
  ggplot(aes(x = Season, y = Value, color = Metric)) + 
  geom_line(aes(x = Season)) + labs(title = "Elo Across Seasons") + 
  theme_bw()

# Adding in team name
teamNames <- read_csv(paste0(stageDir, ifelse(womens, "W", "M"), "Teams.csv")) %>% 
  dplyr::select(TeamID, TeamName)

# Visualizing elo across season
if(FALSE) {
eloSpaghetti <- fullElo1 %>% filter(Season > 2015) %>% left_join(teamNames, by = c("Afix_TeamID" = "TeamID")) %>% 
  dplyr::mutate(Afix_elo = round(Afix_elo)) %>% 
  dplyr::rename(Team = TeamName, Day = DayNum, ELO = Afix_elo) %>% 
  ggplot(aes(y = ELO, x = Day, color = Team)) + 
  geom_line() + facet_grid(rows = vars(Season)) +
  labs(title = "Team Elos by Season", y = "Elo", x = "Day of Season") + theme_bw() +
  theme(legend.position = "none") 

eloSpaghetti

plotly::ggplotly(eloSpaghetti)
}

# Rolling Team Statistics -------------------------------------------------

fullElo2 <- fullRaw2 %>% mutate(Afix_elo = fullElo1$Bfix_elo,
                                Bfix_elo = fullElo1$Afix_elo)

# Combine into single data set with 2 rows for each game; 
# 1 where loser is Team A and 1 where loser is Team B
longElo <- bind_rows(fullElo1, fullElo2) %>% 
  group_by(Season, Afix_TeamID) %>% arrange(DayNum) %>% 
  mutate(Afix_count = seq(n()), across(.cols = starts_with(c("A_", "B_")),
                                       .fns = ~ cumsum(.x) / Afix_count)) %>% 
  dplyr::select(Season, DayNum, GameID, Afix_count, starts_with(c("Afix_", "Bfix_")),
         starts_with(c("A_", "B_")), -Bfix_Loc) %>% 
  mutate(across(starts_with(c("A_", "B_")), ~ lag(.x))) %>% ungroup() %>% 
  group_by(Season, Bfix_TeamID) %>% mutate(Bfix_count = seq(n())) %>% ungroup()

# Including both 'for' and 'against' info for each game, rather than just 'for'
wideElo <- longElo %>% 
  rename_with(.fn = ~ gsub("A_", "A_for_", .x,
                           fixed = TRUE), .cols = starts_with("A_")) %>% 
  rename_with(.fn = ~ gsub("B_", "A_against_", .x,
                           fixed = TRUE), .cols = starts_with("B_")) %>% 
  arrange(GameID) %>% mutate(Ref = ifelse(Afix_mov > 0, "-A", "-B")) %>% 
  pivot_wider(id_cols = c(Season, DayNum, GameID), 
              values_from = starts_with(c("A_", "B_")), 
              names_from = Ref) %>% 
  rename_with(.fn =  ~ gsub("_-A", "", .x, fixed = TRUE), .cols = ends_with("_-A")) %>% 
  rename_with(.fn =  ~ gsub("A_", "B_", gsub("_-B", "", .x, fixed = TRUE), fixed = TRUE),
              .cols = ends_with("_-B")) %>% left_join(longElo %>% 
                                                        filter(Afix_mov > 0) %>% dplyr::select(-starts_with(c("A_", "B_")))) %>% 
  dplyr::select(Season, DayNum, GameID, starts_with(c("Afix_", "Bfix_")), everything())

t2 <- Sys.time()

t2 - t1

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
  conf <- confer %>% filter(Season == oldMets$Season[1])
  suppressMessages(confAvg <- oldMets %>% 
                     left_join(conf, by = c("team" = "TeamID",
                                            "Season" = "Season")) %>% 
                     group_by(Conference) %>% 
                     summarize(adjMetricOffAvg = center(adjMetricOff),
                               adjMetricDefAvg = center(adjMetricDef)))
  
  # End of season metrics
  endMets <- oldMets %>% rename(TeamID = team, 
                                adjMetricOffEnd = adjMetricOff,
                                adjMetricDefEnd = adjMetricDef)
  
  # Averaging end of season and conference average metrics
  suppressMessages(newMets <- endMets %>% left_join(confAvg %>% 
                                                      left_join(confer %>% filter(Season == oldMets$Season[1]))) %>% 
                     mutate(adjMetricOff = (adjMetricOffEnd + adjMetricOffAvg) / 2,
                            adjMetricDef = (adjMetricDefEnd + adjMetricDefAvg) / 2) %>% 
                     dplyr::select(TeamID, adjMetricOff, adjMetricDef) %>%
                     filter(TeamID %in% teamIDs) %>% 
                     right_join(data.frame(TeamID = teamIDs)) %>% 
                     mutate(adjMetricOff = ifelse(is.na(adjMetricOff), mStart$adjMetricOff, adjMetricOff),
                            adjMetricDef = ifelse(is.na(adjMetricDef), mStart$adjMetricDef, adjMetricDef)))
  
  return(newMets)
}

# Function for adding opponent-adjusted metric values to data frame
# boxscores: A data frame with columns Season, DayNum, Afix_TeamID,
# Bfix_TeamID, Afix_Loc, A_metric, B_metric
# steep: Smoothing parameter for metric calculation
# kVal: Second smoothing parameter for metric calculation
# mStarts: Optional. Data frame with columns 
# offensive (adjMetricOff) and defensive (adjMetricDef) initial adjusted metric values for unique TeamID's
# tau: Second smoothing parameter for metric calculation
addMetric <- function(boxscores, 
                      steepness = 2, 
                      kfloor = 0.20, 
                   startOff = 1, 
                   startDef = 1, 
                   homeAdvantage = 0.03,
                   centerFun = mean, returnRecent = FALSE) {
  
  # Sorting rows to start
  boxscores <- boxscores %>% dplyr::arrange(Season, DayNum)
  
  seasons <- unique(boxscores$Season)
  nSeasons <- length(seasons)
  output <- vector("list", nSeasons)
  
  newTeams <- unique(c(dplyr::pull(dplyr::filter(boxscores, Season == seasons[1]), Afix_TeamID), 
                       dplyr::pull(dplyr::filter(boxscores, Season == seasons[1]), Bfix_TeamID)))
  
  # Starting values
  mStarts <- data.frame(TeamID = newTeams,
                        adjMetricOff = startOff, adjMetricDef = startDef)
  
  pb <- progress::progress_bar$new(total = nSeasons)
  
  for(s in 1:nSeasons) {
    seasonData <- boxscores %>% dplyr::filter(Season == seasons[s])
    
    homes <- ifelse(seasonData$Afix_Loc == "N", 0,
                    ifelse(seasonData$Afix_Loc == "A", -1,
                           ifelse(seasonData$Afix_Loc == "H", 1, NA)))
    
    mets <- data.frame(team = newTeams, adjMetricOff = mStarts$adjMetricOff, 
                       adjMetricDef = mStarts$adjMetricDef)
    
    # Initialize metric columns
    seasonData <- seasonData %>% 
      dplyr::left_join(mStarts, by = c("Afix_TeamID" = "TeamID")) %>% 
      dplyr::rename(A_adjMetricOff = adjMetricOff,
                    A_adjMetricDef = adjMetricDef) %>% 
      dplyr::left_join(mStarts, by = c("Bfix_TeamID" = "TeamID")) %>% 
      dplyr::rename(B_adjMetricOff = adjMetricOff,
                    B_adjMetricDef = adjMetricDef) 
    
    seasonData <- seasonData %>% 
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
                           homeAdv = homeAdvantage)
        
        predB <- metricPred(mOff = seasonData$B_adjMetricOff[i],
                            mDef = seasonData$A_adjMetricDef[i], 
                            home = homes[i]*(-1), 
                            homeAdv = homeAdvantage)
        
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
    
    if(s < nSeasons) {
      newSeason <- boxscores %>% dplyr::filter(Season == seasons[s+1])
      newTeams <- unique(c(newSeason$Afix_TeamID, newSeason$Bfix_TeamID))
      
      newMets <- metricSeasonReset(oldMets = mets %>% mutate(Season = seasonData$Season[1]),
                             teamIDs = newTeams, center = centerFun)
      
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
fullRaw1 %>% 
  mutate(A_metric = A_Score / A_Poss,
         B_metric = B_Score / B_Poss) %>% 
  group_by(Afix_Loc) %>% 
  summarize(A_metric = mean(A_metric), B_metric = mean(B_metric))

# Calculating adjusted metric values across all seasons
bs <- fullRaw1 %>% 
  mutate(A_metric = A_Score / A_Poss,
         B_metric = B_Score / B_Poss) %>% 
  select(Season, DayNum, Afix_TeamID, Bfix_TeamID, Afix_Loc, Afix_mov, Afix_win, 
         A_metric, B_metric) %>% 
  group_by(Season, Afix_TeamID) %>% arrange(DayNum) %>% 
  mutate(Afix_count = seq(n())) %>% ungroup() %>% 
  group_by(Season, Bfix_TeamID) %>% arrange(DayNum) %>% 
  mutate(Bfix_count = seq(n())) %>% 
  filter(Season >= 2010)

# Detrending Scoring Efficiencies -----------------------------------------

# Looking at rolling average SE across season (is there upward drift?)
winLength <- 3
roll_mean_na_rm <- tibbletime::rollify(~mean(.x, na.rm = TRUE), window = winLength)

# Offensive rolling avg SE
rollMeansOff <- bs %>% group_by(Season, Afix_TeamID) %>% 
  summarize(nGames = n(), A_metric = A_metric) %>% 
  filter(nGames >= winLength) %>% 
  summarize(SErollingOff = roll_mean_na_rm(A_metric),
            Afix_count = 1:n()) %>% ungroup() %>% 
  group_by(Season, Afix_count) %>% 
  summarize(SEoff = mean(SErollingOff, na.rm = TRUE))

# Defensive rolling avg SE
rollMeansDef <- bs %>% group_by(Season, Bfix_TeamID) %>% 
  summarize(nGames = n(), B_metric = B_metric) %>% 
  filter(nGames >= winLength) %>% 
  summarize(SErollingDef = roll_mean_na_rm(B_metric),
            Bfix_count = 1:n()) %>% ungroup() %>% 
  group_by(Season, Bfix_count) %>% 
  summarize(SEdef = mean(SErollingDef, na.rm = TRUE))

# Plotting average of rolling average of offensive scoring efficiency
offGG <- rollMeansOff %>%  
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
defGG <- rollMeansDef %>%  
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
  
  data3Dclean <- data3D %>% tidyr::drop_na() %>% 
    dplyr::rename("x" = 1, "y" = 2, "z" = 3)
  
  suppressWarnings(grid <- akima::interp(dplyr::pull(data3Dclean, 1), 
                                         dplyr::pull(data3Dclean, 2),
                                         dplyr::pull(data3Dclean, 3),
                                         duplicate = dupes))
  
  griddf <- data.frame(x = rep(grid$x, ncol(grid$z)), 
                       y = rep(grid$y, each = nrow(grid$z)), 
                       z = as.numeric(grid$z))
  
  myVars <- colnames(data3D)
  
  griddf %>%
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
            data = rollMeansOff %>% filter(Afix_count <= 30, Season >= 2010))

seModDef <- lm(SEdef ~ 1 + Bfix_count + I(Bfix_count^2) + factor(Season), 
               data = rollMeansDef %>% filter(Bfix_count <= 30, Season >= 2010))

coeffResOff <- broom::tidy(seModOff)
coeffResDef <- broom::tidy(seModDef)

# Based on p-values, the trend is significant, and the average
# SE varies by season, but the trend does not vary by season

# Detrending scoring efficiencies
vanillaGames <- expand_grid(Afix_count = 1:30, Bfix_count = 1:30,
                            Season = min(seModOff$xlevels$`factor(Season)`):max(seModOff$xlevels$`factor(Season)`))
vanillaGames$expected_met_off <- predict(seModOff, newdata = vanillaGames)
vanillaGames$expected_met_def <- predict(seModDef, newdata = vanillaGames)

bsDetrended <- bs %>% left_join(vanillaGames, by = c("Season", "Afix_count", "Bfix_count")) %>% 
  mutate(A_metric = A_metric - expected_met_off,
         B_metric = B_metric - expected_met_def) %>% 
  dplyr::select(-expected_met_off, -expected_met_def)

rollMeansDetrendedOff <- bsDetrended %>% group_by(Season, Afix_TeamID) %>% 
  summarize(nGames = n(), A_metric = A_metric) %>% 
  filter(nGames >= winLength) %>% 
  summarize(SErolling = roll_mean_na_rm(A_metric),
            Afix_count = 1:n()) %>% ungroup() %>% 
  group_by(Season, Afix_count) %>% 
  summarize(SE = mean(SErolling, na.rm = TRUE))

# Plotting average of rolling average of offensive scoring efficiency
rollMeansDetrendedOff %>% 
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
kVal = 0.10
startOff = 1
startDef = 1
centerFun = mean
homeAdvantage = 0.05 
returnRecent = FALSE
}

# Trying using points per possessoion (offensive efficiency)
metRes <- addMetric(boxscores = bs, 
                    kVal = 1,
                    startOff = 1.02,
                    startDef = 1.02, 
                    centerFun = mean,
                    homeAdvantage = 0.03, 
                    returnRecent = FALSE)

# Plotting as sanity check
metRes %>% pivot_longer(cols = c(A_adjMetricOff, A_adjMetricDef, B_adjMetricOff, B_adjMetricDef),
                        names_to = "Category", values_to = "ScoringEfficiency") %>% 
  mutate(Category = ifelse(str_detect(Category, pattern = "Off"), "Offensive", "Defensive")) %>% 
  group_by(Season, Category) %>% 
  summarize(Avg = mean(ScoringEfficiency), Min = min(ScoringEfficiency), Max = max(ScoringEfficiency)) %>% 
  pivot_longer(cols = Avg:Max, names_to = "Metric", values_to = "Value") %>% 
  ggplot(aes(x = Season, y = Value, color = Metric, linetype = Category)) + 
  geom_line(aes(x = Season)) + labs(title = "Scoring Efficiency Across Seasons") + 
  theme_bw()
}

# Massey Ordinal Rankings -------------------------------------------------

if(womens == FALSE) {
  # Adding in Massey ordinal ranking data
  # Descriptions / info for systems: https://masseyratings.com/cb/compare.htm
  massey <- data.table::fread(paste0(stageDir, "MMasseyOrdinals.csv")) %>% 
    as.data.frame() %>% dplyr::rename(DayNum = RankingDayNum) 
  
  # Exploring which systems have collective most complete data
  # The `values_fn` option keeps first ranking when multiple for single day provided.
  # Filling down so ratings carry forward if provided day before game or so
  # Names from https://masseyratings.com/cb/compare.htm
  wideMassey <- massey %>% 
    mutate(SystemName = case_when(SystemName == "MOR" ~ "Moore",
                                  SystemName == "SAG" ~ "Sagarin",
                                  SystemName == "MAS" ~ "Massey",
                                  SystemName == "POM" ~ "Pomeroy",
                                  TRUE ~ SystemName)) %>% 
    pivot_wider(id_cols = c(Season, DayNum, TeamID), 
                names_from = SystemName, values_from = OrdinalRank, values_fill = NA, 
                values_fn = function(x){x[1]}) %>% 
    arrange(Season, DayNum) %>% group_by(Season, TeamID) %>% 
    fill(SEL:REI, .direction = "down") %>% ungroup()
  
  # Exploring pattern of missing ratings.
  # Imputation for ratings helps retain large amount of data.
  # In preliminary modelling Massey ratings not important, so drop here to retain ~ 3000 more games
  wideMassey %>% dplyr::select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey) %>% 
    md.pattern()
  
  # Imputing for ordinal rankings when 1 of 4 are missing. 
  # Can use rowSums since Season, DayNum, & TeamID are all complete
  masseyMiss <- wideMassey %>% dplyr::select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin) %>% 
    filter((is.na(Pomeroy) + is.na(Moore) + is.na(Sagarin)) < 2)
  
  # Impute ordinal ratings when 1 of 4 is missing using random forest
  masseyImpute <- bind_cols(masseyMiss %>% dplyr::select(-c(Pomeroy:Sagarin)), 
                            masseyMiss %>% dplyr::select(Pomeroy:Sagarin) %>% 
                              mice(defaultMethod = "rf", seed = 1994, m = 1, maxit = 1) %>% 
                              complete())
  
  # Adding ordinal ratings to box scores and elo data
  # Carry last rating forward for each team in each season via the fill function
  eloMassey <- wideElo %>% full_join(masseyImpute %>% 
                                       dplyr::select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin),
                                     by = c("Season" = "Season", "DayNum" = "DayNum",
                                            "Afix_TeamID" = "TeamID")) %>% 
    rename(Afix_Pomeroy = Pomeroy, Afix_Moore = Moore, 
           Afix_Sagarin = Sagarin) %>% 
    full_join(masseyImpute %>% 
                dplyr::select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin),
              by = c("Season" = "Season", "DayNum" = "DayNum",
                     "Bfix_TeamID" = "TeamID")) %>% 
    rename(Bfix_Pomeroy = Pomeroy, Bfix_Moore = Moore, 
           Bfix_Sagarin = Sagarin) %>% 
    arrange(Season, DayNum) %>% group_by(Season, Afix_TeamID) %>% 
    tidyr::fill(c(Afix_Pomeroy, Afix_Moore, Afix_Sagarin), .direction = "down") %>% 
    ungroup() %>% group_by(Season, Bfix_TeamID) %>% 
    tidyr::fill(c(Bfix_Pomeroy, Bfix_Moore, Bfix_Sagarin), .direction = "down") %>% 
    ungroup() %>% drop_na()
  
  # Testing to see if columns seem reasonable
  
  # Randomly dplyr::selecting some games to have Team A lose
  eloMasseyMix <- eloMassey %>% filter(Afix_mov %% 2 == 1) %>% 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Afix_" = "Xfix_", "A_" = "X_",
                                                        "Bfix_" = "Yfix_", "B_" = "Y_")),
                .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc"))) %>% 
    bind_rows(eloMassey %>% filter(Afix_mov %% 2 == 0) %>% 
                mutate(Afix_mov = -Afix_mov, Afix_win = !Afix_win,
                       Afix_Loc = case_when(Afix_Loc == "A" ~ "H",
                                            Afix_Loc == "H" ~ "A",
                                            TRUE ~ Afix_Loc)) %>% 
                rename_with(.fn =  ~ str_replace_all(string = .x, c("Bfix_" = "Xfix_", "B_" = "X_",
                                                                    "Afix_" = "Yfix_", "A_" = "Y_")),
                            .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc")))) %>% 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Xfix_" = "Afix_", "X_" = "A_",
                                                        "Yfix_" = "Bfix_", "Y_" = "B_")),
                .cols = everything())
  
  summary(lm(Afix_mov ~ 0 + Afix_Loc + Afix_Pomeroy + Afix_Moore + Afix_Sagarin + 
               Bfix_Pomeroy + Bfix_Moore + Bfix_Sagarin + 
               Afix_elo + Bfix_elo, 
             data = eloMasseyMix))
}

# Adding in pre-season AP rank --------------------------------------------

if(womens == FALSE) {
  # Preseason poll data from sports-reference: https://www.sports-reference.com/cbb/seasons/2003-polls.html
  # AP rankings from Kaggle did not seem to have preseason AP rankings for teams
  apPres <- list.files("AP/") %>% str_subset(pattern = "csv") %>% map_dfr(.f = function(myFile) {
    season <- str_sub(str_split(myFile, pattern = "-")[[1]][2], start = 1, end = 4)
    ret <- read_csv(paste0("AP/", myFile), 
                    skip = 2, show_col_types = FALSE) %>% dplyr::select(School, Pre) %>% drop_na() %>% 
      mutate(Season = as.numeric(season))
  }) %>% mutate(TeamNameSpelling = tolower(School)) %>% 
    left_join(read_csv(paste0(stageDir, ifelse(womens, "W", "M"), "TeamSpellings.csv"))) %>% 
    rename(AP = Pre) %>% dplyr::select(Season, TeamID, AP)
  
  # Imputing initial AP ranks using Massey ordinals
  apMiss <- masseyImpute %>% arrange(Season, DayNum) %>% group_by(Season, TeamID) %>%
    slice(1) %>% ungroup() %>% full_join(apPres) %>% group_by(Season) %>% 
    mutate(AggRank = rank(Pomeroy+Moore+Sagarin)) %>% ungroup() %>% 
    mutate(AP = case_when(is.na(AP) ~ AggRank, 
                          TRUE ~ AP)) %>% dplyr::select(Season, TeamID, AP)
  
  # Adding into full data
  eloMasseyAP <- eloMassey %>% 
    left_join(apMiss, by = c("Season" = "Season",  "Afix_TeamID" = "TeamID")) %>% 
    left_join(apMiss, by = c("Season" = "Season",  "Bfix_TeamID" = "TeamID")) %>% 
    rename(Afix_AP = AP.x, Bfix_AP = AP.y)
  
  # Randomly dplyr::selecting some games to have Team A lose
  eloMasseyAPMix <- eloMasseyAP %>% filter(Afix_mov %% 2 == 1) %>% 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Afix_" = "Xfix_", "A_" = "X_",
                                                        "Bfix_" = "Yfix_", "B_" = "Y_")),
                .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc"))) %>% 
    bind_rows(eloMasseyAP %>% filter(Afix_mov %% 2 == 0) %>% 
                mutate(Afix_mov = -Afix_mov, Afix_win = !Afix_win,
                       Afix_Loc = case_when(Afix_Loc == "A" ~ "H",
                                            Afix_Loc == "H" ~ "A",
                                            TRUE ~ Afix_Loc)) %>% 
                rename_with(.fn =  ~ str_replace_all(string = .x, c("Bfix_" = "Xfix_", "B_" = "X_",
                                                                    "Afix_" = "Yfix_", "A_" = "Y_")),
                            .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc")))) %>% 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Xfix_" = "Afix_", "X_" = "A_",
                                                        "Yfix_" = "Bfix_", "Y_" = "B_")),
                .cols = everything())
  
  # Removing first G games of the season for each team
  # e.g., G=1 means exclude games where it is any teams first game of season
  G <- 3
  smallEloMasseyAPMix <- eloMasseyAPMix %>% filter(Afix_count > G, Bfix_count > G)
  eloMasseyAPMix <- smallEloMasseyAPMix
  
  summary(lm(Afix_mov ~ Afix_elo + Bfix_elo, 
             data = smallEloMasseyAPMix))
  
} else {
  # Randomly dplyr::selecting some games to have Team A lose
  eloMasseyAPMix <- wideElo %>% filter(Afix_mov %% 2 == 1) %>% 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Afix_" = "Xfix_", "A_" = "X_",
                                                        "Bfix_" = "Yfix_", "B_" = "Y_")),
                .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc"))) %>% 
    bind_rows(wideElo %>% filter(Afix_mov %% 2 == 0) %>% 
                mutate(Afix_mov = -Afix_mov, Afix_win = !Afix_win,
                       Afix_Loc = case_when(Afix_Loc == "A" ~ "H",
                                            Afix_Loc == "H" ~ "A",
                                            TRUE ~ Afix_Loc)) %>% 
                rename_with(.fn = ~ str_replace_all(string = .x, c("Bfix_" = "Xfix_", "B_" = "X_",
                                                                    "Afix_" = "Yfix_", "A_" = "Y_")),
                            .cols = -any_of(c("Afix_mov", "Afix_win", "Afix_Loc")))) %>% 
    rename_with(.fn =  ~ str_replace_all(string = .x, c("Xfix_" = "Afix_", "X_" = "A_",
                                                        "Yfix_" = "Bfix_", "Y_" = "B_")),
                .cols = everything())
  
  # Removing first G games of the season for each team
  # e.g., G=1 means exclude games where it is any teams first game of season
  G <- 3
  eloMasseyAPMix <- eloMasseyAPMix %>% filter(Afix_count > G, Bfix_count > G)
  
  summary(lm(Afix_mov ~ Afix_elo + Bfix_elo, 
             data = eloMasseyAPMix))
}

# Add Pythagorean Winning Percentage --------------------------------------

# Formula from Ken Pom: https://kenpom.com/blog/ratings-glossary/
# Ken Pom uses 11.5 for the exponent, but there is not one correct value as others use 13.91
# Also adding in Luck: difference between actual winning % & Pythagorean expected winning %
expValue <- 11.5
eloMasseyAPMix <- eloMasseyAPMix %>% 
mutate(Afix_Pyth = (A_for_Score_Eff^expValue) / (A_for_Score_Eff^expValue + A_against_Score_Eff^expValue),
       Bfix_Pyth = (B_for_Score_Eff^expValue) / (B_for_Score_Eff^expValue + B_against_Score_Eff^expValue),
       Afix_Luck = A_for_win_rate - Afix_Pyth,
       Bfix_Luck = B_for_win_rate - Bfix_Pyth)

# Sanity Checks ---------------------------------------------

# Sanity check for data leakage or other issues using cv.glmnet

# Creating design matrix
designMat <- eloMasseyAPMix %>% dplyr::select(-Afix_count, -Bfix_count,
                                       -Season, -DayNum, -GameID, -Afix_TeamID,
                                       -Bfix_TeamID, -Afix_Conference,
                                       -Bfix_Conference, -Afix_mov) %>% 
  model.matrix(object = formula(Afix_win ~ 0 + .)) %>% 
  as.data.frame() %>% 
  mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
  as.matrix()

# Fitting lasso-penalized model
library(glmnet)
set.seed(1994)
cvModel <- cv.glmnet(x = designMat, y = eloMasseyAPMix$Afix_win,
                     family = "binomial", type.measure = "class",
                     nfolds = 10)

plot(cvModel)
coef(cvModel, s = "lambda.min")

# Important predictors from lasso model (kept for both A and B and for and against):
fitRes <- data.frame(Predictor = coef(cvModel, s = "lambda.min") %>%
                       as.matrix() %>% as.data.frame() %>% rownames(),
                     Value = coef(cvModel, s = "lambda.min") %>% as.matrix() %>%
                       as.data.frame() %>% unlist()) %>% filter(abs(Value) > 0.00001)

preds <- fitRes %>% pull(Predictor) %>%
  str_split(pattern = "A_|B_|Afix|Bfix") %>% unlist() %>%
  trimws(whitespace = "_") %>% table() %>% as.data.frame() %>%
  arrange(desc(Freq)) %>% filter(`.` != "")

# Since predictors are standardized, size of coefficients indicates "importance"
predSummary <- tibble(Pred = names(coef(cvModel, s = "lambda.min")[, 1]), 
                      Coeff = coef(cvModel, s = "lambda.min")[, 1]) %>% 
  arrange(desc(abs(Coeff)))

if(womens == FALSE) {
  # Creating first-order design matrix for model fitting
  designMatFit <- eloMasseyAPMix %>% 
    dplyr::select(Afix_win, Afix_Loc, 
           A_for_Ast, B_for_Ast, A_against_Ast, B_against_Ast,
           A_for_OR, B_for_OR, A_against_OR, B_against_OR, 
           A_against_FGA, B_against_FGA,
           A_for_Blk, B_for_Blk, 
           A_for_Stl, B_for_Stl,
           A_for_Score, B_for_Score,
           Afix_AP, Bfix_AP, Afix_elo, Bfix_elo,
           Afix_Moore, Bfix_Moore, Afix_Pomeroy, Bfix_Pomeroy,
           Afix_Sagarin, Bfix_Sagarin, 
           A_for_TO, B_for_TO,
           A_against_FTA, B_against_FTA, 
           A_against_DR, B_against_DR) %>%  
    model.matrix(object = formula(Afix_win ~ 0 + .)) %>% 
    as.data.frame() %>% 
    mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
    as.matrix()
  
  # Creating second-order design matrix for model fitting
  designMatFit2 <- eloMasseyAPMix %>% 
    dplyr::select(Afix_win, Afix_Loc, 
           A_for_Ast, B_for_Ast, A_against_Ast, B_against_Ast,
           A_for_OR, B_for_OR, A_against_OR, B_against_OR, 
           A_against_FGA, B_against_FGA,
           A_for_Blk, B_for_Blk, 
           A_for_Stl, B_for_Stl,
           A_for_Score, B_for_Score,
           Afix_AP, Bfix_AP, Afix_elo, Bfix_elo,
           Afix_Moore, Bfix_Moore, Afix_Pomeroy, Bfix_Pomeroy,
           Afix_Sagarin, Bfix_Sagarin, 
           A_for_TO, B_for_TO,
           A_against_FTA, B_against_FTA, 
           A_against_DR, B_against_DR) %>%  
    model.matrix(object = formula(Afix_win ~ 0 + .^2)) %>% 
    as.data.frame() %>% 
    mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
    as.matrix()
} else {
  # Creating first-order design matrix for model fitting
  designMatFit <- eloMasseyAPMix %>% 
    dplyr::select(Afix_win, Afix_Loc, Afix_elo,
           Bfix_elo, A_for_Score:B_against_Poss, 
           -contains(c("against_FTM", "against_Score", "against_Poss",
                       "for_FTA", "for_OR", "for_DR", "for_FGA"))) %>%  
    model.matrix(object = formula(Afix_win ~ 0 + .)) %>% 
    as.data.frame() %>% 
    mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
    as.matrix()
  
  # Creating second-order design matrix for model fitting
  designMatFit2 <- eloMasseyAPMix %>% 
    dplyr::select(Afix_win, Afix_Loc, Afix_elo,
           Bfix_elo, A_for_Score:B_against_Poss, 
           -contains(c("against_FTM", "against_Score", "against_Poss",
                       "for_FTA", "for_OR", "for_DR", "for_FGA"))) %>%  
    model.matrix(object = formula(Afix_win ~ 0 + .^2)) %>% 
    as.data.frame() %>% 
    mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
    as.matrix()
}

# Checking predictor effects across time ----------------------------------

# Function to obtain size of coefficients for lasso penalized model to plot over time
predImportance <- function(seasons = 2003, drops = "ratings", modelData = eloMasseyAPMix) {
  
  nseasons <- length(seasons)
  
  if(drops == "ratings") {
    modelData <- modelData %>% 
      dplyr::select(-contains(c("Moore", "Sagarin", "Pomeroy", "AP")))
  } else if(drops == "boxscores") {
    modelData <- modelData %>% 
      dplyr::select(Season:Bfix_count,
                    contains(c("Moore", "Sagarin", "Pomeroy", "AP", "elo")))
  }
  
  # Creating design matrix
  modelMat <- modelData %>% 
    dplyr::filter(Season %in% seasons) %>% 
    dplyr::select(-Afix_count, -Bfix_count,
                  -Season, -DayNum, -GameID, -Afix_TeamID,
                  -Bfix_TeamID, -Afix_Conference,
                  -Bfix_Conference, -Afix_mov) %>% 
    model.matrix(object = formula(Afix_win ~ 0 + .)) %>% 
    as.data.frame() %>% 
    mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
    as.matrix()
  
  # Fitting lasso-penalized model
  set.seed(1994)
  cvMod <- glmnet::cv.glmnet(x = modelMat, 
                               y = modelData %>% 
                                 dplyr::filter(Season %in% seasons) %>% dplyr::pull(Afix_win),
                       family = "binomial", type.measure = "class",
                       nfolds = 10)
  
  # Since predictors are standardized, size of coefficients indicates "importance"
  overallRes <- tibble(Pred = names(coef(cvMod, s = "lambda.min")[, 1]), 
                        Coeff = coef(cvMod, s = "lambda.min")[, 1]) %>% 
    arrange(desc(abs(Coeff))) %>%
    mutate(Pred = str_remove_all(str_sub(Pred, start = 2, end = -1),
                                 pattern = "fix_"),
           Pred = case_when(str_sub(Pred, 1, 1) == "_" ~ str_sub(Pred, start = 2, end = -1),
                            TRUE ~ Pred)) %>% 
    group_by(Pred) %>% dplyr::summarize(Size = mean(abs(Coeff))) %>% 
    ungroup() %>% mutate(Season = ifelse(nseasons == 1, seasons, 
                                         paste0(min(seasons), "-", max(seasons)))) %>% 
    dplyr::filter(!str_detect(Pred, pattern = "Intercept"))
  
  return(list(overallRes = overallRes, 
              medianAccuracy = 1 - median(cvMod[["cvm"]]),
              meanAccuracy = 1 - mean(cvMod[["cvm"]])))
}

# Joint importance of boxscore metrics
boxscoreImps <- purrr::map(.f = predImportance, .x = min(eloMasseyAPMix$Season):max(eloMasseyAPMix$Season),
                        drops = "ratings", modelData = eloMasseyAPMix)

# Function for plotting predictor & model performances across seasons
impPlotter <- function(impRes = boxscoreImps, nPreds = NULL, seasons = "all") {
  
  # Tidy summary output
  modSummary <- map_dfr(.x = impRes, .f = function(x) {
    ret <- x[["overallRes"]]
    ret$`Full model accuracy` <- x[["meanAccuracy"]]
    return(ret)})
  
  if(length(seasons) > 1) {
    modSummary <- modSummary %>% dplyr::filter(Season %in% seasons)
  }
  
  modSummary <- modSummary %>% 
    dplyr::mutate(Pred = fct_reorder(Pred, Size, .fun = mean, .desc = TRUE),
                  Pred = fct_recode(Pred, "Away" = "LocA", "Home" = "LocH",
                                    "Neutral" = "LocN"))
  
  # Most important predictors
  preds <- modSummary %>% group_by(Pred) %>% dplyr::summarize(Size = mean(Size)) %>% 
    ungroup() %>% dplyr::arrange(desc(Size)) %>% pull(Pred)
  
  if(is.null(nPreds)) {
    nPreds <- length(preds)
  }
  
  # Color palette
  colourCount <- length(unique(modSummary$Pred))
  getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
  set.seed(1994)
  myColors <- sample(getPalette(colourCount), size = colourCount, replace = FALSE)
    
# Scatter plot of accuracy & predictor performance across time
scatGG <- modSummary %>% 
  dplyr::filter(Pred %in% preds[1:nPreds]) %>% 
  ggplot(aes(x = Season, y = Size, color = Pred)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_smooth(aes(x = Season, y = `Full model accuracy`), 
              color = "black", se = FALSE, linetype = "dotted") +
  scale_y_continuous(limits = c(0, 1),
                     sec.axis = dup_axis(name="Model classification accuracy")) +
  scale_x_continuous(breaks = unique(modSummary$Season)) +
  scale_color_manual(values = myColors) +
  geom_point(aes(x = Season, y = `Full model accuracy`), color = "black") +
  labs(title = "Predictor importance & classification accuracy of LASSO GLM",
       subtitle = paste0("Best: ", 
      sprintf("%.3f", round(modSummary %>% slice_max(`Full model accuracy`, n = 1, with_ties = FALSE) %>% pull(`Full model accuracy`), 3)), 
       " in ", 
      modSummary %>% slice_max(`Full model accuracy`, n = 1, with_ties = FALSE) %>% pull(`Season`),
       ", Worst: ", 
      sprintf("%.3f", round(modSummary %>% slice_min(`Full model accuracy`, n = 1, with_ties = FALSE) %>% pull(`Full model accuracy`), 3)), 
       " in ",
      modSummary %>% slice_min(`Full model accuracy`, n = 1, with_ties = FALSE) %>% pull(`Season`)),
       y = "Predictor importance (coefficient size)",
      color = "Predictor") +
  theme_bw() +
  theme(legend.position = "bottom")

# Bar plot of importance values
barGG <- modSummary %>% group_by(Pred) %>% 
  summarize(Size = mean(Size)) %>% ungroup() %>% 
  ggplot(aes(x = fct_reorder(Pred, Size), y = Size,
             fill = Pred)) +
  geom_col(color = "black") +
  scale_fill_manual(values = myColors) +
  labs(x = "Predictor",
       y = "Average predictor importance (coefficient size)",
       caption = "Predictors standardized prior to model fitting") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank())

return(scatGG / barGG)
}

# Plotting for box scores
boxRes <- impPlotter(impRes = boxscoreImps, nPreds = 8, seasons = 2010:2022)
boxRes

if(womens == FALSE) {
  # Joint importance of rating metrics
  ratingsImps <- map(.f = predImportance, .x = min(eloMasseyAPMix$Season):max(eloMasseyAPMix$Season),
                     drops = "boxscores", modelData = eloMasseyAPMix)
  
  # Plotting for box scores
  ratingsRes <- impPlotter(impRes = ratingsImps)
  ratingsRes
  
  impPlotter(impRes = ratingsImps, seasons = 2010:2022)
}

# Saving objects for model fitting ----------------------------------------

# # Fitting lasso-penalized model to try & drop more predictors
# library(glmnet)
# set.seed(1994)
# cvModelwin <- cv.glmnet(x = designMatFit, y = eloMasseyAPMix$Afix_win,
#                      family = "binomial", type.measure = "class",
#                      nfolds = 10)
# 
# cvModelmov <- cv.glmnet(x = designMatFit, y = eloMasseyAPMix$Afix_mov,
#                         family = "gaussian", type.measure = "mse",
#                         nfolds = 10)
# 
# plot(cvModelwin)
# plot(cvModelmov)
# 
# coef(cvModelwin, s = "lambda.min")
# coef(cvModelmov, s = "lambda.min")

# Saving for model fitting
saveRDS(list(Awin = eloMasseyAPMix$Afix_win, 
             MOV = eloMasseyAPMix$Afix_mov, 
             Design = designMatFit, FullData = eloMasseyAPMix), 
        file = paste0(tourneyYear, ifelse(womens, "-Womens/", "/"), 
                      "designResponseFinal.rds"))

saveRDS(list(Awin = eloMasseyAPMix$Afix_win, 
             MOV = eloMasseyAPMix$Afix_mov, 
             Design = designMatFit2, FullData = eloMasseyAPMix), 
        file = paste0(tourneyYear, ifelse(womens, "-Womens/", "/"),
                      "designResponseFinal2.rds"))

# Data for Tournament Predictions -----------------------------------------

if(stage == 2) {
  # Adding empty tournament games to predict for
  matchups <- read_csv(paste0(stageDir, ifelse(womens, "W", "M"), "SampleSubmissionStage2.csv")) %>% 
    mutate(Season = as.integer(map_chr(str_split(ID, pattern = "_"), 1)), 
           Afix_TeamID = as.integer(map_chr(str_split(ID, pattern = "_"), 2)), 
           Bfix_TeamID = as.integer(map_chr(str_split(ID, pattern = "_"), 3))) %>% 
    dplyr::select(-ID, -Pred)
  
  tourneyTeams <- unique(c(matchups$Afix_TeamID, matchups$Bfix_TeamID))
  
  # Data on last games of season for each team
  fullEloLast <- bind_rows(fullElo1, fullElo2) %>% filter(Season == tourneyYear) %>% 
    arrange(DayNum) %>% group_by(Afix_TeamID) %>% mutate(Afix_count = 1:n()) %>% 
    slice_tail(n = 1) %>% ungroup() %>% dplyr::select(-starts_with("Bfix_")) %>% 
    rename_with(.fn =  ~ gsub("A_", "A_for_", .x,
                              fixed = TRUE), .cols = starts_with("A_")) %>% 
    rename_with(.fn =  ~ gsub("B_", "A_against_", .x,
                              fixed = TRUE), .cols = starts_with("B_")) %>% 
    filter(Afix_TeamID %in% tourneyTeams) %>% dplyr::select(-Afix_elo) %>% 
    left_join(newElos %>% rename(Afix_TeamID = team, Afix_elo = elo))
  
  # Data on 2nd to last game of season
  wideEloLast <- wideElo %>% filter(Season == tourneyYear) %>% arrange(DayNum) %>% 
    group_by(Afix_TeamID) %>% slice_tail(n = 1) %>% ungroup() %>% 
    dplyr::select(colnames(fullEloLast)) %>% filter(Afix_TeamID %in% tourneyTeams)
  
  # Final tourney stats for each team except massey ordinals & AP pre-ranks
  tourneyStats <- map_dfr(.x = tourneyTeams, .f = function(tteam) {
    lastGame <- fullEloLast %>% filter(Afix_TeamID == tteam) %>% 
      mutate(across(starts_with("A_"), ~ .x*1/Afix_count))
    
    nGames <- lastGame$Afix_count
    
    finalStats <- wideEloLast %>% filter(Afix_TeamID == tteam) %>% 
      mutate(across(starts_with("A_"),  ~ .x*(nGames-1)/nGames)) %>% 
      bind_rows(lastGame) %>% mutate(across(starts_with("A_"),  ~ sum(.x))) %>% 
      tail(1)
    
    return(finalStats)
  }) %>% dplyr::select(-GameID, -Afix_mov, -Afix_win, -Afix_count)
  
  # Added last Massey ratings before tournament & preSeason AP ranks for Men's games
  if(womens == FALSE) {
    tourneyMasseyStats <- tourneyStats %>% left_join(masseyImpute %>% 
                                                       filter(Season == tourneyYear) %>% arrange(DayNum) %>% group_by(TeamID) %>%
                                                       slice_tail(n = 1) %>% ungroup() %>% dplyr::select(-DayNum) %>% 
                                                       rename_with(.fn =  ~ paste0("Afix_", .x), .cols = -Season)) %>% 
      left_join(eloMasseyAPMix %>% filter(Season == tourneyYear, Afix_TeamID %in% tourneyTeams) %>% 
                  group_by(Afix_TeamID) %>% slice(1) %>% ungroup() %>% dplyr::select(Afix_TeamID, Afix_AP)) %>% 
      left_join(eloMasseyAPMix %>% filter(Season == tourneyYear, Bfix_TeamID %in% tourneyTeams) %>% 
                  group_by(Bfix_TeamID) %>% slice(1) %>% ungroup() %>% dplyr::select(Bfix_TeamID, Bfix_AP) %>% 
                  rename(Afix_TeamID = Bfix_TeamID, Afix_AP = Bfix_AP)) %>% 
      mutate(Afix_Loc = rep("N", times = 68)) %>% dplyr::select(-DayNum)
  } else {
    tourneyMasseyStats <- tourneyStats %>% 
      mutate(Afix_Loc = rep("N", times = 68)) %>% dplyr::select(-DayNum)
  }
  
  # Stats for all combinations of matchups
  allTourney <- expand_grid(Afix_TeamID = tourneyTeams, Bfix_TeamID = tourneyTeams) %>% 
    left_join(tourneyMasseyStats) %>% left_join(tourneyMasseyStats %>% 
                                                  rename_with(.fn =  ~ gsub("Afix_", "Bfix_", gsub("A_", "B_", .x,
                                                                                                   fixed = TRUE), fixed = TRUE),
                                                              .cols = starts_with(c("A_", "Afix_")))) %>% 
    mutate(Afix_win = FALSE) %>% dplyr::select(Season, Afix_TeamID,
                                               Bfix_TeamID, colnames(eloMasseyAPMix %>% dplyr::select(-Afix_count, -Bfix_count,
                                                                                                      -Season, -DayNum, -GameID, -Afix_TeamID,
                                                                                                      -Bfix_TeamID, -Afix_Conference,
                                                                                                      -Bfix_Conference, -Afix_mov)))
  
  if(womens == FALSE) {
  # First-order design matrix
  firstPreds <- str_split(str_squish("Afix_win, Afix_Loc, 
                A_for_Ast, B_for_Ast, A_against_Ast, B_against_Ast,
                A_for_OR, B_for_OR, A_against_OR, B_against_OR, 
                A_against_FGA, B_against_FGA,
                A_for_Blk, B_for_Blk, 
                A_for_Stl, B_for_Stl,
                A_for_Score, B_for_Score,
                Afix_AP, Bfix_AP, Afix_elo, Bfix_elo,
                Afix_Moore, Bfix_Moore, Afix_Pomeroy, Bfix_Pomeroy,
                Afix_Sagarin, Bfix_Sagarin, 
                A_for_TO, B_for_TO,
                A_against_FTA, B_against_FTA, 
                A_against_DR, B_against_DR"), pattern = ",")[[1]]
  designTourney <- eloMasseyAPMix %>% ungroup() %>% dplyr::select(firstPreds) %>% 
    bind_rows(allTourney %>% dplyr::select(firstPreds)) %>% 
    model.matrix(object = formula(Afix_win ~ 0 + .)) %>% 
    as.data.frame() %>% mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
    slice_tail(n = nrow(allTourney)) %>% as.matrix()
  
  # Second-order design matrix
  secondPreds <- str_split(str_squish("Afix_win, Afix_Loc, 
                                                              A_for_Ast, B_for_Ast, A_against_Ast, B_against_Ast,
                                                              A_for_OR, B_for_OR, A_against_OR, B_against_OR, 
                                                              A_against_FGA, B_against_FGA,
                                                              A_for_Blk, B_for_Blk, 
                                                              A_for_Stl, B_for_Stl,
                                                              A_for_Score, B_for_Score,
                                                              Afix_AP, Bfix_AP, Afix_elo, Bfix_elo,
                                                              Afix_Moore, Bfix_Moore, Afix_Pomeroy, Bfix_Pomeroy,
                                                              Afix_Sagarin, Bfix_Sagarin, 
                                                              A_for_TO, B_for_TO,
                                                              A_against_FTA, B_against_FTA, 
                                                              A_against_DR, B_against_DR"), pattern = ",")[[1]]
  designTourney2 <- eloMasseyAPMix %>% ungroup() %>% dplyr::select(secondPreds) %>% 
    bind_rows(allTourney %>% dplyr::select(secondPreds)) %>% 
    model.matrix(object = formula(Afix_win ~ 0 + .^2)) %>% 
    as.data.frame() %>% mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
    slice_tail(n = nrow(allTourney)) %>% as.matrix()
  } else {
    # First-order design matrix
    designTourney <- eloMasseyAPMix %>% ungroup() %>% 
      dplyr::select(Afix_win, Afix_Loc, Afix_elo,
                    Bfix_elo, A_for_Score:B_against_Poss, 
                    -contains(c("against_FTM", "against_Score", "against_Poss",
                                "for_FTA", "for_OR", "for_DR", "for_FGA"))) %>% 
      bind_rows(allTourney %>% dplyr::select(Afix_win, Afix_Loc, Afix_elo,
                                             Bfix_elo, A_for_Score:B_against_Poss, 
                                             -contains(c("against_FTM", "against_Score", "against_Poss",
                                                         "for_FTA", "for_OR", "for_DR", "for_FGA")))) %>% 
      model.matrix(object = formula(Afix_win ~ 0 + .)) %>% 
      as.data.frame() %>% mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
      slice_tail(n = nrow(allTourney)) %>% as.matrix()
    
    # Second-order design matrix
    designTourney2 <- eloMasseyAPMix %>% ungroup() %>% dplyr::select(Afix_win, Afix_Loc, Afix_elo,
                                                                     Bfix_elo, A_for_Score:B_against_Poss, 
                                                                     -contains(c("against_FTM", "against_Score", "against_Poss",
                                                                                 "for_FTA", "for_OR", "for_DR", "for_FGA"))) %>% 
      bind_rows(allTourney %>% dplyr::select(Afix_win, Afix_Loc, Afix_elo,
                                             Bfix_elo, A_for_Score:B_against_Poss, 
                                             -contains(c("against_FTM", "against_Score", "against_Poss",
                                                         "for_FTA", "for_OR", "for_DR", "for_FGA")))) %>% 
      model.matrix(object = formula(Afix_win ~ 0 + .^2)) %>% 
      as.data.frame() %>% mutate(across(.cols = everything(), .fns = scale, center = TRUE, scale = TRUE)) %>% 
      slice_tail(n = nrow(allTourney)) %>% as.matrix()
  }
  
  # Saving objects for final predictions
  saveRDS(list(allTourney = allTourney, designTourney = designTourney,
               designTourney2 = designTourney2), paste0(tourneyYear, ifelse(womens, "-Womens", ""),
                                                        "/finalPredObjects.rds"))
}
