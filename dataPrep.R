#################
# Title: Prepping data for NCAA Tournament
# Author: Andrew DiLernia
# Date: 12/17/2021
# Purpose: Prep data to help find optimal model for predicting NCAA Tournament games
#################

library(tidyverse)
library(dtplyr)
library(furrr)
library(readr)
library(progress)
library(mice)
library(ranger)

t1 <- Sys.time()

if(Sys.info()["sysname"] == "Darwin") {
  setwd("/Volumes/GoogleDrive/My Drive/Other/Fun/March Madness")
  stageDir <- "/Volumes/GoogleDrive/My Drive/Other/Fun/March Madness/2022/ncaam-march-mania-2021/MDataFiles_Stage1/"
} else {
stageDir <- "2022/ncaam-march-mania-2021/MDataFiles_Stage1/"
}

# Importing & Cleaning Box Score Data -------------------------------------

# Importing raw detailed box score data
regRaw <- read_csv(paste0(stageDir, "MRegularSeasonDetailedResults.csv")) %>% 
  mutate(Tourney = FALSE)
tourneyRaw <- read_csv(paste0(stageDir, "MNCAATourneyDetailedResults.csv")) %>% 
  mutate(Tourney = TRUE)

# Importing conference data
confInfo <- read_csv(paste0(stageDir, "MTeamConferences.csv")) %>% 
  left_join(read_csv(paste0(stageDir, "Conferences.csv"))) %>% 
  rename(Conference = Description) %>% select(Season, TeamID, Conference)

# Merging tournament and regular season data 
# and adding conference info & possession column
# POSS calculation from https://thepowerrank.com/cbb-analytics/
fullRaw <- bind_rows(regRaw, tourneyRaw) %>% 
  mutate(LLoc = case_when(WLoc == "H" ~ "A",
                              WLoc == "A" ~ "H",
                              WLoc == "N" ~ "N",)) %>% 
  select(Season:WTeamID, LTeamID, WScore, LScore, WLoc, LLoc, WFGM:LPF) %>% 
  left_join(confInfo, by = c("Season" = "Season",
                             "WTeamID" = "TeamID")) %>% 
  left_join(confInfo, suffix = c("W", "L"),
            by = c("Season" = "Season", "LTeamID" = "TeamID")) %>% 
  rename(WConference = ConferenceW, LConference = ConferenceL) %>%
  arrange(Season, DayNum, WTeamID) %>% 
  mutate(WPoss = ((WFGA - WOR + WTO + (0.475 * WFTA)) + 
                  (LFGA - LOR + LTO + (0.475 * LFTA))) / 2,
         LPoss = WPoss, GameID = row_number(), Afix_mov = WScore - LScore,)

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
  mutate(Afix_win = TRUE) %>% 
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

# Function for calculating game prediction from elo values
# home: -1 for away, 0 for neutral, 1 for home
eloPred <- function(elo1, elo2, homeAdv = 140, home = 0) {
  return(1 / (1 + 10^((elo2 - (elo1 + home*homeAdv)) / 400)))
}

# Function for calculating updated elo values
eloUpdate <- function(elo, pred, actual, k = 20) {
  return(elo + k*(actual - pred))
}

# Function to reset team elos to average of 
# previous season and conference average
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
    select(TeamID, elo) %>% filter(TeamID %in% teamIDs) %>% 
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
                   centerFun = mean) {
  
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
                        home = homes[i])
        
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
                        home = homes[i])
        
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
  return(bind_rows(output))
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
    select(DayNum, Afix_win2, Afix_elo2, Bfix_elo2)
  
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
  params <- expand.grid(kVal = seq(3, 60, by = 3),
                        tau = 0.002,
                        method = c("NFL"),
                        centerName = c("mean", "median"))
  
# Finding optimal tuning parameters
plan(multisession, workers = 3)
eloSimRes <- future_pmap_dfr(.l = params, .f = eloSim, .progress = TRUE)

# Saving results
saveRDS(eloSimRes, "2022/eloSimRes.rds")
eloSimRes <- readRDS("2022/eloSimRes_01-14-2022.rds")

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
fullElo1 <- addElo(scores = fullRaw1, method = "NFL", kVal = 45,
                   eloStarts = 1500, centerFun = mean)

# Performance of calculated elo scores
eloMod1 <- lm(Afix_mov ~ 0 + Afix_elo + Bfix_elo, 
              data = fullElo1)
r2Val <- summary(eloMod1)$r.squared

# Binary win / loss response
eloShuffled <- fullElo1 %>% 
  mutate(Afix_win2 = ifelse(Afix_mov %% 2 == 0, FALSE, TRUE),
         Afix_elo2 = ifelse(Afix_mov %% 2 == 0, Bfix_elo, Afix_elo),
         Bfix_elo2 = ifelse(Afix_mov %% 2 == 0, Afix_elo, Bfix_elo)) %>% 
  select(Afix_win2, Afix_elo2, Bfix_elo2)

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

# Visualizing elo across season
fullElo1 %>% filter(Season > 2015) %>% ggplot(aes(y = Afix_elo, x = DayNum, 
                                                  color = factor(Afix_TeamID))) + 
  geom_line() + facet_grid(rows = vars(Season)) +
  labs(title = "Team Elos by Season", y = "Elo") + theme_bw() +
  theme(legend.position = "none") 

# Rolling Team Statistics -------------------------------------------------

fullElo2 <- fullRaw2 %>% mutate(Afix_elo = fullElo1$Bfix_elo,
                                Bfix_elo = fullElo1$Afix_elo)

# Need statistics recorded as rates FOR and AGAINST Team's A and B for each game

# Combine into single data set with 2 rows for each game; 
# 1 where loser is Team A and 1 where loser is Team B
longElo <- map_dfr(.x = list(fullElo1, fullElo2), .f = function(x) {
  x %>% arrange(Season, DayNum, Afix_TeamID) %>% 
    group_by(Season, Afix_TeamID) %>% mutate(count = seq(n()), 
         across(.cols = starts_with(c("A_", "B_")),
                .fns = cumsum)) %>% 
  mutate(across(.cols = starts_with(c("A_", "B_")),
                .fns = ~ .x / count)) %>% 
  select(Season, DayNum, GameID, count, starts_with(c("Afix_", "Bfix_")),
         starts_with(c("A_", "B_")), -Bfix_Loc)
})

# Including both 'for' and 'against' info for each game, rather than just 'for'
wideElo <- longElo %>% group_by(GameID) %>% slice(1) %>% 
  rename_with(.fn =  ~ gsub("A_", "A_for_", .x,
                            fixed = TRUE), .cols = starts_with("A_")) %>% 
  rename_with(.fn =  ~ gsub("B_", "A_against_", .x,
                            fixed = TRUE), .cols = starts_with("B_")) %>% 
  rename(countA = count) %>% 
  ungroup() %>% mutate(across(starts_with(c("A_", "B_")), ~ lag(.x))) %>% 
  arrange(GameID) %>% bind_cols(longElo %>% group_by(GameID) %>% slice(2) %>% 
                                  rename_with(.fn =  ~ gsub("B_", "B_against_", .x,
                                                            fixed = TRUE), .cols = starts_with("B_")) %>% 
                                  rename_with(.fn =  ~ gsub("A_", "B_for_", .x,
                                                            fixed = TRUE), .cols = starts_with("A_")) %>% 
                                  ungroup() %>% mutate(across(starts_with(c("A_", "B_")), ~ lag(.x))) %>% 
                                  rename(countB = count) %>% arrange(GameID) %>% 
                                  select(-c("GameID", "Season", "DayNum", contains("fix_")))) %>% 
  rename(Afix_count = countA, Bfix_count = countB)

t2 <- Sys.time()

t2 - t1

# Massey Ordinal Rankings -------------------------------------------------

# Adding in Massey ordinal ranking data
# Descriptions / info for systems: https://masseyratings.com/cb/compare.htm
massey <- data.table::fread(paste0(stageDir, "MMasseyOrdinals.csv")) %>% 
  as.data.frame() %>% rename(DayNum = RankingDayNum) 

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
  pivot_wider(id_cols = c(Season, DayNum, TeamID, SystemName), 
              names_from = SystemName, values_from = OrdinalRank, values_fill = NA, 
              values_fn = function(x){x[1]}) %>% 
  arrange(Season, DayNum) %>% group_by(Season, TeamID) %>% 
  fill(SEL:REI, .direction = "down") %>% ungroup()

# Since 2010 and later, POM, MOR, SAG, PGH appear to have most pairwise complete obs
wideMassey %>% filter(Season >= 2010) %>% map_int(.f = function(x){sum(!is.na(x))}) %>% 
  sort(decreasing = TRUE) %>% as.data.frame()

(wideMassey %>% filter(Season >= 2010) %>% 
    select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey) %>% drop_na() %>% nrow())

# Trying using mice package for imputation for wideMassey for 
# Pomeroy, Moore, Sagarin, and Massey ratings
wideMassey %>% select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey) %>% 
  mutate(missCount = rowSums(is.na(.)))

wideMassey %>% select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey) %>% 
  mutate(missCount = rowSums(is.na(.))) %>% pull(missCount) %>% table()

# Exploring pattern of missing ratings.
# Imputation for ratings could help retain large amount of data.
wideMassey %>% select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey) %>% 
  md.pattern()

# Imputing for ordinal rankings when 1 of 4 are missing. 
# Can use rowSums since Season, DayNum, & TeamID are all complete
masseyMiss <- wideMassey %>% select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey) %>% 
  filter((is.na(Pomeroy) + is.na(Moore) + is.na(Sagarin) + is.na(Massey)) < 2)

# Impute ordinal ratings when 1 of 4 is missing using random forest
masseyImpute <- bind_cols(masseyMiss %>% select(-c(Pomeroy:Massey)), 
                          masseyMiss %>% select(Pomeroy:Massey) %>% 
  mice(defaultMethod = "rf", seed = 1994, m = 1, maxit = 1) %>% 
  complete())

# Adding ordinal ratings to box scores and elo data
# Carry last rating forward for each team in each season via the fill function
eloMassey <- wideElo %>% full_join(masseyImpute %>% 
                                     select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey),
                                   by = c("Season" = "Season", "DayNum" = "DayNum",
                                          "Afix_TeamID" = "TeamID")) %>% 
  rename(Afix_Pomeroy = Pomeroy, Afix_Moore = Moore, 
         Afix_Sagarin = Sagarin, Afix_Massey = Massey) %>% 
  full_join(masseyImpute %>% 
              select(Season, DayNum, TeamID, Pomeroy, Moore, Sagarin, Massey),
            by = c("Season" = "Season", "DayNum" = "DayNum",
                   "Bfix_TeamID" = "TeamID")) %>% 
  rename(Bfix_Pomeroy = Pomeroy, Bfix_Moore = Moore, 
         Bfix_Sagarin = Sagarin, Bfix_Massey = Massey) %>% 
  arrange(Season, DayNum) %>% group_by(Season, Afix_TeamID) %>% 
  tidyr::fill(c(Afix_Pomeroy, Afix_Moore, Afix_Sagarin, Afix_Massey), .direction = "down") %>% 
  ungroup() %>% group_by(Season, Bfix_TeamID) %>% 
  tidyr::fill(c(Bfix_Pomeroy, Bfix_Moore, Bfix_Sagarin, Bfix_Massey), .direction = "down") %>% 
  ungroup() %>% drop_na()

# Testing to see if columns seem reasonable

# Randomly selecting some games to have Team A lose
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

summary(lm(Afix_mov ~ 0 + Afix_Loc + Afix_Pomeroy + Afix_Moore + Afix_Sagarin + Afix_Massey + 
               Bfix_Pomeroy + Bfix_Moore + Bfix_Sagarin + Bfix_Massey + 
             Afix_elo + Bfix_elo, 
    data = eloMasseyMix))

# Adding in pre-season AP rank --------------------------------------------

# Adding in pre-season AP rank as a fixed predictor for each team in each season 
# as suggested by 538
startDays <- wideMassey %>% select(Season, DayNum, TeamID, AP) %>% 
  drop_na() %>% group_by(Season) %>% summarize(DayNum = min(DayNum))

apStarts <- startDays %>% left_join(wideMassey %>% select(Season, DayNum, TeamID, AP))

masseyImpute %>% arrange(Season, DayNum) %>% group_by(Season, TeamID) %>%
  slice(1) %>% ungroup() %>% full_join(apStarts) %>% group_by(Season, TeamID) %>% 
  fill(c(Pomeroy, Moore, Sagarin, Massey), .direction = "updown")

# Full season data for imputing initial AP ranks using Massey ordinals
apMiss <- masseyImpute %>% arrange(Season, DayNum) %>% group_by(Season, TeamID) %>%
  slice(1) %>% ungroup() %>% full_join(apStarts) %>% group_by(Season, TeamID) %>% 
  fill(c(Pomeroy, Moore, Sagarin, Massey), .direction = "updown") %>% ungroup()

# Consider using mice package for imputation
# Model for imputing AP ranks using full season

# Impute initial AP rankings > 25 using Lasso selection + linear regression
# Could alternatively use initial sum of Pomeroy:Massey to manually impute ranked order of teams > AP 25
apImpute <- bind_cols(apMiss %>% select(-c(Pomeroy:AP)), 
                      apMiss %>% select(Pomeroy:AP) %>% 
                            mice(defaultMethod = "lasso.select.norm", seed = 1994, m = 1, maxit = 1) %>% 
                            complete())

fullMod <- lm(AP ~ Pomeroy + Moore + Sagarin + Massey,
            data = apMiss)

# Removing first G games of the season for each team
# e.g., G=1 means exclude games where it is any teams first game of season
G <- 1
smallWideElo <- wideElo %>% filter(Afix_count > G, Bfix_count > G)

summary(lm(Afix_mov ~ Afix_elo + Bfix_elo, 
           data = smallWideElo))
