#################
# Title: Prepping data for NCAA Tournament
# Author: Andrew DiLernia
# Date: 12/17/2021
# Purpose: Prep data to help find optimal model for predicting NCAA Tournament games
#################

library(tidyverse)
library(readr)
library(tibbletime)

stageDir <- "2022/ncaam-march-mania-2021/MDataFiles_Stage1/"

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
# and adding conference info
fullRaw <- bind_rows(regRaw, tourneyRaw) %>% 
  mutate(LLoc = case_when(WLoc == "H" ~ "A",
                              WLoc == "A" ~ "H",
                              WLoc == "N" ~ "N",)) %>% 
  select(Season:WTeamID, LTeamID, WScore, LScore, WLoc, LLoc, WFGM:LPF) %>% 
  left_join(confInfo, by = c("Season" = "Season",
                             "WTeamID" = "TeamID")) %>% 
  left_join(confInfo, suffix = c("W", "L"),
            by = c("Season" = "Season", "LTeamID" = "TeamID")) %>% 
  rename(WConference = ConferenceW, LConference = ConferenceL)

# Creating duplicate rows to be agnostic to team winning or not
# and allow calculating rolling statistics
fullRaw1 <- fullRaw %>% 
  rename_with(.fn =  ~ gsub("_L", "B_", paste0("_", .x),
            fixed = TRUE), .cols = starts_with("L")) %>% 
  rename_with(.fn =  ~ gsub("_W", "A_", paste0("_", .x),
            fixed = TRUE), .cols = starts_with("W"))

fullRaw2 <- fullRaw %>% 
  rename_with(.fn =  ~ gsub("_W", "B_", paste0("_", .x),
              fixed = TRUE), .cols = starts_with("W")) %>% 
  rename_with(.fn =  ~ gsub("_L", "A_", paste0("_", .x), 
              fixed = TRUE), .cols = starts_with("L")) %>% 
  select(colnames(fullRaw1))

# Combine into single data set with 2 rows for each game; 
# 1 where loser is Team A and 1 where loser is Team B
fullRawAB <- fullRaw1 %>% bind_rows(fullRaw2) %>% 
  mutate(GameID = rep(1:nrow(fullRaw1), 2),
         Awin = A_Score > B_Score,
         Amov = A_Score - B_Score)

# Calculating stats prior to each game
fullClean <- fullRawAB %>% 
  group_by(Season, A_TeamID) %>% 
  mutate(count = seq(n()), 
         across(.cols = c(A_Score:B_Score, A_FGM:B_PF),
                .fns = cumsum)) %>% 
  mutate(across(.cols = c(A_Score:B_Score, A_FGM:B_PF),
                .fns = ~ .x / count)) %>% 
  select(Season:B_TeamID, Awin, Amov, GameID, count, 
         A_Loc, B_Loc, A_Conference, B_Conference,
         everything())

# Calculating ELO ratings: https://www.kaggle.com/lpkirwin/fivethirtyeight-elo-ratings
# Wikipedia elo: https://en.wikipedia.org/wiki/Elo_rating_system

# Parameters to be optimized: k, homeAdv
# Initial values from 538's values for NBA elo ratings:
# https://fivethirtyeight.com/features/how-we-calculate-nba-elo-ratings/

# Smoothing parameter for elo calculation
kVal <- 50

# Function for calculating game prediction form elo values
# home: -1 for away, 0 for neutral, 1 for home
eloPred <- function(elo1, elo2, homeAdv = 140, home = 0) {
  return(1 / (1 + 10^((elo2 - (elo1 + home*homeAdv)) / 400)))
}

# Function for calculating updated elo values
eloUpdate <- function(elo, pred, actual, k = 50) {
  return(elo + k*(actual - pred))
}

# Function for adding elo values to data frame
# seasonData: A data frame with columns A_TeamID,
# B_TeamID, A_Loc, Amov, Awin
# kVal: Smoothing parameter for elo calculation
# method: One of "NBA" or "NFL". Specifies smoothing method in elo update.
# eloStarts: Optional. Scalar or vector of starting elo values for unique TeamID's
addElo <- function(seasonData = full2003, method = "NBA", kVal = 50, eloStarts = 1500) {
  teamIDs <- unique(seasonData$A_TeamID)
  
  homes <- ifelse(seasonData$A_Loc == "N", 0,
                  ifelse(seasonData$A_Loc == "A", -1,
                         ifelse(seasonData$A_Loc == "H", 1, NA)))
  
  mmNumerator <- (seasonData$Amov + 3)^0.80
  Awin10 <- ifelse(seasonData$Awin, 1, 0)
  ks <- kVal*log(1 + abs(seasonData$Amov))
  
  elos <- data.frame(team = teamIDs, elo = 1500)
  
  # Initialize elo columns
  seasonData <- seasonData %>% 
      mutate(eloA = 1500, eloB = 1500)
  elos <- data.frame(team = teamIDs, elo = eloStarts)
  
  if(method == "NBA") {
  for(i in 1:nrow(seasonData)) {
    # Storing current elo values
    Ainds <- elos$team == seasonData$A_TeamID[i]
    Binds <- elos$team == seasonData$B_TeamID[i]
    seasonData$eloA[i] <- elos$elo[Ainds]
    seasonData$eloB[i] <- elos$elo[Binds]
    
    # Elo prediction
    pred <- eloPred(elo1 = seasonData$eloA[i],
                    elo2 = seasonData$eloB[i], 
                    home = homes[i])
    
    # Margin of victory multipliers
    movMultiA <- mmNumerator[i] /
      (7.5 + 0.006*(seasonData$eloA[i] - seasonData$eloB[i]))
    movMultiB <- mmNumerator[i] /
      (7.5 + 0.006*(seasonData$eloB[i] - seasonData$eloA[i]))
    
    # Calculating new elo values
    newA <- eloUpdate(elo = seasonData$eloA[i], pred = pred,
                      actual = Awin10[i], k = movMultiA*kVal)
    newB <- eloUpdate(elo = seasonData$eloB[i], pred = 1 - pred,
                      actual = (1-Awin10[i]), k = movMultiB*kVal)
    
    # Updating elo values
    elos$elo[Ainds] <- newA
    elos$elo[Binds] <- newB
  }
  } else if(method == "NFL"){
    for(i in 1:nrow(seasonData)) {
      # Storing current elo values
      Ainds <- elos$team == seasonData$A_TeamID[i]
      Binds <- elos$team == seasonData$B_TeamID[i]
      seasonData$eloA[i] <- elos$elo[Ainds]
      seasonData$eloB[i] <- elos$elo[Binds]
      
      # Elo prediction
      pred <- eloPred(elo1 = seasonData$eloA[i],
                      elo2 = seasonData$eloB[i], 
                      home = homes[i])
      
      # Margin of victory multipliers
      movMultiA <- mmNumerator[i] /
        (7.5 + 0.006*(seasonData$eloA[i] - seasonData$eloB[i]))
      movMultiB <- mmNumerator[i] /
        (7.5 + 0.006*(seasonData$eloB[i] - seasonData$eloA[i]))
      
      # Calculating new elo values
      newA <- eloUpdate(elo = full2003$eloA[i], pred = pred,
                        actual = Awin10[i], k = ks[i])
      newB <- eloUpdate(elo = full2003$eloB[i], pred = 1 - pred,
                        actual = (1-Awin10[i]), k = ks[i])
      
      # Updating elo values
      elos$elo[Ainds] <- newA
      elos$elo[Binds] <- newB
    }
  }
  
  return(seasonData)
}

# Function to reset team elos to average of 
# previous season and conference average
seasonReset <- function(oldSeason, newSeason) {
  
  # Conference average elos
  confAvg <- oldSeason %>% group_by(A_Conference) %>% 
    summarize(eloA = mean(eloA), countA = n()) %>% 
    rename(Conference = A_Conference) %>% 
    full_join(oldSeason %>% group_by(B_Conference) %>% 
                summarize(eloB = mean(eloA), countB = n()) %>% 
                rename(Conference = B_Conference)) %>% 
    mutate(eloAvg = (eloA*countA + eloB*countB) / (countA + countB)) %>% 
    select(Conference, eloAvg)
  
  # End of season elos
  endElos <- oldSeason %>% 
    select(A_TeamID, B_TeamID, eloA, eloB, GameID) %>% 
    pivot_longer(cols = c(A_TeamID, B_TeamID),
                 values_to = "TeamID") %>% 
    mutate(eloEnd = ifelse(name == "A_TeamID", eloA, eloB)) %>% 
    group_by(TeamID) %>% slice(n()) %>% ungroup() %>% 
    select(Season, TeamID, eloEnd)
  
  # Averaging end of season and conference average elos
  newElos <- endElos %>% left_join(confAvg %>% 
    left_join(confInfo %>% filter(Season == oldSeason$Season[1]))) %>% 
    mutate(elo =  (eloEnd + eloAvg) / 2) %>% 
    select(TeamID, elo)
  
  # Adding in next season starting elos
  newImpute <- newSeason %>% select(-eloA, -eloB) %>% 
    left_join(newElos, by = c("A_TeamID" = "TeamID")) %>% 
    rename(eloA = elo) %>% 
    left_join(newElos, by = c("B_TeamID" = "TeamID")) %>%
    rename(eloB = elo) %>% 
    mutate(eloA = ifelse(is.na(eloA), 1500, eloA),
           eloB = ifelse(is.na(eloB), 1500, eloB))
  
  return(newImpute)
}

results <- data.frame(k = seq(10, 100, by = 10), 
                      mod1 = NA, mod2 = NA)

full2003 <- addElo(fullClean %>% filter(Season == 2003) %>% 
                 head(4680), eloStarts = 1500)

# Performance of calculated elo scores
eloMod1 <- lm(Amov ~ 0 + eloA + eloB, 
              data = full2003)
summary(eloMod1)$r.squared

# Finding optimal k parameter
results %>% pivot_longer(cols = c(mod1, mod2), values_to = "rsq") %>% 
  ggplot(aes(x = k, y = rsq, color = name)) + geom_point() +
  geom_line(se = FALSE) + labs()

# Visualizing elo across season
full2003 %>% ggplot(aes(y = eloA, x = count, 
                        color = factor(A_TeamID))) + 
  geom_line() + theme(legend.position = "none")
