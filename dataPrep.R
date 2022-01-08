#################
# Title: Prepping data for NCAA Tournament
# Author: Andrew DiLernia
# Date: 12/17/2021
# Purpose: Prep data to help find optimal model for predicting NCAA Tournament games
#################

library(tidyverse)
library(readr)
library(progress)

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
  rename(WConference = ConferenceW, LConference = ConferenceL) %>%
  mutate(GameID = row_number(), Amov = WScore - LScore)

# Creating duplicate rows to be agnostic to team winning or not
# and allow calculating rolling statistics
fullRaw1 <- fullRaw %>% 
  rename_with(.fn =  ~ gsub("_L", "B_", paste0("_", .x),
            fixed = TRUE), .cols = starts_with("L")) %>% 
  rename_with(.fn =  ~ gsub("_W", "A_", paste0("_", .x),
            fixed = TRUE), .cols = starts_with("W")) %>%
  mutate(Awin = TRUE)

fullRaw2 <- fullRaw %>% 
  rename_with(.fn =  ~ gsub("_W", "B_", paste0("_", .x),
              fixed = TRUE), .cols = starts_with("W")) %>% 
  rename_with(.fn =  ~ gsub("_L", "A_", paste0("_", .x), 
              fixed = TRUE), .cols = starts_with("L")) %>% 
  mutate(Awin = FALSE) %>% select(colnames(fullRaw1))

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
seasonReset <- function(oldElos, teamIDs, center = mean, confer = confInfo) {
  
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
# scores: A data frame with columns Season, A_TeamID,
# B_TeamID, A_Loc, Amov, Awin
# kVal: Smoothing parameter for elo calculation
# method: One of "NBA" or "NFL". Specifies smoothing method in elo update.
# eloStarts: Optional. Scalar or vector of starting elo values for unique TeamID's
# tau: Second smoothing parameter for elo calculation
addElo <- function(scores, method = "NBA", 
                   kVal = 10, eloStarts = 1500, tau = 0.006) {
  seasons <- unique(scores$Season)
  nSeasons <- length(seasons)
  output <- vector("list", nSeasons)
  
  pb <- progress_bar$new(total = nSeasons)
  
  for(s in 1:nSeasons) {
    seasonData <- scores %>% filter(Season == seasons[s])
    teamIDs <- unique(c(seasonData$A_TeamID, 
                        seasonData$B_TeamID))
    
    homes <- ifelse(seasonData$A_Loc == "N", 0,
                    ifelse(seasonData$A_Loc == "A", -1,
                           ifelse(seasonData$A_Loc == "H", 1, NA)))
    
    mmNumerator <- sqrt(seasonData$Amov)
    Awin10 <- ifelse(seasonData$Awin, 1, 0)
    
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
          (7.5 + tau*(seasonData$eloA[i] - seasonData$eloB[i]))
        
        # Calculating new elo values
        newA <- eloUpdate(elo = seasonData$eloA[i], pred = pred,
                          actual = Awin10[i], k = movMultiA*kVal)
        newB <- seasonData$eloB[i] - (newA - seasonData$eloA[i])
        
        # Updating elo values
        elos$elo[Ainds] <- newA
        elos$elo[Binds] <- newB
      }
    } else if(method == "NFL"){
      
      ks <- kVal*log(1 + abs(seasonData$Amov))
      
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
        
        # Calculating new elo values
        newA <- eloUpdate(elo = seasonData$eloA[i], pred = pred,
                          actual = Awin10[i], k = ks[i])
        newB <- seasonData$eloB[i] - (newA - seasonData$eloA[i])
        
        # Updating elo values
        elos$elo[Ainds] <- newA
        elos$elo[Binds] <- newB
      }
    }
    
    output[[s]] <- seasonData
    
    if(s < nSeasons) {
      newSeason <- scores %>% filter(Season == seasons[s+1])
      newTeams <- unique(c(newSeason$A_TeamID, newSeason$B_TeamID))
      
      newElos <- seasonReset(oldElos = elos %>% mutate(Season = seasonData$Season[1]),
                             teamIDs = newTeams)
      
      eloStarts <- newElos %>% pull(elo)
    }
    
    # Progress bar update
    pb$tick()
  }
  return(bind_rows(output))
}

# Calculating elo values across all seasons
fullElo1 <- addElo(scores = fullRaw1, 
                   eloStarts = 1500)

# Note elos are drifting higher - not good for models, so fix it using Nate Silver's 
# advice of reducing MOV multiplier for heavier favorites: https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/
fullElo1 %>% group_by(Season) %>% 
  summarize(Avg = mean(eloA), Min = min(eloA), Max = max(eloA)) %>% 
  pivot_longer(cols = Avg:Max, names_to = "Metric", values_to = "Value") %>% 
  ggplot(aes(x = Season, y = Value, color = Metric)) + 
  geom_line(aes(x = Season)) + labs(title = "Elo Across Seasons") + 
  theme_bw()

# Visualizing elo across season
fullElo1 %>% filter(Season > 2015) %>% ggplot(aes(y = eloA, x = DayNum, 
                        color = factor(A_TeamID))) + 
  geom_line() + facet_grid(rows = vars(Season)) +
  labs(title = "Team Elos by Season", y = "Elo") + theme_bw() +
  theme(legend.position = "none") 

# Performance of calculated elo scores
eloMod1 <- lm(Amov ~ 0 + eloA + eloB, 
              data = fullElo1)
summary(eloMod1)$r.squared

# Finding optimal k parameter
results %>% pivot_longer(cols = c(mod1, mod2), values_to = "rsq") %>% 
  ggplot(aes(x = k, y = rsq, color = name)) + geom_point() +
  geom_line(se = FALSE) + labs()

# Rolling Team Statistics -------------------------------------------------

# Combine into single data set with 2 rows for each game; 
# 1 where loser is Team A and 1 where loser is Team B
fullRawAB <- fullRaw1 %>% bind_rows(fullRaw2)

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
