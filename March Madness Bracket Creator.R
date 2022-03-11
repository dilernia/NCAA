#################
# Title: March Madness Bracket Creator
# Author: Andrew DiLernia
# Date: 07/19/2017
# Purpose: Randomly generate filled out NCAA bracket given probability of team reaching each round
#################

#Explanation of how 538 generated probabilities: https://fivethirtyeight.com/features/how-fivethirtyeight-is-forecasting-the-2017-ncaa-tournament/
#Good package for scraping ncaa bball data: library(mRchmadness)

library(tidyverse)

dir <- ""

Rand_Brack <- function(seed) {

seed_value <- seed

ncaa538 <- read.csv(paste0(dir, "fivethirtyeight_ncaa_forecasts.csv"), header = TRUE, stringsAsFactors = T)
ncaa538 <- ncaa538[order(c(ncaa538$team_region, ncaa538$team_seed)) , ]
ncaa538 <- ncaa538[complete.cases(ncaa538),]

#Renames round 1 probability column
colnames(ncaa538)[1] <- "R1win_Prob"

print_info <- data.frame(cbind(rep(as.character(ncaa538$team_region), 5), rep(as.character(ncaa538$team_seed), 5), rep(as.character(ncaa538$team_name), 5)))

#sort(unique(slot_print_positions$y))
#sort(unique(slot_print_positions$x))

print_info$print_posx <- c(rep(9.8, 16), rep(209.8, 16), rep(9.8, 16), rep(209.8, 16))
print_info$print_posy <- c(rep(c(63.5), 2) )

#####Round 1 Predictions#####

#Sets seed value for generating predictions
set.seed(seed_value)

#Reference_teams has half of teams still in current round, and other_teams has other half of teams
Reference_teams <- as.vector(subset(ncaa538, team_seed<9)$team_name)

#Need to update assignment of Win_Prob and data set used for each round
Region <- ncaa538[ ncaa538$team_name %in% Reference_teams, ]$team_region
Seed <- ncaa538[ ncaa538$team_name %in% Reference_teams, ]$team_seed
Win_Prob <- ncaa538[ ncaa538$team_name %in% Reference_teams, ]$R1win_Prob

#Instantiates 1 prediction column
prediction_columns <- c()
for (i in 1:1){ 
  assign(paste("Round1_Prediction",i, sep=""), rep(NA, 32))
  prediction_columns <- c(prediction_columns, paste("Round1_Prediction",i, sep=""))
}

#Generates predictions
for (n in prediction_columns) {
  dummy <- get(n)
  for (i in 1:32) 
  {
    dummy[i] <- rbinom(1, 1, Win_Prob[i])
  }
  for (i in 1:32)
  {
    if (dummy[i] == 1)
    {dummy[i] <- "Win"}
    if (dummy[i] == 0)
    {dummy[i] <- "Loss"}
  }
  assign(n, dummy)
}

Round1_data <- cbind(Round1_Prediction1, Reference_teams)

#Creates and cleans data set with predictions for reference teams
Tourney_data1 <- merge(ncaa538, Round1_data,  by.x="team_name", by.y="Reference_teams", all = TRUE)
Tourney_data1 <- Tourney_data1[order(c(Tourney_data1$team_seed, Tourney_data1$team_region)) , ]
Tourney_data1 <- Tourney_data1[order(c(Tourney_data1$team_region, Tourney_data1$team_seed)) , ]
Tourney_data1 <- Tourney_data1[!(is.na(Tourney_data1$team_seed)),]

#Adjustment for error showing up due to prediction being created as a factor variable
Tourney_data1$Round1_Prediction1 <- as.character(Tourney_data1$Round1_Prediction1)

#Filling in Win/Loss for non-reference team
for (i in 1:8)
{
  if (subset(Tourney_data1, team_region=="East")[i , 10] == "Win")
   {Tourney_data1[ 17-i, 10] <- "Loss"}
   else {Tourney_data1[ 17-i, 10] <- "Win"}
  
  if (subset(Tourney_data1, team_region=="Midwest")[i , 10] == "Win")
   {Tourney_data1[ 33-i, 10] <- "Loss"}
   else {Tourney_data1[ 33-i, 10] <- "Win"}
  
  if (subset(Tourney_data1, team_region=="South")[i , 10] == "Win")
   {Tourney_data1[ 49-i, 10] <- "Loss"}
   else {Tourney_data1[ 49-i, 10] <- "Win"}
  
  if (subset(Tourney_data1, team_region=="West")[i , 10] == "Win")
   {Tourney_data1[ 65-i, 10] <- "Loss"}
   else {Tourney_data1[ 65-i, 10] <- "Win"}
  }

#####Round 2 Predictions#####

#Sets seed value for generating predictions
set.seed(seed_value)

#Reference_teams has half of teams still in current round, and other_teams has other half of teams
Reference_teams <- as.vector(subset(Tourney_data1, Round1_Prediction1 == "Win" & team_seed %in% c(1, 16, 2, 15, 3, 14, 4, 13))$team_name)

#Need to update assignment of Win_Prob and data set used for each round
Region <- Tourney_data1[ Tourney_data1$team_name %in% Reference_teams, ]$team_region
Seed <- Tourney_data1[ Tourney_data1$team_name %in% Reference_teams, ]$team_seed
Win_Prob <- Tourney_data1[ Tourney_data1$team_name %in% Reference_teams, ]$R2win_Prob / Tourney_data1[ Tourney_data1$team_name %in% Reference_teams, ]$R1win_Prob

#Instantiates 1 prediction column
prediction_columns <- c()
for (i in 1:1){ 
  assign(paste("Round2_Prediction",i, sep=""), rep(NA, 16))
  prediction_columns <- c(prediction_columns, paste("Round2_Prediction",i, sep=""))
}

#Generates predictions
for (n in prediction_columns) {
  dummy <- get(n)
  for (i in 1:16) 
  {
    dummy[i] <- rbinom(1, 1, Win_Prob[i])
  }
  for (i in 1:16)
  {
    if (dummy[i] == 1)
    {dummy[i] <- "Win"}
    if (dummy[i] == 0)
    {dummy[i] <- "Loss"}
  }
  assign(n, dummy)
}

Round2_data <- cbind(Round2_Prediction1, Reference_teams)

#Creates and cleans data set with predictions for reference teams
Tourney_data2 <- merge(Tourney_data1, Round2_data,  by.x="team_name", by.y="Reference_teams", all = TRUE)
Tourney_data2 <- Tourney_data2[order(c(Tourney_data2$team_seed, Tourney_data2$team_region)) , ]
Tourney_data2 <- Tourney_data2[order(c(Tourney_data2$team_region, Tourney_data2$team_seed)) , ]
Tourney_data2 <- Tourney_data2[!(is.na(Tourney_data2$team_seed)),]

#Adjustment for error showing up due to prediction being created as a factor variable
Tourney_data2$Round2_Prediction1 <- as.character(Tourney_data2$Round2_Prediction1)

#Filling in Win/Loss for non-reference team
for(i in 1:length(Tourney_data2$Round1_Prediction1))
{if(Tourney_data2$Round1_Prediction1[i] == "Loss")
 {
  Tourney_data2$Round2_Prediction1[i] <-  "Loss"
}
}

Round2 <- function(input_region, offset ){
  for (i in 1:64)
  {
    if (Tourney_data2$team_seed[i] == 1 & Tourney_data2$team_region[i] == input_region & Tourney_data2$Round2_Prediction1[i] %in% c("Win"))
    {
      Tourney_data2$Round2_Prediction1[8+offset] <- "Loss"
      Tourney_data2$Round2_Prediction1[9+offset] <- "Loss"
    }
    if (Tourney_data2$team_seed[i] == 2 & Tourney_data2$team_region[i] == input_region & Tourney_data2$Round2_Prediction1[i] %in% c("Win"))
    {
      Tourney_data2$Round2_Prediction1[7+offset] <- "Loss"
      Tourney_data2$Round2_Prediction1[10+offset] <- "Loss"
    }
    if (Tourney_data2$team_seed[i] == 3 & Tourney_data2$team_region[i] == input_region & Tourney_data2$Round2_Prediction1[i] %in% c("Win"))
    {
      Tourney_data2$Round2_Prediction1[6+offset] <- "Loss"
      Tourney_data2$Round2_Prediction1[11+offset] <- "Loss"
    }
    if (Tourney_data2$team_seed[i] == 4 & Tourney_data2$team_region[i] == input_region & Tourney_data2$Round2_Prediction1[i] %in% c("Win"))
    {
      Tourney_data2$Round2_Prediction1[5+offset] <- "Loss"
      Tourney_data2$Round2_Prediction1[12+offset] <- "Loss"
    }
    if (Tourney_data2$team_seed[i] == 13 & Tourney_data2$team_region[i] == input_region & Tourney_data2$Round2_Prediction1[i] %in% c("Win"))
    {
      Tourney_data2$Round2_Prediction1[5+offset] <- "Loss"
      Tourney_data2$Round2_Prediction1[12+offset] <- "Loss"
    }
    if (Tourney_data2$team_seed[i] == 14 & Tourney_data2$team_region[i] == input_region & Tourney_data2$Round2_Prediction1[i] %in% c("Win"))
    {
      Tourney_data2$Round2_Prediction1[6+offset] <- "Loss"
      Tourney_data2$Round2_Prediction1[11+offset] <- "Loss"
    }
    if (Tourney_data2$team_seed[i] == 15 & Tourney_data2$team_region[i] == input_region & Tourney_data2$Round2_Prediction1[i] %in% c("Win"))
    {
      Tourney_data2$Round2_Prediction1[7+offset] <- "Loss"
      Tourney_data2$Round2_Prediction1[10+offset] <- "Loss"
    }
    if (Tourney_data2$team_seed[i] == 16 & Tourney_data2$team_region[i] == input_region & Tourney_data2$Round2_Prediction1[i] %in% c("Win"))
    {
      Tourney_data2$Round2_Prediction1[8+offset] <- "Loss"
      Tourney_data2$Round2_Prediction1[9+offset] <- "Loss"
    }
  }
  assign('Tourney_data2',Tourney_data2,envir=.GlobalEnv)
}
Round2("East", 0)
Round2("Midwest", 16)
Round2("South", 32)
Round2("West", 48)

for (i in 1:64)
{if (is.na(Tourney_data2$Round2_Prediction1[i]))
{Tourney_data2$Round2_Prediction1[i] <- "Win"}
  assign('Tourney_data2',Tourney_data2,envir=.GlobalEnv)
  }

#####Round 3 Predictions#####

#Sets seed value for generating predictions
set.seed(seed_value)

#Reference_teams has half of teams still in current round, and other_teams has other half of teams
Reference_teams <- as.vector(subset(Tourney_data2, Round2_Prediction1 == "Win" & team_seed %in% c(1, 16, 8, 9, 6, 11, 3, 14))$team_name)

#Need to update assignment of Win_Prob and data set used for each round
Region <- Tourney_data2[ Tourney_data2$team_name %in% Reference_teams, ]$team_region
Seed <- Tourney_data2[ Tourney_data2$team_name %in% Reference_teams, ]$team_seed
Win_Prob <- Tourney_data2[ Tourney_data2$team_name %in% Reference_teams, ]$R3win_Prob / Tourney_data2[ Tourney_data2$team_name %in% Reference_teams, ]$R2win_Prob

#Instantiates 1 prediction column
prediction_columns <- c()
for (i in 1:1){ 
  assign(paste("Round3_Prediction",i, sep=""), rep(NA, 8))
  prediction_columns <- c(prediction_columns, paste("Round3_Prediction",i, sep=""))
}

#Generates predictions
for (n in prediction_columns) {
  dummy <- get(n)
  for (i in 1:8) 
  {
    dummy[i] <- rbinom(1, 1, Win_Prob[i])
  }
  for (i in 1:8)
  {
    if (dummy[i] == 1)
    {dummy[i] <- "Win"}
    if (dummy[i] == 0)
    {dummy[i] <- "Loss"}
  }
  assign(n, dummy)
}

Round3_data <- cbind(Round3_Prediction1, Reference_teams)

#Creates and cleans data set with predictions for reference teams
Tourney_data3 <- merge(Tourney_data2, Round3_data,  by.x="team_name", by.y="Reference_teams", all = TRUE)
Tourney_data3 <- Tourney_data3[order(c(Tourney_data3$team_seed, Tourney_data3$team_region)) , ]
Tourney_data3 <- Tourney_data3[order(c(Tourney_data3$team_region, Tourney_data3$team_seed)) , ]
Tourney_data3 <- Tourney_data3[!(is.na(Tourney_data3$team_seed)),]

#Adjustment for error showing up due to prediction being created as a factor variable
Tourney_data3$Round3_Prediction1 <- as.character(Tourney_data3$Round3_Prediction1)

#Filling in Win/Loss for non-reference team
for(i in 1:length(Tourney_data3$Round2_Prediction1))
 { if(Tourney_data3$Round2_Prediction1[i] == "Loss")
  {
    Tourney_data3$Round3_Prediction1[i] <-  "Loss"
 }
}

Round3 <- function(input_region, offset ){
  for (i in 1:64)
  {
    if (Tourney_data3$team_seed[i] == 1 & Tourney_data3$team_region[i] == input_region & Tourney_data3$Round3_Prediction1[i] %in% c("Win"))
    {
      Tourney_data3$Round3_Prediction1[5+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[12+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[4+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[13+offset] <- "Loss"
    }
    if (Tourney_data3$team_seed[i] == 3 & Tourney_data3$team_region[i] == input_region & Tourney_data3$Round3_Prediction1[i] %in% c("Win"))
    {
      Tourney_data3$Round3_Prediction1[7+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[10+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[2+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[15+offset] <- "Loss"
    }
    if (Tourney_data3$team_seed[i] == 6 & Tourney_data3$team_region[i] == input_region & Tourney_data3$Round3_Prediction1[i] %in% c("Win"))
    {
      Tourney_data3$Round3_Prediction1[7+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[10+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[2+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[15+offset] <- "Loss"
    }
    if (Tourney_data3$team_seed[i] == 8 & Tourney_data3$team_region[i] == input_region & Tourney_data3$Round3_Prediction1[i] %in% c("Win"))
    {
      Tourney_data3$Round3_Prediction1[5+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[12+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[4+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[13+offset] <- "Loss"
    }
    if (Tourney_data3$team_seed[i] == 9 & Tourney_data3$team_region[i] == input_region & Tourney_data3$Round3_Prediction1[i] %in% c("Win"))
    {
      Tourney_data3$Round3_Prediction1[5+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[12+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[4+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[13+offset] <- "Loss"
    }
    if (Tourney_data3$team_seed[i] == 11 & Tourney_data3$team_region[i] == input_region & Tourney_data3$Round3_Prediction1[i] %in% c("Win"))
    {
      Tourney_data3$Round3_Prediction1[7+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[10+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[2+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[15+offset] <- "Loss"
    }
    if (Tourney_data3$team_seed[i] == 14 & Tourney_data3$team_region[i] == input_region & Tourney_data3$Round3_Prediction1[i] %in% c("Win"))
    {
      Tourney_data3$Round3_Prediction1[7+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[10+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[2+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[15+offset] <- "Loss"
    }
    if (Tourney_data3$team_seed[i] == 16 & Tourney_data3$team_region[i] == input_region & Tourney_data3$Round3_Prediction1[i] %in% c("Win"))
    {
      Tourney_data3$Round3_Prediction1[5+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[12+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[4+offset] <- "Loss"
      Tourney_data3$Round3_Prediction1[13+offset] <- "Loss"
    }
  }
  assign('Tourney_data3',Tourney_data3,envir=.GlobalEnv)
}

Round3("East", 0)
Round3("Midwest", 16)
Round3("South", 32)
Round3("West", 48)

for (i in 1:64)
{if (is.na(Tourney_data3$Round3_Prediction1[i]))
{Tourney_data3$Round3_Prediction1[i] <- "Win"}
  assign('Tourney_data3',Tourney_data3,envir=.GlobalEnv)
}

#####Round 4 Predictions#####

#Sets seed value for generating predictions
set.seed(seed_value)

#Reference_teams has half of teams still in current round, and other_teams has other half of teams
Reference_teams <- as.vector(subset(Tourney_data3, Round3_Prediction1 == "Win" & team_seed %in% c(1, 16, 8, 9, 5, 12, 4, 13))$team_name)

#Need to update assignment of Win_Prob and data set used for each round
Region <- Tourney_data3[ Tourney_data3$team_name %in% Reference_teams, ]$team_region
Seed <- Tourney_data3[ Tourney_data3$team_name %in% Reference_teams, ]$team_seed
Win_Prob <- Tourney_data3[ Tourney_data3$team_name %in% Reference_teams, ]$R4win_Prob / Tourney_data3[ Tourney_data3$team_name %in% Reference_teams, ]$R3win_Prob

#Instantiates 1 prediction column
prediction_columns <- c()
for (i in 1:1){ 
  assign(paste("Round4_Prediction",i, sep=""), rep(NA, 4))
  prediction_columns <- c(prediction_columns, paste("Round4_Prediction",i, sep=""))
}

#Generates predictions
for (n in prediction_columns) {
  dummy <- get(n)
  for (i in 1:4) 
  {
    dummy[i] <- rbinom(1, 1, Win_Prob[i])
  }
  for (i in 1:4)
  {
    if (dummy[i] == 1)
    {dummy[i] <- "Win"}
    if (dummy[i] == 0)
    {dummy[i] <- "Loss"}
  }
  assign(n, dummy)
}

Round4_data <- cbind(Round4_Prediction1, Reference_teams)

#Creates and cleans data set with predictions for reference teams
Tourney_data4 <- merge(Tourney_data3, Round4_data,  by.x="team_name", by.y="Reference_teams", all = TRUE)
Tourney_data4 <- Tourney_data4[order(c(Tourney_data4$team_seed, Tourney_data4$team_region)) , ]
Tourney_data4 <- Tourney_data4[order(c(Tourney_data4$team_region, Tourney_data4$team_seed)) , ]
Tourney_data4 <- Tourney_data4[!(is.na(Tourney_data4$team_seed)),]

#Adjustment for error showing up due to prediction being created as a factor variable
Tourney_data4$Round4_Prediction1 <- as.character(Tourney_data4$Round4_Prediction1)

#Filling in Win/Loss for non-reference team
for(i in 1:length(Tourney_data4$Round3_Prediction1))
  {if(Tourney_data4$Round3_Prediction1[i] == "Loss")
  {
    Tourney_data4$Round4_Prediction1[i] <-  "Loss"
  }
}

Round4 <- function(input_region, offset ){
  for (i in 1:64)
  {
    if ((Tourney_data4$team_seed[i] %in% c(1, 16, 8, 9, 5, 12, 4, 13)) & Tourney_data4$team_region[i] == input_region & Tourney_data4$Round4_Prediction1[i] %in% c("Win"))
    {
      Tourney_data4$Round4_Prediction1[6+offset] <- "Loss"
      Tourney_data4$Round4_Prediction1[11+offset] <- "Loss"
      Tourney_data4$Round4_Prediction1[3+offset] <- "Loss"
      Tourney_data4$Round4_Prediction1[14+offset] <- "Loss"
      Tourney_data4$Round4_Prediction1[7+offset] <- "Loss"
      Tourney_data4$Round4_Prediction1[10+offset] <- "Loss"
      Tourney_data4$Round4_Prediction1[2+offset] <- "Loss"
      Tourney_data4$Round4_Prediction1[15+offset] <- "Loss"
    }
  }
  assign('Tourney_data4',Tourney_data4,envir=.GlobalEnv)
}
Round4("East", 0)
Round4("Midwest", 16)
Round4("South", 32)
Round4("West", 48)

for (i in 1:64)
{if (is.na(Tourney_data4$Round4_Prediction1[i]))
{Tourney_data4$Round4_Prediction1[i] <- "Win"}
  assign('Tourney_data4',Tourney_data4,envir=.GlobalEnv)
}

#####Round 5 Predictions#####

#Sets seed value for generating predictions
set.seed(seed_value)

#Reference_teams has half of teams still in current round, and other_teams has other half of teams
Reference_teams <- as.vector(subset(Tourney_data4, Round4_Prediction1 == "Win" & team_region %in% c("East", "Midwest"))$team_name)

#Need to update assignment of Win_Prob and data set used for each round
Region <- Tourney_data4[ Tourney_data4$team_name %in% Reference_teams, ]$team_region
Seed <- Tourney_data4[ Tourney_data4$team_name %in% Reference_teams, ]$team_seed
Win_Prob <- Tourney_data4[ Tourney_data4$team_name %in% Reference_teams, ]$R5win_Prob / Tourney_data4[ Tourney_data4$team_name %in% Reference_teams, ]$R4win_Prob

#Instantiates 1 prediction column
prediction_columns <- c()
for (i in 1:1){ 
  assign(paste("Round5_Prediction",i, sep=""), rep(NA, 2))
  prediction_columns <- c(prediction_columns, paste("Round5_Prediction",i, sep=""))
}

#Generates predictions
for (n in prediction_columns) {
  dummy <- get(n)
  for (i in 1:2) 
  {
    dummy[i] <- rbinom(1, 1, Win_Prob[i])
  }
  for (i in 1:2)
  {
    if (dummy[i] == 1)
    {dummy[i] <- "Win"}
    if (dummy[i] == 0)
    {dummy[i] <- "Loss"}
  }
  assign(n, dummy)
}

Round5_data <- cbind(Round5_Prediction1, Reference_teams)

#Creates and cleans data set with predictions for reference teams
Tourney_data5 <- merge(Tourney_data4, Round5_data,  by.x="team_name", by.y="Reference_teams", all = TRUE)
Tourney_data5 <- Tourney_data5[order(c(Tourney_data5$team_seed, Tourney_data5$team_region)) , ]
Tourney_data5 <- Tourney_data5[order(c(Tourney_data5$team_region, Tourney_data5$team_seed)) , ]
Tourney_data5 <- Tourney_data5[!(is.na(Tourney_data5$team_seed)),]

#Adjustment for error showing up due to prediction being created as a factor variable
Tourney_data5$Round5_Prediction1 <- as.character(Tourney_data5$Round5_Prediction1)

#Filling in Win/Loss for non-reference team
for(i in 1:length(Tourney_data5$Round4_Prediction1))
{
  if(Tourney_data5$Round4_Prediction1[i] == "Loss")
  {
    Tourney_data5$Round5_Prediction1[i] <-  "Loss"
  }
}

for (i in 1:64)
{
  if ((Tourney_data5$team_region[i] == "East") & Tourney_data5$Round5_Prediction1[i] %in% c("Win"))
  {
    for (i in 1:64)
    {
      if(Tourney_data5$team_region[i] == "West")	
      {Tourney_data5$Round5_Prediction1[i] <- "Loss"}
      assign('Tourney_data5',Tourney_data5,envir=.GlobalEnv)
    }
  }
  if ((Tourney_data5$team_region[i] == "Midwest") & Tourney_data5$Round5_Prediction1[i] %in% c("Win"))
  {
    for (i in 1:64)
    {
      if(Tourney_data5$team_region[i] == "South")	
      {Tourney_data5$Round5_Prediction1[i] <- "Loss"}
      assign('Tourney_data5',Tourney_data5,envir=.GlobalEnv)
    }
  }
  assign('Tourney_data5',Tourney_data5,envir=.GlobalEnv)
}

for (i in 1:64)
{if (is.na(Tourney_data5$Round5_Prediction1[i]))
{Tourney_data5$Round5_Prediction1[i] <- "Win"}
  assign('Tourney_data5',Tourney_data5,envir=.GlobalEnv)
}

#####Round 6 Predictions#####

#Sets seed value for generating predictions
set.seed(seed_value)

#Reference_teams has half of teams still in current round, and other_teams has other half of teams
Reference_teams <- as.vector(subset(Tourney_data5, Round5_Prediction1 == "Win" & team_region %in% c("East", "West"))$team_name)

#Need to update assignment of Win_Prob and data set used for each round
Region <- Tourney_data5[ Tourney_data5$team_name %in% Reference_teams, ]$team_region
Seed <- Tourney_data5[ Tourney_data5$team_name %in% Reference_teams, ]$team_seed
Win_Prob <- Tourney_data5[ Tourney_data5$team_name %in% Reference_teams, ]$R6win_Prob / Tourney_data5[ Tourney_data5$team_name %in% Reference_teams, ]$R5win_Prob

#Instantiates 1 prediction column
prediction_columns <- c()
for (i in 1:1){ 
  assign(paste("Round6_Prediction",i, sep=""), NA)
  prediction_columns <- c(prediction_columns, paste("Round6_Prediction",i, sep=""))
}

#Generates predictions
for (n in prediction_columns) {
  dummy <- get(n)
  for (i in 1:1) 
  {
    dummy[i] <- rbinom(1, 1, Win_Prob[i])
  }
  for (i in 1:1)
  {
    if (dummy[i] == 1)
    {dummy[i] <- "Win"}
    if (dummy[i] == 0)
    {dummy[i] <- "Loss"}
  }
  assign(n, dummy)
}

Round6_data <- cbind(Round6_Prediction1, Reference_teams)

#Creates and cleans data set with predictions for reference teams
Tourney_data6 <- merge(Tourney_data5, Round6_data,  by.x="team_name", by.y="Reference_teams", all = TRUE)
Tourney_data6 <- Tourney_data6[order(c(Tourney_data6$team_seed, Tourney_data6$team_region)) , ]
Tourney_data6 <- Tourney_data6[order(c(Tourney_data6$team_region, Tourney_data6$team_seed)) , ]
Tourney_data6 <- Tourney_data6[!(is.na(Tourney_data6$team_seed)),]

#Adjustment for error showing up due to prediction being created as a factor variable
Tourney_data6$Round6_Prediction1 <- as.character(Tourney_data6$Round6_Prediction1)

#Filling in Win/Loss for non-reference team
for(i in 1:length(Tourney_data6$Round5_Prediction1))
{
  if(Tourney_data6$Round5_Prediction1[i] == "Loss")
  {
    Tourney_data6$Round6_Prediction1[i] <-  "Loss"
  }
}

#Sets other teams' predictions to Loss if reference team won
for (i in 1:64)
{
  if ((Tourney_data6$team_region[i] %in% c("East")) & Tourney_data6$Round6_Prediction1[i] %in% c("Win"))
  {
    for (i in 1:64)
    {
      if(Tourney_data6$team_region[i] == "Midwest")	
      {Tourney_data6$Round6_Prediction1[i] <- "Loss"}
      if(Tourney_data6$team_region[i] == "South")	
      {Tourney_data6$Round6_Prediction1[i] <- "Loss"}
      if(Tourney_data6$team_region[i] == "West")	
      {Tourney_data6$Round6_Prediction1[i] <- "Loss"}
    }
  }
  if ((Tourney_data6$team_region[i] %in% c("West")) & Tourney_data6$Round6_Prediction1[i] %in% c("Win"))
  {
    for (i in 1:64)
    {
      if(Tourney_data6$team_region[i] == "East")	
      {Tourney_data6$Round6_Prediction1[i] <- "Loss"}
      if(Tourney_data6$team_region[i] == "South")	
      {Tourney_data6$Round6_Prediction1[i] <- "Loss"}
      if(Tourney_data6$team_region[i] == "Midwest")	
      {Tourney_data6$Round6_Prediction1[i] <- "Loss"}
    }
  }
}

for (i in 1:64)
{if (is.na(Tourney_data6$Round6_Prediction1[i]))
{Tourney_data6$Round6_Prediction1[i] <- "Win"}
  assign('Tourney_data6',Tourney_data6,envir=.GlobalEnv)
}

#Summarizing results
Results <- t(Tourney_data6[ , -c(2:9)])
colnames(Results) <- t(Results[1 , ])
Results2 <- Results[-1, ]

for (j in 1:6)
{
  for (i in 1:64)
  {
    if (Results2[j, i] == "Win")
    {Results2[j, i] <- 1}
    else {Results2[j, i] <- 0}
  }
}
Results2 <- as.matrix(Results2)
class(Results2) <- "numeric"

wins <- c()
for (j in 1:64)
{wins[j] <- sum(Results2[ , j])}

Final_Results <- as.data.frame(cbind(Results[1 , ], wins))
colnames(Final_Results) <- c("Team", "Wins")
rownames(Final_Results) <- NULL
Final_Results$Region <- ncaa538$team_region
Final_Results$Seed <- ncaa538$team_seed

#Exports Final_Results to csv
write_csv(Final_Results, file = paste0(dir, "Tourney_Final_Results.csv"))
}

Rand_Brack(1211)


# Drawing Bracket with mRchmadness ----------------------------------------

# This person did it with ggplot: https://stackoverflow.com/questions/16290052/march-madness-brackets-with-ggplot2

library(tidyverse)
library(png)
library(shadowtext)


# This person did it with ggplot: https://stackoverflow.com/questions/16290052/march-madness-brackets-with-ggplot2
bracketDraw <- function(probs = NULL) {
### Helper functions
first_evens <- function(x) {seq(from=2,to=2*x,length.out=x)}
first_odds <- function(x) {seq(from=1,to=2*x-1,length.out=x)}

### calculate y-values for horizontal lines:
### this is for top-left corner of the bracket,
### but multiplying sequences by -1 makes these 
### values work for bottom right and left corners;
### final round has teams at y=2*off.set

r1.y.width <- 1.5*strheight(s="Virginia Common",units="in") # this effects the width of the first round
r1.y.offset <- 0.125*r1.y.width # this effects distance from y=0

r1.y <- seq(from=r1.y.offset,to=r1.y.offset+r1.y.width,length.out=16)
r2.y <- seq(from=mean(r1.y[1:2]),to=mean(r1.y[15:16]),length.out=8)
r3.y <- seq(from=mean(r2.y[1:2]),to=mean(r2.y[7:8]),length.out=4)
r4.y <- seq(from=mean(r3.y[1:2]),to=mean(r3.y[3:4]),length.out=2)
r5.y <- seq(from=mean(r4.y[1:2]),to=mean(r4.y[1:2]),length.out=1)
r6.y <- 1.5*r1.y.offset

### calculate horizontal bar start and stop coordinates
### note that there are 6 total rounds -- 5 rounds per quadrant
r1.x.width <- 1.25*strwidth("Viriginia Commonwealth","inches") # how long should horizontal lines be?
r1.x.offset <- 1
round.break.points <- -(seq(from=0,to=7*r1.x.width,by=r1.x.width)+r1.x.offset)

r1.x <- round.break.points[7:6]
r2.x <- round.break.points[6:5]
r3.x <- round.break.points[5:4]
r4.x <- round.break.points[4:3]
r5.x <- round.break.points[3:2]
r6.x <- round.break.points[2:1]

### calculate verticals line coordinates: these are based off of
### r1.y values. Round 5 verticals need to connect the four subtrees
### via the top-left <-> bottom-left and top-right <-> bottom-right

r1.verticals.start <- r1.y[first_odds(8)]
r1.verticals.stop <- r1.y[first_evens(8)]

r2.verticals.start <- r2.y[first_odds(4)]
r2.verticals.stop <- r2.y[first_evens(4)]

r3.verticals.start <- r3.y[first_odds(2)]
r3.verticals.stop <- r3.y[first_evens(2)]

r4.verticals.start <- r4.y[first_odds(1)]
r4.verticals.stop <- r4.y[first_evens(1)]

r5.verticals.start <- r5.y[1]
r5.verticals.stop <- -r5.y[1]

empty.bracket <- ggplot() + theme_bw() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(), plot.margin=unit(c(0,0,-6,-6),"mm"), text=element_text(size=12,hjust=0,vjust=0)) + coord_cartesian(ylim = c(-1.05*r1.y[16],1.05*r1.y[16]), xlim = c(1.025*r1.x[1],-1.025*r1.x[1]))

### add first round bars, and vertical connectors, make addition of each quadrant verbose
empty.bracket <- empty.bracket + 
  geom_segment(aes(x=r1.x[1],y=r1.y,yend=r1.y,xend=r1.x[2])) + 
  geom_segment(aes(x=r1.x[1],y=-r1.y,yend=-r1.y,xend=r1.x[2])) + 
  geom_segment(aes(x=-r1.x[1],y=r1.y,yend=r1.y,xend=-r1.x[2])) + 
  geom_segment(aes(x=-r1.x[1],y=-r1.y,yend=-r1.y,xend=-r1.x[2])) + 
  geom_segment(aes(x=r1.x[2],xend=r1.x[2],y=r1.verticals.start,yend=r1.verticals.stop)) +
  geom_segment(aes(x=r1.x[2],xend=r1.x[2],y=-r1.verticals.start,yend=-r1.verticals.stop)) +
  geom_segment(aes(x=-r1.x[2],xend=-r1.x[2],y=r1.verticals.start,yend=r1.verticals.stop)) + 
  geom_segment(aes(x=-r1.x[2],xend=-r1.x[2],y=-r1.verticals.start,yend=-r1.verticals.stop)) 

### add second round
empty.bracket <- empty.bracket + 
  geom_segment(aes(x=r2.x[1],y=r2.y,yend=r2.y,xend=r2.x[2])) + 
  geom_segment(aes(x=r2.x[1],y=-r2.y,yend=-r2.y,xend=r2.x[2])) + 
  geom_segment(aes(x=-r2.x[1],y=r2.y,yend=r2.y,xend=-r2.x[2])) + 
  geom_segment(aes(x=-r2.x[1],y=-r2.y,yend=-r2.y,xend=-r2.x[2])) + 
  geom_segment(aes(x=r2.x[2],xend=r2.x[2],y=r2.verticals.start,yend=r2.verticals.stop)) + 
  geom_segment(aes(x=r2.x[2],xend=r2.x[2],y=-r2.verticals.start,yend=-r2.verticals.stop)) + 
  geom_segment(aes(x=-r2.x[2],xend=-r2.x[2],y=r2.verticals.start,yend=r2.verticals.stop)) + 
  geom_segment(aes(x=-r2.x[2],xend=-r2.x[2],y=-r2.verticals.start,yend=-r2.verticals.stop))
  
  
### add third round
empty.bracket <- empty.bracket + 
  geom_segment(aes(x=r3.x[1],y=r3.y,yend=r3.y,xend=r3.x[2])) + 
  geom_segment(aes(x=r3.x[1],y=-r3.y,yend=-r3.y,xend=r3.x[2])) + 
  geom_segment(aes(x=-r3.x[1],y=r3.y,yend=r3.y,xend=-r3.x[2])) + 
  geom_segment(aes(x=-r3.x[1],y=-r3.y,yend=-r3.y,xend=-r3.x[2])) + 
  geom_segment(aes(x=r3.x[2],xend=r3.x[2],y=r3.verticals.start,yend=r3.verticals.stop)) + 
  geom_segment(aes(x=r3.x[2],xend=r3.x[2],y=-r3.verticals.start,yend=-r3.verticals.stop)) + 
  geom_segment(aes(x=-r3.x[2],xend=-r3.x[2],y=r3.verticals.start,yend=r3.verticals.stop)) + 
  geom_segment(aes(x=-r3.x[2],xend=-r3.x[2],y=-r3.verticals.start,yend=-r3.verticals.stop))

### add fourth round
empty.bracket <- empty.bracket + 
  geom_segment(aes(x=r4.x[1],y=r4.y,yend=r4.y,xend=r4.x[2])) + 
  geom_segment(aes(x=r4.x[1],y=-r4.y,yend=-r4.y,xend=r4.x[2])) + 
  geom_segment(aes(x=-r4.x[1],y=r4.y,yend=r4.y,xend=-r4.x[2])) + 
  geom_segment(aes(x=-r4.x[1],y=-r4.y,yend=-r4.y,xend=-r4.x[2])) + 
  geom_segment(aes(x=r4.x[2],xend=r4.x[2],y=r4.verticals.start,yend=r4.verticals.stop)) + 
  geom_segment(aes(x=r4.x[2],xend=r4.x[2],y=-r4.verticals.start,yend=-r4.verticals.stop)) + 
  geom_segment(aes(x=-r4.x[2],xend=-r4.x[2],y=r4.verticals.start,yend=r4.verticals.stop)) + 
  geom_segment(aes(x=-r4.x[2],xend=-r4.x[2],y=-r4.verticals.start,yend=-r4.verticals.stop)) 

### add fifth round: add necessary horizontal bars and then
### vertical bars
empty.bracket <- empty.bracket + 
  geom_segment(aes(x=r5.x[1],y=r5.y,yend=r5.y,xend=r5.x[2])) +  
  geom_segment(aes(x=r5.x[1],y=-r5.y,yend=-r5.y,xend=r5.x[2])) + 
  geom_segment(aes(x=r5.x[2],y=-r5.y, yend=r5.y, xend=r5.x[2])) + 
  geom_segment(aes(x=-r5.x[1],y=r5.y,yend=r5.y,xend=-r5.x[2])) + 
  geom_segment(aes(x=-r5.x[1],y=-r5.y,yend=-r5.y,xend=-r5.x[2])) + 
  geom_segment(aes(x=-r5.x[2],y=-r5.y,yend=r5.y,xend=-r5.x[2])) 

### due to symmetry, the 6th (and final round)
empty.bracket <- empty.bracket  + 
  geom_segment(aes(x=r6.x[1],y=r6.y,xend=r6.x[2],yend=r6.y)) + 
  geom_segment(aes(x=-r6.x[1],y=-r6.y,xend=-r6.x[2],yend=-r6.y))

### add winner location
empty.bracket <- empty.bracket + 
  geom_segment(aes(x=mean(r6.x),xend=-mean(r6.x),y=0,yend=0))

### add NCAA logo
empty.bracket + annotation_raster(png::readPNG("2022/March_Madness_logo.png"), 
                  xmin = mean(r6.x), xmax = -mean(r6.x), 
                  ymin = r1.y[13], ymax = r1.y[16]) 

### put some test labels on the bracket slots
Labels <- c("Alabama", "Alaska", "Arizona", "Arkansas", "Virginia Commonwealth")
TextFrame <- data.frame(X = r1.x[1], Y = sample(r1.y,5), LAB = Labels)
TextFrame <- transform(TextFrame, w = strwidth(LAB, 'inches') + 0.05, h = strheight(LAB, 'inches') + 0.5)

### display results
empty.bracket + geom_rect(data = TextFrame, aes(xmin = X, xmax = X + w, ymin = Y, ymax = Y + h),alpha=0) + 
  geom_text(data=TextFrame,aes(x=X,y=Y,label=LAB),size=rel(2),hjust=0,vjust=0)
}

bracketDraw()

# Tadoo:
# (1) Add West (top left), South (top right), East (bottom left), and Midwest (bottom right) as shadow text labels
# (2) Draw teams round by round using data frame in form:
# Round, TeamNum, TeamName, ProbWin

submission <- readr::read_csv("2021/SubmissionFiles/SubmissionLDA_AwinStage2.csv")
seeds <- read_csv("2021/bracketData/MNCAATourneySeeds.csv") %>% filter(Season == 2021) %>% 
  mutate(Seed = str_remove(Seed, pattern = "b"))

preds <- submission %>% mutate(Team1 = as.integer(map_chr(str_split(ID, pattern = "_"), 2)), 
                               Team2 = as.integer(map_chr(str_split(ID, pattern = "_"), 3))) %>% 
  select(Team1, Team2, Pred)

# Adding team names
teams <- read_csv("2022/mens-march-mania-2022/MDataFiles_Stage1/MTeams.csv") %>% 
  select(TeamID:TeamName)

# Data for drawing bracket
bracketData <- preds %>% left_join(seeds, by = c("Team1" = "TeamID")) %>% 
  rename(Seed1 = Seed, Pred1 = Pred) %>% 
  left_join(seeds, by = c("Team2" = "TeamID", "Season" = "Season")) %>% 
  rename(Seed2 = Seed) %>% mutate(Pred2 = 1 - Pred1) %>% 
  select(Season, Team1:Pred1, Pred2, Seed1, Seed2) %>% 
  filter(!str_detect(Seed2, pattern = "a"), !str_detect(Seed1, pattern = "a")) %>% 
  mutate(Region1 = str_sub(Seed1, start = 1, end = 1),
         Region2 = str_sub(Seed2, start = 1, end = 1),
         Seed1 = as.integer(str_sub(Seed1, start = 2, end = -1)),
         Seed2 = as.integer(str_sub(Seed2, start = 2, end = -1))) %>% 
  mutate(Region1 = case_when(Region1 == "W" ~ "East",
                             Region1 == "X" ~ "West",
                             Region1 == "Y" ~ "Midwest",
                             Region1 == "Z" ~ "South"),
         Region2 = case_when(Region2 == "W" ~ "East",
                             Region2 == "X" ~ "West",
                             Region2 == "Y" ~ "Midwest",
                             Region2 == "Z" ~ "South")) %>% 
  left_join(teams, by = c("Team1" = "TeamID")) %>% 
  left_join(teams, by = c("Team2" = "TeamID"), suffix = c("1", "2"))

bracketDataFull <- bracketData %>% 
  bind_rows(bracketData %>% 
              rename_with(~ gsub("1", "A", .x, fixed = TRUE), ends_with("1")) %>% 
              rename_with(~ gsub("2", "B", .x, fixed = TRUE), ends_with("2")) %>% 
              rename_with(~ gsub("A", "2", .x, fixed = TRUE), ends_with("A")) %>% 
              rename_with(~ gsub("B", "1", .x, fixed = TRUE), ends_with("B")) %>% 
              select(colnames(bracketData)))

# Possible matchups
round1 <- data.frame(Seed1 = 1:8, Seed2 = 16:9, 
  Region1 = rep(c("East", "West", "Midwest", "South"), each = 8)) %>% 
  mutate(Region2 = Region1, Round = 1,
         SeedProd = Seed1*Seed2,
         Matchup = case_when(SeedProd == 1*16 ~ 1,
                             SeedProd == 8*9 ~ 2,
                             SeedProd == 5*12 ~ 3,
                             SeedProd == 4*13 ~ 4,
                             SeedProd == 6*11 ~ 5,
                             SeedProd == 3*14 ~ 6,
                             SeedProd == 7*10 ~ 7,
                             SeedProd == 2*15 ~ 8)) %>% select(-SeedProd)

round2 <- map_dfr(.x = rep(list(list(c(1, 16), c(8, 9)), list(c(5, 12), c(4, 13)), 
                            list(c(6, 11), c(3, 14)), list(c(7, 10), c(2, 15))), 4), 
                  .f = function(x) {expand_grid(Seed1 = x[[1]], Seed2 = x[[2]])}) %>%  
                     mutate(Region1 = rep(c("East", "West", "Midwest", "South"), each = 16)) %>% 
  mutate(Region2 = Region1, Round = 2,
         Matchup = case_when(Seed1 %in% c(1, 16) ~ 1,
                             Seed1 %in% c(5, 12) ~ 2,
                             Seed1 %in% c(6, 11) ~ 3,
                             Seed1 %in% c(7, 10) ~ 4))

round3 <- expand_grid(Seed1 = c(1, 16, 8, 9), Seed2 = c(5, 12, 4, 13), 
                      Region1 = c("East", "West", "Midwest", "South")) %>%
  bind_rows(expand_grid(Seed1 = c(6, 11, 3, 14), Seed2 = c(7, 10, 2, 15), 
              Region1 = c("East", "West", "Midwest", "South"))) %>% 
  mutate(Region2 = Region1, Round = 3, Matchup = rep(1:2, each = 64))

round4 <- expand_grid(Seed1 = c(1, 16, 8, 9, 5, 12, 4, 13), 
                      Seed2 = c(6, 11, 3, 14, 7, 10, 2, 15), 
                      Region1 = c("East", "West", "Midwest", "South")) %>% 
  mutate(Region2 = Region1, Round = 4, Matchup = rep(1:2, each = 128))

round5 <- expand_grid(Seed1 = 1:16, Seed2 = 1:16, Region1 = c("West", "South")) %>% 
  mutate(Region2 = ifelse(Region1 == "West", "East", "Midwest"), Round = 5, Matchup = rep(1:2, each = 256))

round6 <- expand_grid(Seed1 = 1:16, Seed2 = 1:16, Region1 = c("West", "East"),
                      Region2 = c("South", "Midwest")) %>% 
  mutate(Round = 6, Matchup = 1)

roundsWide <- bind_rows(round1, round2, round3, 
                    round4, round5, round6) %>% 
  left_join(bracketDataFull %>% select(-Season, -Team1, -Team2))

roundsLong <- roundsWide %>% pivot_longer(cols = c(Seed1, Seed2), 
                                         names_to = "Team", values_to = "Seed") %>% 
  mutate(Region1 = fct_relevel(Region1, "West", "East", "South", "Midwest"),
         Region2 = fct_relevel(Region2, "West", "East", "South", "Midwest")) %>% 
  arrange(Round, Region1, Matchup, Team, Seed) %>% 
  mutate(Pred1 = sprintf('%.3f',Pred1), Pred2 = sprintf('%.3f',Pred2),
      Winner = ifelse(Pred1 > Pred2, TeamName1, TeamName2), TeamName = case_when(
    Team == "Seed1" & Region1 %in% c("West", "East") ~ paste0(Seed, " ", TeamName1, " (", Pred1, ")"),
    Team == "Seed2" & Region2 %in% c("West", "East") ~ paste0(Seed, " ", TeamName2, " (", Pred2, ")"),
    Team == "Seed1" & Region1 %in% c("South", "Midwest") ~ paste0("(", Pred1, ") ", TeamName1, " ", Seed),
    Team == "Seed2" & Region2 %in% c("South", "Midwest") ~ paste0("(", Pred2, ") ", TeamName2, " ", Seed)))

# Printing round 1 teams
r1Labs <- roundsLong %>% filter(Round == 1) %>% 
  mutate(Xstart = c(rep(r1.x[1], 16), rep(r1.x[1], 16),
                    rep(-r1.x[2], 16), rep(-r1.x[2], 16)),
         Y = c(rep(c(rev(r1.y), r1.y), 2)*rep(c(1, -1), each = 16))+0.001)

empty.bracket +
geom_text(data=r1Labs,aes(x=Xstart,y=Y,label=TeamName),size=rel(2),hjust=0,vjust=0)

# Winners from round 1
r1Winners <- roundsLong %>% filter(Round == 1) %>% pull(Winner) %>% unique()

# Printing round 2 teams
r2Labs <- roundsLong %>% filter(Round == 2, TeamName1 %in% r1Winners, 
                                TeamName2 %in% r1Winners) %>% distinct() %>% 
  mutate(Xstart = c(rep(r2.x[1], 8), rep(r2.x[1], 8),
                    rep(-r2.x[2], 8), rep(-r2.x[2], 8)),
         Y = c(rev(r2.y), -r2.y, rev(r2.y), -r2.y))

# Winners from round 2
r2Winners <- roundsLong %>% filter(Round == 2, TeamName1 %in% r1Winners, 
                                   TeamName2 %in% r1Winners) %>% distinct() %>%
  pull(Winner) %>% unique()

# Printing round 3 teams
r3Labs <- roundsLong %>% filter(Round == 3, TeamName1 %in% r2Winners, 
                                TeamName2 %in% r2Winners) %>% distinct() %>% 
  mutate(Xstart = c(rep(r3.x[1], 4), rep(r3.x[1], 4),
                    rep(-r3.x[2], 4), rep(-r3.x[2], 4)),
         Y = c(rev(r3.y), -r3.y, rev(r3.y), -r3.y))

# Winners from round 3
r3Winners <- roundsLong %>% filter(Round == 3, TeamName1 %in% r2Winners, 
                                   TeamName2 %in% r2Winners) %>% distinct() %>%
  pull(Winner) %>% unique()

# Printing round 4 teams
r4Labs <- roundsLong %>% filter(Round == 4, TeamName1 %in% r3Winners, 
                                TeamName2 %in% r3Winners) %>% distinct() %>% 
  mutate(Xstart = c(rep(r4.x[1], 2), rep(r4.x[1], 2),
                    rep(-r4.x[2], 2), rep(-r4.x[2], 2)),
         Y = c(rev(r4.y), -r4.y, rev(r4.y), -r4.y))

# Winners from round 4
r4Winners <- roundsLong %>% filter(Round == 4, TeamName1 %in% r3Winners, 
                                   TeamName2 %in% r3Winners) %>% distinct() %>%
  pull(Winner) %>% unique()

# Printing round 5 teams
r5Labs <- roundsLong %>% filter(Round == 5, TeamName1 %in% r4Winners, 
                                TeamName2 %in% r4Winners) %>% distinct() %>% 
  mutate(Xstart = c(rep(r5.x[1], 1), rep(r5.x[1], 1),
                    rep(-r5.x[2], 1), rep(-r5.x[2], 1)),
         Y = c(rev(r5.y), -r5.y, rev(r5.y), -r5.y))

# Winners from round 5
r5Winners <- roundsLong %>% filter(Round == 5, TeamName1 %in% r4Winners, 
                                   TeamName2 %in% r4Winners) %>% distinct() %>%
  pull(Winner) %>% unique()

# Printing round 6 teams
r6Labs <- roundsLong %>% filter(Round == 6, TeamName1 %in% r5Winners, 
                                TeamName2 %in% r5Winners) %>% distinct() %>% 
  mutate(Xstart = c(rep(r6.x[1], 1), rep(-r6.x[2], 1)),
         Y = c(r6.y, -r6.y))

# Winners from round 6 (grand overall winner)
r6Winner <- roundsLong %>% filter(Round == 6, TeamName1 %in% r5Winners, 
                                   TeamName2 %in% r5Winners) %>% distinct() %>%
  pull(Winner) %>% unique()

empty.bracket +
  geom_text(data=r1Labs,aes(x=Xstart,y=Y,label=TeamName),size=rel(2),hjust=0,vjust=0) +
  geom_text(data=r2Labs,aes(x=Xstart,y=Y,label=TeamName),size=rel(2),hjust=0,vjust=0) +
  geom_text(data=r3Labs,aes(x=Xstart,y=Y,label=TeamName),size=rel(2),hjust=0,vjust=0) +
  geom_text(data=r4Labs,aes(x=Xstart,y=Y,label=TeamName),size=rel(2),hjust=0,vjust=0) +
  geom_text(data=r5Labs,aes(x=Xstart,y=Y,label=TeamName),size=rel(2),hjust=0,vjust=0) +
  geom_text(data=r6Labs,aes(x=Xstart,y=Y,label=TeamName),size=rel(2),hjust=0,vjust=0) +
  geom_text(data=r6Labs,aes(x=0-nchar(r6Winner)/10,y=0,label=r6Winner),size=rel(2),hjust=0,vjust=0) 
