#################
# Title: March Madness Bracket Creator
# Author: Andrew DiLernia
# Date: 07/19/2017
# Purpose: Generate filled out NCAA bracket given Kaggle predictions
#################

library(tidyverse)
library(png)

womens <- FALSE

# This person did it with ggplot: https://stackoverflow.com/questions/16290052/march-madness-brackets-with-ggplot2
bracketDraw <- function(submission, seeds, teams, ncaaLogo, lineColor = "black") {
  
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
  
  vec1x <- c(1, 1, -1, -1)
  vec1y <- c(1, -1, 1, -1)
  vec11 <- c(1, -1)
  
  lineCoords <- list()
  
  lineCoords$R1 <- tibble(x = c(rep(as.numeric(vec1x %o% r1.x[1]), each = 16),
                                rep(as.numeric(vec1x %o% r1.x[2]), each = 8)),
                          y = c(as.numeric(r1.y %o% vec1y), 
                                as.numeric(r1.verticals.start %o% vec1y)),
                          xend = c(rep(as.numeric(vec1x %o% r1.x[2]), each = 16),
                                   rep(as.numeric(vec1x %o% r1.x[2]), each = 8)),
                          yend = c(as.numeric(r1.y %o% vec1y),
                                   as.numeric(r1.verticals.stop %o% vec1y)),
                          Round = c(rep(1, times = 96)))
  
  lineCoords$R2 <- tibble(x = c(rep(as.numeric(vec1x %o% r2.x[1]), each = 8),
                                rep(as.numeric(vec1x %o% r2.x[2]), each = 4)),
                          y = c(as.numeric(r2.y %o% vec1y), 
                                as.numeric(r2.verticals.start %o% vec1y)),
                          xend = c(rep(as.numeric(vec1x %o% r2.x[2]), each = 8),
                                   rep(as.numeric(vec1x %o% r2.x[2]), each = 4)),
                          yend = c(as.numeric(r2.y %o% vec1y),
                                   as.numeric(r2.verticals.stop %o% vec1y)),
                          Round = c(rep(2, times = 48)))
  
  lineCoords$R3 <- tibble(x = c(rep(as.numeric(vec1x %o% r3.x[1]), each = 4),
                                rep(as.numeric(vec1x %o% r3.x[2]), each = 2)),
                          y = c(as.numeric(r3.y %o% vec1y), 
                                as.numeric(r3.verticals.start %o% vec1y)),
                          xend = c(rep(as.numeric(vec1x %o% r3.x[2]), each = 4),
                                   rep(as.numeric(vec1x %o% r3.x[2]), each = 2)),
                          yend = c(as.numeric(r3.y %o% vec1y),
                                   as.numeric(r3.verticals.stop %o% vec1y)),
                          Round = c(rep(3, times = 24)))
  
  lineCoords$R4 <- tibble(x = c(rep(as.numeric(vec1x %o% r4.x[1]), each = 2),
                                rep(as.numeric(vec1x %o% r4.x[2]), each = 1)),
                          y = c(as.numeric(r4.y %o% vec1y), 
                                as.numeric(r4.verticals.start %o% vec1y)),
                          xend = c(rep(as.numeric(vec1x %o% r4.x[2]), each = 2),
                                   rep(as.numeric(vec1x %o% r4.x[2]), each = 1)),
                          yend = c(as.numeric(r4.y %o% vec1y),
                                   as.numeric(r4.verticals.stop %o% vec1y)),
                          Round = c(rep(4, times = 12)))
  
  lineCoords$R5 <- tibble(x = r5.x[rep(c(1, 1, 2), 2)]*rep(vec11, each = 3),
                          y = r5.y*rep(c(1, -1, -1), times = 2),
                          xend = r5.x[2]*rep(vec11, each = 3),
                          yend = r5.y*rep(c(1, -1, 1), times = 2),
                          Round = c(rep(5, times = 6)))
  
  lineCoords$R6 <- tibble(x = r6.x[1]*vec11, 
                          y = r6.y*vec11, 
                          xend = r6.x[2]*vec11, 
                          yend = r6.y*vec11,
                          Round = c(rep(6, times = 2)))
  
  lineCoords$R7 <- tibble(x = mean(r6.x),
                          y = 0,
                          xend = -mean(r6.x),
                          yend = 0,
                          Round = 7)
  
  lineCoords$R0 <- tibble(x = rep(c(r4.x[1], -(2*r4.x[2]-r4.x[1])), each = 2),
                          y = -r1.y[16]*vec1y,
                          xend = rep(c(2*r4.x[2]-r4.x[1], -r4.x[1]), each = 2),
                          yend = -r1.y[16]*vec1y,
                          Round = rep(0, 4))
  
  fullCoords <- bind_rows(lineCoords)
  
  # Printing bracket with lines
  empty.bracket <- empty.bracket + 
    geom_segment(data = fullCoords, 
                 color = lineColor, 
                 mapping = aes(x = x, xend = xend, y = y, yend = yend))
  
  # Add NCAA logo
  empty.bracket <- empty.bracket + 
    annotation_raster(ncaaLogo, xmin = mean(r6.x), xmax = -mean(r6.x),
                      ymin = r1.y[13]*1.05, ymax = r1.y[16]*1.05) 
  
  # Cleaning up predictions
  preds <- submission %>% mutate(Team1 = as.integer(map_chr(str_split(ID, pattern = "_"), 2)), 
                                 Team2 = as.integer(map_chr(str_split(ID, pattern = "_"), 3))) %>% 
    dplyr::select(Team1, Team2, Pred)
  
  # Data for drawing bracket
  bracketData <- preds %>% left_join(seeds, by = c("Team1" = "TeamID")) %>% 
    dplyr::rename(Seed1 = Seed, Pred1 = Pred) %>% 
    left_join(seeds, by = c("Team2" = "TeamID", "Season" = "Season")) %>% 
    dplyr::rename(Seed2 = Seed) %>% mutate(Pred2 = 1 - Pred1) %>% 
    dplyr::select(Season, Team1:Pred1, Pred2, Seed1, Seed2) %>% 
    mutate(Region1 = str_sub(Seed1, start = 1, end = 1),
           Region2 = str_sub(Seed2, start = 1, end = 1),
           PlayIn = case_when(str_detect(Seed1, pattern = "a|b") &
                                str_detect(Seed2, pattern = "a|b") &
                                Region1 == Region2 & 
                                str_remove(Seed1, pattern = "a|b") == str_remove(Seed2, pattern = "a|b")~ TRUE,
                              TRUE ~ FALSE),
           Seed1 = as.integer(str_sub(str_remove(Seed1, pattern = "a|b"), start = 2, end = -1)),
           Seed2 = as.integer(str_sub(str_remove(Seed2, pattern = "a|b"), start = 2, end = -1))) %>% 
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
  
  # First four play-in games
  firstFour <- bracketDataFull %>% filter(PlayIn) %>% 
    mutate(Round = 0, Matchup = as.integer(factor(paste0(ifelse(Team1 < Team2, paste0(Team1, "_", Team2), 
                                                                paste0(Team2, "_", Team1)))))) %>% 
    select(Seed1, Seed2, Region1, Region2, Round, Matchup)
  
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
    mutate(Region2 = ifelse(Region1 == "West", "East", "Midwest"), Round = 5, 
           Matchup = rep(1:2, each = 256))
  
  round6 <- expand_grid(Seed1 = 1:16, Seed2 = 1:16, Region1 = c("West", "East"),
                        Region2 = c("South", "Midwest")) %>% 
    mutate(Round = 6, Matchup = 1)
  
  roundsWide <- bind_rows(firstFour, round1, round2, round3, 
                          round4, round5, round6) %>% 
    left_join(bracketDataFull %>% select(-Season, -Team1, -Team2))
  
  roundsLong <- roundsWide %>% pivot_longer(cols = c(Seed1, Seed2), 
                                            names_to = "Team", values_to = "Seed") %>% 
    mutate(Region1 = fct_relevel(Region1, "West", "East", "South", "Midwest"),
           Region2 = fct_relevel(Region2, "West", "East", "South", "Midwest")) %>% 
    arrange(Round, Region1, Matchup, Team, Seed) %>% 
    mutate(Pred1 = sprintf('%.3f',Pred1), Pred2 = sprintf('%.3f',Pred2),
           Winner = ifelse(Pred1 > Pred2, TeamName1, TeamName2), TeamName = case_when(
             Team == "Seed1" & PlayIn ~ paste0(paste0("(", Pred1, ") ", TeamName1, " ", Seed),
                                               " vs. ", paste0(Seed, " ", TeamName2, " (", Pred2, ")")),
             Team == "Seed2" & PlayIn ~ paste0(paste0("(", Pred2, ") ", TeamName2, " ", Seed),
                                               " vs. ", paste0(Seed, " ", TeamName1, " (", Pred1, ")")),
             Team == "Seed1" & Region1 %in% c("West", "East") ~ paste0(Seed, " ", TeamName1, " (", Pred1, ")"),
             Team == "Seed2" & Region2 %in% c("West", "East") ~ paste0(Seed, " ", TeamName2, " (", Pred2, ")"),
             Team == "Seed1" & Region1 %in% c("South", "Midwest") ~ paste0("(", Pred1, ") ", TeamName1, " ", Seed),
             Team == "Seed2" & Region2 %in% c("South", "Midwest") ~ paste0("(", Pred2, ") ", TeamName2, " ", Seed)))
  
  # Printing first four play-in games
  r0Labs <- roundsLong %>% filter(Round == 0) %>% arrange(Matchup) %>% 
    group_by(Matchup) %>% slice(1) %>% ungroup() %>% 
    mutate(Xstart = c(r4.x[1], r4.x[1],
                      -(2*r4.x[2]-r4.x[1]), -(2*r4.x[2]-r4.x[1])),
           Y = c(r1.y[16], -r1.y[16], r1.y[16], -r1.y[16]))
  
  # Winners from first four play-in games
  r0Winners <- roundsLong %>% filter(Round == 0) %>% arrange(Matchup) %>% 
    group_by(Matchup) %>% slice(1) %>% ungroup() %>% pull(Winner) %>% unique()
  
  playins <- roundsLong %>% filter(Round == 0) %>% arrange(Matchup) %>% 
    group_by(Matchup) %>% slice(1) %>% ungroup() %>% select(TeamName1:TeamName2) %>% 
    unlist()
  
  r0Losers <- playins[which(!(playins %in% r0Winners))]
  
  # Printing round 1 teams
  r1Labs <- roundsLong %>% filter(Round == 1, !(TeamName1 %in% r0Losers), 
                                  !(TeamName2 %in% r0Losers)) %>% 
    mutate(Xstart = c(rep(r1.x[1], 16), rep(r1.x[1], 16),
                      rep(-r1.x[2], 16), rep(-r1.x[2], 16)),
           Y = c(rep(c(rev(r1.y), r1.y), 2)*rep(c(1, -1), each = 16))+0.001)
  
  empty.bracket +
    geom_text(data=r1Labs,aes(x=Xstart,y=Y,label=TeamName),size=rel(2),hjust=0,vjust=0)
  
  # Winners from round 1
  r1Winners <- roundsLong %>% filter(Round == 1, !(TeamName1 %in% r0Losers), 
                                     !(TeamName2 %in% r0Losers)) %>% pull(Winner) %>% unique()
  
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
  
  # Printing filled out bracket
  sizeParam <- 3
  labData <- bind_rows(r0Labs, r1Labs, r2Labs, r3Labs, r4Labs, r5Labs, r6Labs)
  return(empty.bracket +
           geom_text(data=labData,aes(x=Xstart,y=Y,label=TeamName),size=rel(sizeParam),hjust=0,vjust=0) +
           geom_text(data=r6Labs,aes(x=0-nchar(r6Winner)/10,y=0,label=r6Winner),size=rel(sizeParam),hjust=0,vjust=0))
}

# Inputs for drawing bracket
seedsPath <- ifelse(womens, "2022-Womens/womens-march-mania-2022/WDataFiles_Stage2/WNCAATourneySeeds.csv",
      "2022/mens-march-mania-2022/MDataFiles_Stage2/MNCAATourneySeeds.csv")
mySeeds <- read_csv(paste0(seedsPath)) %>% 
  filter(Season == 2022)

teamsPath <- ifelse(womens, "2022-Womens/womens-march-mania-2022/WDataFiles_Stage2/WTeams.csv",
                    "2022/mens-march-mania-2022/MDataFiles_Stage2/MTeams.csv")

myTeams <- read_csv(teamsPath) %>% 
  select(TeamID:TeamName)

myLogo <- png::readPNG("2022/March_Madness_logo.png")

mySubmission <- readr::read_csv(paste0(ifelse(womens, "2022-Womens", "2022"), "/SubmissionFiles/Submission", 
                                       "nnet", "_Stage2.csv"))

# Drawing bracket
bracketDraw(submission = mySubmission, seeds = mySeeds, teams = myTeams,
            ncaaLogo = myLogo)

lapply(X = c("cvglmnet", "cvglmnet2", "nnet"), FUN = function(model) {
mySub <- readr::read_csv(paste0(ifelse(womens, "2022-Womens", "2022"), "/SubmissionFiles/Submission", 
                                       model, "_Stage2.csv"))

ggsave(plot = bracketDraw(submission = mySub, seeds = mySeeds, teams = myTeams,
                          ncaaLogo = myLogo),
       filename = paste0(ifelse(womens, "2022-Womens", "2022"), "/Brackets/", model, ".pdf"), width = 11.69, height = 8.27, units = "in")
})
