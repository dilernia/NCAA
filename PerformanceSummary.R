#################
# Title: Summarizing NCAA Tournament Predictions
# Author: Andrew DiLernia
# Date: 04/18/2019
# Purpose: Summarize model performances for predicting NCAA Tournament games
#################

library(tidyverse)

# Reading in Test Data from Fitting ---------------------------------------

# Reading in data
desResp <- readRDS("Our_Data/designResponse.rds")

# Standardizing predictors and removing some columns
designMat <- subset(desResp[[1]], select = -c(Awin, Season, Tourney, Spread,
                                              ATeamID, BTeamID, Aloc, Site))
Aloc <- desResp[[1]]$Aloc
designMat <- as.data.frame(lapply(designMat, FUN = scale, 
                                  center = TRUE, scale = TRUE))

# Adding location column back in
designMat$Aloc <- as.factor(Aloc)

# Removing any rows with missing values for now. Maybe consider imputation later
compInds <- complete.cases(designMat)
designMat <- designMat[compInds, ]

# Creating model matrices (with outcome) and storing in list
modelDats <- list(MOV = designMat, Awin = designMat)
modelDats$MOV$outcome <- desResp[[2]]$MOV[compInds]
modelDats$Awin$outcome <- factor(desResp[[2]]$Awin[compInds])

# Creating model form objects and storing in list
forms <- list(MOV = outcome ~ 0 + ., Awin =  outcome ~ 0 + .)

# Creating model design matrices (without outcome) and storing in list
designMats <- list(MOV = model.matrix(forms$MOV, data = modelDats$MOV),
                   Awin = model.matrix(forms$Awin, data = modelDats$Awin))

# Setting parameters ------------------------------------------------------

# Should be one of 'MOV' or 'Awin'
model <- "MOV"
#model <- "Awin"
seed <- 1994
trainSize <- 0.80
set.seed(seed)

# Randomly selecting training set
trInds <- sample(1:nrow(modelDats[[model]]), replace = FALSE, 
                 size = ceiling(trainSize*nrow(modelDats[[model]])))
# Training objects
modelData <- modelDats[[model]][trInds, ]
designMat <- designMats[[model]][trInds, ]
outcome <- desResp[[2]][[model]][trInds]
form <- forms[[model]]

# Test Objects
modelData.test <- modelDats[[model]][-trInds, ]
designMat.test <- designMats[[model]][-trInds, ]

# Reobtaining Train/Test Performance --------------------------------------

# Reading in best model fits
models <- list.files("Runs/") %>% str_subset("Seed") %>% 
  map(.f = function(x) {temp <- readRDS(paste0("Runs/", x));
  return(temp[2:length(temp)])}) %>% unlist(recursive = FALSE)

# Loading or Saving performance results
#saveRDS(perfResults, "perfResults.rds")
if(file.exists("perfResults.rds")) {
perfResults <- readRDS("perfResults.rds")
} else {
perfResults <- list.files("Runs/") %>% str_subset("Seed") %>% 
  map_dfr(.f = function(x) {readRDS(paste0("Runs/", x))[[1]]})
}

# Naming models
names(models) <- perfResults$Model

# Creating separate model lists by outcome
modelsMOV <- models[which(perfResults$Response == "MOV")]
modelsBin <- models[which(perfResults$Response == "Win/Loss")]

# Function for obtaining predicted probabilities
predCalc <- function(modelData = modelData.test, designMat = designMat.test, modelList = modelsMOV) {
  
  treePreds <- predict(modelList[["Full Tree"]], modelData)
  prunedPreds <- predict(modelList[["Pruned Tree"]], modelData)
  rfPreds <- predict(modelList[["Random Forest"]], newdata = modelData, 
                     type = ifelse(model == "Awin", "prob", "raw"))
  boostPreds <- predict(modelList[["Grad Boost"]], modelData, 
                        type = ifelse(model == "Awin", "prob", "raw"))
  nnetPreds <- predict(modelList[["NNet"]], modelData, 
                       type = ifelse(model == "Awin", "prob", "raw"))
  # nnet3Preds <- predict(modelList[[]], modelData, 
  #                       type = ifelse(model == "Awin", "prob", "raw"))
  nnetfPreds <- predict(modelList[["NNetF"]], modelData, 
                        type = ifelse(model == "Awin", "prob", "raw"))
  svmPreds <- predict(modelList[["SVM Radial"]], modelData, 
                      type = ifelse(model == "Awin", "prob", "raw"))
  cvglmPreds <- predict(modelList[["CV glmnet"]], newx = designMat, 
                        s = "lambda.min", type = "response")
  
  # Finds optimal number of components
  ncomps <- as.numeric(strsplit(colnames(modelList[["PCR"]]$validation$PRESS)[
    which(modelList[["PCR"]]$validation$PRESS == min(modelList[["PCR"]]$validation$PRESS))], split = " ")[[1]][1])
  
  pcrPreds <- predict(modelList[["PCR"]], newdata = modelData, ncomp = ncomps, type = "response")
  ldaPreds <- predict(modelList[["LDA"]], newdata = modelData)$posterior
  
  return(setNames(list(treePreds, prunedPreds, rfPreds, boostPreds, 
                       nnetPreds, nnetfPreds, svmPreds, 
                       cvglmPreds, pcrPreds, ldaPreds), 
                  c("Full Tree", 'Pruned Tree', 
                    "Random Forest", 'Grad Boost', 
                    "NNet", "NNetF",
                    "SVM Radial",
                    "CV glmnet", "PCR", "LDA")))
}

# Preventing extreme predictions. Need to tune thresh for final preds
capper <- function(x, thresh = 0.025){
  if(is.null(dim(x)) == FALSE) {if(dim(x)[2] == 2) {x <- x[, 2]}}
  for(i in 1:length(x)) {
    x[i] <- ifelse(x[i] < thresh, thresh, ifelse(x[i] > 1-thresh, 1-thresh, x[i]))
  }
  return(as.numeric(x))
}

if(model == "MOV") {
  # Obtaining predicted probs for training data set
  trPreds <- predCalc(modelData = modelData, designMat = designMat)
  
  # Estimating custom conversion between MOV and win probability using training data
  # model fits
  converts <- lapply(FUN = function(MOVhats) {
    fit <- glm(formula = form, data = data.frame(outcome = desResp[[2]][["Awin"]][trInds], 
                                                 movHat = MOVhats), family = "binomial")
    return(fit)
  }, X = trPreds)
  
  # Converting from MOV to estimated win prob
  movList <- predCalc()
  predList <- mapply(fit = converts, movs = movList, 
                     FUN = function(fit, movs) {predict(fit, newdata = data.frame(movHat = movs), 
                                                        type = 'response')}, SIMPLIFY = FALSE)
} else {
  predList <- predCalc(modelList = modelsBin)
}

# Capping predicted probabilities
predList <- lapply(predList, FUN = capper)

# Evaluating methods
predSummary <- function(preds, response, 
                        truths = as.numeric(as.character(modelDats[["Awin"]][-trInds, "outcome"]))) {
  
  misRate <- round(mean(abs(as.integer(preds > 0.50) - truths)), 4)
  meanSqErr <- round(mean((preds - truths)^2), 4)
  
  sensitivity <- round(mean(preds[which(truths == 1)] > 0.50), 4)
  specificity <- round(1 - mean(preds[which(truths == 0)] > 0.50), 4)
  
  logLoss <- -mean(truths*log(preds) + (1-truths)*log(1-preds))
  result <- data.frame(`Misclassification Rate` = misRate,
                       `Mean Squared Error` = meanSqErr,
                       Sensitivity = sensitivity,
                       Specificity = specificity,
                       `Log Loss` = round(logLoss, 4),
                       Response = ifelse(response == "Awin", "Win/Loss", "MOV"))
  return(result)
}

# Summarizing performance of methods
results <- do.call("rbind", mapply(FUN = predSummary, preds = predList, 
                                   response = model, SIMPLIFY = FALSE))

xtable::xtable(results, digits = 4)

saveRDS(results, paste0("results_", model, "_.rds"))

# Obtaining Final Predictions -------------------------------------------------------

# Reading in design matrix for final predicitons
subA <- read.csv("SubmissionFiles/SubmissionFileA.csv")
finalDesign <- readRDS("SubmissionFiles/tournamentGames2019.rds")

# Cleaning submission files
subA <- subA %>% select(-X) %>% rename(id = ID, pred = Pred) %>% 
  mutate(pred = 0.50)

# Centering and scaling design matrix
fAloc <- finalDesign$Aloc
finalDesign <- as.data.frame(lapply(finalDesign, FUN = scale, center = TRUE,
                                    scale = TRUE))
finalDesign$Aloc <- fAloc

# Renaming outcome column
colnames(finalDesign)[which(colnames(finalDesign) == "Awin")] <- "outcome"

# Switching which team is A and B
colnames(finalDesign)[which(colnames(finalDesign) != "Aloc")] <- colnames(finalDesign)[which(colnames(finalDesign) != "Aloc")] %>%
  gsub(pattern = "AstRatio", replacement = "astratio") %>%
  gsub(pattern = "A", replacement = "xxx") %>%
  gsub(pattern = "B", replacement = "A") %>%
  gsub(pattern = "xxx", replacement = "B") %>%
  gsub(pattern = "astratio", replacement = "AstRatio")

# Rearranging columns to be same as when fitting models
finalDesign <- finalDesign[, colnames(modelData.test)]

# Recoding Aloc as factor
finalDesign$Aloc <- factor(finalDesign$Aloc, 
                           levels = levels(modelData.test$Aloc))

# Creating final design matrices for CV glmnet
finalDesignMat <- model.matrix(outcome ~ 0 + ., data = mutate(finalDesign, outcome = 1))

# Obtaining predictions for tournament games for each model
if(model == "MOV") {
  # Converting from MOV to estimated win prob
  fmovList <- predCalc(modelData = finalDesign, designMat = finalDesignMat, modelList = modelsMOV)
  fpredList <- mapply(fit = converts, movs = fmovList, 
                     FUN = function(fit, movs) {predict(fit, newdata = data.frame(movHat = movs), 
                                                        type = 'response')}, SIMPLIFY = FALSE)
} else {
  fpredList <- predCalc(modelData = finalDesign, designMat = finalDesignMat, modelList = modelsBin)
}

# Capping predicted probabilities
fpredList <- lapply(fpredList, FUN = capper)

# Reading in and cleaning tournament results ------------------------------

# Reading in actual tournament game outcomes and excluding play-ins
tourn19 <- readxl::read_excel("NCAA_Tourn_Results19.xlsx") %>% filter(Round != "First Four") %>% select(School, Opponent, Diff) %>% 
  rename(TeamW = School, TeamL = Opponent, MOV = Diff)

# Cleaning team names
tourn19$TeamW <- tolower(map(str_split(tourn19$TeamW, pattern = "\\s+", n = 2), 2))
tourn19$TeamL <- tolower(map(str_split(tourn19$TeamL, pattern = "\\s+", n = 2), 2))

# Converting to kaggle format for team names/ids
teamIDs <- read.csv("TeamSpellings.csv")
res19 <- tourn19 %>% left_join(teamIDs, by = c("TeamW" = "TeamNameSpelling")) %>% 
  rename(Wid = TeamID) %>% left_join(teamIDs, by = c("TeamL" = "TeamNameSpelling")) %>%
  rename(Lid = TeamID) %>% mutate(MOVnew = ifelse(Wid < Lid, MOV, -MOV),
                                  Awin = as.integer(MOVnew > 0)) %>% as.data.frame()
res19[, c("smID", "bigID", "id")] <- NA
for(r in 1:nrow(res19)) {
  res19[r, "smID"] <- min(c(res19[r, "Wid"], res19[r, "Lid"]))
  res19[r, "bigID"] <- max(c(res19[r, "Wid"], res19[r, "Lid"]))
  res19[r, "id"] <- paste0("2019_", res19[r, "smID"], "_", res19[r, "bigID"])
}
res19 <- res19 %>% select(id, Awin)

res19 <- left_join(subA, res19, by = c("id" = "id"))
keeps <- complete.cases(res19)
f.truths <- res19[complete.cases(res19), ] %>% select(Awin) %>% unlist()

# Subsetting predictions to only games that occurred
trimFpredList <- lapply(X = fpredList, FUN = function(x){return(x[keeps])})

# Summarizing performance of methods
f.results <- do.call("rbind", lapply(FUN = predSummary, X = trimFpredList, 
                                   response = model, truths = f.truths))

xtable::xtable(f.results, digits = 4)
saveRDS(f.results, paste0("f.results_", model, "_.rds"))

# Reading in and cleaning final results for slides
f.resultsMOV <- readRDS(paste0("f.results_", "MOV", "_.rds"))
f.resultsAwin <- readRDS(paste0("f.results_", "Awin", "_.rds"))

f.resultsMOV$Model <- rownames(f.resultsMOV)
f.resultsMOV <- f.resultsMOV %>% arrange(Log.Loss) %>% 
  select(Model, Misclassification.Rate, Log.Loss)
xtable::xtable(f.resultsMOV, digits = 4)

f.resultsAwin$Model <- rownames(f.resultsAwin)
f.resultsAwin <- f.resultsAwin %>% arrange(Log.Loss) %>% 
  select(Model, Misclassification.Rate, Log.Loss)
xtable::xtable(f.resultsAwin, digits = 4)

# Kaggle Competition Summary ----------------------------------------------

kaggleRes <- readxl::read_excel("Kaggle_Team_Results.xlsx") %>% 
  select(`Team Name`, Score, `#`) %>% 
  rename(Team = `Team Name`, LogLoss = Score, Rank = `#`) %>% 
  mutate(Model = "Other Kagglers", Response = "Other Kagglers")

aggRes <- rbind(cbind(f.resultsMOV, data.frame(Response = rep("MOV", nrow(f.resultsMOV)))), 
                cbind(f.resultsAwin, data.frame(Response = rep("Win/Loss", nrow(f.resultsAwin))))) %>% 
  arrange(Log.Loss)

# Combining kaggle competition data and our model results
temp <- aggRes %>% select(Log.Loss, Model, Response) %>% 
  rename(LogLoss = Log.Loss) %>% mutate(Team = NA, Rank = NA) %>% 
  select(Team, LogLoss, Rank, Model, Response)

combined <- rbind(kaggleRes, temp) %>% arrange(LogLoss)
combined$Rank <- 1:nrow(combined)

# Creating bar chart
if(is.factor(aggRes$Model) == FALSE) {
aggRes$Model <- factor(aggRes$Model, 
                        levels = rev(unique(aggRes$Model)))
}


ggBars <- aggRes %>% ggplot(aes(x = Model, y = Log.Loss, fill = Response)) +
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values = c("firebrick2", "skyblue")) +
  labs(title = "Performance for 2019 NCAA Tournament", y = "Log Loss", x = "") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
ggBars
ggsave(ggBars, filename = "barPlot.pdf", device = cairo_pdf)

# Plotting points 
if(is.factor(combined$Response) == FALSE) {
combined$Response <- factor(combined$Response, levels = c("Win/Loss", "MOV", "Other Kagglers"))
}
minRank <- 800
ggPoints <- combined %>% filter(Rank <= minRank) %>% ggplot(aes(x = Rank, y = LogLoss, color = Response)) +
  geom_point()  + 
  geom_point(data = combined %>% filter(Response != "Other Kagglers", Rank <= minRank)) +
  scale_color_manual(values = c("skyblue", "firebrick2", "black"), name = "Model") +
  geom_hline(yintercept = 0.69314, linetype = 2) +
  labs(title = "NCAA Kaggle Competition 2019", y = "Log Loss") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1, minRank), # This focuses the x-axis on the range of interest
                  ylim = c(0.40, 1), clip = 'off') +
  annotate(geom = "segment", x = 750, xend = 800, yend = 0.25,
           y = 0.25, color = "black", linetype = "dashed") +
  annotate(geom = "text", x = c(955), 
           y = c(0.25), label = ": naive model")
ggPoints
ggsave(ggPoints, filename = "pointsPlot.pdf", device = cairo_pdf)
