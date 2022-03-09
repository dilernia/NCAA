#################
# Title: Methods for Analyzing NCAA Tournament Data
# Author: Andrew DiLernia
# Date: 03/07/2019
# Purpose: Find best model for predicting NCAA Tournament games
#################

library(glmnet)
library(class)
library(pls)
library(MASS)
library(tree)
library(randomForest)
library(gbm)
library(e1071)
library(nnet)
library(caret)
library(kernlab)
library(RSNNS)
library(xgboost)
library(tidyverse)

# Whether to run in parallel or not
runParallel <- FALSE

# Use 3 most seasons for testing (TRUE) or randomly separate all games into training & test (FALSE)
trainRecent <- FALSE

# Should be one of 'MOV' or 'Awin'
model <- "MOV"
#model <- "Awin"

# Setting training size and seed. Size only matters if trainRecent = FALSE
seed <- 1994
trainSize <- 0.01
set.seed(seed)

if(runParallel) {
  library(doFuture)
  registerDoFuture()
  plan(multisession, workers = 5)
}

# Preparing design matrix and response vector for fitting -----------------

t1 <- Sys.time()

# Reading in data
desResp <- readRDS("2022/designResponse.rds")
desResp2 <- readRDS("2022/designResponse2.rds")

# Standardizing predictors and removing some columns
designMat <- desResp$Design
designMat2 <- desResp2$Design

# Removing any rows with missing values for now. Maybe consider imputation later
compInds <- complete.cases(designMat)
designMat <- designMat[compInds, ] %>% as.matrix() %>% as.data.frame()
designMat2 <- designMat2[compInds, ] %>% as.matrix() %>% as.data.frame()

# Creating model matrices (with outcome) and storing in list
modelDats <- list(MOV = designMat, Awin = designMat)
modelDats$MOV$outcome <- desResp$FullData$Afix_mov[compInds]
modelDats$Awin$outcome <- desResp$FullData$Afix_win[compInds]

modelDats2 <- list(MOV = designMat2, Awin = designMat2)
modelDats2$MOV$outcome <- desResp2$FullData$Afix_mov[compInds]
modelDats2$Awin$outcome <- desResp2$FullData$Afix_win[compInds]

# Creating model form objects and storing in list
forms <- list(MOV = outcome ~ 0 + ., Awin =  outcome ~ 0 + .)

# Creating model design matrices (without outcome) and storing in list
designMats <- list(MOV = model.matrix(forms$MOV, data = modelDats$MOV),
                   Awin = model.matrix(forms$Awin, data = modelDats$Awin))

designMats2 <- list(MOV = model.matrix(forms$MOV, data = modelDats2$MOV),
                    Awin = model.matrix(forms$Awin, data = modelDats2$Awin))

# Setting model parameters ------------------------------------------------------

# Setting aside 3 most reason seasons for test set
testSeasons <- desResp$FullData$Season %>% unique() %>% sort() %>% tail(3)

# Randomly selecting training set
if(trainRecent) {
  trInds <- which(!(desResp$FullData$Season %in% testSeasons)) %>% head()
} else {
  trInds <- sample(1:nrow(modelDats[[model]]), replace = FALSE, 
                   size = ceiling(trainSize*nrow(modelDats[[model]])))
}

# Training objects
modelData <- modelDats[[model]][trInds, ]
designMat <- designMats[[model]][trInds, ]
outcome <- desResp[[model]][trInds]
form <- forms[[model]]

modelData2 <- modelDats2[[model]][trInds, ]
designMat2 <- designMats2[[model]][trInds, ]

# Test Objects
modelData.test <- modelDats[[model]][-trInds, ]
designMat.test <- designMats[[model]][-trInds, ]

modelData.test2 <- modelDats2[[model]][-trInds, ]
designMat.test2 <- designMats2[[model]][-trInds, ]

# Changing factor level names to work with train() function
if(model == "Awin") {
  modelData$outcome <- as.factor(modelData$outcome) %>% 
    fct_recode(c("loss" = "FALSE"), c("win" = "TRUE"))
  
  modelData2$outcome <- as.factor(modelData2$outcome) %>% 
    fct_recode(c("loss" = "FALSE"), c("win" = "TRUE"))
  
  ctrl <- trainControl(method = "cv", number = 10, classProbs =  TRUE,
                       allowParallel = runParallel)
  
  # Changing outcome for PCR to be numeric
  modelData.pcr <- modelData
  modelData.pcr$outcome <- as.integer(as.character(modelData$outcome) == "win")
  
  modelData.pcr2 <- modelData2
  modelData.pcr2$outcome <- as.integer(as.character(modelData2$outcome) == "win")
  
} else {
  ctrl <- trainControl(method = "cv", number = 10, allowParallel = runParallel)
  modelData.pcr <- modelData
  modelData.pcr2 <- modelData2
}

# (1) Linear/Logistic Regression with Lasso Penalty using 10-fold CV
cvglmFit <- cv.glmnet(x = designMat, y = outcome[complete.cases(modelData)], 
                      nfolds = 10, family = ifelse(model == "Awin", "binomial", "gaussian"),
                      lambda = c(seq(0.00005, 0.00010, by = 0.00001), seq(0.005, 0.05, by = 0.005)),
                      gamma = c(0, 0.25, 0.5, 0.75, 1), parallel = runParallel)

cvglmFit2 <- cv.glmnet(x = designMat2, y = outcome[complete.cases(modelData)], 
                       nfolds = 10, family = ifelse(model == "Awin", "binomial", "gaussian"),
                       lambda = c(seq(0.0001, 0.001, by = 0.0001), seq(0.005, 0.05, by = 0.005)),
                       gamma = c(0, 0.25, 0.5, 0.75, 1), parallel = runParallel)

# (2) Principle Components Regression with 10-fold CV
pcrFit <- pcr(form, data = modelData.pcr, validation = "CV", segments = 10)
pcrFit2 <- pcr(form, data = modelData.pcr2, validation = "CV", segments = 10)

# Finds optimal number of components
ncomps <- as.numeric(strsplit(colnames(pcrFit$validation$PRESS)[
  which(pcrFit$validation$PRESS == min(pcrFit$validation$PRESS))], split = " ")[[1]][1])

ncomps2 <- as.numeric(strsplit(colnames(pcrFit2$validation$PRESS)[
  which(pcrFit2$validation$PRESS == min(pcrFit2$validation$PRESS))], split = " ")[[1]][1])

# (3) Linear Discriminant Analysis
ldaFit <- lda(form, data = modelData)
ldaFit2 <- lda(form, data = modelData2)

Sys.time() - t1

# (4) Optimally tuned mtry value random forest
defMtry <- floor(sqrt(ncol(modelData)))
my.rf <- caret::train(form = form,
                      data = modelData, method = "rf",
                      trControl = ctrl, ntree = 1000,
                      tuneGrid = expand.grid(mtry =(defMtry - 4):(defMtry + 3)))

Sys.time() - t1

# (5) Stochastic Gradient boosting
# For final run try n.trees = 5000
my.boost <- caret::train(form = form,
                         data = modelData, method = "gbm",
                         verbose = FALSE, distribution = ifelse(model == "Awin",
                                                                "bernoulli", "gaussian"),
                         trControl = ctrl, tuneGrid = expand.grid(n.trees = c(seq(800, 1400, by = 100)),
                                                                  shrinkage = c(seq(0.01, 0.07, by = 0.01)),
                                                                  interaction.depth = 1:3, n.minobsinnode = 5:9))
my.boost2 <- caret::train(form = form,
                          data = modelData2, method = "gbm",
                          verbose = FALSE, distribution = ifelse(model == "Awin",
                                                                 "bernoulli", "gaussian"),
                          trControl = ctrl, tuneGrid = expand.grid(n.trees = c(seq(800, 1400, by = 100)),
                                                                   shrinkage = c(0.05, 0.10, 0.16, seq(0.20, 0.50, by = 0.10)),
                                                                   interaction.depth = 1:3, n.minobsinnode = 6:10))


Sys.time() - t1

# (6) Artificial Neural Network w/ 1 hidden layer
my.nnet <- caret::train(form,
                        data = modelData, method = "nnet",
                        trControl = ctrl, trace = FALSE, maxit = 10000,
                        tuneGrid = expand.grid(size = c(2:5),
                                               decay = c(0.003, 0.005, 0.008, 0.01, 0.015, 0.02)),
                        linout = ifelse(model == "Awin", FALSE, TRUE))

Sys.time() - t1

# (7) eXtremeGradient Boosting (w/ trees)
# https://xgboost.readthedocs.io/en/latest/parameter.html
my.boostTree <- caret::train(form,
                             data = modelData, method = 'xgbTree', 
                             trControl = ctrl, verbose = 0,
                             tuneGrid = expand.grid(max_depth = c(4, 5, 6, 7, 8), eta = c(0.15, 0.20, 0.25, 0.30, 0.35), 
                                                    nrounds = 2, gamma = c(0, 1, 2), colsample_bytree = 1, 
                                                    min_child_weight = c(1, 3, 5), subsample = c(0.80, 1)))

my.boostTree2 <- caret::train(form,
                              data = modelData2, method = 'xgbTree', 
                              trControl = ctrl, verbose = 0,
                              tuneGrid = expand.grid(max_depth = c(4, 5, 6, 7, 8), eta = c(0.15, 0.20, 0.25, 0.30, 0.35), 
                                                     nrounds = 2, gamma = c(0, 1, 2), colsample_bytree = 1, 
                                                     min_child_weight = c(1, 3, 5), subsample = c(0.80, 1)))

Sys.time() - t1

# (8) Support Vector Machine (SVM)
# For final run try kernal = c("sigmoid", "radial"), 
# gamma = c(1/ncol(modelData), seq(0.005, 0.05, by = 0.005)), cost = seq(0.50, 2, by = 0.50)
my.svm <- caret::train(form,
                       data = modelData, method = 'svmRadialCost',
                       trControl = ctrl,
                       tuneGrid = expand.grid(C = c(0.01, 0.1, 0.30, 0.50, 0.70, 1,
                                                    2, 2.5, 3)))


Sys.time() - t1

# Function for obtaining predicted probabilities
predCalc <- function(modelData = modelData.test, designMat = designMat.test,
                     modelData2 = modelData.test2, designMat2 = designMat.test2) {
  
  rfPreds <- predict(my.rf, newdata = modelData, 
                     type = ifelse(model == "Awin", "prob", "raw"))
  boostPreds <- predict(my.boost, modelData, 
                        type = ifelse(model == "Awin", "prob", "raw"))
  boost2Preds <- predict(my.boost2, modelData2, 
                         type = ifelse(model == "Awin", "prob", "raw"))
  nnetPreds <- predict(my.nnet, modelData, 
                       type = ifelse(model == "Awin", "prob", "raw"))
  boostTreePreds <- predict(my.boostTree, modelData, 
                            type = ifelse(model == "Awin", "prob", "raw"))
  boostTree2Preds <- predict(my.boostTree2, modelData2, 
                             type = ifelse(model == "Awin", "prob", "raw"))
  svmPreds <- predict(my.svm, modelData, 
                      type = ifelse(model == "Awin", "prob", "raw"))
  cvglmPreds <- predict(cvglmFit, newx = designMat, 
                        s = "lambda.min", type = "response")
  cvglm2Preds <- predict(cvglmFit2, newx = designMat2, 
                         s = "lambda.min", type = "response")
  
  pcrPreds <- predict(pcrFit, newdata = modelData, ncomp = ncomps, type = "response")
  pcr2Preds <- predict(pcrFit2, newdata = modelData2, ncomp = ncomps2, type = "response")
  
  ldaPreds <- predict(ldaFit, newdata = modelData)$posterior
  lda2Preds <- predict(ldaFit2, newdata = modelData2)$posterior
  
  return(setNames(list(rfPreds, boostPreds, boost2Preds, 
                       nnetPreds, boostTreePreds, boostTree2Preds,
                       svmPreds, cvglmPreds, cvglm2Preds,
                       pcrPreds, pcr2Preds, ldaPreds, lda2Preds), 
                  c("Random Forest", "Grad Boost", "Grad Boost2", 
                    "NNet", "XGBoost Tree", "XGBoost Tree2",
                    "SVM Radial", "CV glmnet", "CV glmnet2",
                    "PCR", "PCR2", "LDA", "LDA2")))
}

# Preventing extrememe predictions. Need to tune thresh for final preds
capper <- function(x, thresh = 0.01){
  if(is.null(dim(x)) == FALSE) {if(dim(x)[2] == 2) {x <- x[, 2]}}
  for(i in 1:length(x)) {
    x[i] <- ifelse(is.na(x[i]), NA, ifelse(x[i] < thresh, thresh, 
                                           ifelse(x[i] > 1-thresh, 1-thresh, x[i])))
  }
  return(as.numeric(x))
}

if(model == "MOV") {
  # Obtaining predicted probs for training data set
  trPreds <- predCalc(modelData = modelData, designMat = designMat,
                      modelData2 = modelData2, designMat2 = designMat2)
  
  # Estimating custom conversion between MOV and win probability using training data
  # model fits
  converts <- lapply(FUN = function(MOVhats) {
    fit <- glm(formula = form, data = data.frame(outcome = desResp[["Awin"]][trInds], 
                                                 movHat = MOVhats), family = "binomial")
    return(fit)
  }, X = trPreds)
  
  # Converting from MOV to estimated win prob
  movList <- predCalc()
  predList <- mapply(fit = converts, movs = movList, 
                     FUN = function(fit, movs) {predict(fit, newdata = data.frame(movHat = movs), 
                                                        type = 'response')}, SIMPLIFY = FALSE)
} else {
  predList <- predCalc()
}

# Capping predicted probabilities
predList <- lapply(predList, FUN = capper)

# Evaluating methods
predSummary <- function(preds, response, 
                        truths = as.numeric(modelDats[["Awin"]][-trInds, "outcome"])) {
  
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
t2 <- Sys.time()

t2 - t1

# Saving results
saveRDS(list(results = results, 
             mods = list(cvglmFit, cvglmFit2, pcrFit, pcrFit2, ldaFit, ldaFit2,
                         my.rf, my.boost, my.boost2, my.nnet, my.boostTree, my.boostTree2)),
        paste0("2022/Fits/", model, "_Tr", trainSize, "_Seed", seed, ".rds"))

# Available models fit using caret package:
#http://topepo.github.io/caret/train-models-by-tag.html

# Making a bracket from our predictions:
#https://www.kaggle.com/c/mens-machine-learning-competition-2019/discussion/81642

# Previous winners
#https://storage.googleapis.com/kaggle-forum-message-attachments/473390/11340/Perennial%20Favories%202014-2018.png

# Summarizing performances ------------------------------------------------
library(tidyverse)

perfResults <- list.files("Runs/") %>% str_subset("Seed") %>% 
  map_dfr(.f = function(x) {readRDS(paste0("Runs/", x))[[1]]})

models <- list.files("Runs/") %>% str_subset("Seed") %>% 
  map(.f = function(x) {temp <- readRDS(paste0("Runs/", x));
  return(temp[2:length(temp)])}) %>% unlist(recursive = FALSE)

mNames <- map(.x = models, .f = function(x) {ifelse(is.null(unlist(x["method"])), "Unsure",
                                                       x["method"])}) %>% unlist() %>% 
  recode(rf = "Random Forest", svdpc = "PCR", gbm = "Grad Boost", nnet = "NNet",
         pcaNNet = "NNetF", svmRadialCost = "SVM Radial")

mNames[c(2, 6, 10, 12)] <- rep(c("CV glmnet", "LDA"), 2)

perfResults$Model <- mNames

print(xtable::xtable(perfResults[, 
                     colnames(perfResults)[c(ncol(perfResults), 
                     1:(ncol(perfResults)-1))]], digits = 4), include.rownames = FALSE)


# Final Predictions -------------------------------------------------------

library(tidyverse)

# Reading in design matrix for final predicitons
subA <- read.csv("2021/SubmissionFileA.csv")
subB <- read.csv("2021/SubmissionFileB.csv")
finalDesign <- readRDS("2021/tournamentGames2021.rds")

# Cleaning submission files
subA <- subA %>% select(-X) %>% rename(id = ID, pred = Pred) %>% 
  mutate(pred = 0.50)
subB <- subB %>% select(-X) %>% rename(id = ID, pred = Pred) %>% 
  mutate(pred = 0.50)

# Whether to manually fix some probs to 0/1
manual <- FALSE

if(manual == TRUE) {
# Setting 1 vs 16 seed games manually
teamA <- c("1233", "1205", "1181", "1181", "1192", "1211")
teamB <- c("1314", "1438", "1300", "1295", "1211", "1341")
probs <- c(1, 1, 0, 0, 1, 0)
aInds <- which(subA$id %in% paste0("2019_", teamA, "_", teamB))
bInds <- which(subB$id %in% paste0("2019_", teamA, "_", teamB))
subA[aInds, "pred"] <- probs
subB[bInds, "pred"] <- probs

# Setting all possible championship games manually
roundData <- read.csv("Kaggle Data/Stage2/NCAATourneySeedRoundSlots.csv") %>% 
  filter(GameRound == 6)
seedData <- read.csv("Kaggle Data/Stage2/NCAATourneySeeds.csv") %>% 
  filter(Season == 2019) %>% select(-Season)

# Unique combinations and obtaining teamID for each seedID
champGames <- expand.grid(t1 = roundData$Seed, t2 = roundData$Seed) %>% 
  filter(substring(t1, 1, 1) == "Y" & substring(t2, 1, 1) == "W" |
         substring(t1, 1, 1) == "Y" & substring(t2, 1, 1) == "X" |
         substring(t1, 1, 1) == "Z" & substring(t2, 1, 1) == "W" |
         substring(t1, 1, 1) == "Z" & substring(t2, 1, 1) == "X" |
         substring(t1, 1, 1) == "X" & substring(t2, 1, 1) == "Y" |
         substring(t1, 1, 1) == "X" & substring(t2, 1, 1) == "Z" |
         substring(t1, 1, 1) == "W" & substring(t2, 1, 1) == "Y" |
         substring(t1, 1, 1) == "W" & substring(t2, 1, 1) == "Z") %>% 
  left_join(seedData, by = c("t1" = "Seed")) %>% 
  left_join(seedData, by = c("t2" = "Seed"))
champGames <- champGames[complete.cases(champGames), ] %>% 
  filter(TeamID.x < TeamID.y)

acInds <- which(subA$id %in% paste0("2019_", champGames$TeamID.x, 
                                   "_", champGames$TeamID.y))
bcInds <- which(subB$id %in% paste0("2019_", champGames$TeamID.x, 
                                    "_", champGames$TeamID.y))
subA[acInds, "pred"] <- 1
subB[bcInds, "pred"] <- 0
}

# Centering and scaling design matrix
Aloc <- finalDesign$Aloc
finalDesign <- as.data.frame(lapply(finalDesign, FUN = scale, center = TRUE,
                      scale = TRUE))
finalDesign$Aloc <- Aloc

# Renaming outcome column
colnames(finalDesign)[which(colnames(finalDesign) == "Awin")] <- "outcome"

# Rearranging columns
newDatFormat <- readRDS("SubmissionFiles/newDatFormat.rds")
finalDesign <- finalDesign[, colnames(newDatFormat)]

# Switching which team is A and B
colnames(finalDesign)[which(colnames(finalDesign) != "Aloc")] <- colnames(finalDesign)[which(colnames(finalDesign) != "Aloc")] %>% 
  gsub(pattern = "AstRatio", replacement = "astratio") %>% 
  gsub(pattern = "A", replacement = "xxx") %>% 
  gsub(pattern = "B", replacement = "A") %>% 
  gsub(pattern = "xxx", replacement = "B") %>% 
gsub(pattern = "astratio", replacement = "AstRatio")

# Recoding Aloc as factor
finalDesign$Aloc <- factor(finalDesign$Aloc, 
                           levels = levels(newDatFormat$Aloc))

# Reading in best model fits
models <- list()

models$Awin <- list.files("Runs/") %>% str_subset("Awin") %>% 
  map(.f = function(x) {temp <- readRDS(paste0("Runs/", x));
  print("Complete.");
  return(temp[2:length(temp)])}) %>% unlist(recursive = FALSE)

models$MOV <- list.files("Runs/") %>% str_subset("MOV") %>% 
  map(.f = function(x) {temp <- readRDS(paste0("Runs/", x));
  print("Complete.");
  return(temp[2:length(temp)])}) %>% unlist(recursive = FALSE)

# Names of model fits
mNames <- c("randomForest", "cvGlmnet", "PCR", "LDA", "GBM", "NNet", "NNetF", "SVM")
names(models$Awin) <- names(models$MOV) <- mNames

# Estimating conversion between MOV and probability

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

# Training objects
modelData <- modelDats[["MOV"]]
designMat <- designMats[["MOV"]]

# Function for obtaining predicted probabilities
predCalc <- function(modelData = modelData.test, designMat = designMat.test, model = "MOV") {
  
  rfPreds <- predict(models[[model]]$randomForest, newdata = modelData, 
                     type = ifelse(model == "Awin", "prob", "raw"))
  cvglmPreds <- predict(models[[model]]$cvGlmnet, newx = designMat, 
                        s = "lambda.min", type = "response")
  pcrPreds <- predict(models[[model]]$PCR, newdata = modelData, ncomp = models[[model]]$PCR[["validation"]][["ncomp"]], type = "response")
  ldaPreds <- predict(models[[model]]$LDA, newdata = modelData)$posterior
  boostPreds <- predict(models[[model]]$GBM, modelData, 
                        type = ifelse(model == "Awin", "prob", "raw"))
  nnetPreds <- predict(models[[model]]$NNet, modelData, 
                       type = ifelse(model == "Awin", "prob", "raw"))
  svmPreds <- predict(models[[model]]$SVM, modelData, 
                      type = ifelse(model == "Awin", "prob", "raw"))
  
  return(list(rfPreds, cvglmPreds, 
                       pcrPreds, ldaPreds, boostPreds, nnetPreds, svmPreds))
}

if(file.exists("converts.rds") == FALSE) {
  # Obtaining estimated probabilities for full data set
  movPreds <- setNames(lapply(predCalc(modelData = modelData, designMat = designMat, model = "MOV"),
                     FUN = function(x){x <- as.matrix(x); as.numeric(x)[1:nrow(x)]}), mNames)
  
  # Estimating custom conversion between MOV and win probability using model fits
  converts <- setNames(lapply(X = movPreds, FUN = function(MOVhats) {
    return(glm(formula = outcome ~ 0 + ., data = data.frame(outcome = desResp[[2]][["Awin"]][compInds], 
                                                            movHat = MOVhats), family = "binomial"))}), mNames)
  
  # Saving conversion model objects
  saveRDS(converts, "converts.rds")
} else {
  converts <- readRDS("converts.rds")
}

# Selecting our optimal model (modelNum = 6 for 2019 submission)
for(modelNum in 3:9) {
finalModel <- models$Awin[[modelNum]]

# Obtaining predictions and saving
response <- "Awin"
thresh <- 0.99

if(mNames[modelNum] == "cvGlmnet") {
    glmnetDesign <- finalDesign %>% mutate(outcome = 0)
      finalPreds <- predict(finalModel, newx = model.matrix(outcome ~ ., glmnetDesign), s = "lambda.min", type = "response")
      } else if(mNames[modelNum] == "PCR") {
        finalPreds <- predict(finalModel, newdata = finalDesign, ncomp = finalModel[["ncomp"]], type = "response")
        } else if(mNames[modelNum] == "LDA") {
          finalPreds <- predict(finalModel, newdata = finalDesign)$posterior
        } else {
          finalPreds <- predict(finalModel, finalDesign,  type = "prob")
          }

  # Capping estimated probabilities
if(ncol(finalPreds) == 2) {
  finalPreds <- finalPreds[, 2]
}

  finalPreds <- ifelse(finalPreds <= thresh, finalPreds, thresh)

  write.csv(subA %>% mutate(Pred = finalPreds), file = paste0("2021/SubmissionFiles/Submission", mNames[modelNum], "_AwinStage2.csv"), 
            row.names = FALSE)
  
print(paste0(mNames[modelNum], "_Awin"))
}

# Obtaining MOV predictions for Kaggle submission
kaggleMOVs <- setNames(lapply(predCalc(modelData = finalDesign, 
                            designMat = model.matrix(outcome ~ ., mutate(finalDesign, outcome = 0)), model = "MOV"),
                            FUN = function(x){x <- as.matrix(x); as.numeric(x)[1:nrow(x)]}), mNames)

# Reading in blank MOV submission files
subA <- read_csv("2021/SubmissionFileA.csv") %>% dplyr::select(-X1) %>% 
  mutate(Pred = kaggleMOVs$NNet)
subB <- read_csv("2021/SubmissionFileB.csv") %>% dplyr::select(-X1) %>% 
  mutate(Pred = kaggleMOVs$randomForest)

# Neural network MOV submission
write.csv(subA, file = paste0("2021/SubmissionFiles/Submission", 
                              "NNet", "_", "MOV", "Stage2.csv"), 
          row.names = FALSE)

# Random forest MOV submission
write.csv(subB, file = paste0("2021/SubmissionFiles/Submission", 
                              "randomForest", "_", "MOV", "Stage2.csv"), 
          row.names = FALSE)
