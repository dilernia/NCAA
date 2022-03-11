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
runParallel <- TRUE

# Use 3 most seasons for testing (TRUE) or randomly separate all games into training & test (FALSE)
trainRecent <- TRUE

if(runParallel) {
  library(doFuture)
  registerDoFuture()
  plan(multisession, workers = 6)
}

myMods <- list()

# Should be one of 'MOV' or 'Awin'
for(model in c("Awin", "MOV")) {
  
  # Setting training size and seed. Size only matters if trainRecent = FALSE
  seed <- 1994
  trainSize <- 0.99
  set.seed(seed)
  
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
    trInds <- which(!(desResp$FullData$Season %in% testSeasons))
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
  myMods$cvglmFit <- cv.glmnet(x = designMat, y = outcome[complete.cases(modelData)], 
                               nfolds = 10, family = ifelse(model == "Awin", "binomial", "gaussian"),
                               lambda = c(0, seq(0.00005, 0.00010, by = 0.00001), seq(0.005, 0.03, by = 0.005)),
                               gamma = c(0, 0.25, 0.5, 0.75, 1), parallel = runParallel)
  
  myMods$cvglmFit2 <- cv.glmnet(x = designMat2, y = outcome[complete.cases(modelData)], 
                                nfolds = 10, family = ifelse(model == "Awin", "binomial", "gaussian"),
                                lambda = c(0, seq(0.0001, 0.001, by = 0.0001), seq(0.005, 0.03, by = 0.005)),
                                gamma = c(0, 0.25, 0.5, 0.75, 1), parallel = runParallel)
  
  # (2) Principle Components Regression with 10-fold CV
  myMods$pcrFit <- pcr(form, data = modelData.pcr, validation = "CV", segments = 10)
  myMods$pcrFit2 <- pcr(form, data = modelData.pcr2, validation = "CV", segments = 10)
  
  # Finds optimal number of components
  ncomps <- as.numeric(strsplit(colnames(myMods$pcrFit$validation$PRESS)[
    which(myMods$pcrFit$validation$PRESS == min(myMods$pcrFit$validation$PRESS))], split = " ")[[1]][1])
  
  ncomps2 <- as.numeric(strsplit(colnames(myMods$pcrFit2$validation$PRESS)[
    which(myMods$pcrFit2$validation$PRESS == min(myMods$pcrFit2$validation$PRESS))], split = " ")[[1]][1])
  
  # (3) Linear Discriminant Analysis
  myMods$ldaFit <- lda(form, data = modelData)
  myMods$ldaFit2 <- lda(form, data = modelData2)
  
  print(Sys.time() - t1)
  
  # (4) Optimally tuned mtry value random forest
  defMtry <- floor(sqrt(ncol(modelData)))
  rfFile <- paste0("2022/Fits/RF_", model, "_Tr", 
              ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
  if(file.exists(rfFile) == FALSE) {
  myMods$my.rf <- caret::train(form = form,
                               data = modelData, method = "rf",
                               trControl = ctrl, ntree = c(1000, 1500, 2000),
                               tuneGrid = expand.grid(mtry =(defMtry - 3):(defMtry + 3)))
  # Saving results
  saveRDS(myMods$my.rf, file = rfFile)
  
  } else {
    myMods$my.rf <- readRDS(rfFile)
  }
  
  print(Sys.time() - t1)
  
  # (5) Stochastic Gradient boosting
  # For final run try n.trees = 5000
  boostFile <- paste0("2022/Fits/Boost_", model, "_Tr", 
              ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
  if(file.exists(boostFile) == FALSE) {
  myMods$my.boost <- caret::train(form = form,
                                  data = modelData, method = "gbm",
                                  verbose = FALSE, distribution = ifelse(model == "Awin",
                                                                         "bernoulli", "gaussian"),
                                  trControl = ctrl, tuneGrid = expand.grid(n.trees = c(seq(1200, 2200, by = 200)),
                                                                           shrinkage = c(0.005, seq(0.01, 0.05, by = 0.01)),
                                                                           interaction.depth = 1:3, n.minobsinnode = 6:10))
  
  # Saving results
  saveRDS(myMods$my.boost, boostFile)
  } else {
    myMods$my.boost <- readRDS(boostFile)
  }
  
  print(Sys.time() - t1)
  
  # (6) Artificial Neural Network w/ 1 hidden layer
  nnetFile <- paste0("2022/Fits/NNet_", model, "_Tr", 
              ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
  if(file.exists(nnetFile) == FALSE) {
  myMods$my.nnet <- caret::train(form,
                                 data = modelData, method = "nnet",
                                 trControl = ctrl, trace = FALSE, maxit = 10000,
                                 tuneGrid = expand.grid(size = c(2:5),
                                                        decay = c(0.003, 0.005, 0.008, 0.01, 0.015, 0.02)),
                                 linout = ifelse(model == "Awin", FALSE, TRUE))
  
  # Saving results
  saveRDS(myMods$my.nnet, nnetFile)
  } else {
    myMods$my.nnet <- readRDS(nnetFile)
  }
  
  
  print(Sys.time() - t1)
  
  # (7) eXtremeGradient Boosting (w/ trees)
  # https://xgboost.readthedocs.io/en/latest/parameter.html
  boostTreeFile <- paste0("2022/Fits/BoostTree_", model, "_Tr", 
              ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
  if(file.exists(boostTreeFile) == FALSE) {
  myMods$my.boostTree <- caret::train(form,
                                      data = modelData, method = 'xgbTree', 
                                      trControl = ctrl, verbose = 0,
                                      tuneGrid = expand.grid(max_depth = c(4, 5, 6, 7, 8), eta = c(0.15, 0.20, 0.25, 0.30, 0.35), 
                                                             nrounds = 2, gamma = c(0, 1, 2), colsample_bytree = 1, 
                                                             min_child_weight = c(1, 3, 5), subsample = c(0.80, 1)))
  
  # Saving results
  saveRDS(myMods$my.boostTree, file = boostTreeFile)
  } else {
    myMods$my.boostTree <- readRDS(boostTreeFile) 
  }
  
  print(Sys.time() - t1)
  
  # (8) Support Vector Machine (SVM)
  # For final run try kernal = c("sigmoid", "radial"), 
  # gamma = c(1/ncol(modelData), seq(0.005, 0.05, by = 0.005)), cost = seq(0.50, 2, by = 0.50)
  svmFile <- paste0("2022/Fits/SVM_", model, "_Tr", 
          ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
  if(file.exists(svmFile) == FALSE) {
  myMods$my.svm <- caret::train(form,
                                data = modelData, method = 'svmRadialCost',
                                trControl = ctrl,
                                tuneGrid = expand.grid(C = c(0.01, 0.1, 0.30, 0.50, 0.70, 1,
                                                             2, 3)))
  
  # Saving results
  saveRDS(myMods$my.svm, file = svmFile)
  } else {
    myMods$my.svm <- readRDS(svmFile)
  }
  
  print(Sys.time() - t1)
  
  # Function for obtaining predicted probabilities
  predCalc <- function(modelData = modelData.test, designMat = designMat.test,
                       modelData2 = modelData.test2, designMat2 = designMat.test2) {
    
    rfPreds <- predict(myMods$my.rf, newdata = modelData, 
                       type = ifelse(model == "Awin", "prob", "raw"))
    boostPreds <- predict(myMods$my.boost, modelData, 
                          type = ifelse(model == "Awin", "prob", "raw"))
    nnetPreds <- predict(myMods$my.nnet, modelData, 
                         type = ifelse(model == "Awin", "prob", "raw"))
    boostTreePreds <- predict(myMods$my.boostTree, modelData, 
                              type = ifelse(model == "Awin", "prob", "raw"))
    svmPreds <- predict(myMods$my.svm, modelData, 
                        type = ifelse(model == "Awin", "prob", "raw"))
    cvglmPreds <- predict(myMods$cvglmFit, newx = designMat, 
                          s = "lambda.min", type = "response")
    cvglm2Preds <- predict(myMods$cvglmFit2, newx = designMat2, 
                           s = "lambda.min", type = "response")
    
    pcrPreds <- predict(myMods$pcrFit, newdata = modelData, ncomp = ncomps, type = "response")
    pcr2Preds <- predict(myMods$pcrFit2, newdata = modelData2, ncomp = ncomps2, type = "response")
    
    ldaPreds <- predict(myMods$ldaFit, newdata = modelData)$posterior
    lda2Preds <- predict(myMods$ldaFit2, newdata = modelData2)$posterior
    
    return(setNames(list(rfPreds, boostPreds, 
                         nnetPreds, boostTreePreds,
                         svmPreds, cvglmPreds, cvglm2Preds,
                         pcrPreds, pcr2Preds, ldaPreds, lda2Preds), 
                    c("Random Forest", "Grad Boost", 
                      "NNet", "XGBoost Tree",
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
  saveRDS(list(results = results, mods = myMods),
          paste0("2022/Fits/", model, "_Tr", ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds"))
}

# Available models fit using caret package:
#http://topepo.github.io/caret/train-models-by-tag.html

# Making a bracket from our predictions:
#https://www.kaggle.com/c/mens-machine-learning-competition-2019/discussion/81642

# Previous winners
#https://storage.googleapis.com/kaggle-forum-message-attachments/473390/11340/Perennial%20Favories%202014-2018.png
