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

# NCAA Tournament year
tourneyYear <- 2023

# Use 3 most seasons for testing (TRUE) or randomly separate all games into training & test (FALSE)
trainRecent <- FALSE

if(runParallel) {
  library(doFuture)
  registerDoFuture()
  plan(multisession, workers = 3)
}

# Should be one of 'MOV' or 'Awin'
for(model in c("MOV", "Awin")) {
  for(womens in c(TRUE, FALSE)) {
  
  # Setting training size and seed. Size only matters if trainRecent = FALSE
  seed <- 1994
  trainSize <- 0.90
  set.seed(seed)
  myMods <- list()
  # Preparing design matrix and response vector for fitting -----------------
  
  t1 <- Sys.time()
  
  # Reading in data
  desResp <- readRDS(paste0(ifelse(womens, "W_", "M_"), tourneyYear, "_",
                            "designResponseFinal.rds"))
  desResp2 <- readRDS(paste0(ifelse(womens, "W_", "M_"), tourneyYear, "_",
                             "designResponseFinal2.rds"))
  
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
  
  # Setting aside 3 recent seasons for test set
  testSeasons <- c(2019, 2022, 2023)
  
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
      fct_recode("loss" = "FALSE", "win" = "TRUE")
    
    modelData2$outcome <- as.factor(modelData2$outcome) %>% 
      fct_recode("loss" = "FALSE", "win" = "TRUE")
    
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
  cvglmnetFile <- paste0(tourneyYear, "/", ifelse(womens, "W", "M"),
                    "_Fits/cvglmnet_", model, "_Tr", 
                    ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
  
  if(file.exists(cvglmnetFile) == FALSE) {
  myMods$cvglmFit <- cv.glmnet(x = designMat, y = outcome[complete.cases(modelData)], 
                               nfolds = 10, family = ifelse(model == "Awin", "binomial", "gaussian"),
                               lambda = c(0, seq(0.00005, 0.00010, by = 0.00001), seq(0.005, 0.06, by = 0.005)),
                               gamma = c(0, 0.25, 0.5, 0.75, 1), parallel = runParallel, type.measure="class")
  
  myMods$cvglmFit2 <- cv.glmnet(x = designMat2, y = outcome[complete.cases(modelData)], 
                                nfolds = 10, family = ifelse(model == "Awin", "binomial", "gaussian"),
                                lambda = c(0, 0.001, seq(0.005, 0.06, by = 0.005)),
                                gamma = c(0, 0.25, 0.5, 0.75, 1), parallel = runParallel, type.measure="class")
  # Saving results
  saveRDS(list(cvglmFit = myMods$cvglmFit, cvglmFit2 = myMods$cvglmFit2), cvglmnetFile)
  } else {
    myMods$cvglmFit <- readRDS(cvglmnetFile)$cvglmFit
    myMods$cvglmFit2 <- readRDS(cvglmnetFile)$cvglmFit2
  }
  
  # (2) Principle Components Regression with 10-fold CV
  pcrFile <- paste0(tourneyYear, "/", ifelse(womens, "W", "M"),
                    "_Fits/PCR_", model, "_Tr", 
                    ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
  
  if(file.exists(pcrFile) == FALSE) {
  myMods$pcrFit <- pcr(form, data = modelData.pcr, validation = "CV", segments = 10)
  myMods$pcrFit2 <- pcr(form, data = modelData.pcr2, validation = "CV", segments = 10)
  
  # Saving results
  saveRDS(list(pcrFit = myMods$pcrFit, pcrFit2 = myMods$pcrFit2), pcrFile)
  } else {
    myMods$pcrFit <- readRDS(pcrFile)$pcrFit
    myMods$pcrFit2 <- readRDS(pcrFile)$pcrFit2
  }
  
  # Finds optimal number of components
  ncomps <- as.numeric(strsplit(colnames(myMods$pcrFit$validation$PRESS)[
    which(myMods$pcrFit$validation$PRESS == min(myMods$pcrFit$validation$PRESS))], split = " ")[[1]][1])
  
  ncomps2 <- as.numeric(strsplit(colnames(myMods$pcrFit2$validation$PRESS)[
    which(myMods$pcrFit2$validation$PRESS == min(myMods$pcrFit2$validation$PRESS))], split = " ")[[1]][1])
  
  # (3) Linear Discriminant Analysis
  ldaFile <- paste0(tourneyYear, "/", ifelse(womens, "W", "M"),
                     "_Fits/LDA_", model, "_Tr", 
                     ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
  
  if(file.exists(ldaFile) == FALSE) {
  myMods$ldaFit <- lda(form, data = modelData)
  myMods$ldaFit2 <- lda(form, data = modelData2)
  
  # Saving results
  saveRDS(list(ldaFit = myMods$ldaFit, ldaFit2 = myMods$ldaFit2), ldaFile)
  } else {
    myMods$ldaFit <- readRDS(ldaFile)$ldaFit
    myMods$ldaFit2 <- readRDS(ldaFile)$ldaFit2
  }
  
  print(Sys.time() - t1)
  
  # (4) Artificial Neural Network w/ 1 hidden layer
  nnetFile <- paste0(tourneyYear, "/", ifelse(womens, "W", "M"),
                     "_Fits/NNet_", model, "_Tr", 
              ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
  if(file.exists(nnetFile) == FALSE) {
  myMods$my.nnet <- caret::train(form,
                                 data = modelData, method = "nnet",
                                 trControl = ctrl, trace = FALSE, maxit = 15000, # 15000
                                 tuneGrid = expand.grid(size = c(1:5), # c(1:4)
                                                        decay = c(0.002, 0.006, 0.010, 0.014, 0.018)), # c(0.004, 0.006, 0.008, 0.010, 0.012)
                                 linout = ifelse(model == "Awin", FALSE, TRUE))
  
  # Saving results
  saveRDS(myMods$my.nnet, nnetFile)
  } else {
    myMods$my.nnet <- readRDS(nnetFile)
  }
  
  
  print(Sys.time() - t1)
  
  # Function for obtaining predicted probabilities
  predCalc <- function(modelData = modelData.test, designMat = designMat.test,
                       modelData2 = modelData.test2, designMat2 = designMat.test2) {
    
    myPreds <- lapply(X = myMods, FUN = function(mod) {
      if("bestTune" %in% names(mod)) {
        if(model == "Awin") {
      predict(mod, newdata = modelData, 
              type = "prob")[, "win"]
        } else {
          predict(mod, newdata = modelData, 
                  type = "raw")
        }
      } else if("glmnet.fit" %in% names(mod))  {
        if(length(mod$glmnet.fit$beta@Dimnames[[1]]) == ncol(designMat.test)) {
          predict(mod, newx = designMat, 
                  s = "lambda.min", type = "response")
        } else {
          predict(mod, newx = designMat2, 
                  s = "lambda.min", type = "response")
        }
      } else if("projection" %in% names(mod)) {
        if(length(mod$Xmeans) == ncol(designMat.test)) {
          predict(mod, newdata = modelData, ncomp = mod$ncomp, 
                  type = "response")
        } else {
          predict(mod, newdata = modelData2, ncomp = mod$ncomp, 
                  type = "response")
        }
      } else if("svd" %in% names(mod) & model == "Awin") {
        if(ncol(mod$means) == ncol(designMat.test)) {
          predict(mod, newdata = modelData)$posterior[, "win"]
        } else {
          predict(mod, newdata = modelData2)$posterior[, "win"]
        }
      } else if("svd" %in% names(mod) & model == "MOV") {
        if(ncol(mod$means) == ncol(designMat.test)) {
          predict(mod, newdata = modelData)$posterior
        } else {
          predict(mod, newdata = modelData2)$posterior
        }
      }
    })
    
    return(myPreds)
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
          paste0(tourneyYear, "/", ifelse(womens, "W", "M"),
                 "_Fits/", model, "_Tr", 
                 ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds"))
  }
}

# Fit optimal models to full data -----------------------------------------

library(tidyverse)
  
# Setting training size and seed. Size only matters if trainRecent = FALSE
seed <- 1994
trainSize <- 0.90
tourneyYear <- 2023
trainRecent <- FALSE
set.seed(seed)

for(model in c("MOV", "Awin")) {
  for(womens in c(TRUE, FALSE)) {
if(file.exists(paste0(tourneyYear, "/finalFits_", ifelse(womens, "W", "M"),
                      "_", model, ".rds")) == FALSE) {
  
# Import list of model fits
myRes <- readRDS(paste0(tourneyYear, "/", ifelse(womens, "W", "M"),
                              "_Fits/", model, "_Tr", 
                              ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds"))
  
# Reading in data
desResp <- readRDS(paste0(ifelse(womens, "W_", "M_"), tourneyYear, "_",
                          "designResponseFinal.rds"))
desResp2 <- readRDS(paste0(ifelse(womens, "W_", "M_"), tourneyYear, "_",
                           "designResponseFinal2.rds"))

# Standardizing predictors and removing some columns
designMat <- desResp$Design %>% as.matrix() %>% as.data.frame()
designMat2 <- desResp2$Design %>% as.matrix() %>% as.data.frame()

# Removing any rows with missing values
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

# Data for model fitting
modelData <- modelDats[[model]]
designMat <- designMats[[model]]
outcome <- desResp[[model]]
form <- forms[[model]]

modelData2 <- modelDats2[[model]]
designMat2 <- designMats2[[model]]

# Changing factor level names to work with train() function
if(model == "Awin") {
  modelData$outcome <- as.factor(modelData$outcome) %>% 
    fct_recode("loss" = "FALSE", "win" = "TRUE")
  
  modelData2$outcome <- as.factor(modelData2$outcome) %>% 
    fct_recode("loss" = "FALSE", "win" = "TRUE")
  
  ctrl <- trainControl(method = "none", classProbs =  TRUE,
                       allowParallel = runParallel)
  
  # Changing outcome for PCR to be numeric
  modelData.pcr <- modelData
  modelData.pcr$outcome <- as.integer(as.character(modelData$outcome) == "win")
  
  modelData.pcr2 <- modelData2
  modelData.pcr2$outcome <- as.integer(as.character(modelData2$outcome) == "win")
  
} else {
  ctrl <- trainControl(method = "none", allowParallel = runParallel)
  modelData.pcr <- modelData
  modelData.pcr2 <- modelData2
}

myFinalMods <- list()
t1 <- Sys.time()

# (1) Linear/Logistic Regression with Lasso Penalty using 10-fold CV
myFinalMods$cvglmFit <- glmnet(x = designMat, y = outcome[complete.cases(modelData)], 
                             family = ifelse(model == "Awin", "binomial", "gaussian"),
                             lambda = myRes[["mods"]][["cvglmFit"]][["lambda.min"]])

myFinalMods$cvglmFit2 <- glmnet(x = designMat2, y = outcome[complete.cases(modelData)], 
                              family = ifelse(model == "Awin", "binomial", "gaussian"),
                              lambda = myRes[["mods"]][["cvglmFit2"]][["lambda.min"]])

# (2) Principle Components Regression with 10-fold CV
myFinalMods$pcrFit <- pcr(form, data = modelData.pcr, validation = "none")
myFinalMods$pcrFit2 <- pcr(form, data = modelData.pcr2, validation = "none")

# Finds optimal number of components
ncomps <- as.numeric(strsplit(colnames(myRes[["mods"]]$pcrFit$validation$PRESS)[
  which(myRes[["mods"]]$pcrFit$validation$PRESS == min(myRes[["mods"]]$pcrFit$validation$PRESS))], split = " ")[[1]][1])

ncomps2 <- as.numeric(strsplit(colnames(myRes[["mods"]]$pcrFit2$validation$PRESS)[
  which(myRes[["mods"]]$pcrFit2$validation$PRESS == min(myRes[["mods"]]$pcrFit2$validation$PRESS))], split = " ")[[1]][1])

# (3) Linear Discriminant Analysis
myFinalMods$ldaFit <- lda(form, data = modelData)
myFinalMods$ldaFit2 <- lda(form, data = modelData2)
print(Sys.time() - t1)

# (4) Artificial Neural Network w/ 1 hidden layer
  myFinalMods$my.nnet <- caret::train(form,
                                 data = modelData, method = "nnet",
                                 trControl = ctrl, trace = FALSE, maxit = 15000,
                                 tuneGrid = expand.grid(size = myRes[["mods"]]$my.nnet$bestTune$size,
                                                        decay = myRes[["mods"]]$my.nnet$bestTune$decay),
                                 linout = ifelse(model == "Awin", FALSE, TRUE))

print(Sys.time() - t1)

# Saving
saveRDS(myFinalMods, paste0(tourneyYear, "/finalFits_", ifelse(womens, "W", "M"),
                            "_", model, ".rds"))
}
}
}

# Final Predictions -------------------------------------------------------

womens <- FALSE
stage <- 2
tourneyYear <- 2023

# Setting file paths
stageDir <- paste0(tourneyYear, "/march-machine-learning-mania-", tourneyYear, "/")

# Final models
finalMods <- setNames(lapply(c("MOV", "Awin"), FUN = function(modList) {
  readRDS(paste0(tourneyYear, "/finalFits_", ifelse(womens, "W", "M"),
                 "_", modList, ".rds"))
  }), c("MOV", "Awin"))

# Adding empty tournament games to predict for
matchups <- read_csv(paste0(stageDir, "SampleSubmission", tourneyYear, ".csv")) %>% 
  dplyr::mutate(Season = as.integer(map_chr(str_split(ID, pattern = "_"), 1)), 
         Afix_TeamID = as.integer(map_chr(str_split(ID, pattern = "_"), 2)), 
         Bfix_TeamID = as.integer(map_chr(str_split(ID, pattern = "_"), 3))) %>% 
  dplyr::select(-ID, -Pred)

# Tournament prediction data
tourneyDesign <- readRDS(paste0(tourneyYear, "/finalPredObjects.rds"))

# Obtaining predicted values
finalPreds <- lapply(c("MOV", "Awin"), FUN = function(model) {
  myPreds <- lapply(finalMods[[model]], FUN = function(mod) {
      if("bestTune" %in% names(mod)) {
        preds <- predict(mod, newdata = tourneyDesign$designTourney, 
                type = ifelse(model == "Awin", "prob", "raw"))
        if(model == "Awin") {
          preds <- preds[[2]]
        }
      } else if("lambda" %in% names(mod))  {
        if(mod$dim[1] == ncol(tourneyDesign$designTourney)) {
          preds <- predict(mod, newx = tourneyDesign$designTourney, 
                  s = "lambda.min", type = "response")
        } else {
          preds <- predict(mod, newx = tourneyDesign$designTourney2, 
                  s = "lambda.min", type = "response")
        }
      } else if("projection" %in% names(mod)) {
        if(length(mod$Xmeans) == ncol(tourneyDesign$designTourney)) {
          preds <- predict(mod, newdata = tourneyDesign$designTourney, ncomp = mod$ncomp, type = "response")
        } else {
          preds <- predict(mod, newdata = tourneyDesign$designTourney2, ncomp = mod$ncomp, type = "response")
        }
      } else if("svd" %in% names(mod)) {
        if(ncol(mod$means) == ncol(tourneyDesign$designTourney)) {
          preds <- predict(mod, newdata = as.data.frame(tourneyDesign$designTourney))$posterior[, 2]
        } else {
          preds <- predict(mod, newdata = as.data.frame(tourneyDesign$designTourney2))$posterior[, 2]
        }
      }
    
    return(preds)
    })
    return(myPreds)
  })

fullPreds <- tourneyDesign$allTourney %>% select(Afix_TeamID, Bfix_TeamID) %>% 
  bind_cols(data.frame(cvglmnet = finalPreds[[2]]$cvglmFit,
                        cvglmnet2 = finalPreds[[2]]$cvglmFit2,
                        lda = finalPreds[[2]]$ldaFit,
                        lda2 = finalPreds[[2]]$ldaFit2,
                        nnet = finalPreds[[2]]$my.nnet)) %>% 
  mutate(ID = paste0("2022_", Afix_TeamID, "_", Bfix_TeamID)) %>% 
  select(ID, everything(), -Afix_TeamID, -Bfix_TeamID) %>% 
right_join(read_csv(paste0(stageDir, ifelse(womens, "W", "M"), "SampleSubmissionStage2.csv")) %>% select(ID))

# Saving final predictions
write.csv(fullPreds %>% select(ID, cvglmnet) %>% rename(Pred = cvglmnet), 
          file = paste0("2022/SubmissionFiles/Submission", 
                              "cvglmnet", "_Stage2.csv"), 
          row.names = FALSE)

write.csv(fullPreds %>% select(ID, cvglmnet2) %>% rename(Pred = cvglmnet2), 
          file = paste0("2022/SubmissionFiles/Submission", 
                        "cvglmnet2", "_Stage2.csv"), 
          row.names = FALSE)

write.csv(fullPreds %>% select(ID, lda) %>% rename(Pred = lda), 
          file = paste0("2022/SubmissionFiles/Submission", 
                        "lda", "_Stage2.csv"), 
          row.names = FALSE)

write.csv(fullPreds %>% select(ID, lda2) %>% rename(Pred = lda2), 
          file = paste0("2022/SubmissionFiles/Submission", 
                        "lda2", "_Stage2.csv"), 
          row.names = FALSE)

write.csv(fullPreds %>% select(ID, nnet) %>% rename(Pred = nnet), 
          file = paste0("2022/SubmissionFiles/Submission", 
                        "nnet", "_Stage2.csv"), 
          row.names = FALSE)



# Quick Fits for Women's --------------------------------------------------

library(glmnet)
library(class)
library(MASS)
library(e1071)
library(nnet)
library(caret)
library(RSNNS)
library(tidyverse)

# Whether to run in parallel or not
runParallel <- TRUE

# For Men's or Women's NCAA
womens <- TRUE

# Use 3 most seasons for testing (TRUE) or randomly separate all games into training & test (FALSE)
trainRecent <- TRUE

if(runParallel) {
  library(doFuture)
  registerDoFuture()
  plan(multisession, workers = 5)
}

myMods <- list()
stage <- 2
model <- "Awin"

# Setting file paths
if(Sys.info()["sysname"] == "Darwin") {
  setwd("/Volumes/GoogleDrive/My Drive/Other/Fun/March Madness")
  stageDir <- paste0("/Volumes/GoogleDrive/My Drive/Other/Fun/March Madness/2022", 
                     ifelse(womens, "-Womens/womens", "/mens"), 
                     "-march-mania-2022/", ifelse(womens, "W", "M"), "DataFiles_Stage", stage, "/")
} else {
  stageDir <- paste0("2022", ifelse(womens, "-Womens/womens", "/mens"), 
                     "-march-mania-2022/", ifelse(womens, "W", "M"), "DataFiles_Stage", stage, "/")
}


# Setting training size and seed. Size only matters if trainRecent = FALSE
seed <- 1994
trainRecent <- FALSE
trainSize <- 1.00
set.seed(seed)

# Preparing design matrix and response vector for fitting -----------------

t1 <- Sys.time()

# Reading in data
desResp <- readRDS(paste0(ifelse(womens, "2022-Womens", "2022"), 
                          "/designResponse.rds"))
desResp2 <- readRDS(paste0(ifelse(womens, "2022-Womens", "2022"), 
                           "/designResponse2.rds"))

# Standardizing predictors and removing some columns
designMat <- desResp$Design
designMat2 <- desResp2$Design

# Removing any rows with missing values for now
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
    fct_recode("loss" = "FALSE", "win" = "TRUE")
  
  modelData2$outcome <- as.factor(modelData2$outcome) %>% 
    fct_recode("loss" = "FALSE", "win" = "TRUE")
  
  ctrl <- trainControl(method = "cv", number = 10, classProbs =  TRUE,
                       allowParallel = runParallel)
  
} else {
  ctrl <- trainControl(method = "cv", number = 10, allowParallel = runParallel)
}

# (1) Linear/Logistic Regression with Lasso Penalty using 10-fold CV
myMods$cvglmFit <- cv.glmnet(x = designMat, y = outcome[complete.cases(modelData)], 
                             nfolds = 10, family = ifelse(model == "Awin", "binomial", "gaussian"),
                             lambda = c(0, seq(0.00005, 0.00010, by = 0.00001), seq(0.005, 0.03, by = 0.005)),
                             parallel = runParallel, type.measure="class")

myMods$cvglmFit2 <- cv.glmnet(x = designMat2, y = outcome[complete.cases(modelData)], 
                              nfolds = 10, family = ifelse(model == "Awin", "binomial", "gaussian"),
                              lambda = c(0, seq(0.0001, 0.001, by = 0.0001), seq(0.005, 0.03, by = 0.005)),
                              parallel = FALSE, type.measure="class")

# (2) Artificial Neural Network w/ 1 hidden layer
nnetFile <- paste0(ifelse(womens, "2022-Womens", "2022"), "/Fits/NNet_", model, "_Tr", 
                   ifelse(!trainRecent, trainSize, "Recent"), "_Seed", seed, ".rds")
if(file.exists(nnetFile) == FALSE) {
  myMods$my.nnet <- caret::train(form,
                                 data = modelData, method = "nnet",
                                 trControl = ctrl, trace = FALSE, maxit = 15000,
                                 tuneGrid = expand.grid(size = c(1:6),
                                                        decay = c(0.004, 0.006, 0.008, 0.010, 0.012)),
                                 linout = ifelse(model == "Awin", FALSE, TRUE))
  # Saving results
  saveRDS(myMods$my.nnet, nnetFile)
} else {
  myMods$my.nnet <- readRDS(nnetFile)
}

# Saving
saveRDS(myMods, paste0(ifelse(womens, "2022-Womens", "2022"), "/finalFits_", model, ".rds"))
myMods <- readRDS(paste0(ifelse(womens, "2022-Womens", "2022"), "/finalFits_", model, ".rds"))

finalMods <- myMods
finalMods$cvglmFit <- glmnet(x = designMat, y = outcome[complete.cases(modelData)], 
                               family = ifelse(model == "Awin", "binomial", "gaussian"),
                               lambda = myMods[["cvglmFit"]][["lambda.1se"]])

finalMods$cvglmFit2 <- glmnet(x = designMat2, y = outcome[complete.cases(modelData)], 
                                family = ifelse(model == "Awin", "binomial", "gaussian"),
                                lambda = myMods[["cvglmFit2"]][["lambda.1se"]])


# Design Matrices for Tourney Predictions ---------------------------------

womens <- FALSE
tourneyYear <- 2023

t1 <- Sys.time()

# Reading in data
desResp <- readRDS(paste0(ifelse(womens, "W_", "M_"), tourneyYear, "_",
                          "designResponseFinal.rds"))
desResp2 <- readRDS(paste0(ifelse(womens, "W_", "M_"), tourneyYear, "_",
                           "designResponseFinal2.rds"))

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

# Objects for obtaining predictions
lastInds <- c()
modelData <- modelDats[[model]][lastInds, ]
designMat <- designMats[[model]][lastInds, ]
outcome <- desResp[[model]][lastInds]
form <- forms[[model]]

modelData2 <- modelDats2[[model]][lastInds, ]
designMat2 <- designMats2[[model]][lastInds, ]

# Obtaining Tourney Predictions ---------------------------------------------

# Tournament prediction data
tourneyDesign <- readRDS(paste0(ifelse(womens, "2022-Womens", "2022"), "/finalPredObjects.rds"))

# Obtaining predicted values
finalPreds <- lapply(finalMods, FUN = function(mod) {
    if("bestTune" %in% names(mod)) {
      preds <- predict(mod, newdata = tourneyDesign$designTourney, 
                       type = ifelse(model == "Awin", "prob", "raw"))
      if(model == "Awin") {
        preds <- preds[[2]]
      }
    } else if("lambda" %in% names(mod))  {
      if(mod$dim[1] == ncol(tourneyDesign$designTourney)) {
        preds <- predict(mod, newx = tourneyDesign$designTourney, 
                         s = "lambda.min", type = "response")
      } else {
        preds <- predict(mod, newx = tourneyDesign$designTourney2, 
                         s = "lambda.min", type = "response")
      }
    } else if("projection" %in% names(mod)) {
      if(length(mod$Xmeans) == ncol(tourneyDesign$designTourney)) {
        preds <- predict(mod, newdata = tourneyDesign$designTourney, ncomp = mod$ncomp, type = "response")
      } else {
        preds <- predict(mod, newdata = tourneyDesign$designTourney2, ncomp = mod$ncomp, type = "response")
      }
    } else if("svd" %in% names(mod)) {
      if(ncol(mod$means) == ncol(tourneyDesign$designTourney)) {
        preds <- predict(mod, newdata = as.data.frame(tourneyDesign$designTourney))$posterior[, 2]
      } else {
        preds <- predict(mod, newdata = as.data.frame(tourneyDesign$designTourney2))$posterior[, 2]
      }
    }
    
    return(preds)
  })

fullPreds <- tourneyDesign$allTourney %>% select(Afix_TeamID, Bfix_TeamID) %>% 
  bind_cols(data.frame(cvglmnet = finalPreds$cvglmFit,
                       cvglmnet2 = finalPreds$cvglmFit2,
                       nnet = finalPreds$my.nnet)) %>% 
  mutate(ID = paste0("2022_", Afix_TeamID, "_", Bfix_TeamID)) %>% 
  select(ID, everything(), -Afix_TeamID, -Bfix_TeamID) %>% 
  right_join(read_csv(paste0(stageDir, ifelse(womens, "W", "M"), "SampleSubmissionStage2.csv")) %>% select(ID))

# Saving final predictions
write.csv(fullPreds %>% select(ID, cvglmnet) %>% rename(Pred = cvglmnet), 
          file = paste0(ifelse(womens, "2022-Womens", "2022"), "/SubmissionFiles/Submission", 
                        "cvglmnet", "_Stage2.csv"), 
          row.names = FALSE)

write.csv(fullPreds %>% select(ID, cvglmnet2) %>% rename(Pred = cvglmnet2), 
          file = paste0(ifelse(womens, "2022-Womens", "2022"), "/SubmissionFiles/Submission", 
                        "cvglmnet2", "_Stage2.csv"), 
          row.names = FALSE)

write.csv(fullPreds %>% select(ID, nnet) %>% rename(Pred = nnet), 
          file = paste0(ifelse(womens, "2022-Womens", "2022"), "/SubmissionFiles/Submission", 
                        "nnet", "_Stage2.csv"), 
          row.names = FALSE)
