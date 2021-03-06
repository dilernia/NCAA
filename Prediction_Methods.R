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

# Setting training size and seed
seed <- 1994
trainSize <- 0.01
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

# Changing factor level names to work with train() function
if(model == "Awin") {
  levels(modelData$outcome) <- c("loss", "win")
  ctrl <- trainControl(method = "cv", number = 5, classProbs =  TRUE)
  
  # Changing outcome for PCR to be numeric
  modelData.pcr <- modelData
  modelData.pcr$outcome <- as.integer(as.character(modelData$outcome) == "win")
} else {
  ctrl <- trainControl(method = "cv", number = 5)
  modelData.pcr <- modelData
}

# (1) Fully grown tree
my.tree <- tree(formula = form, data = modelData)

# (2) Using 10-fold CV to select a tree with best predictive power (optimally pruned)
# output k is the cost-complexity parameter:
my.tree.cv <- cv.tree(object = my.tree, FUN = prune.tree, K = 10)
opt.k <- my.tree.cv$k[which(is.finite(my.tree.cv$k))][which.min(my.tree.cv$dev[which(is.finite(my.tree.cv$k))])]
my.tree.pruned <- prune.tree(my.tree, k = opt.k)

# (3) Linear/Logistic Regression with Lasso Penalty using 10-fold CV
cvglmFit <- cv.glmnet(x = designMat, y = outcome[complete.cases(modelData)], 
                      nfolds = 10, family = ifelse(model == "Awin", "binomial", "gaussian"),
                      lambda = c(seq(0.0001, 0.001, by = 0.0001), seq(0.005, 0.05, by = 0.005)))

# (4) Principle Components Regression with 10-fold CV
pcrFit <- pcr(form, data = modelData.pcr, validation = "CV", segments = 10)

# Finds optimal number of components
ncomps <- as.numeric(strsplit(colnames(pcrFit$validation$PRESS)[
  which(pcrFit$validation$PRESS == min(pcrFit$validation$PRESS))], split = " ")[[1]][1])

# (5) Linear Discriminant Analysis
ldaFit <- lda(form, data = modelData)

# (6) Optimally tuned mtry value random forest
# For final run increase ntreeTry, decrease improve to 0.01
defMtry <- floor(sqrt(ncol(modelData)))
my.rf <- caret::train(form = form, data = modelData, method = "rf", 
                      trControl = ctrl, ntree = 1000,
                      tuneGrid = expand.grid(mtry =(defMtry - 2):(defMtry + 3)))

# (7) Stochastic Gradient boosting
# For final run try n.trees = 5000
my.boost <- caret::train(form = form, data = modelData, method = "gbm", 
                         distribution = ifelse(model == "Awin", 
                                               "bernoulli", "gaussian"),
                         trControl = ctrl, tuneGrid = expand.grid(n.trees = c(25, 50, 100, seq(250, 2500, by = 250)), 
                                                                  shrinkage = c(0.05, 0.10, 0.16, seq(0.20, 1, by = 0.20)),
                                                                  interaction.depth = 1:5, n.minobsinnode = 8:12))

# (8) Artificial Neural Network w/ 1 hidden layer
my.nnet <- caret::train(form, data = modelData, method = "nnet", 
                        trControl = ctrl, trace = FALSE, maxit = 10000,
                        tuneGrid = expand.grid(size = c(3:7),
                                               decay = c(0.001, 0.005, 0.01, 0.015, 0.02, 0.03)),
                        linout = ifelse(model == "Awin", FALSE, TRUE))

# (9) Artificial Neural Network w/ 3 hidden layers
my.nnet3 <- caret::train(form, data = modelData, method = 'mlpWeightDecayML', 
                         trControl = ctrl, trace = FALSE, maxit = 10000,
                         tuneGrid = expand.grid(layer1 = c(3, 5, 7),
                                                layer2 = c(0, 3, 5, 7),
                                                layer3 = c(0, 3, 5, 7),
                                                decay = c(0.001, 0.005, 0.01, 0.015, 0.02)),
                         linout = ifelse(model == "Awin", FALSE, TRUE))

# (10) Neural Network with Feature Extraction
my.nnetf <- caret::train(form, data = modelData, method = 'pcaNNet', 
                         trControl = ctrl, trace = FALSE, maxit = 10000,
                         tuneGrid = expand.grid(size = c(3, 5, 8, 10, 12, 15, 16),
                                                decay = c(0.0001, 0.0005, 0.001, 0.003, 0.004, 0.005)),
                         linout = ifelse(model == "Awin", FALSE, TRUE))

# (11) Support Vector Machine (SVM)
# For final run try kernal = c("sigmoid", "radial"), 
# gamma = c(1/ncol(modelData), seq(0.005, 0.05, by = 0.005)), cost = seq(0.50, 2, by = 0.50)
my.svm <- caret::train(form, data = modelData, method = 'svmRadialCost', 
                       trControl = ctrl,
                       tuneGrid = expand.grid(C = c(0.1, 0.30, 0.40, 0.50, 0.60, 0.70, 1, 
                                                    2, 2.5, 2.75, 3, 3.25, 3.5)))

# Function for obtaining predicted probabilities
predCalc <- function(modelData = modelData.test, designMat = designMat.test) {
  
  treePreds <- predict(my.tree, modelData)
  prunedPreds <- predict(my.tree.pruned, modelData)
  rfPreds <- predict(my.rf, newdata = modelData, 
                     type = ifelse(model == "Awin", "prob", "raw"))
  boostPreds <- predict(my.boost, modelData, 
                        type = ifelse(model == "Awin", "prob", "raw"))
  nnetPreds <- predict(my.nnet, modelData, 
                       type = ifelse(model == "Awin", "prob", "raw"))
  nnet3Preds <- predict(my.nnet3, modelData, 
                        type = ifelse(model == "Awin", "prob", "raw"))
  nnetfPreds <- predict(my.nnetf, modelData, 
                        type = ifelse(model == "Awin", "prob", "raw"))
  svmPreds <- predict(my.svm, modelData, 
                      type = ifelse(model == "Awin", "prob", "raw"))
  cvglmPreds <- predict(cvglmFit, newx = designMat, 
                        s = "lambda.min", type = "response")
  pcrPreds <- predict(pcrFit, newdata = modelData, ncomp = ncomps, type = "response")
  ldaPreds <- predict(ldaFit, newdata = modelData)$posterior
  
  return(setNames(list(treePreds, prunedPreds, rfPreds, boostPreds, 
                       nnetPreds, nnet3Preds, nnetfPreds, svmPreds, 
                       cvglmPreds, pcrPreds, ldaPreds), 
                  c("Full Tree", 'Pruned Tree', 
                    "Random Forest", 'Grad Boost', 
                    "NNet", "NNet3", "NNetF",
                    "SVM Radial",
                    "CV glmnet", "PCR", "LDA")))
}

# Preventing extreme predictions. Need to tune thresh for final preds
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
  trPreds <- predCalc(modelData = modelData, designMat = designMat)
  
  # Estimating custom conversion between MOV and win probability using training data
  # model fits
  converts <- lapply(FUN = function(MOVhats) {
    fit <- glm(formula = form, data = data.frame(outcome = desResp[[2]][["Awin"]][trInds], 
                                                 movHat = MOVhats), family = "binomial")
    return(fit)
  }, X = trPreds)
  
  # Saving conversion fits
  saveRDS(converts, "convertFits.rds")
  
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

# Saving results
saveRDS(results, paste0(model, "_Tr", trainSize, "_Seed", seed, ".rds"))

# Available models fit using caret package:
#http://topepo.github.io/caret/train-models-by-tag.html

# Making a bracket from our predictions:
#https://www.kaggle.com/c/mens-machine-learning-competition-2019/discussion/81642

# Previous winners
#https://storage.googleapis.com/kaggle-forum-message-attachments/473390/11340/Perennial%20Favories%202014-2018.png
