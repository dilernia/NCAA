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
library(fastAdaboost)

# Reading in data
desResp <- readRDS("Our_Data/designResponse.rds")

# Standardizing predictors and removing some columns
# Aloc: location coding needs to be fixed
designMat <- subset(desResp[[1]], select = -c(Awin, Season, Tourney, Spread))
Aloc <- designMat$Aloc
designMat <- as.data.frame(lapply(designMat, FUN = scale, center = TRUE, scale = TRUE))

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

# Estimating relationship between MOV and win prob
SDmov <- sd(modelDats$MOV$outcome)

# Setting parameters ------------------------------------------------------

# Should be one of 'MOV' or 'Awin'
#model <- "MOV"
model <- "Awin"

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
outcome.test <- desResp[[2]][["Awin"]][-trInds]

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
                      lambda = c(0.001, 0.003, seq(0.005, 0.50, by = 0.005)))

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
my.rf <- train(form = form, data = modelData, method = "rf", 
                 trControl = ctrl, ntree = 1000,
                 tuneGrid = expand.grid(mtry = floor(defMtry/2):(defMtry + 3)))

# (7) Stochastic Gradient boosting
# For final run try n.trees = 5000
my.boost <- train(form = form, data = modelData, method = "gbm", 
                  distribution = ifelse(model == "Awin", 
                                        "bernoulli", "gaussian"),
      trControl = ctrl, tuneGrid = expand.grid(n.trees = c(100), 
                             shrinkage = c(0.01, seq(0.04, 0.16, by = 0.04)),
                             interaction.depth = 1, n.minobsinnode = 10))


# (8) Artificial Neural Network w/ 1 hidden layer
# For final run try tuneLength = 10, maxit = 5000
my.nnet <- train(form, data = modelData, method = "nnet", 
                        trControl = ctrl, trace = FALSE, maxit = 10,
                        tuneGrid = expand.grid(size = c(5, 10, 15, 20),
                                               decay = c(0.005, 0.01, 0.015)),
                 linout = ifelse(model == "Awin", FALSE, TRUE))

# (9) Support Vector Machine (SVM)
# For final run try kernal = c("sigmoid", "radial"), 
# gamma = c(1/ncol(modelData), seq(0.005, 0.05, by = 0.005)), cost = seq(0.50, 2, by = 0.50)
my.svm <- train(form, data = modelData, method = 'svmRadialCost', 
      trControl = trainControl(method = "cv", number = 5,
                               classProbs =  ifelse(model == "Awin", TRUE, FALSE)),
      tuneGrid = expand.grid(C = c(0.1, 0.50, 1, 3, 5, 10)))


# (10) k-Nearest Neighbour
knnFit <- train(form, data = modelData,
                method = "knn", trControl = ctrl, tuneLength = 20)

# (11) AdaBoost Classification Tree
my.ada <- train(form, data = modelData, method = 'adaboost', 
                trControl = trainControl(method = "cv", number = 5,
                                         classProbs =  ifelse(model == "Awin", TRUE, FALSE)))

# Obtaining predicted probabilities
treePreds <- predict(my.tree, modelData.test)
prunedPreds <- predict(my.tree.pruned, modelData.test)
rfPreds <- predict(my.rf, newdata = modelData.test, 
                   type = ifelse(model == "Awin", "prob", "raw"))
boostPreds <- predict(my.boost, modelData.test, 
                          type = ifelse(model == "Awin", "prob", "raw"))
nnetPreds <- predict(my.nnet, modelData.test, 
                     type = ifelse(model == "Awin", "prob", "raw"))
svmPreds <- predict(my.svm, modelData.test, 
                     type = ifelse(model == "Awin", "prob", "raw"))
cvglmPreds <- predict(cvglmFit, newx = designMat.test, 
                      s = "lambda.min", type = "response")
pcrPreds <- predict(pcrFit, newdata = modelData.test, ncomp = ncomps, type = "response")
ldaPreds <- predict(ldaFit, newdata = modelData.test)$posterior
knnPreds <- predict(knnFit, newdata = modelData.test,
                    type = ifelse(model == "Awin", "prob", "raw"))
  
# Preventing extrememe predictions. Need to tune thresh for final preds
capper <- function(x, thresh = 0.01){
  if(is.null(dim(x)) == FALSE) {if(dim(x)[2] == 2) {x <- x[, 2]}}
  for(i in 1:length(x)) {
  x[i] <- ifelse(x[i] < thresh, thresh, ifelse(x[i] > 1-thresh, 1-thresh, x[i]))
  }
  return(as.numeric(x))
}

# Capping predicted probabilities
predList <- lapply(list(treePreds, prunedPreds, rfPreds, boostPreds, 
                        nnetPreds, svmPreds, cvglmPreds, pcrPreds, 
                        ldaPreds, knnPreds), FUN = capper)
names(predList) <- c("Full Tree", 'Pruned Tree', "Random Forest",
                     'Grad Boost', "NNet", "SVM Radial",
                     "CV glmnet", "PCR", "LDA", "KNN")

# Evaluating methods
predSummary <- function(preds, response, truths = as.numeric(as.character(modelData.test$outcome))) {
  
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
  rownames(result) <- NULL
  return(result)
}

# Summarizing performance of methods
results <- do.call("rbind", mapply(FUN = predSummary, preds = predList, 
                            response = model, SIMPLIFY = FALSE))

# Saving results
saveRDS(results, paste0("Runs/", model, "_Tr", trainSize, "_Seed", seed, ".rds"))

# Available models fit using caret package:
# http://topepo.github.io/caret/train-models-by-tag.html

# Making a bracket from our predictions:
#https://www.kaggle.com/c/mens-machine-learning-competition-2019/discussion/81642

# Previous winners
#https://storage.googleapis.com/kaggle-forum-message-attachments/473390/11340/Perennial%20Favories%202014-2018.png
