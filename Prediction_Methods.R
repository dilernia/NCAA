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
modelDats$Awin$outcome <- desResp[[2]]$Awin[compInds]

# Creating model form objects and storing in list
forms <- list(MOV = outcome ~ 0 + ., Awin =  outcome ~ 0 + .)

# Creating model design matrices (without outcome) and storing in list
designMats <- list(MOV = model.matrix(forms$MOV, data = modelDats$MOV),
                   Awin = model.matrix(forms$Awin, data = modelDats$Awin))

# Shoulde be one of 'MOV' or 'Awin'
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
outcome.test <- desResp[[2]][[model]][-trInds]

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
pcrFit <- pcr(form, data = modelData, validation = "CV", segments = 10)

# Finds optimal number of components
ncomps <- as.numeric(strsplit(colnames(pcrFit$validation$PRESS)[
  which(pcrFit$validation$PRESS == min(pcrFit$validation$PRESS))], split = " ")[[1]][1])

# (5) Linear Discriminant Analysis
ldaFit <- lda(form, data = modelData)

# (6) Optimally tuned mtry value random forest
# For final run increase ntreeTry, decrease improve to 0.01
my.rf <- tuneRF(x = modelData[complete.cases(modelData), -ncol(modelData)], 
                y = outcome[complete.cases(modelData)], 
                  ntreeTry = 100, stepFactor = 2, improve = 0.05,
                  trace = FALSE, plot = FALSE, doBest = TRUE)

# (7) Gradient boosting
# For final run try n.trees = 5000
my.boost <- gbm(form, data = modelData,
                       distribution = ifelse(model == "Awin", 
                                             "bernoulli", "gaussian"),
                       n.trees = 5000, shrinkage = 0.01)
optTrees <- as.integer(gbm.perf(my.boost, method = "OOB"))

# (8) Artificial Neural Network w/ 1 hidden layer
# For final run try size = c(5, 10, 15:20), decay = seq(0.005, 0.05, by = 0.005), maxit = 20000
my.nnet <- best.tune(nnet, form, data = modelData, maxit = 10000,
              ranges = list(size = c(10), decay = c(0.01)),
              control = tune.control(sampling = "cross", cross = 5))

# (9) Support Vector Machine (SVM)
# For final run try kernal = c("sigmoid", "radial"), 
# gamma = c(1/ncol(modelData), seq(0.005, 0.05, by = 0.005)), cost = seq(0.50, 2, by = 0.50)
my.svm <- best.tune(svm, form, data = modelData, kernel = "radial", cross = 5,
              ranges = list(cost = c(1), gamma = c(1/ncol(modelData))),
              control = tune.control(sampling = "cross", cross = 5))

# (10) k-Nearest Neighbour
ctrl <- caret::trainControl(method="repeatedcv",repeats = 5) #,c
knnFit <- caret::train(form, data = modelData, metric = "logLoss",
                method = "knn", trControl = ctrl, tuneLength = 20)


# Obtaining predicted probabilities
treePreds <- predict(my.tree, modelData.test)
prunedPreds <- predict(my.tree.pruned, modelData.test)
rfPreds <- predict(my.rf, newdata = modelData.test, 
                   type = ifelse(model == "Awin", "prob"))[, "1"]
boostPreds <- predict.gbm(my.boost, newdata = modelData.test, 
                          n.trees = as.integer(optTrees), 
                          type = "response")
nnetPreds <- predict(my.nnet, modelData.test, type = "raw")
svmPreds <- predict(my.svm, modelData.test)
cvglmPreds <- predict(cvglmFit, newx = designMat.test, 
                      s = "lambda.min", type = "response")
pcrPreds <- predict(pcrFit, newdata = modelData.test, ncomp = ncomps, type = "response")
ldaPreds <- predict(ldaFit, newdata = modelData.test)$posterior[, "1"]
knnPreds <- predict(knnFit, newdata = modelData.test )
  

# Preventing extrememe predictions. Need to tune thresh for final preds
capper <- function(x, thresh = 0.01){
  for(i in 1:length(x)) {
  x[i] <- ifelse(x[i] < thresh, thresh, ifelse(x[i] > 1-thresh, 1-thresh, x[i]))
  }
  return(x)
}

# Capping predicted probabilities
predList <- lapply(list(treePreds, prunedPreds, boostPreds, 
                 cvglmPreds, pcrPreds, ldaPreds), FUN = capper)
names(predList) <- c("Full Tree", 'Pruned Tree', 'Grad Boost', 
                     "CV glmnet", "PCR", "LDA")

# Evaluating methods
predSummary <- function(preds, response, truths = modelData.test$outcome) {
  
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