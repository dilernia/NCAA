#################
# Title: Predictive Models
# Author: Andrew DiLernia
# Date: 03/14/2020
# Purpose: Implement predictive models using design matrix and response
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
library(parallel)
library(doParallel)
library(foreach)

# Reading in data
desResp <- readRDS("designResponse.rds")

# Creating design matrix and response vector
designData <- subset(desResp[[1]], select = -c(Awin, Season, Tourney, Spread,
                                               ATeamID, BTeamID, Aloc, Site)) 
resp <- desResp[[2]]$Awin

# Clearing from environment
remove("desResp")

seed <- 1994

# Available models to fit
models <- c("glmnet", "PCR", "LDA", "GB",
            "NNF", "RRF", "NNet", "NNet3",
            "SVMrad", "SVMpoly")

# Setting parameters ------------------------------------------------------
model <- "GB"
trainSize <- 0.05
secOrder <- FALSE # Fit a 1st or 2nd order model
binary <- TRUE # Binary or continuous response
ncores <- 2
set.seed(seed)

# Function for fitting machine learning model
machineLearn <- function(designData, resp, model = "glmnet",
                         trainSize = 0.05, secOrder = FALSE,
                         binary = FALSE, ncores = 1) {

# Removing any rows with missing values
compInds <- complete.cases(designData)
designData <- designData[compInds, ]
resp <- resp[compInds]

# Creating model data frame
modelData <- as.data.frame(cbind(resp, designData))

# Specifying model order
modOrder <- ifelse(secOrder, "order2", "order1")

# Creating model form object
form <- list(order1 = resp ~ 0 + ., 
             order2 =  resp ~ 0 + .^2)[[modOrder]]

# Creating model design matrix
designMat <- model.matrix(form, data = designData)

# Randomly selecting indices for training set
trInds <- sample(1:nrow(designMat), replace = FALSE, 
                 size = ceiling(trainSize*nrow(designMat)))

# Changing factor level names to work with train() function
if(binary == TRUE) {
  modelData$resp <- factor(gsub(gsub(as.character(modelData$resp), pattern = "1",
                                replacement = "yes"), pattern = "0",
                                replacement = "no"))
  ctrl <- trainControl(method = "cv", number = 5, classProbs =  TRUE)
  
  if(model == "PCR") {
  # Changing response for PCR to be numeric
  modelData.pcr <- modelData
  modelData.pcr$resp <- as.integer(as.character(modelData$resp) == "yes")
  }
} else {
  ctrl <- trainControl(method = "cv", number = 5)
  if(model == "PCR") {
  modelData.pcr <- modelData
  
  # Training and test data sets
  modelData.pcr.tr <- modelData.pcr[trInds, ]
  modelData.pcr.test <- modelData.pcr[-trInds, ]
  }
}

if(model == "glmnet") {
# (1) Linear/Logistic Regression with Lasso Penalty using 10-fold CV
  bestMod <- cv.glmnet(x = designMat[trInds, ], y = resp[trInds], 
                      nfolds = 10, family = ifelse(binary == TRUE, "binomial", "gaussian"),
                      lambda = c(0.0001, 0.0005, seq(0.001, 0.10, by = 0.001)))
}

if(model == "PCR") {
# (2) Principle Components Regression with 10-fold CV
  bestMod <- pcr(form, data = modelData[trInds, ], validation = "CV", segments = 10)

# Finds optimal number of components
ncomps <- as.numeric(strsplit(colnames(bestMod$validation$PRESS)[
  which(bestMod$validation$PRESS == min(bestMod$validation$PRESS))], split = " ")[[1]][1])
}

if(model == "LDA") {
# (3) Linear Discriminant Analysis
  bestMod <- suppressWarnings(lda(form, data = modelData[trInds, ]))
}

if(model == "GB") {
# (4) Stochastic Gradient boosting
inputs <- expand.grid(n.trees = 1000,
                      shrinkage = 0.20,
                      interaction.depth = 2, 
                      n.minobsinnode = 8)
argus <- list(method = "gbm", distribution = ifelse(binary == TRUE,
                                              "bernoulli", "gaussian"))
}

if(model == "NNF") {
# (5) Neural Network with Feature Extraction
inputs <- expand.grid(size = c(2, 3, 4),
                      decay = c(0.0001))
argus <- list(method = 'pcaNNet', maxit = 10000,
              linout = ifelse(binary == TRUE, FALSE, TRUE))
}

if(model == "RRF") {
# (6) Regularized Random Forest
defMtry <- floor(sqrt(ncol(modelData[trInds, ])))
inputs <- expand.grid(mtry =(defMtry - 2):(defMtry + 3),
                      coefReg = 0.80, coefImp = c(0))
argus <- list(method = "RRF")
}

if(model == "NNet") {
# (7) Artificial Neural Network w/ 1 hidden layer
inputs <- expand.grid(size = c(4),
                      decay = c(0.001))
argus <- list(method = "nnet", trace = FALSE, maxit = 10000,
              linout = ifelse(binary == TRUE, FALSE, TRUE))
}

if(model == "NNet3") {
# (8) Artificial Neural Network w/ 3 hidden layers
inputs <- expand.grid(layer1 = c(3),
                      layer2 = c(2),
                      layer3 = c(2),
                      decay = c(0.01))
argus <- list(method = 'mlpWeightDecayML', trace = FALSE, maxit = 5000,
              linout = ifelse(binary == TRUE, FALSE, TRUE))
}

if(model == "SVMrad") {
# (9) Radial Support Vector Machine (SVM)
inputs <- expand.grid(sigma = c(0.01, .015, 0.02),
                      C = c(0.50, 1, 2, 3))
argus <- list(method = "svmRadial")
}

if(model == "SVMpoly") {
# (10) Polynomial SVM
inputs <- expand.grid(degree = 2, scale = 1, C = 2)
argus <- list(method = 'svmPoly')
}

if(model %in% c("GB", "NNF", "RRF", "NNet", "NNet3",
                 "SVMrad", "SVMpoly")) {
# Training method in parallel
ncores <- ifelse(is.null(ncores), 1, ncores)
cl <- makeCluster(ncores)
registerDoParallel(cl)

mods <- foreach(iter = 1:nrow(inputs)) %dopar% {
  
  # Supplying conditional and unconditional arguments
  my.model <- do.call(caret::train, args = c(list(form = form, 
                      data = modelData[trInds, ], 
                      trControl = ctrl, method = argus$method,
                      tuneGrid = inputs[iter, ]),
                      list(distribution = argus$distribution)[!is.null(argus$distribution)],
                      list(trace = argus$trace)[!is.null(argus$trace)],
                      list(maxit = argus$maxit)[!is.null(argus$maxit)],
                      list(linout = argus$linout)[!is.null(argus$linout)]))
  return(my.model)
}
stopCluster(cl)

# Selecting optimal model
bestMod <- mods[[which.max(sapply(mods, 
                FUN = function(x) {x$results[ifelse(binary, "Accuracy", "Rsquared")]}))]]
}

# Obtaining predicted probabilities for test set
  preds <- predict(bestMod, modelData[-trInds, ], 
                     type = ifelse(binary, "prob", "raw"))
  
  
# Calculating test set performance summary
   if(binary == TRUE) {
  preds <- preds[, "yes"]
  meanSqErr <- round(mean((preds - resp[-trInds])^2), 4)
  logLoss <- -mean(resp[-trInds]*log(preds) + (1-resp[-trInds])*log(1-preds))
     
  misRate <- round(mean(abs(as.integer(preds > 0.50) - resp[-trInds])), 4)  
  sensitivity <- round(mean(preds[which(resp[-trInds] == 1)] > 0.50), 4)
  specificity <- round(1 - mean(preds[which(resp[-trInds] == 0)] > 0.50), 4)
  
  testSummary <- data.frame(Model = model,
                       Accuracy = 1 - misRate,
                       MisRate = misRate,
                       MSE = meanSqErr,
                       Sensitivity = sensitivity,
                       Specificity = specificity,
                       LogLoss = round(logLoss, 4))
  } else {
    testSummary <- data.frame(Model = model,
                              MSE = round(mean((preds - resp[-trInds])^2), 4))
  }
  
return(list(bestMod, testSummary))
}
