#################
# Title: Predictive Models
# Author: Andrew DiLernia
# Date: 03/14/2020
# Purpose: Implement predictive models given design matrix and response
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

# Creating design matrix and response vector
designData <- desResp[[1]]
respVec <- desResp[[2]]$MOV

# Setting parameters ------------------------------------------------------
seed <- 1994
trainSize <- 0.10
secOrder <- FALSE # Fit a 1st or 2nd order model
binary <- FALSE # Binary or continuous response
set.seed(seed)

# Removing any rows with missing values
compInds <- complete.cases(designData)
designData <- designData[compInds, ]
respVec <- respVec[compInds]

# Creating model data frame
modelData <- as.data.frame(cbind(respVec, designData))

# Specifying model order
modOrder <- ifelse(secOrder, "order2", "order1")

# Creating model form object
form <- list(order1 = respVec ~ 0 + ., order2 =  respVec ~ 0 + .^2)[[modOrder]]

# Creating model design matrix
designMat <- model.matrix(form, data = designData)

# Randomly selecting training set
trInds <- sample(1:nrow(designMat), replace = FALSE, 
                 size = ceiling(trainSize*nrow(designMat)))

# Training objects
modelData <- modelData[trInds, ]
designMat <- designMat[trInds, ]
outcome <- respVec[trInds]

# Test Objects
modelData.test <- modelData[-trInds, ]
designMat.test <- designMat[-trInds, ]

# Changing factor level names to work with train() function
if(binary == TRUE) {
  levels(modelData$outcome) <- c("no", "yes")
  ctrl <- trainControl(method = "cv", number = 5, classProbs =  TRUE)
  
  # Changing outcome for PCR to be numeric
  modelData.pcr <- modelData
  modelData.pcr$outcome <- as.integer(as.character(modelData$outcome) == "yes")
} else {
  ctrl <- trainControl(method = "cv", number = 5)
  modelData.pcr <- modelData
}

# (1)a 1st-order Linear/Logistic Regression with Lasso Penalty using 10-fold CV
cvglmFit <- cv.glmnet(x = designMat, y = outcome, 
                      nfolds = 10, family = ifelse(binary == TRUE, "binomial", "gaussian"),
                      lambda = c(0.0001, 0.0005, seq(0.001, 0.10, by = 0.001)))
