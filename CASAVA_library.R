# last time modified: 2020/12/26
# Author: Zhen Cao
# Contact: cz@amss.ac.cn
################################################################################
# Dependency
library('data.table')
library("dplyr")
library("xgboost")  # Version 0.82 is used during experiment
library('mltools')
library('ROCR')
################################################################################
# under sampling + XGB with weighted sample

# params:
#  dtrain:a xgb.Dmatrix object, need to contain feature,label and weights
#  param:the parameters to train the xgboost
#  nrounds:the rounds of each booster
#  nBooster:the number of under sampling procedure
#  outName:the name for the output model
TrainBagXGB <- function(dtrain,
                        param = "default", 
                        nrounds = 10,
                        nBooster = 100,
                        outName = "temp.RData",
                        gcStep = 10,
                        printStep = 10,
                        seed=1) {
  # preparation inputs
  if (param[1] == "default") {
    param <- list(colsample_bytree = 1,
                  eta = 0.7,
                  tree_method = "exact",
                  nthread = 4,
                  objective = "binary:logistic",
                  seed = 0)
  }
  label <- xgboost::getinfo(dtrain, "label")
  weight <- xgboost::getinfo(dtrain, "weight")
  if (is.null(weight)) {
    xgboost::setinfo(dtrain, 'weight', rep(1, nrow(dtrain)))
    weight <- xgboost::getinfo(dtrain, "weight")
    print("Message: No weight is assigned. Each sample will use weight=1")
  }
  
  posIndex <- which(label == 1)
  negIndex <- which(label == 0)
  n <- ceiling(sum(weight[label==1]))
  replace <- FALSE
  if (n > sum(negIndex)) {
    replace <- TRUE
    print("Message: The number of negative sample is smaller than the number positive sample,
          the under-sampling precedure will use replace=TRUE.")
  }
  
  # training
  trees <- list()
  for (i in 1:nBooster) {

    set.seed(i + seed)
    tempIndex <- c(posIndex, sample(negIndex, n, replace = replace))
    tempTrain <- xgboost::slice(dtrain, tempIndex)
    bst <- xgboost::xgb.train(param, tempTrain, nrounds = nrounds)
    
    # remark:deal with the RAM problem
    save(bst, file = outName)
    rm(bst)
    load(outName)
    
    if (i %% gcStep == 0) {
      gc()
    }
    if (i %% printStep == 0) {
      print(paste0(date(), " Finish under sampling: ", i))
    }
    
    trees[[i]] <- bst
  }
  
  save(trees, file = outName)
  return(trees)
}

################################################################################
# params:
#  model:models from the TrainBagXGB function
#  feature:testing features,a matrix or a data.frame
#  nBooster:the number of boosters for testing

PredictBagXGB <- function(model, feature, slice = 50000, nBooster = -1) {
  if (nBooster > 0 && nBooster <= length(model)) model <- model[1:nBooster]
  
  if (slice < 0 || nrow(feature) <= slice) {
    dtest <- xgb.DMatrix(as.matrix(feature))
    predictions <- lapply(model, predict, dtest)
    predictions <- do.call(cbind, predictions)
    predictions <- rowMeans(predictions)
  } else {
    predictions <- list()
    indexes <- rep(1:ceiling(nrow(feature)/slice), each = slice)
    indexes <- indexes[1:nrow(feature)]
    indexes <- split(1:nrow(feature), indexes)
    for (i in seq_along(indexes)) {
      dtest <- feature[indexes[[i]], ] 
      dtest <- xgb.DMatrix(as.matrix(dtest)) 
      pred <- lapply(model, predict, dtest)
      pred <- do.call(cbind, pred)
      pred <- rowMeans(pred)
      predictions[[i]] <- pred
    }
    predictions <- do.call(c, predictions)
  }
  return(predictions)
}
################################################################################
# Calculate Accuracy, Precision, Recall, F1, MCC
SimpleEval <- function(predict, actual_labels, cutoff = 0.5) {
  predict <- predict > cutoff
  accuracy <- sum(predict == actual_labels) / length(actual_labels)
  precision <- sum(predict & actual_labels) / sum(predict)
  recall <- sum(predict & actual_labels) / sum(actual_labels)
  fmeasure <- 2 * precision * recall / (precision + recall)
  mccValue <- mltools::mcc(as.numeric(predict), actual_labels)
  return(c(accuracy = accuracy, precision = precision, recall = recall,
           F1 = fmeasure, MCC = mccValue))
}
################################################################################
# Calculate Accuracy, Precision, Recall, F1, MCC, AUROC, AUPRC
MeasureAll <- function(raw_predict, actual_label, cutoff = 0.5) {
  
  if (!is.list(raw_predict)) raw_predict <- list(raw_predict)
  if (!is.list(actual_label)) actual_label <- list(actual_label)
  
  evals <- mapply(SimpleEval, 
                  raw_predict, 
                  actual_label,
                  cutoff = cutoff, 
                  SIMPLIFY = F)
  evals <- do.call(rbind, evals)
  pred <- ROCR::prediction(raw_predict, actual_label)
  auc <- ROCR::performance(pred, "auc")@y.values
  aupr <- ROCR::performance(pred, "aucpr")@y.values

  return(cbind(AUC = unlist(auc), AUPR = unlist(aupr), evals))
}
################################################################################
helpFunc <- function(x, group) {
  index1 <- group == 0
  output <- list()
  for (i in 1:10) {
    index2 <- group == i
    index <- index1 | index2
    output[[i]] <- x[index]
  }
  return(output)
}
