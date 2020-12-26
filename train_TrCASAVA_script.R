################################################################################
# last time modified: 2020/12/26
# This script is used to train TrCASAVA models in our experiment.
# The inputs file have been uploaded to https://zenodo.org/record/4365899#.X-b3CdgzaUk.
# See the folder /feature/ in 03_Disease_and_TrCASAVA_related.zip.

# Remark: It is recommend to use saved results in Zenodo.
################################################################################

source('CASAVA_library.R')
startTime <- format(Sys.time(), "%Y%m%d%H%M")

# 1) configuration
# change the following four lines according to your own
inputPath <- 'K:/project-four/CleanUp/data/Disease_and_TrCASAVA_related/feature/'
index <- 60  # Eye diseases
outName <- sprintf('BagXGB_temp_output_%s_%s.RData', startTime, index)
modelName <- sprintf('BagXGB_temp_model_%s_%s.RData', startTime, index)

# 2) preparation, get dtrain + dtransfer + test + label + group
trainName <- sprintf('%s/%s_train.RData', inputPath, index)
testName <- sprintf('%s/%s_test.RData', inputPath, index)
transferName <- sprintf('%s/%s_transfer.RData', inputPath, index)
load(trainName)
load(testName)
load(transferName)

trainLabel <- train[['label']]
train[['label']] <- NULL
train[['subset']] <- NULL
train[['group']] <- NULL
weight <- rep(1, length(trainLabel))
trainFeature <- as.matrix(train)
dtrain <-  xgboost::xgb.DMatrix(trainFeature, label = trainLabel, weight = weight)

transLabel <- transfer[['label']]
transfer[['label']] <- NULL
weight <- rep(1, length(transLabel))
transFeature <- as.matrix(transfer)
dtransfer <-  xgboost::xgb.DMatrix(transFeature, label = transLabel, weight = weight)

testLabel <- test[['label']]
group <- test[['group']]
test[['label']] <- NULL
test[['group']] <- NULL
test <- as.matrix(test)

# 3) train +test + evaluation
specificModel <- TrainBagXGB(dtrain, outName = modelName)
totalPosWeight <- sum(trainLabel) + sum(transLabel)
weight <- lapply(specificModel, predict, dtransfer)
weight <- colMeans(do.call(rbind, weight))
weight[transLabel == 0] <- 1
weight <- c(trainLabel * 4 + 1, weight)
label <- c(trainLabel, transLabel)
scale <- totalPosWeight / sum(weight[label == 1])
weight[label == 1] <- scale * weight[label == 1]

totalTrain <- xgboost::xgb.DMatrix(rbind(trainFeature, transFeature), 
                                   label = label,
                                   weight = weight)
transModel <- TrainBagXGB(totalTrain, outName = modelName)


predictions <- PredictBagXGB(model = transModel, feature = test)
pre <- helpFunc(predictions, group)
lab <- helpFunc(testLabel, group)
evaluation <- MeasureAll(pre, lab)
evaluation <- evaluation[, c('AUC', 'AUPR')]
evaluation <- colMeans(evaluation)

# 4) Save out
predictions <- pre
labels <- lab
fileList <- c('specificModel', 'transModel', 'predictions', 'labels', 'evaluation')
save(list = fileList, file = outName)
