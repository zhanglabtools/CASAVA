################################################################################
# last time modified: 2020/8/16
# Set the train_file, test_file, and will give the results
# Output = model + prediction + evaluation

source('CASAVA_library.R')
startTime <- format(Sys.time(), "%Y%m%d%H%M")

# 1) configuration
# change the following four lines according to your own
inputPath <- 'K:/project-four/CleanUp/data/CASAVA_related/feature/'
index <- 8  # Eye diseases
outName <- sprintf('BagXGB_temp_output_%s_%s.RData', startTime, index)
modelName <- sprintf('BagXGB_temp_model_%s_%s.RData', startTime, index)

# 2) preparation, get dtrain + test + label + group
trainName <- sprintf('%s/%s_train.RData', inputPath, index)
testName <- sprintf('%s/%s_test.RData', inputPath, index)
load(trainName)
load(testName)
label <- train[['label']]
train[['label']] <- NULL
train[['subset']] <- NULL
train[['group']] <- NULL
weight <- rep(1, length(label))
feature <- as.matrix(train)
dtrain <-  xgboost::xgb.DMatrix(feature, label = label, weight = weight)
label <- test[['label']]
group <- test[['group']]
test[['label']] <- NULL
test[['group']] <- NULL
test <- as.matrix(test)

# 3) train +test + evaluation
model <- TrainBagXGB(dtrain, outName = modelName)
predictions <- PredictBagXGB(model = model, feature = test)
pre <- helpFunc(predictions, group)
lab <- helpFunc(label, group)
evaluation <- MeasureAll(pre, lab)
evaluation <- evaluation[, c('AUC', 'AUPR')]
evaluation <- colMeans(evaluation)

# 4) Save out
predictions <- pre
labels <- lab
fileList <- c('model', 'predictions', 'labels', 'evaluation')
save(list = fileList, file = outName)
