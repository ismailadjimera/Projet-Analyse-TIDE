setwd("C:/Users/Data 1/Documents/KAGGLE")

library(data.table)
library(readr)
library(caret)
library(stringdist)

Sys.setlocale('LC_ALL', 'russian');

location = read.csv('Location.csv')
category = read.csv('Category.csv')
itempairs_train = read.csv('ItemPairs_train.csv')
itempairs_test = read.csv('ItemPairs_test.csv')
iteminfo_test = read.csv('ItemInfo_test.csv',encoding='UTF-8')
iteminfo_train = read.csv('ItemInfo_train.csv')

avito_train<-read.csv('avito_train1.csv',sep=";")


avito_test<-read.csv('avito_test1.csv',sep=";")



#samp_avito_train <- stratified(avito_train, "isDuplicate",25000)


#samp_avito<-subset(avito_train,!(avito_train$X%in%samp_avito_train$x))

#samp_avito_test<-samp_avito[sample(nrow(samp_avito), 25000),]

samp_avito_test<-avito_train[sample(nrow(avito_train), 10000),]

samp_avito_train<-subset(avito_train,!(avito_train$X%in%samp_avito_test$X))

samp_avito_test$X<-NULL
samp_avito_train$X<-NULL

samp_avito_test$itemID_x<-NULL
samp_avito_train$itemID_x<-NULL

samp_avito_test$itemID_y<-NULL
samp_avito_train$itemID_y<-NULL

library(h2o)
library(devtools)
install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")
library(h2oEnsemble)
library(SuperLearner)  # For metalearner such as "SL.glm"
library(cvAUC)
h2o.init()
localH2O <-  h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, nthreads = -1)


y <- "isDuplicate"
x <- setdiff(names(samp_avito_train), y)

family <- "binomial"

samp_avito_train<-as.h2o(samp_avito_train)
samp_avito_train$isDuplicate<-as.factor(samp_avito_train$isDuplicate)
samp_avito_test<-as.h2o(samp_avito_test)

h2o.randomForest.1 <- function(..., ntrees = 1000, nbins = 100, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.deeplearning.1 <- function(..., hidden = c(500,500), activation = "Rectifier", seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.2 <- function(..., hidden = c(200,200,200), activation = "Tanh", seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.gbm.3 <- function(..., ntrees = 200, max_depth = 5, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
learner <- c("h2o.randomForest.1",
             "h2o.gbm.3"
             , "h2o.deeplearning.1", 
             "h2o.deeplearning.2"
)
metalearner <- c("SL.glm")

fit <- h2o.ensemble(x = x, y = y,samp_avito_train, family = family, 
                    learner = learner, metalearner = metalearner,
                    cvControl = list(V=5))



pred <- predict(fit, samp_avito_test)
labels <- as.data.frame(samp_avito_test[,c(y)])[,1]

#a<-as.data.frame(pred$pred)
AUC(predictions=as.data.frame(pred$pred)[,1], labels=labels)

samp_avito_test<-as.h2o(avito_test)
pred <- predict(fit, samp_avito_test)
predictions=as.data.frame(pred$pred)[,1]
predictions=as.data.frame(predictions)
a<-as.data.frame(avito_test[,1])
names(a)<-'id'
names(predictions)<-'probability'
soumission<-cbind(a,predictions)

write.csv(soumission,"soumission4.csv",row.names = F)




#### xgboost 

samp_avito_train <- stratified(avito_train, "isDuplicate",25000)

samp_avito<-subset(avito_train,!(avito_train$X%in%samp_avito_train$x))

samp_avito_test<-samp_avito[sample(nrow(samp_avito), 25000),]

samp_avito_test$X<-NULL
samp_avito_train$X<-NULL

library(xgboost)
library(data.table)
library(readr)
library(caret)
library(stringdist)

maxTrees <- 10000
shrinkage <- 0.01
gamma <- 1
depth <- 8
minChildWeight <- 20
colSample <- 0.9
subSample <- 0.9
earlyStopRound <- 2


samp_avito_train$itemID_x<-NULL
samp_avito_train$itemID_y<-NULL

modelVars <- names(samp_avito_train)[which(!(names(samp_avito_train) %in% c("isDuplicate")))]

samp_avito_train=data.frame(samp_avito_train)
samp_avito_test=data.frame(samp_avito_test)



dtrain <- xgb.DMatrix(as.matrix(samp_avito_train[, modelVars]), label=samp_avito_train$isDuplicate)
dtest <- xgb.DMatrix(as.matrix(samp_avito_test[, modelVars]))

# xgboost cross-validated
set.seed(1984)
xgbCV <- xgb.cv(params=list(max_depth=depth,
                            eta=shrinkage,
                            gamma=gamma,
                            colsample_bytree=colSample,
                            min_child_weight=minChildWeight,
                            subsample=subSample,
                            objective="binary:logistic"),
                data=dtrain,
                nrounds=maxTrees,
                eval_metric ="auc",
                nfold=30,
                early.stop.round=earlyStopRound)

numTrees <- min(which(xgbCV$test.auc.mean==max(xgbCV$test.auc.mean)))





## GRID
# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 200,
  eta = c(0.1, 0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 0,
  colsample_bytree = 1,    #default=1
  min_child_weight = 1     #default=1
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",       
  classProbs = TRUE,                            
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
samp_avito_train$isDuplicate[samp_avito_train$isDuplicate==1] <- "yes"
samp_avito_train$isDuplicate[samp_avito_train$isDuplicate==0] <- "no"

xgb_train_1 = train(
  x = as.matrix(samp_avito_train[, modelVars]),
  y = as.factor(samp_avito_train$isDuplicate),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  verbose = 1,
  metric = "Accuracy",
  method = "xgbTree"
)



# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) +
  geom_point() +
  theme_bw() +
  scale_size_continuous(guide = "none")

































### Recherche d'hyperparametre optimaux

## Hyper-Parameter Search

## Construct a large Cartesian hyper-parameter space
ntrees_opts <- c(10000) ## early stopping will stop earlier
max_depth_opts <- seq(1,20)
min_rows_opts <- c(1,5,10,20,50,100)
learn_rate_opts <- seq(0.001,0.01,0.001)
sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_per_tree_opts = seq(0.3,1,0.05)
nbins_cats_opts = seq(100,10000,100) ## no categorical features in this dataset

hyper_params = list( ntrees = ntrees_opts, 
                     max_depth = max_depth_opts, 
                     min_rows = min_rows_opts, 
                     learn_rate = learn_rate_opts,
                     sample_rate = sample_rate_opts,
                     col_sample_rate = col_sample_rate_opts,
                     col_sample_rate_per_tree = col_sample_rate_per_tree_opts
                     #,nbins_cats = nbins_cats_opts
)


## Search a random subset of these hyper-parmameters (max runtime and max models are enforced, and the search will stop after we don't improve much over the best 5 random models)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 600, max_models = 100, stopping_metric = "AUTO", stopping_tolerance = 0.00001, stopping_rounds = 5, seed = 123456)

gbm.grid <- h2o.grid("gbm", 
                     grid_id = "mygrid",
                     x = x, 
                     y = y, 
                     
                     # faster to use a 80/20 split
                     training_frame = samp_avito_train,
                     validation_frame = samp_avito_test,
                     nfolds = 5,
                     
                     # alternatively, use N-fold cross-validation
                     #training_frame = train,
                     #nfolds = 5,
                     
                     distribution="AUTO", ## best for MSE loss, but can try other distributions ("laplace", "quantile")
                     
                     ## stop as soon as mse doesn't improve by more than 0.1% on the validation set, 
                     ## for 2 consecutive scoring events
                     stopping_rounds = 2,
                     stopping_tolerance = 1e-3,
                     stopping_metric = "AUC",
                     
                     score_tree_interval = 100, ## how often to score (affects early stopping)
                     seed = 123456, ## seed to control the sampling of the Cartesian hyper-parameter space
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

gbm.sorted.grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "auc")
print(gbm.sorted.grid)

best_model <- h2o.getModel(gbm.sorted.grid@model_ids[[1]])
summary(best_model)
