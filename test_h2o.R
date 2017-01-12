library(h2o)
library(sas7bdat)
setwd("C:/Users/Thomas/Documents/01_Projet_L_EQUIPE/table_sas")

ana<-read.sas7bdat("table_analyse.sas7bdat")

h2o.init()

df <- h2o.importFile(path = normalizePath("table_analyse.csv"))

split<-h2o.splitFrame(df,
               c(0.6,0.2),
               seed = 1)

train<-h2o.assign(split[[1]], "train.hex")
test<-h2o.assign(split[[2]], "test.hex")
validation<-h2o.assign(split[[3]], "validation.hex")

y="statut_resil"
x=setdiff(names(train),c(y,"serviceexpiry"))

rf<-h2o.randomForest(x,
                     y,
                     training_frame = train,
                     validation_frame = validation,
                     model_id = "rf_covType_v1",
                     stopping_rounds = 2,
                     score_each_iteration = T,
                     ntrees = 200)

a<-as.data.frame(summary(rf))
ggplot(a[1:10,], aes(x=variable, y=percentage)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue")+
  theme_minimal()

rf@model$validation_metrics



gbm1 <- h2o.gbm(
  training_frame = train,        ## the H2O frame for training
  validation_frame = validation,      ## the H2O frame for validation (not required)
  x=x,                        ## the predictor columns, by column index
  y=y,                          ## the target index (what we are predicting)
  model_id = "gbm_covType1",     ## name the model in H2O
  seed = 2000000)                ## Set the random seed for reproducability

###############################################################################
summary(gbm1)                   ## View information about the model.

gbm2 <- h2o.gbm(
  training_frame = train,     ##
  validation_frame = validation,   ##
  x=x,                     ##
  y=y,                       ## 
  ntrees = 20,                ## decrease the trees, mostly to allow for run time
  ##  (from 50)
  learn_rate = 0.2,           ## increase the learning rate (from 0.1)
  max_depth = 10,             ## increase the depth (from 5)
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType2",  ##
  seed = 2000000)

summary(gbm2)


glm <- h2o.glm(
         training_frame = train,        ## the H2O frame for training
         validation_frame = validation,      ## the H2O frame for validation (not required)
         x=x,                        ## the predictor columns, by column index
        y=y,
        family="binomial"
)
summary(glm)


