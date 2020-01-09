library(tidyverse)
library(data.table)
library(h2o)


#Daten einlesen

train_o <- fread("/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/test_python.csv")
test_o <- fread("/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train_python.csv")

test_o$accuracy_group = NULL





h2o.init(
  nthreads=8,            ## -1: use all available threads
  max_mem_size = "6G")

pred<-test_o[,"installation_id"]
pred<-as.data.frame(pred)

names<-as.vector(colnames(train_o))
names<-names[names != c("accuracy_group")]
names<-names[names != c("installation_id")]

trainaccuracy_group<-as.factor(train_o$accuracy_group)
train <- subset(train_o, select = -c(installation_id) )
test <- subset(test_o, select = -c(installation_id) )
train<-as.h2o(train)
test<-as.h2o(test)

gbm5_b <- h2o.gbm(
  training_frame = train,     ##
  #validation_frame = NM_VAL_1,   ##
  x=names,                     ##
  y=c("accuracy_group"),                       ## 
  ntrees = 500,                ## add a few trees (from 20, though default is 50)
  learn_rate = 0.02,           ## increase the learning rate even further
  max_depth = 20,             ## 
  sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType1",
  #stopping_metric="logloss",
  nfolds = 4,
  fold_assignment = "AUTO",
  keep_cross_validation_predictions = TRUE,
  seed = 1)

xgb_linear_b_2 <- h2o.xgboost(x = names
                              ,y = c("accuracy_group")
                              ,training_frame = train
                              #validation_frame = NM_VAL
                              ,stopping_rounds = 2
                              ,stopping_metric = "RMSE"
                              #,distribution = "bernoulli"
                              ,score_tree_interval = 1
                              ,learn_rate=0.04
                              ,ntrees=500
                              ,subsample = 0.75
                              ,colsample_bytree = 0.75
                              ,tree_method = "approx"
                              ,grow_policy = "depthwise"
                              ,booster = "gbtree"
                              ,gamma = 0.1,nfolds = 4,
                              fold_assignment = "AUTO",
                              reg_lambda = 1,
                              reg_alpha = 1,
                              keep_cross_validation_predictions = TRUE,
                              seed = 1
)

#summary(xgb_linear_b_2)

rf7 <- h2o.randomForest(        ##
  training_frame = train,       ##
  #validation_frame = NM_VAL_1,     ##
  x=names,                       ##
  y=c("accuracy_group"),                         ##
  model_id = "rf_covType21",     ## 
  ntrees = 700,                 ##
  max_depth = 20,               ## Increase depth, from 20
  stopping_rounds = 20,          ##
  stopping_tolerance = 1e-2,    ##
  score_each_iteration = T,
  stopping_metric = "RMSE",
  nfolds = 4,
  fold_assignment = "AUTO",
  keep_cross_validation_predictions = TRUE,
  seed = 1)


ensemble <- h2o.stackedEnsemble(x = names,
                                y = c("accuracy_group"),
                                training_frame = train,
                                #validation_frame = NM_VAL,
                                model_id = "my_ensemble_2",
                                base_models = list(xgb_linear_b_2,gbm5_b,rf7),
                                metalearner_algorithm="glm")


pred_en<-predict(ensemble,test)

pred[,2]<-as.data.frame(pred_en[,1])

colnames(pred)[1]<-'installation_id'
colnames(pred)[2]<-'accuracy_group'
write.csv(pred, "submission.csv", row.names = F)

summary(pred)





