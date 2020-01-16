library(tidyverse)
library(data.table)
library(h2o)
install.packages("h2o")

#Daten einlesen

train_o <- fread("/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/test_python.csv")
test_o <- fread("/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train_python.csv")

test_o$accuracy_group = NULL


table(train_o$accuracy_group)


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



xgb_linear_b_2 <- h2o.xgboost(x = names
                              ,y = c("accuracy_group")
                              ,training_frame = train
                              #validation_frame = NM_VAL
                              ,stopping_rounds = 2
                              ,stopping_metric = "RMSE"
                              #,distribution = "bernoulli"
                              ,score_tree_interval = 1
                              ,learn_rate=0.04
                              ,ntrees=50
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

summary(xgb_linear_b_2)

rf7 <- h2o.randomForest(        ##
  training_frame = train,       ##
  #validation_frame = NM_VAL_1,     ##
  x=names,                       ##
  y=c("accuracy_group"),                         ##
  model_id = "rf_covType21",     ## 
  ntrees = 250,                 ##
  max_depth = 20,               ## Increase depth, from 20
  stopping_rounds = 20,          ##
  stopping_tolerance = 1e-2,    ##
  score_each_iteration = T,
  stopping_metric = "RMSE",
  nfolds = 4,
  fold_assignment = "AUTO",
  keep_cross_validation_predictions = TRUE,
  seed = 1)


#ssdf


ensemble <- h2o.stackedEnsemble(x = names,
                                y = c("accuracy_group"),
                                training_frame = train,
                                #validation_frame = NM_VAL,
                                model_id = "my_ensemble_12",
                                base_models = list(xgb_linear_b_2,rf7),
                                metalearner_algorithm="glm")


pred_en<-predict(ensemble,test)

pred[,2]<-as.data.frame(pred_en[,1])

colnames(pred)[1]<-'installation_id'
colnames(pred)[2]<-'accuracy_group'
write.csv(pred, "submission.csv", row.names = F)

summary(pred)





