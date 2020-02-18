library(tidyverse)
library(data.table)
library(ranger)
library(tidyverse)
library(data.table)
library(caret)
library(ranger)


#Daten einlesen

train <- fread("/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/test_python.csv")
test <- fread("/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train_python.csv")

#test_o$accuracy_group = NULL


# train_index <- sample(1:nrow(train), 0.9 * nrow(train))
# df_train <- train[train_index, ]
# df_test <- train[-train_index, ]


pred<-test[,"installation_id"]
pred<-as.data.frame(pred)

names<-as.vector(colnames(train))
names<-names[names != c("accuracy_group")]
names<-names[names != c("installation_id")]

trainaccuracy_group<-as.factor(train$accuracy_group)
train <- subset(train, select = -c(installation_id) )
test <- subset(test, select = -c(installation_id) )


predict <- ranger(
  # training_frame = train,
   x = names,
   y=c("accuracy_group"), 
   model_id = "rf_covType21",
   ntrees = 250,           
   max_depth = 20,               ## Increase depth, from 20
   stopping_rounds = 20,          ##
   stopping_tolerance = 1e-2,    ##
   score_each_iteration = T,
   stopping_metric = "RMSE",
   nfolds = 4,
   fold_assignment = "AUTO",
   keep_cross_validation_predictions = TRUE,
)

# predict <- randomForest(
#   training_frame = train,       
#   #validation_frame = NM_VAL_1,     
#   x=names,                       
#   y=c("accuracy_group"),                         ##
#   model_id = "rf_covType21",     ## 
#   ntrees = 250,                 ##
#   max_depth = 20,               ## Increase depth, from 20
#   stopping_rounds = 20,          ##
#   stopping_tolerance = 1e-2,    ##
#   score_each_iteration = T,
#   stopping_metric = "RMSE",
#   nfolds = 4,
#   fold_assignment = "AUTO",
#   keep_cross_validation_predictions = TRUE,
#   seed = 1))


test$accuracy_group <- predict(rf_fit, test)



pred[,2]<-as.data.frame(pred_en[,1])

colnames(pred)[1]<-'installation_id'
colnames(pred)[2]<-'accuracy_group'
write.csv(pred, "submission.csv", row.names = F)

summary(pred)





