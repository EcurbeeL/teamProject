library(tidyverse)
library(data.table)
library(h2o)

#Daten einlesen

train <- fread("/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/test_python.csv")
test <- fread("/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train_python.csv")


h2o.init(
  nthreads=7,            ## -1: use all available threads
  max_mem_size = "6G")

pred<-test[,"installation_id"]
pred<-as.data.frame(pred)

names<-as.vector(colnames(train))
names<-names[names != c("accuracy_group")]
names<-names[names != c("installation_id")]

train$accuracy_group<-as.factor(train$accuracy_group)
#train = train[,!(names(train) %in% c("installation_id"))]
#test = test[,!(names(test) %in% c("installation_id"))]
train <- subset(train, select = -c(installation_id) )
test <- subset(test, select = -c(installation_id) )
train<-as.h2o(train)
test<-as.h2o(test)