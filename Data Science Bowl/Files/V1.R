library(tidyverse)
library(data.table)
library(caret)
library(ranger)

train <- fread("C:/Users/Ivan/Documents/teamProject/Data Science Bowl/Data/s_train.csv")
train_labels <- fread("C:/Users/Ivan/Documents/teamProject/Data Science Bowl/Data/train_labels.csv")

#read test data
test <- fread("C:/Users/Ivan/Documents/teamProject/Data Science Bowl/Data/test.csv")

#get last assessments
test %>% 
  filter(type == "Assessment") %>% 
  arrange(desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T) %>% 
  select(installation_id, title) -> last_assessment

#generate random forest model
train(as.factor(accuracy_group) ~ title, #num mitnehmen
      data = train_labels, 
      method = "ranger") -> rf_fit

pred <- predict(rf_fit, last_assessment)
last_assessment$accuracy_group <- pred
last_assessment %>% select(-title) -> submission

write.csv(submission, "submission2611.csv", row.names = F)

summary(submission)
table(submission$accuracy_group)

#mehr feature komponeten
#siehe zeile 20
#score lokal reprodezieren (kappa)
#trainings daten in zwei teilen aufteilen (in test und training) und accuracy sowie score ausrechnen 


