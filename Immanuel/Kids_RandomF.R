library(tidyverse)
library(data.table)
library(caret)
library(ranger)

#Read Files
 
train <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/s_train.csv', stringsAsFactors = F)
train_labels <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train_labels.csv', stringsAsFactors = F)
test <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/test.csv')


train %>%
  filter(type =="Assessment") %>% 
  distinct(installation_id) %>% 
  left_join(train, by = "installation_id") -> train


test %>% 
  filter(type == "Assessment") %>% 
  arrange(desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T) %>% 
  select(installation_id, title) -> last_assessment


train(as.factor(accuracy_group) ~ title, 
      data = train_labels,
      method = "ranger") -> rf_fit


pred <- predict(rf_fit, last_assessment)
last_assessment$accuracy_group <- pred
last_assessment %>% select(-title) -> submission

write.csv(submission, "submission.csv", row.names = F)
