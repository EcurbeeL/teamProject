
library(tidyverse)


train <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train.csv', stringsAsFactors = F)
train_labels <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train_labels.csv', stringsAsFactors = F)


#Daten reduzieren
train %>%
  filter(type =="Assessment") %>% 
  distinct(installation_id) %>% 
  left_join(train, by = "installation_id") -> train

test <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/test.csv')

test %>% 
  filter(type == "Assessment") %>% 
  arrange(desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T) %>% 
  select(installation_id, title) -> last_assessment

train_labels %>% 
  group_by(title) %>% 
  summarise(accuracy_group = median(accuracy_group, na.rm = T)) %>% 
  ungroup() -> median_table

last_assessment %>% 
  left_join(median_table, by = "title") %>% 
  select(-title) -> submission

write.csv(submission, "submission.csv", row.names = F)
