library(tidyverse)
library(data.table)
library(caret)
library(ranger)
library(caTools)
library(xgboost)

score_events <- function(df){
  
  df %>%
    distinct(game_session, installation_id, .keep_all = T) %>%
    select(game_session, installation_id, timestamp, title) -> install_ids
  
  df %>% 
    filter(grepl('""correct""', event_data)) %>%
    mutate(correct = as.numeric(grepl('""correct"":true', event_data))) %>%
    select(game_session, title, event_id, event_code, correct) %>%
    group_by(game_session) %>%
    summarize(num_correct = sum(correct),
              num_incorrect = n() - sum(correct),
              accuracy = sum(correct) / n()) %>%
    select(game_session, num_correct, num_incorrect, accuracy) %>%
    left_join(., install_ids, by = c('game_session'))
}




train_data <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/2s_train.csv', stringsAsFactors = F)
train_labels <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train_labels.csv', stringsAsFactors = F)
test <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/test.csv')


#Die zu Bewertenden Assessments Herausfinden
test %>%
  filter(type == "Assessment") %>%
  arrange(desc(timestamp)) %>%
  distinct(installation_id, .keep_all = T) %>%
  select(installation_id, title) -> last_assessment


# Train Daten aufbereiten um damit ein Modell zu erstellen
train_data %>%
  filter(installation_id %in% train_labels$installation_id,
         type == 'Game') %>%
  score_events() %>%
  mutate(accuracy_for_title = title) %>%
  group_by(installation_id, accuracy_for_title) %>%
  summarize(sum_incorrect = sum(num_incorrect),
            sum_correct = sum(num_correct),
            mean_accuracy = mean(accuracy, na.rm=T),
            median_accuracy = median(accuracy, na.rm=T),
            sd_accuracy = sd(accuracy, na.rm=T),
            n_games = n(),
            n_distinct_games = n_distinct(accuracy_for_title)) -> player_game_history


#Hier wird die Spieler Performance mit den Train Labels verbunden
train_labels %>%
  left_join(., player_game_history, by = 'installation_id') -> player_game_history_with_labels

# Play Performance Werte berechnen
player_game_history_with_labels %>%
  distinct(installation_id, title, accuracy_for_title, .keep_all = T) %>%
  select(installation_id, accuracy_group, median_accuracy, title, accuracy_for_title) %>%
  spread(., key = accuracy_for_title, value = median_accuracy) -> train_set


#Hier werden alle Werte die NA sind mit einem Medianwert ersetzt
NA_to_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
replace(train_set, TRUE, lapply(train_set, NA_to_median)) %>% select(-`<NA>`) -> for_training

training <- matrix(for_training)

train <- xgboost(params = for_training %>% select(-installation_id))

help("xgboost")

boost <- xgb.train()

boost <- xgboost(data = training %>% select(-installation_id), label = for_training$accuracy_group, max_depth = 2, eta = 0,25, nthread = 7, nrounds = 10, objective = "binary:logistic")



# Testdaten wie Oben bearbeiten
test %>% 
  filter(type == "Assessment") %>% 
  arrange(desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T) %>% 
  select(installation_id, title, game_session) %>%
  mutate(person_title = paste0(installation_id, title)) -> last_assessment

test %>%
  filter(type == 'Game') %>%
  score_any_scorable_events() -> test_player_history

test_player_history %>%
  mutate(accuracy_for_title = title) %>%
  group_by(installation_id, accuracy_for_title) %>%
  summarize(sum_incorrect = sum(num_incorrect),
            sum_correct = sum(num_correct),
            mean_accuracy = mean(accuracy, na.rm=T),
            median_accuracy = median(accuracy, na.rm=T),
            sd_accuracy = sd(accuracy, na.rm=T),
            n_games = n(),
            n_distinct_games = n_distinct(accuracy_for_title)) -> test_player_history_by_title

test_player_history_by_title %>%
  select(installation_id, median_accuracy, accuracy_for_title) %>%
  spread(., key = accuracy_for_title, value = median_accuracy) -> test_set


last_assessment %>%
  left_join(., test_set, by = 'installation_id') -> test_last_with_history

NA_to_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
replace(test_last_with_history, TRUE, lapply(test_last_with_history, NA_to_median)) -> test_set_impute

test_set_impute$accuracy_group <- predict(rf_fit, test_set_impute)

test_set_impute %>% select(installation_id, accuracy_group) -> submission

write.csv(submission, "submission.csv", row.names = F)













