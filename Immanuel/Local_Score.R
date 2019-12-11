library(tidyverse)
library(data.table)
library(caret)
library(ranger)
library(caTools)

# Funktion um alle Richtig gelösten aufgaben zu Finden
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


#Read Files

train_data <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/2s_train.csv', stringsAsFactors = F)
train_labels <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train_labels.csv', stringsAsFactors = F)
test <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/test.csv')



# Daten in Zwei teile aufteilen
# Train Labels
set.seed(101)
sample = sample.split(train_labels$game_session, SplitRatio = 0.80)
train_label_1 = subset(train_labels, sample == TRUE)
train_label_2 = subset(train_labels, sample == FALSE)


#Train Data 
set.seed(102)
sample = sample.split(train_data$event_id, SplitRatio = 0.80)
p_data = subset(train_data, sample == TRUE)
v_data = subset(train_data, sample = FALSE)


#Die zu Bewertenden Assessments Herausfinden
test %>% 
  filter(type == "Assessment") %>% 
  arrange(desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T) %>% 
  select(installation_id, title) -> last_assessment


# Train Daten aufbereiten um damit ein Modell zu erstellen
p_data %>%
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
train_label_1 %>%
  left_join(., player_game_history, by = 'installation_id') -> player_game_history_with_labels

player_game_history_with_labels %>%
  distinct(installation_id, title, accuracy_for_title, .keep_all = T) %>%
  select(installation_id, accuracy_group, median_accuracy, title, accuracy_for_title) %>%
  spread(., key = accuracy_for_title, value = median_accuracy) -> train_set_2

#Hier werden alle Werte die NA sind mit einem Medianwert ersetzt 
NA_to_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
replace(train_set_2, TRUE, lapply(train_set_2, NA_to_median)) %>% select(-`<NA>`) -> for_training

train(as.factor(accuracy_group) ~ .,
      data = for_training,
      metric = "Accuracy",
      importance = "impurity",
      num.trees = 50,
      method = "ranger") -> rf_fit

















