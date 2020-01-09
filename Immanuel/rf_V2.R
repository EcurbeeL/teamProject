
score_any_scorable_events <- function(df){
  
  library(tidyverse)
  
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

load_quietly <- function(package) {
  suppressWarnings(suppressMessages(library(deparse(substitute(package)), character.only=TRUE))) 
}
load_quietly(tidyverse)
load_quietly(data.table)
load_quietly(caret)
load_quietly(ranger)


help("data.table")

load_data <- function() {
  
  train_data <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/2s_train.csv', stringsAsFactors = F)
  train_labels <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/train_labels.csv', stringsAsFactors = F)
  test <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/test.csv')
  
}

test %>% 
  filter(type == "Assessment") %>% 
  arrange(desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T) %>% 
  select(installation_id, title) -> last_assessment


train_data %>%
  filter(installation_id %in% train_labels$installation_id,
         type == 'Game') %>%
  score_any_scorable_events() %>%
  mutate(accuracy_for_title = title) %>%
  group_by(installation_id, accuracy_for_title) %>%
  summarize(sum_incorrect = sum(num_incorrect),
            sum_correct = sum(num_correct),
            mean_accuracy = mean(accuracy, na.rm=T),
            median_accuracy = median(accuracy, na.rm=T),
            sd_accuracy = sd(accuracy, na.rm=T),
            n_games = n(),
            n_distinct_games = n_distinct(accuracy_for_title)) -> player_game_history

train_labels %>%
  left_join(., player_game_history, by = 'installation_id') -> player_game_history_with_labels


set.seed(23489)

player_game_history_with_labels %>%
  distinct(installation_id, title, accuracy_for_title, .keep_all = T) %>%
  select(installation_id, accuracy_group, median_accuracy, title, accuracy_for_title) %>%
  spread(., key = accuracy_for_title, value = median_accuracy) -> train_set_2

NA_to_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
replace(train_set_2, TRUE, lapply(train_set_2, NA_to_median)) %>% select(-`<NA>`) -> for_training

train_index <- sample(1:nrow(for_training), 0.9 * nrow(for_training))
df_train <- for_training[train_index, ]
df_test <- for_training[-train_index, ]

trainControl(
  method = "repeatedcv",
  repeats = 5) -> trctrl

train(as.factor(accuracy_group) ~ .,
      data = df_train %>% select(-installation_id), 
      trControl = trctrl,
      metric = "Accuracy",
      importance = "impurity",
      num.trees = 50,
      method = "ranger") -> rf_fit

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

#df_train %>% select(installation_id, accuracy_group) -> for_kappa_train
#df_test %>% select(installation_id, accuracy_group) -> for_kappa_test

#for_kappa_test %>% left_join(., for_kappa_train, by = 'installation_id') -> for_kappa_fin

#for_kappa_fin %>% select(accuracy_group.y != NA) -> for_kappa_fin2

#submission %>% select(installation_id, accuracy_group) %>% left_join(for_kappa, accuracy_group, by = 'installation_id') -> for_kappa2

#for_kappa %>% select(installation_id, accuracy_group) %>% left_join(submission, accuracy_group, by = 'installation_id') -> for_kappa3
  
#q_kappa(for_kappa_fin$accuracy_group.x[1:3],for_kappa_fin$accuracy_group.y[1:3],3:3)


#q_kappa(1,2)
  
  
  
  
  