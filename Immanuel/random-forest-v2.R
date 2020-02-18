## Importing packages

library(tidyverse)
library(data.table)
library(caret)
library(ranger)
library(caTools)

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats




score_any_scorable_events <- function(df){

  
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






train_data <- fread("../input/data-science-bowl-2019/train.csv")
train_labels <- fread("../input/data-science-bowl-2019/train_labels.csv")
test <- fread("../input/data-science-bowl-2019/test.csv")



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
      num.trees = 500,
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





## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.