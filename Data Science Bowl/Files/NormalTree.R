library(data.table)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(dplyr)

# Read Train Data 
df.train <- data.table::fread("C:/Users/Ivan/Documents/2s_train.csv")
head(df.train)
str(df.train)

# Read Label Data
df.label <- data.table::fread("C:/Users/Ivan/Documents/teamProject/Data Science Bowl/Data/train_labels.csv")

# Clean Train Data
df.train %>% 
  filter(type == "Assessment") %>% 
  arrange(desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T) -> clean.train.df

# Merge 'clean.train.df' with 'df.label' 
merge.data.frame(clean.train.df, df.label, by = c("installation_id", "game_session")) -> merge.df

# Clean merge.df
str(merge.df)
select(merge.df, 'event_count', 'event_code', 'game_time', 'title.x', 'num_correct', 'num_incorrect', 'accuracy', 'accuracy_group') -> clean.merge.df
tree <- rpart(accuracy_group ~ . , method='class', data= clean.merge.df)
printcp(tree)
plot(tree, uniform = TRUE, main="Accuracy Group")
text(tree, use.n = TRUE, all=TRUE)
prp(tree, main = "Accuracy")
