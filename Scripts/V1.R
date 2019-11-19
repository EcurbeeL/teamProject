library(rpart) # for regression trees
library(randomForest) # for random forests
library(tidyverse)
library(set)
library(do)

# read in the test and train data
train <- read.csv("C:/Users/Ivan/Documents/teamProject/HouseData/train.csv")
test <- read.csv("C:/Users/Ivan/Documents/teamProject/HouseData/test.csv")

# fit our model
model <- randomForest(SalePrice ~ LotArea + OverallQual + YearBuilt + TotRmsAbvGrd, data = train)

# use our model to make predictions
predicted_prices <- predict(model, newdata = test)

# look at the first few predicted prices to ensure we have something sensible.
head(predicted_prices)

# create a dataframe with our results
my_submission <- data_frame('Id' = as.integer(test$Id), 'SalePrice' = predicted_prices)
# save our file
write_csv(my_submission, 'Submissions/V1.csv')

summary(train)

train[is.na(train)]=0
train <- train[is.na(train)]=0

ind <-   which(sapply(train, is.numeric))
for(j in ind){
  set(train, i = which(is.na(train[[j]])), j = j, value = 0)
}

train[is.na(train)] <- 0
