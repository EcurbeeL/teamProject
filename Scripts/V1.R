library(rpart) # for regression trees
library(randomForest) # for random forests
library(tidyverse)
library(set)
library(do)
library(dplyr)

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


train -> data_5

i <- sapply(data_5, is.factor) # Identify all factor variables in your data
data_5[i] <- lapply(data_5[i], as.character) # Convert factors to character variables
data_5[is.na(data_5)] <- 0 # Replace NA with 0
data_5[i] <- lapply(data_5[i], as.factor) # Convert character columns back to factors

colSums(sapply(data_5, is.na))

         