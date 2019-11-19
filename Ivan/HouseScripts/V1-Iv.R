library(rpart) # for regression trees
library(randomForest) # for random forests
library(tidyverse)
library(dplyr)

# read in the test and train data
train <- read.csv("C:/Users/Ivan/Documents/teamProject/HouseData/train.csv")
test <- read.csv("C:/Users/Ivan/Documents/teamProject/HouseData/test.csv")

#replace NA with 0
i <- sapply(train, is.factor) # Identify all factor variables in your data
train[i] <- lapply(train[i], as.character) # Convert factors to character variables
train[is.na(train)] <- 0 # Replace NA with 0
train[i] <- lapply(train[i], as.factor) # Convert character columns back to factors

colSums(sapply(train, is.na))

# fit our model
<<<<<<< HEAD
model <- randomForest(SalePrice ~ LotArea + OverallQual + YearBuilt +  TotalBsmtSF+  TotRmsAbvGrd, data = train)

#YearRemodAdd +  MasVnrArea + BsmtFinSF1 +
 
=======
model <- randomForest(SalePrice ~ LotArea + OverallQual + YearBuilt + TotRmsAbvGrd, data = train)

>>>>>>> 3d0aa16c23e27171bb4a2905c77830f3a94550a8
# use our model to make predictions
predicted_prices <- predict(model, newdata = test)

# look at the first few predicted prices to ensure we have something sensible.
head(predicted_prices)

# create a dataframe with our results
my_submission <- data_frame('Id' = as.integer(test$Id), 'SalePrice' = predicted_prices)
# save our file
<<<<<<< HEAD
write_csv(my_submission, 'Submissions/V1-Iv.csv')
=======
write_csv(my_submission, 'Submissions/V1.csv')
>>>>>>> 3d0aa16c23e27171bb4a2905c77830f3a94550a8


