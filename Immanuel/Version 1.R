library(randomForest)
library(rpart)

#read in test and train data

#train1 <- read.csv('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/Housing/Data/train.csv')
train <- read.csv('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/teamProject/train_final.csv')
test <- read.csv('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/Housing/Data/test.csv')



#train3 <- train2 + train1(SalePrice)


#fit model 
model <- randomForest(SalePrice ~ OverallQual+ GrLivArea + GarageCars + BsmtFinSF1 + TotalBsmtSF+ X1stFlrSF + YearBuilt + TotRmsAbvGrd ,data = train, ntree=500 , importance = TRUE , mtry = 5)

plot(model)

#predikt Prices
predicted_prices <- predict(model, newdata = test)
head(predicted_prices)

# Ergebnisse ausgeben

ergebnisse <- data.frame('Id' = as.integer(test$Id), 'SalePrice' = predicted_prices)

write.csv(ergebnisse, 'Immanuel_V1_Erg.csv', row.names = FALSE, quote = FALSE)
