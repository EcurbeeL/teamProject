library(randomForest)
library(rpart)

#read in test and train data
train <- read.csv('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/Housing/Data/train.csv')
test <- read.csv('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/Housing/Data/test.csv')


#fit model 
model <- randomForest(SalePrice ~ GrLivArea + OverallQual + YearBuilt + TotRmsAbvGrd , 
                      data = train, ntree=500)

plot(model)

#predikt Prices
predicted_prices <- predict(model, newdata = test)
head(predicted_prices)

# Ergebnisse ausgeben

ergebnisse <- data.frame('Id' = as.integer(test$Id), 'SalePrice' = predicted_prices)

write.csv(ergebnisse, 'Immanuel_V1_Erg.csv', row.names = FALSE, quote = FALSE)
