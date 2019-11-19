#Einbinden der Librarys

install.packages("magrittr")

library(ggplot2)
library(readr) # CSV file I/O, e.g. the read_csv function
require(xgboost)
require(methods)
require(data.table)
require(magrittr)

#Datenset Importieren

train <- fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/Housing/Data/train.csv', header = T, stringsAsFactors = T)
test <- fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/Housing/Data/test.csv', header = TRUE, stringsAsFactors = T)

trainY <- train[]
trainX <- train[]
step <- rbind(trainX, test, fill=TRUE)


char <- t(data.frame(lapply(step, is.function)))
char <- data.frame(char)
name = row.names(char)
char_n_idx = which(char$char == TRUE)

# in Nummer Konvertieren

char <- t(data.frame(lapply(step, is.factor))) #we need to use t to transpose it 
#as it originally a 2*n dataframe but n*2 is more user friendly
char <- data.frame(char) #change it to dataframe to get dimension

name = row.names(char)
char_n_idx = which(char$char == TRUE)

#--------- Convert them to number---------#
step2 = data.frame(step)
n_idx = length(char_n_idx)
counter = 1
for (i in seq(n_idx)){
  idx = char_n_idx[i]
  n_lv = length(unique(step2[,idx]))
  levels(step2[,idx]) = counter:(counter+n_lv-1)
  counter = (counter+n_lv-1) 

  step2[,idx] <- as.numeric(step2[,idx])
}



train_len <- dim(trainX)[1]
step_len <- dim(step2)[1]

train_x <- step2[1:train_len,]
test_x <- step2[(train_len+1):step_len,]

train_final <- as.matrix(train_x, sparse = TRUE) 
test_final <- as.matrix(test_x, sparse = TRUE)



boost <- xgboost(data = train_final, label = train$SalePrice, max.depth = 5, eta = 0.15, print_every_n = 100, nthread = 8, nround = 1500)

pred <- predict(boost, test_final)
ergebnisseV2 <- data.frame('Id' = as.integer(test$Id), 'SalePrice' = pred)
write.csv(ergebnisseV2, 'Immanuel_V2_Erg.csv', row.names = FALSE, quote = FALSE)
write.csv(pred, file="answer.csv")
imp <- xgb.importance (model = boost)
xgb.plot.importance (importance_matrix = imp[1:20]) 

write.csv(train_final, 'train_final.csv', row.names = FALSE, quote = FALSE)

