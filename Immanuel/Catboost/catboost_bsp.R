library(catboost)


#install.packages('devtools')
#devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.20/catboost-R-Windows-0.20.tgz', INSTALL_opts = c("--no-multiarch"))

#trainRF = df.train
#trainRF$y = as.factor(trainRF$y)

features <- df.train
features$x_31 <- as.numeric(features$x_31)
features$x_90 <- as.numeric(features$x_90) #nochmal nachschauen chr
#class(features$x_31)
#str(features)
trainCAT = features
labels = as.integer(trainCAT$y)
class(labels)
length(unique(trainCAT$y))

train_pool <- catboost.load_pool(data = features, label = labels) 
# für chr daten cat_features = c(features$x_31, features$x_90))

fit_params <- list(iterations = 100,
                   #random_seed = 101,
                   loss_function = 'MultiClass',
                   classes_count = 8)

model <- catboost.train(train_pool, params = fit_params)

featuresTest <- df.test
featuresTest$x_31 <- as.numeric(featuresTest$x_31)
featuresTest$x_90 <- as.numeric(featuresTest$x_90)
featuresTest$"y" <- rep(0, nrow(featuresTest))



real_pool <- catboost.load_pool(featuresTest)


prediction <- catboost.predict(model, real_pool, prediction_type = "Probability") #type="prob" für catboost
#print(prediction)
class(prediction)
predicition <- as.matrix(prediction)

predCat = cbind( 1:nrow(df.test), prediction )
class(predCat)
sum( rowSums(predCat) ) == nrow(df.test)


colnames(predCat) = c('id', paste0('y_', 1:9))
summary(predCat)

# save and submit predictions
write.table( predCat, sep = ',', file = 'SUBMISSIONCAT.csv',
             row.names = FALSE)
