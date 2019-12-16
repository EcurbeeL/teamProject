connect <- function(trainLabel, submission){
  new.df <- NULL
  
}

machDF <- function(df){
  write.csv(df, file = 'newDF')
  dfn <- read.csv('newDF')
  head(dfn)
}

parreSub <- function(df1, subdf){
  zeile = 0
  for (variable in df1$installation_id) {
    zeile = zeile + 1
    for (subv in subdf$installation_id) {
      
    }
    ifelse(subdf$installation_id == variable){
      
    }
    
  }
}

new.df <- NULL
new.df
new.df <- data.frame(train_labels$installation_id, train_labels$accuracy_group)
machDF(new.df)
#new.df$sub_group <- rep(NA, nrow(new.df)) # NA Spalte

machDF(new.df)

head(new.df[2,3])
new.df[2,3] <- 4 #eine Zeile überschreiben 
head(new.df)



subBeispiel <- data.frame(sample_submission)

#m <- merge.data.frame(new.df, subBeispiel, by = new.df$installation_id )
names(new.df)[names(new.df) == "train_labels.installation_id"] <- "installation_id"
head(new.df)
subBeispiel <- data.frame(submission2611)

order(new.df$installation_id)
order(subBeispiel)

y <- merge(subBeispiel, new.df, by.x = c( 'installation_id' ))



