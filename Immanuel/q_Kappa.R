library(compiler)

# funktion erstellen
q_kappa <-  cmpfun(function(observed, predicted, levels = 0L:4L){ #Anlegen der Funktion
    require(Matrix, quietly = TRUE)
    n <- length(levels) #Anzahl der zu verarbeitenenden Werte
    
    if(length(observed) != length(predicted)) # Überprüfung der übergabeparameter
      stop("Die Länge der beiden übergebenen Parameter ist nicht gleich")
    
    if(!is.factor(observed))
      observed <- factor(observed, levels = levels)
    if(!is.factor(predicted))
      predicted <- factor(predicted, levels = levels)
    
    # Matrix erstellen
    i <- i2 <- rep(1L:n, each = n)
    j <- j2 <- rep(1L:n, n)
    ind <- j > i
    
    # Unnötige Werte berechnen
    i <- i[ind]
    j <- j[ind]
    
    #Gewichtungsmatrix berechnen
    W <- sparseMatrix(i, j, (i - j)^2/(n-1)^2, c(n, n), symmetric = TRUE)
    O <- table(observed, predicted)
    
    #Histogram berechnen
    observed <- table(observed)
    predicted <- table(predicted)
    
    #Werte berechnen
    E <- sparseMatrix(i2, j2, observed[i2] * predicted[j2], c(n, n))
    
    #Kappa Berechnen
    1 - (sum(O * W) * sum(E)) / (sum(E * W) * sum(O))
})

