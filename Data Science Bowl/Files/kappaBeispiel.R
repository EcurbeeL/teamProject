#Test Kappa funktion

kappa.df <- data.frame()
kappa.df

#random numbers
x <- floor(runif(15, min=0, max=4))
x

y <- floor(runif(15, min=0, max=4))
y

#kappa test df

kappa.df <- data.frame(aktuel=x, predict=y)
kappa.df


library(Metrics)
ScoreQuadraticWeightedKappa(x, y, 1, 4)


test1 <- c(30,20,50,500,600,700)
test2 <- c(1,2,3,5,6,8)

ScoreQuadraticWeightedKappa(test1, test2, 1, 400)


bsp1 <- c(4, 4, 3, 2, 4, 4, 1, 1, 2, 1)
bsp2 <- c(4, 4, 3, 4, 4, 4, 1, 1, 2, 1)

ScoreQuadraticWeightedKappa(bsp1, bsp2)
