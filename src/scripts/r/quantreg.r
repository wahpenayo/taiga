# wahpenayo at gmail dot com
# 2018-04-05
#-----------------------------------------------------------------
if (file.exists('e:/porta/projects/taiga')) {
  setwd('e:/porta/projects/taiga')
} else {
  setwd('c:/porta/projects/taiga')
}
source('src/scripts/r/functions.r')
#-----------------------------------------------------------------
library(quantreg)
#example(rq)
data(stackloss)
yx <- data.frame(
  stackloss=stackloss$stack.loss,
  acidconc=stackloss$Acid.Conc.,
  airflow=stackloss$Air.Flow,
  watertemp=stackloss$Water.Temp)
x <- data.frame(
  acidconc=stackloss$Acid.Conc.,
  airflow=stackloss$Air.Flow,
  watertemp=stackloss$Water.Temp)
model.rq <- rq(stackloss ~ acidconc + airflow + watertemp,data=yx,,tau=0.5) 
y <- yx$stackloss
yhat <- predict(model.rq,newdata=x)
n <- length(yhat)

r <- y -yhat
rmean <- sum(r)/n
r <- y-yhat
rmse <- sqrt(sum(r*r/n))
mad <- sum(abs(r))/n

data(engel)
lm(foodexp~income,data=engel)
