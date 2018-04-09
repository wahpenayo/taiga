# wahpenayo at gmail dot com
# 2018-04-05
#-----------------------------------------------------------------
if (file.exists('e:/porta/projects/taiga')) {
  setwd('e:/porta/projects/taiga')
} else {
  setwd('c:/porta/projects/taiga')
}
#source('src/scripts/r/functions.r')
#-----------------------------------------------------------------
library(quantreg)
#example(rq)
#-----------------------------------------------------------------
data(engel)
model.l2 <- lm(foodexp~income,data=engel)
model.l1 <- rq(foodexp~income,data=engel,tau=0.5) 
y <- engel$foodexp
x <- data.frame(income=engel$income)
yhat <- predict(model.l1,newdata=x)
n <- length(yhat)
r <- y -yhat
rmean <- sum(r)/n
r <- y-yhat
rmse <- sqrt(sum(r*r/n))
mad <- sum(abs(r))/n
#-----------------------------------------------------------------
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
#-----------------------------------------------------------------
