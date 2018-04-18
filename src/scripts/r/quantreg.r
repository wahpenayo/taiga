# wahpenayo at gmail dot com
# 2018-04-16
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
fit.summary <- function(y,yhat,p=0.75) {
  n <- length(yhat)
  r <- y -yhat
  rmean <- mean(r)
  rmse <- sqrt(sum(r*r/n))
  rmad <- sum(abs(r))/n
  rqr <- 2.0*sum(ifelse(r>=0,p*r,(p-1)*r))/n
  qrq <- 0.5*sum(ifelse(r>=0,r/(1-p),-r/p))/n
  list(rmean=rmean,rmse=rmse,rmad=rmad,rqr=rqr,qrq=qrq)
}
#-----------------------------------------------------------------
data(engel)
y <- engel$foodexp
x <- data.frame(income=engel$income)

affine.l2 <- lm(foodexp~income,data=engel)
yhat <- predict(affine.l2,newdata=x)
print("l2 affine")
print(affine.l2,digits=16)
print(fit.summary(y,yhat),digits=16)

affine.q50 <- rq(foodexp~income,data=engel,tau=0.5) 
yhat <- predict(affine.q50,newdata=x)
print("q50 affine")
print(affine.q50,digits=16)
print(fit.summary(y=y,yhat=yhat),digits=16)

affine.q75 <- rq(foodexp~income,data=engel,tau=0.75) 
yhat <- predict(affine.q75,newdata=x)
print("q75 affine")
print(affine.q75,digits=16)
print(fit.summary(y=y,yhat=yhat),digits=16)

linear.l2 <- lm(foodexp~income - 1,data=engel) 
yhat <- predict(linear.l2,newdata=x)
print("l2 linear")
print(linear.l2,digits=16)
print(fit.summary(y,yhat),digits=16)

linear.q50 <- rq(foodexp~income - 1,data=engel,tau=0.5) 
yhat <- predict(linear.q50,newdata=x)
print("q50 linear")
print(linear.q50,digits=16)
print(fit.summary(y,yhat),digits=16)

linear.q75 <- rq(foodexp~income - 1,data=engel,tau=0.75) 
yhat <- predict(linear.q75,newdata=x)
print("q75 linear")
print(linear.q75,digits=16)
print(fit.summary(y,yhat),digits=16)

yhat <- rep_len(median(y),length(y))
print("q50 constant")
print(yhat[1],digits=16)
print(fit.summary(y,yhat),digits=16)

yhat <- rep_len(quantile(x=y,probs=0.75),length(y))
print("q75 constant")
print(yhat[1],digits=16)
print(fit.summary(y,yhat),digits=16)

yhat <- rep_len(mean(y),length(y))
print("l2 constant")
print(yhat[1],digits=16)
print(fit.summary(y,yhat),digits=16)
#-----------------------------------------------------------------
data(stackloss)
yx <- data.frame(
  stackloss=stackloss$stack.loss,
  acidconc=stackloss$Acid.Conc.,
  airflow=stackloss$Air.Flow,
  watertemp=stackloss$Water.Temp)
y <- yx$stackloss
x <- data.frame(
  acidconc=yx$acidconc,
  airflow=yx$airflow,
  watertemp=yx$watertemp)

affine.l2 <- lm(stackloss~acidconc+airflow+watertemp,data=yx)
yhat <- predict(affine.l2,newdata=x)
print("l2 affine")
print(affine.l2,digits=16)
print(fit.summary(y,yhat),digits=16)

affine.q50 <- rq(stackloss~acidconc+airflow+watertemp,data=yx,tau=0.5) 
yhat <- predict(affine.q50,newdata=x)
print("q50 affine")
print(affine.q50,digits=16)
print(fit.summary(y=y,yhat=yhat),digits=16)

affine.q75 <- rq(stackloss~acidconc+airflow+watertemp,data=yx,tau=0.75) 
yhat <- predict(affine.q75,newdata=x)
print("q75 affine")
print(affine.q75,digits=16)
print(fit.summary(y=y,yhat=yhat),digits=16)

linear.l2 <- lm(stackloss~acidconc+airflow+watertemp - 1,data=yx) 
yhat <- predict(linear.l2,newdata=x)
print("l2 linear")
print(linear.l2,digits=16)
print(fit.summary(y,yhat),digits=16)

linear.q50 <- rq(stackloss~acidconc+airflow+watertemp - 1,data=yx,tau=0.5) 
yhat <- predict(linear.q50,newdata=x)
print("q50 linear")
print(linear.q50,digits=16)
print(fit.summary(y,yhat),digits=16)

linear.q75 <- rq(stackloss~acidconc+airflow+watertemp - 1,data=yx,tau=0.75) 
yhat <- predict(linear.q75,newdata=x)
print("q75 linear")
print(linear.q75,digits=16)
print(fit.summary(y,yhat),digits=16)

yhat <- rep_len(mean(y),length(y))
print("l2 constant")
print(yhat[1],digits=16)
print(fit.summary(y,yhat),digits=16)

yhat <- rep_len(median(y),length(y))
print("q50 constant")
print(yhat[1],digits=16)
print(fit.summary(y,yhat),digits=16)

yhat <- rep_len(quantile(x=y,probs=0.75),length(y))
print("q75 constant")
print(yhat[1],digits=16)
print(fit.summary(y,yhat),digits=16)
#-----------------------------------------------------------------
