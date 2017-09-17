# John Alan McDonald 2015-11-20
#-------------------------------------------------------------------------------
setwd('/workplace/taiga')
source('src/main/r/functions.r')
#-------------------------------------------------------------------------------
data(BostonHousing2)
summary(BostonHousing2)
write.tsv(BostonHousing2,file='data/BostonHousing2.tsv')

y <- BostonHousing2$cmedv

#BostonHousing2$chas <- as.factor(BostonHousing2$chas)

xcols <- 
#  c('lon','lat') 
  c('lon','lat','crim','zn','indus','nox','rm','age','dis','rad','tax',
    'ptratio','b','lstat','chas') 
x <- BostonHousing2[,xcols,drop=FALSE]

train <- function (y,x,mtry=floor(max(1, length(xcols) / 3)),
                   ntree=128,nodesize=1) {
  print(
    system.time(
      forest <- randomForest(y=y,x=x,nodesize=nodesize,ntree=ntree,mtry=mtry,
        keep.forest=TRUE)))
  yhat <- predict(forest,x)
  
  #print(cbind(y,yhat,(y-yhat)))
#  print(table(yhat-y))
  mad <- function (y, yhat) { 
    r <- (y - yhat)
    sum(abs(r)) / length(y) }
  
  rmse <- function (y, yhat) { 
    r <- (y - yhat)
    sqrt(sum(r*r) / length(y)) }
  
  print(
    list(n=length(y),ntree=ntree,nodesize=nodesize,mtry=mtry,nattr=ncol(x),
      mean=mean(y),
      meanhat=mean(yhat),
      mad=mad(y,yhat),
      bias=(sum(yhat - y) / length(y)),
      #fbias=2*sum(yhat - y)/(sum(y) + sum(yhat)),
      rmse=rmse(y,yhat)))
}
train(y=y,x=x,ntree=512)
