# John Alan McDonald 2015-11-20
#-------------------------------------------------------------------------------
setwd('e:/workplace/tu-projects/taiga')
source('src/scripts/r/functions.r')
#-------------------------------------------------------------------------------
summary(iris)
iris2 <- iris[(iris$Species!='setosa'),]
summary(iris2)
iris2$Species <- as.character(iris2$Species)
iris2$Species <- as.factor(iris2$Species)
summary(iris2)

ind <- sample(2, nrow(iris2), replace=TRUE, prob=c(0.5,0.5))
train <- iris2[ind==1,]
train.x <- train[,names(train)!='Species']
test <- iris2[ind==2,]
test.x <- test[,names(test)!='Species']

rf11 <- randomForest(y=train$Species, x=train.x, ntree=512, classwt=c(1,1))
classes11 <- predict(rf11,test.x,type='response')
scores11 <- predict(rf11,test.x,type='prob')
summary(classes11)
summary(scores11)
table(predict(rf11,train.x), train$Species)
table(predict(rf11,test.x), test$Species)

rf91 <- randomForest(y=train$Species, x=train.x, ntree=512, classwt=c(10000,1))
classes91 <- predict(rf91,test.x,type='response')
scores91 <- predict(rf91,test.x,type='prob')
summary(classes91)
summary(scores91)
table(predict(rf91,test.x), test$Species)

rf9901 <- randomForest(y=train$Species, x=train.x, ntree=512, classwt=c(0.99,0.01))
classes9901 <- predict(rf9901,test.x,type='response')
scores9901 <- predict(rf9901,test.x,type='prob')
summary(classes9901)
summary(scores9901)
table(predict(rf9901,train.x), train$Species)
table(predict(rf9901,test.x), test$Species)

rf999001 <- randomForest(y=train$Species, x=train.x, ntree=512, classwt=c(0.999,0.001))
classes999001 <- predict(rf999001,test.x,type='response')
scores999001 <- predict(rf999001,test.x,type='prob')
summary(classes999001)
summary(scores999001)
table(predict(rf999001,train.x), train$Species)
table(predict(rf999001,test.x), test$Species)

