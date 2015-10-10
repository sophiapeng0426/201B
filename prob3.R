#install.packages("ISLR")
#install.packages("tree")
#install.packages("randomForest")
#install.packages("gbm")
library(ISLR)
library(tree)
library(randomForest)
library(gbm)
library(xtable)

train.number=150
mtry=3
ntree=1000

set.seed(3011)
train.id=sample(1:nrow(Heart),train.number)
train.data=Heart[train.id,]
test.id=setdiff(1:nrow(Heart),train.id)
test.data=Heart[test.id,]

##random forest
rf.heart=randomForest(AHD~.,data=train.data,xtest=test.data[,-12],ytest=test.data$AHD,
                      mtry=3,ntree=1000)
rf.heart2=randomForest(AHD~.,data=train.data,mtry=3,ntree=1000)
rf.test.err1=rf.heart$test$err.rate[1000]
#plot(test.sum$err.rate[,1])

pred.rf.test=predict(rf.heart2,test.data,type="class")
xtable(table(pred.rf.test,test.data$AHD))
rf.test.err2=1-sum(diag(table(pred.rf.test,test.data$AHD)))/nrow(test.data)
rf.oob.err=rf.heart$err.rate[1000]

###effect of mtry
err.rf.mtry=function(train,test,mtry){
  oob.err.mtry=rep(0,mtry)
  test.err.mtry1=rep(0,mtry)
  #test.err.mtry2=rep(0,mtry)
  for (m in 1:mtry){
    rf.fit=randomForest(AHD~.,data=train,xtest=test[,-12],ytest=test$AHD,
                        mtry=m,ntree=500)
    #rf.fit2=randomForest(AHD~.,data=train,mtry=m,ntree=500)
    test.err.mtry1[m]=rf.fit$test$err.rate[500]
    #pred=predict(rf.fit2,test,type="class")
    #test.err.mtry2[m]=1-sum(diag(table(pred,test$AHD)))/nrow(test)
    oob.err.mtry[m]=rf.fit$err.rate[500]
    #cat(m,"")
  }
  return(cbind(test.err.mtry1,oob.err.mtry))  
}

####output error for mtry
mtry.multiple=length(train.data)-1
err.total=err.rf.mtry(train.data,test.data,mtry.multiple)
matplot(1:mtry.multiple,err.total,pch=19,col=c("red","black"),type="b",
        ylab="Error",xlab="mtry")
legend("topright",legend=c("test","OOB"),pch=19,col=c("red","black"))
best.mtry=which(err.total[,1]==min(err.total[,1]))
best.mtry

##lasso logit
library(glmnet)
logit.out=cv.glmnet(y=Heart[train.id,"AHD"], x=as.matrix(Heart[train.id, -12]),family="binomial")
#coef(logit.out)
xtest=test.data[,-12]
logit.class=predict(logit.out, newx = as.matrix(xtest),type="class")
#logit.class=as.numeric(yhat.logit>=.5)
###test error
table(yhat.logit, test.data$AHD)
logit.err=1-sum(diag(table(logit.class,test.data$AHD)))/nrow(test.data)
c(logit.err,rf.test.err1)
