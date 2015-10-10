mse.cv.train=function(degree,kfold,data){
  set.seed(101)
  ##partition to k fold
  library(caret)
  fold.id=createFolds(data[,1],k=kfold)
  mse.total.test=c(rep(0,kfold))
  mse.total.train=c(rep(0,kfold))
  
  for (i in 1:kfold){
    test.data.id=as.matrix(unlist(fold.id[i]))
    train.data.id=setdiff(1:nrow(data),test.data.id)
    test.data=data[test.data.id,]
    train.data=data[train.data.id,] 
    
    ##data frame for train data
    Xtrain=train.data[,1]
    ytrain=train.data[,2]
    polyXtrain=Xtrain^1
    if (degree>=2){
      for (ii in 2:degree){
        polyXtrain=cbind(polyXtrain,Xtrain^ii)
      }
    }
    polyXtrain=data.frame(polyXtrain)
    
    ##fit polynomial
    lm.out=lm(ytrain~., data=polyXtrain)
    
    ##test set
    Xtest=test.data[,1]
    ytest=test.data[,2]
    polyXtest=Xtest^1
    if (degree>=2){
      for (ii in 2:degree){
        polyXtest=cbind(polyXtest,Xtest^ii)
      }
    }
    polyXtest=cbind(1,polyXtest)
    if (degree==0){
      yhat.cv=lm.out$coef[1] 
    }else{
      yhat.cv=polyXtest%*%lm.out$coef
    }
    
    ##train set
    Xsample=as.matrix(polyXtrain)
    Xsample=cbind(1,Xsample)
    if (degree==0){
      yhat.train=lm.out$coef[1] 
    }else{
      yhat.train=Xsample%*%lm.out$coef
    }
    
    mse.total.test[i]=mean((yhat.cv-ytest)^2)  
    mse.total.train[i]=mean((yhat.train-ytrain)^2)
  }
  return(list(mean(mse.total.test),mean(mse.total.train)))
}

#####################################
set.seed(101)
library(lattice)
library(ggplot2)
n=10^4

X=runif(n,-4,4)
epsilon=rnorm(n)
y1=-2*(X < -3)+2.55*(X > -2)-2*(X > 0)+4*(X > 2)-1*(X > 3)+epsilon
y2=6+0.4*X-0.36*X^2+0.005*X^3+epsilon
y3=2.83*sin(3.14/2*X)+epsilon
y4=4*sin(3*3.14*X)*(X>0)+epsilon


dgp1=cbind(X,y1)
dgp2=cbind(X,y2)
dgp3=cbind(X,y3)
dgp4=cbind(X,y4)

###return test and train error
err.all=function(y,kfold,degree){
  test.d=rep(0,11)
  train.d=rep(0,11)
  for (i in 0:degree){
    polyfit.out = mse.cv.train(i,kfold,y)
    test.d[i+1]=unlist(polyfit.out[1])
    train.d[i+1]=unlist(polyfit.out[2])
  }
  return(cbind(test.d,train.d))
}


degree=10
kfold=10
err.dgp1=err.all(dgp1,kfold,degree)
best.degree.dgp1=as.numeric(which(err.dgp1[,1]==min(err.dgp1[,1])))-1

err.dgp2=err.all(dgp2,kfold,degree)
best.degree.dgp2=as.numeric(which(err.dgp2[,1]==min(err.dgp2[,1])))-1

err.dgp3=err.all(dgp3,kfold,degree)
best.degree.dgp3=as.numeric(which(err.dgp3[,1]==min(err.dgp3[,1])))-1

err.dgp4=err.all(dgp4,kfold,degree)
best.degree.dgp4=as.numeric(which(err.dgp4[,1]==min(err.dgp4[,1])))-1

c(best.degree.dgp1,best.degree.dgp2,best.degree.dgp3,best.degree.dgp4)
matplot(c(0:degree),err.dgp1,pch=19,col=c("red","blue"),type="b",xlab="degree of polynomial",
        ylab="Mean Squared Error",main="dgp1")
legend("topright",legend=c("test","train"),pch=19,col=c("red","blue"))

matplot(c(0:degree),err.dgp2,pch=19,col=c("red","blue"),type="b",xlab="degree of polynomial",
        ylab="Mean Squared Error",main="dgp2")
legend("topright",legend=c("test","train"),pch=19,col=c("red","blue"))

matplot(c(0:degree),err.dgp3,pch=19,col=c("red","blue"),type="b",xlab="degree of polynomial",
        ylab="Mean Squared Error",main="dgp3")
legend("topright",legend=c("test","train"),pch=19,col=c("red","blue"))
matplot(c(0:degree),err.dgp4,pch=19,col=c("red","blue"),type="b",xlab="degree of polynomial",
        ylab="Mean Squared Error",main="dgp4")
legend("topright",legend=c("test","train"),pch=19,col=c("red","blue"))

c(best.degree.dgp1,best.degree.dgp2,best.degree.dgp3,best.degree.dgp4)


####(c)fit polynomial
fitpoly=function(X,bestdegree,y){
  polyX=X^1
  for (ii in 2:bestdegree){
    polyX=cbind(polyX,X^ii)
  }
  polyX=data.frame(polyX)
  lm.out=lm(y~., data=polyX)
  return(cbind(lm.out$fitted.values,y))
}

matplot(X,fitpoly(X,8,dgp1[,2]),pch=19,col=c("blue","red"),type=c("p"),xlab="X",
        ylab="",main="dgp1")
legend("topleft",legend=c("fitted","true"),pch=19,col=c("blue","red"))

matplot(X,fitpoly(X,2,dgp2[,2]),pch=19,col=c("blue","red"),type="p",xlab="X",
        ylab="",main="dgp2")
legend("topleft",legend=c("fitted","true"),pch=19,col=c("blue","red"))

matplot(X,fitpoly(X,7,dgp3[,2]),pch=19,col=c("blue","red"),type="p",xlab="X",
        ylab="",main="dgp3")
legend("topleft",legend=c("fitted","true"),pch=19,col=c("blue","red"))

matplot(X,fitpoly(X,0,dgp4[,2]),pch=19,col=c("blue","red"),type="p",xlab="X",
        ylab="",main="dgp4")
legend("topleft",legend=c("fitted","true"),pch=19,col=c("blue","red"))












#yrange=c(floor(range(train.d,test.d)[1]), ceiling(range(train.d,test.d)[2]))
#plot(train.d,type="o",col="blue",ylim=yrange,main="Estimation of DGP3",xlab="Dimension",ylab="Mean Squared Error",axes=FALSE)
#lines(test.d,type="o",pch=22,lty=2,col="red")
#axis(1,at=1:11,lab=c('0','1','2','3','4','5','6','7','8','9','10'))
#axis(2,las=1)
#legend("topright",legend=c("Training","Testing"),col=c("blue","red"),pch=21:22,lty=1:2);
