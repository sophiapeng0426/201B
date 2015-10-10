mse.cv.train=function(degree,kfold,data){
   ##partition to k fold
   set.seed(1234)
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
    
   ###fit polynomial
     lm.out=(lm(ytrain~., data=polyXtrain))
   
   ###test set
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
    
   ###train set
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




