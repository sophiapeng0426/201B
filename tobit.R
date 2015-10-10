setwd("desktop/HW2")
library("gdata")
data=read.csv("brader.csv")
Y=as.matrix(data[,c("immigr")])
X=as.matrix(data[,c("tone","eth","ppage","ppincimp")])

llk.ordlogit.4 = function(par, X, y) {
  #X=as.matrix(X)
  dimX=ncol(X)
  beta=par[1:dimX]
  tau=par[(dimX+1):(dimX+3)]
  #linear predictor (E[y*|x])
  XB = X%*%beta
  #prob y* falls tau_j and tau_(j-1), assuming tau_4=inf
  p4=log(1-exp(tau[3]-XB)/(1+exp(tau[3]-XB)))
  p3=log(exp(tau[3]-XB)/(1+exp(tau[3]-XB))-exp(tau[2]-XB)/(1+exp(tau[2]-XB)))
  p2=log(exp(tau[2]-XB)/(1+exp(tau[2]-XB))-exp(tau[1]-XB)/(1+exp(tau[1]-XB)))
  p1=log(exp(tau[1]-XB)/(1+exp(tau[1]-XB)))
  #keep t?s in right order
  penalty=10^6*as.numeric(tau[2]>tau[3])*as.numeric(tau[1]>tau[2])
  #choose correct probability depending on observed y
  right_probs=p1^(y==1)*p2^(y==2)*p3^(y==3)*p4^(y==4)
  #final log-likelihood
  ll=sum(right_probs)+penalty
  return(ll)
}
ologit.out = optim(par = c(rep(0,ncol(X)),c(-1,0,1)),
                    fn = llk.ordlogit.4,X=X, 
                    y=Y,method = "BFGS", 
                    control = list(fnscale = -1),
                    hessian = TRUE)

ologit.out$sd = sqrt(diag(-solve(ologit.out$hessian)))

###Canned version
polr.out=polr(as.factor(Y)~as.matrix(X),method = c("logistic"))
summary(polr.out)

#bonus
#Define ordered logistic link function
ordlogit.4.g=function(par, X, y){
X=as.matrix(X)
dimX=ncol(X)
beta1=par[1:dimX]
beta2=par[(dimX+1):(2*dimX)]
beta3=par[(2*dimX+1):(3*dimX)]
tau=par[(3*dimX+1):(3*dimX+3)]
#prob y* falls tau_j and tau_(j-1), assuming tau_4=inf
p4=log(1 - exp(tau[3]-X%*%beta3)/(1+exp(tau[3]-X%*%beta3)))
p3=log(exp(tau[3]-X%*%beta3)/(1+exp(tau[3]-
X%*%beta3)) - exp(tau[2]-X%*%beta2)
/(1+exp(tau[2]-X%*%beta2)))
p2=log(exp(tau[2]-X%*%beta2)/(1+exp(tau[2]-
X%*%beta2)) - exp(tau[1]-X%*%beta1)
/(1+exp(tau[1]-X%*%beta1)))
p1=log(exp(tau[1]-X%*%beta1)/(1+exp(tau[1]-X%*%beta1)))
#keep t's in right order
penalty=10^6*as.numeric(tau[2]>tau[3])*as.numeric(tau[1]>tau[2])
#choose correct probability depending on observed y
right_probs=p1^(y==1)*p2^(y==2)*p3^(y==3)*p4^(y==4)
#final log-likelihood
ll=sum(right_probs)+penalty
return(ll)
}

ologit.out.g= optim(par = c(rep(0,3*ncol(X)),c(-1,0,1)),
fn = ordlogit.4.g,X=X, y=Y,method = "BFGS",
control = list(fnscale = -1),hessian = TRUE)
ologit.out.g$std = sqrt(diag(-solve(ologit.out.g$hessian)))

#output coefficients and std
para=as.data.frame(cbind(ologit.out.g$par,ologit.out.g$std))
xtable(para,digits=3)