##define link function
probitlik=function(par,X,y){
      if (prod(apply(X,2,var))!=0) X=cbind(1,X) 
      ##check if there is intercept column in X
      #compute likelihoods for each individual (vectorize, so all at once)
      pi_est=pnorm(X%*%par)  
      ll=sum(y*log(pi_est)+(1-y)*log(1-pi_est))
      return(ll)
   }

## define function, input X,y, output fitte coefficient, 
log-likelihood and its variance
probopt=function(X,y){
     if (prod(apply(X,2,var))!=0) X=cbind(1,X)  
     ## Use optim to maximize likelihood, BFGS procedure
     opt.out=optim(par=matrix(0,ncol(X),1),fn=probitlik,X=X,y=y,
     method="BFGS",control=list(fnscale=-1),hessian=TRUE)
     ## return coefficient, log-likelihood and hessian
     return(list(opt.out$par,opt.out$value,opt.out$hessian))
  }





x1=rnorm(200,0,1)
x2=rnorm(200,0,1)
x3=rnorm(200,0,1)
xx=cbind(x1,x2,x3)
beta_t=c(-0.5,0.2,0.8)
pi_i=pnorm(xx%*%beta_t)

y_sp=matrix(NA,nrow=length(pi_i),ncol=1)
for (j in 1:length(pi_i)){
   y_sp[j,]=rbinom(1,1,pi_i[j])
}

hessian.m=matrix(unlist(hessian),nrow=4,ncol=4)
vcov_g=-solve(hessian.m)
xtable(vcov_g,digits=4)











###analyze and compare the first coefficient as an example
Xfull=cbind(1,xx)
beta_0=probopt(xx,y_sp)[1]
beta_0=unlist(beta_0[1])
var.beta1=vcov_g[2,2]
beta_1=beta_0[2]

b1_test=seq(min(b1_record3),max(b1_record3),length.out = 1000)
b1_analytic_pr=dnorm(mean = beta_1, x=b1_test,sd =var.beta1^.5)

####Use bootstrap from sample
Nsample=200
iters=5000

b1_record3=list()
for (iter in 1:iters){
  thisresample=sample(seq(1,Nsample),Nsample, replace=TRUE)
  xxresample=xx[thisresample,]
  yresample=y_sp[thisresample]
  coef=probopt(xxresample,yresample)[1]
  coef=unlist(coef)
  b1_record3[iter]=coef[2]
}
b1_record3=as.numeric(b1_record3)

###Use data generation
b1_record2=list()
for (iter in 1:iters){
  xx_sup=cbind(rnorm(200),rnorm(200),rnorm(200))
  pi_i_sup=pnorm(xx_sup%*%beta_t)
  y_sp_sup=matrix(NA,nrow=200,ncol=1)
  y_sp_sup=rbinom(length(pi_i_sup),1,pi_i_sup)
  coef=probopt(xx_sup,y_sp_sup)[1]
  coef=unlist(coef)
  b1_record2[iter]=coef[2]
}
b1_record2=as.numeric(b1_record2)

#Get and shade central 95% CI
dens=density(b1_record3)
q975=quantile(b1_record3,.975)
q025=quantile(b1_record3,.025)

cola=rgb(0,50,50,50,maxColorValue=255)
x1 <- min(which(dens$x >= q025))  
x2 <- max(which(dens$x <  q975))

###Plot
plot(density(b1_record3), adjust=3, col=9, lwd=4,lty=2,
     main="", xlab="MLE estimate of beta_1",ylim=c(0,4.5))

lines(b1_test,b1_analytic_pr, col=3, lwd=2)
lines(density(b1_record2), col=2, lwd=2)
abline(v=mean(b1_record2), col=2,lwd=3, lty=3)

##legend("topright",legend=c("Bootstrap","Analytic","From sample generate"),
       lty=c(1,1,2),col=c(9,3,2),lwd=1)
with(data=dens, polygon(density=30,border=NA, 
x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=1))