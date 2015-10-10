setwd("desktop/HW2")
library(gdata)
data2=read.csv("PS1_dataf.csv")
data2["tenure_sq"]=NA
data2["age_sq"]=NA
data2$tenure_sq=data2$inc_tenure^2
data2$age_sq=data2$inc_age^2
data2$comp_challenger=scale(data2$comp_challenger,center=TRUE,scale=TRUE)
data2$comp_incumbent=scale(data2$comp_incumbent,center=TRUE,scale=TRUE)

library(sandwich)
glm.out2=glm(vote_incumbent~cook_inc_risk+diff+same+
                      comp_challenger+comp_incumbent+ inc_tenure+tenure_sq
                      +inc_age+age_sq, data=data2,family=binomial(link="probit"))
bread=vcov(glm.out2)
score=estfun(glm.out2)
meat=t(score)%*%score
sandwich=bread%*%meat%*%bread

library(lmtest)
coeftest(glm.out2,sandwich)


##identify cluster variable
s1=data2$state
m=length(unique(s1))
p=ncol(model.matrix(glm.out2))
s=estfun(glm.out2)
s.cluster=matrix(NA,nrow=m,ncol=p)
for(j in 1:p){
  s.cluster[,j]=tapply(s[,j],s1,sum)
}
dfc=m/(m-1)
meat.s1=dfc*t(s.cluster)%*%s.cluster
vcov.s1=bread%*%meat.s1%*%bread
coeftest(glm.out2,vcov.s1)


Xfull=model.matrix(glm.out2)
X_low=Xfull
X_low[,"comp_challenger"]=-3
 
##get different drawings for beta using distribution
R1=20000
tau_1=matrix(NA,nrow=R1,ncol=1)
beta_sim_record=matrix(NA,nrow=R1,ncol=10)
library(MASS)

for (r in 1:R1){
  ###use cluster SE
  beta_r=mvrnorm(1,glm.out2$coef,vcov.s1)
  beta_sim_record[r,]=beta_r
  pi_low=pnorm(X_low%*%beta_r)
  Y_low=rbinom(length(pi_low),1,pi_low)
  tau_1[r]=mean(Y_low)
 }
mc.tau=tau_1
sd_mc=round(sd(mc.tau),3)
mean_mc=round(mean(mc.tau),3)
plot(density(mc.tau),xlim=c(0.55,0.8),
main="",xlab="vote_percent",ylim=c(0,13),lwd=3)
abline(v=mean_mc,col=2)


c_cha=data2$comp_challenger
x_min=quantile(c_cha,0.25)
x_max=quantile(c_cha,0.75)
X_l=X_h=Xfull
X_l[,"comp_challenger"]=x_min
X_h[,"comp_challenger"]=x_max

###bootstrap
nboots=2000
beta_block_bs_record=matrix(NA,nrow=nboots,ncol=10)
tau.d=matrix(NA,nrow=nboots,ncol=1)

for(iter in 1:nboots){
  clsample=sample(s1,length(s1),replace=TRUE)
  bootsample=c()
  for (j in 1:length(clsample)){
  bootsample=c(bootsample,which(data2$state%in%clsample[j]))
  }
  dat_sample=data2[bootsample,]
  glm.out.iter=glm(vote_incumbent~same+diff+comp_challenger+
  comp_incumbent+cook_inc_risk+inc_tenure+tenure_sq+inc_age+age_sq,          data=dat_sample,family=binomial(link="probit"))
  beta_iter=glm.out.iter$coef
  beta_block_bs_record[iter,]=beta_iter
  pi_l=pnorm(X_l%*%beta_iter)
  pi_h=pnorm(X_h%*%beta_iter)
  Y_l=rbinom(length(pi_l),1,pi_l)
  Y_h=rbinom(length(pi_h),1,pi_h)
  tau.d[iter]=mean(Y_h-Y_l)
 }
 
R1=20000
tau_d2=matrix(NA,nrow=R1,ncol=1)
beta_sim_record=matrix(NA,nrow=R1,ncol=10)
library(MASS)

for (r in 1:R1){
  beta_r=mvrnorm(1,glm.out2$coef,vcov.s1)###use cluster SE
  beta_sim_record[r,]=beta_r
  pi_l=pnorm(X_l%*%beta_r)
  pi_h=pnorm(X_h%*%beta_r)
  Y_l=rbinom(length(pi_l),1,pi_l)
  Y_h=rbinom(length(pi_h),1,pi_h)
  tau_d2[r]=mean(Y_h-Y_l)
}
mc.tau.d=tau_d2
sd_mc.tau.d=round(sd(mc.tau.d),3)
mean_mc.tau.d=round(mean(mc.tau),3)
 
mean_taud=mean(tau.d)
sd_taud=sd(tau.d)
boxplot(tau.d,ylab="difference in voting for incumbent")
boxplot(tau_d2,ylab="difference in voti