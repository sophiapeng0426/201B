setwd("~/desktop/HW3")
n=100
kfold=10
library(caret)
library(MASS)

set.seed(12)
X=runif(n,-4,4)
epsilon=rnorm(n)
y1=-2*(X<3)+2.55*(X>-2)-2*(X>0)+4*(X>2)+epsilon
y2=6+0.4*X-0.36*X^2+0.005*X^3+epsilon
y3=2.83*sin(3.14/2*X)+epsilon
y4=4*sin(3*3.14*X)*(X>0)+epsilon

dgp1=cbind(X,y1)
dgp2=cbind(X,y2)
dgp3=cbind(X,y3)
dgp4=cbind(X,y4)

mse.cv.d=rep(0,11)
mse.train.d=rep(0,11)
for (i in 0:10){
  mse.cv.d[i+1]=unlist(mse.cv.train(i,10,dgp4)[1])
  mse.train.d[i+1]=unlist(mse.cv.train(i,10,dgp4)[2])
}  