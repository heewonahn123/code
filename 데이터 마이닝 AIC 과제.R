
source("C:/Temp/data.mining/function.R")
# X,Y 임의로 만들어서 모든 모델 AIC 적용
x.mat=matrix(rnorm(90),ncol=3)
b.vec=c(1,1,1)
y.vec=x.mat%*%b.vec+rnorm(30)
MOD=matrix(c(FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE),nrow=8)
AIC=rep(NA,nrow(MOD))
as.list(AIC)


# i 가 1 일때 
fit=lm(y.vec~1)
AIC(fit)
AIC[1]=AIC(fit)
# i 가 2~8일때
for (i in 2:8) {
  { 
    trx = x.mat[,MOD[i,]]
    fit=lm(y.vec~trx)
  }
  AIC[i]= AIC(fit)
}              

AIC


plot(AIC)







################# forward selection 이용해서 AIC 적용
x.mat=matrix(rnorm(90),ncol=3)
b.vec=c(1,1,1)
y.vec=x.mat%*%b.vec+rnorm(30)
fit.AIC= forward.fun(y.vec,x.mat,inf.wt=2,trace=T)
fit.AIC
opt=dim(fit.AIC$coef.mat)[2]
b.mat=NULL
b.mat=cbind(b.mat,fit.AIC$coef.mat[,opt])
## AIC가 제일 작은 모델 이 b.mat
b.mat



