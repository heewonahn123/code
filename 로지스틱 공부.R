
library(spatstat)
setwd("C:/Temp/data.mining")
rm(list=ls())

xy.df=read.csv("disease_old.csv")
y.mat=as.matrix(dummify(xy.df[,1])[,-2,drop=F]) #drop F 하면 matrixy 유지
x.mat=as.matrix(cbind(dummify(xy.df[,2])[,-4],dummify(xy.df[,3])[,-2,drop=F] ,xy.df[,-c(1:3)]))
xy.df=data.frame(cbind(y.mat,x.mat))
y.vec = as.numeric(xy.df[,1])



nxy.df=read.csv("disease_new.csv")
ny.mat=as.numeric(dummify(nxy.df[,1])[,-2,drop=F]) #drop F 하면 matrixy 유지
nx.mat=as.matrix(cbind(dummify(nxy.df[,2])[,-4],dummify(nxy.df[,3])[,-2,drop=F] ,nxy.df[,-c(1:3)]))
nxy.df=data.frame(cbind(ny.mat,nx.mat))
ny.vec=as.numeric(nxy.df[,1])
head(nxy.df)



#### logistic regression
fit= glm(case~.,data=xy.df,family="binomial")
b.vec=coef(fit) # 회귀계수, estimated parameter 
s.fit=summary(fit)
names(s.fit)
s.fit
# linear predictor
x.mat
xb.vec=drop(cbind(1,x.mat) %*% b.vec) # 이게 predictor 

# mean predictor
exb.vec=exp(xb.vec)

  
p.vec= exb.vec/(1+exb.vec) #이게 확률 
predict(fit,newdata=xy.df,type="response") #이것도 확률 
# training loss function = 우도비 값 
loss=sum(-y.vec*xb.vec +log(1+exb.vec))


fit$deviance #loss에 두배임 

# information  aic 같은거 
#aic -2*loglike+2df 
aic=2*loss+ 2*length(b.vec)
fit$deviance+2*length(b.vec)
# odds
o.vec=p.vec/(1-p.vec)
o.vec[order(o.vec,decreasing=T)] # 32번째 관측치는 odds 값이 제일 크다
# odds ratio 
or.vec= exp(xb.vec)/exp(xb.vec[32])
or.vec

# increment of oods ratio
i.vec= exp(b.vec)





