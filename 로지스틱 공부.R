install.packages("ROSE") #roc.curve 라는 함수있음
library(ROSE)
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


####평가 측도 계산 방법 ()

# Zero one error 있고 ( yi 랑 yi hat) 같은지, Loss(loglikhood)자체를 평가측도, sensitivy, accuracy 등등
# info 값도 있고 등등 
# 샘플 어떻게 

#평가 측도 어떻게 구하는지 intependent test set, SV error, 등등 
# randomization 도 있음 
dim(xy.df)
length(y.vec)
dim(x.mat)
dim(nxy.df)
length(ny.vec)
dim(nx.mat)


fit=glm(case~.,data=xy.df,family="binomial")
b.vec=coef(fit)

c.val=0.5
wt=2

xb.vec=drop(cbind(1,x.mat)%*%b.vec)
exb.vec=exp(xb.vec)
p.vec=exb.vec/(1+exb.vec)
pred =as.numeric(p.vec>c.val);pred


loss=sum(-y.vec*xb.vec+log(1+exb.vec)) # 로그라이클리후드 
info= 2*loss+wt*length(b.vec) # 인포메이션 
acc=sum(y.vec==pred)     # 맞춘 비율
err=sum(y.vec!=pred) # 잘못분류
sen=sum(pred[y.vec==1]==1) # 1이라고 한애들중 실제로 1인애들
spc=sum(pred[y.vec==0]==0) # 0 이라고 한애들중 실제로 0인애들
  
##위에 여섯개가 training sample 로 계산한 6개 평가 측도 ! 

roc.obj=roc.curve(y.vec,p.vec,n.thresholds=100) 
auc=roc.obj$auc
ass=c(sum(y.vec==1),sum(y.vec==0),loss,info,acc,err,sen,spc,auc )  
#training sample 에 대한 measure 


ass.fun=function(b.vec,x.mat,y.vec,c.val,wt){
  xb.vec=drop(cbind(1,x.mat)%*%b.vec)
  exb.vec=exp(xb.vec)
  p.vec=exb.vec/(1+exb.vec)
  pred =as.numeric(p.vec>c.val);pred 
  loss=sum(-y.vec*xb.vec+log(1+exb.vec)) # 로그라이클리후드 
  info= 2*loss+wt*length(b.vec) # 인포메이션 
  acc=sum(y.vec==pred)     # 맞춘 비율
  err=sum(y.vec!=pred) # 잘못분류
  sen=sum(pred[y.vec==1]==1) # 1이라고 한애들중 실제로 1인애들
  spc=sum(pred[y.vec==0]==0) # 0 이라고 한애들중 실제로 0인애들
  roc.obj=roc.curve(y.vec,p.vec,n.thresholds=100) 
  auc=roc.obj$auc
  ass=c(sum(y.vec==1),sum(y.vec==0),loss,info,acc,err,sen,spc,auc )  
  names(ass)=c("n1","no","loss","info","acc","err","sen","spc","auc")
  return(ass)
 }

tr.ass=ass.fun(b.vec,x.mat,y.vec,c.val=0.5,wt=log(nrow(x.mat))) #training sample 이용 평가 측도 #BIC
ts.ass=ass.fun(b.vec,nx.mat,ny.vec,c.val=0.5,wt=log(nrow(x.mat)));ts.ass # test sample

#measure from cross validation
k = 10; set.seed(1234)
n=nrow(x.mat)
cpos=(1:n)[y.vec==1]  #y가 1 인 위치
npos=(1:n)[y.vec==0]  #y 가 0 인 위치
id.list=Map(c,split(sample(cpos),1:k),split(sample(npos),1:k))
ass=NULL
for(id in 1:k){
  set= id.list[[id]]
  cv.fit = glm(case~.,data=xy.df[-set,],family="binomial")
  b.vec=coef(cv.fit)
  ass=rbind(ass,ass.fun(b.vec=coef(cv.fit),x.mat[set,],y.vec[set],c.val=0.5,wt=log(nrow(x.mat)))) #training sample 이용 평가 측도 #BIC
}

colSums(ass[,-c(1,2)]) 


  