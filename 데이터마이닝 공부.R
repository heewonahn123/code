rm(list=ls())
library(glmnet)
library(ROSE)
library(spatstat)
library(ncpen)
library(ncvreg)

setwd("C:/Temp/data.mining")
source("C:/Temp/data.mining/function.R")



##########comparison by information ##############

sen.df=read.csv("sensitivity_old.csv")
sen.df=sen.df[,1:5]
y.vec=sen.df[,1]
z.mat=cbind(dummify(sen.df[,2])[,-1],dummify(sen.df[,3])[,-1])
x.mat=cbind(sen.df[,-c(1:3)],z.mat)



b.mat=NULL

fit=forward.fun(y.vec,x.mat,inf.wt=2,trace=T) #AIC
opt=dim(fit$coef.mat)[2]
b.mat=cbind(b.mat,fit$coef.mat[,opt])



fit=forward.fun(y.vec,x.mat,inf.wt=log(nrow(x.mat)),trace=T) #BIC 
opt=dim(fit$coef.mat)[2]
b.mat=cbind(b.mat,fit$coef.mat[,opt])





#cri.vec 이 AIC나 bIC 값 나오는거 

#lasso+AIC
fit=ncpen(y.vec,x.mat,penalty="lasso") 
aic=gic.ncpen(fit,weight=2) # 모형들의 AIc 값 계산해주는거
opt=which.min(aic$gic)
b.mat=cbind(b.mat,fit$beta[,opt]) #AIC 로 봤을때 최고 모델 


# SCAD+BIC

fit=ncpen(y.vec,x.mat,penalty="scad")
aic=gic.ncpen(fit,weight=log(nrow(x.mat)))
opt=which.min(aic$gic)
b.mat=cbind(b.mat,fit$beta[,opt])




########comparison by independent test error

sen.df=read.csv("sensitivity_old.csv")
sen.df=sen.df[,1:5]
y.vec=sen.df[,1]
z.mat=cbind(dummify(sen.df[,2])[,-1],dummify(sen.df[,3])[,-1])
x.mat=cbind(sen.df[,-c(1:3)],z.mat)
x.mat=as.matrix(x.mat)
n=nrow(x.mat)


# independent test error 
set=sample(1:n,size=round(n/3),replace=T)  #1/3 뽑음음
fit=forward.fun(y.vec[-set],x.mat[-set,],out.stop=F)  
err=colMeans((y.vec[set]-cbind(1,x.mat[set,])%*%fit$coef.mat)^2) #예측은 set에 들어있는것으로 
opt=which.min(err)
b.mat=fit$coef.mat[,opt]  



# LASSO 
fit=ncpen(y.vec[-set],x.mat[-set,],penalty="lasso")
err=colMeans((y.vec[set]-cbind(1,x.mat[set,])%*%fit$beta)^2) #예측은 set에 들어있는것으로 
opt=which.min(err)
b.mat=cbind(b.mat,fit$beta[,opt])




# scad
fit=ncpen(y.vec[-set],x.mat[-set,],penalty="scad")
err=colMeans((y.vec[set]-cbind(1,x.mat[set,])%*%fit$beta)^2) #예측은 set에 들어있는것으로 
opt=which.min(err)
b.mat=cbind(b.mat,fit$beta[,opt])





#######comparison by cross validation error
set=split(sample(1:n),1:5)
err=0

for(fid in 1:5){
fit=forward.fun(y.vec[-set[[fid]]],x.mat[-set[[fid]],],out.stop=F) # fid 번 빼고  set에 있는 데이터 제외하고 모델 만들기
err= err + colSums((y.vec[set[[fid]]]-cbind(1,x.mat[set[[fid]],])%*%fit$coef.mat)^2) # 그 모델 이용해서 error 값 구하기
}
opt=which.min(err) #에러값이 제일 낮은 모델 찾기
fit=forward.fun(y.vec,x.mat,out.stop=F) #원래 데이터로 모델만들기  
b.mat=cbind(b.mat,fit$coef.mat[,opt])   #그중 cv에서 찾은 에러가 제일 낮은모델 coef 구하기 




fit=cv.ncpen(y.vec,x.mat,penalty="lasso",n.fold=5)
opt=which.min(fit$rmse)
b.mat=cbind(b.mat,fit$ncpen.fit$beta[,opt])



fit=cv.ncpen(y.vec,x.mat,penalty="scad",n.fold=5)
opt=which.min(fit$rmse)
b.mat=cbind(b.mat,fit$ncpen.fit$beta[,opt])



#################



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
fit$deviance+2*length(b.vec)
aic=2*loss+ 2*length(b.vec)
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
  xb.vec[xb.vec>100]=100 #이렇게해도 바뀌는게 없나?
  exb.vec=exp(xb.vec)
  print(exb.vec)
  p.vec=exb.vec/(1+exb.vec)
  pred =as.numeric(p.vec>c.val);pred 
  loss=sum(-y.vec*xb.vec+log(1+exb.vec)) # 로그라이클리후드 
  info= 2*loss+wt*length(b.vec) # 인포메이션 
  acc=sum(y.vec==pred)     # 맞춘 비율
  err=sum(y.vec!=pred) # 잘못분류
  sen=sum(pred[y.vec==1]==1) # 1이라고 한애들중 실제로 1인애들
  spc=sum(pred[y.vec==0]==0) # 0 이라고 한애들중 실제로 0인애들
  print(p.vec)
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
cv.ass=colSums(ass)/10



##### measures from randomization

num=100 #몇번 분할할지 
r=5
ass=NULL
perf=rep(0,num)
set.seed(1234)
cpos=(1:n)[y.vec==1]  #y가 1 인 위치
npos=(1:n)[y.vec==0]  #y 가 0 인 위치
for(id in 1:num){
set=c( sample(cpos)[1:round(n/r)],sample(npos)[1:round(n/r)])   #1부터 20 ,sample(npos)[1:round(n/r)])   #1부터 20 
 r.fit=glm(case~.,data=xy.df[-set,],family="binomial")
 if(max(coef(r.fit))>100) perf[id]=1 # perfect fit 에 대한 indicator
ass=rbind(ass,ass.fun(b.vec=coef(r.fit),x.mat[set,],y.vec[set],c.val=0.5,wt=log(nrow(x.mat))))
}

boxplot(ass[perf==0,])
boxplot(ass[perf==1,])
boxplot(ass)

rbind(colMeans(ass[perf==0,]),colMeans(ass[perf==1,]),colMeans(ass))
#보통 perfect fit 인 경우는 빼도 된다
 # 전체중  몇 %가 perfect fit 인지 





##########패럴라이즈 추정법(튜닝 파라미터 어떻게 결정하는지 생각)


# cv나 vd는 ways of tuning

# assessment measure(측도) error,deviance, information, AUC


#밑에가 training sample
xy.df=read.csv("sensitivity_old_high.csv")
y.vec=as.vector(xy.df$sensitivity)
x.mat=as.matrix(cbind(dummify(xy.df[,2])[,-3],dummify(xy.df[,3])[,-2,drop=F] ,xy.df[,-c(1:3)]))
xy.df=data.frame(y.vec,x.mat)


#밑에가 test sample 
nxy.df=read.csv("sensitivity_new_high.csv")
ny.vec=as.vector(nxy.df$sensitivity) #drop F 하면 matrixy 유지
nx.mat=as.matrix(cbind(dummify(nxy.df[,2])[,-3],dummify(nxy.df[,3])[,-2,drop=F] ,nxy.df[,-c(1:3)]))
nxy.df=data.frame(ny.vec,nx.mat)


#여러가지 방법으로 튜닝을 하고 어떤 값이 이 데이터에 적합한지 결정 할것이다


# ridge, lasso, scad, bridge  해보도록 할것


# ridge 변수 선택 x 람다 control 해서 mse 컨트롤 가능

# lasso 변수 선택 o convex 함 biasd o 

# scad, bridge 변수 선택 o non convex  biased x 



## tunning 
# 1) 내맘
# 2) Cross 
# 3) validation 

# error= (y-y hat)^2 , |y-yhat| 



#### estimation without tuning 
# ready
m.vec=c("ridge","lasso","scad","bridge") #튜닝안한거
m.vec=c(m.vec,paste("cv-",m.vec,sep=""),paste("vd-",m.vec,sep="")) # 열두개중 최종 모형을 찾자
b.mat=matrix(NA,nrow=1+ncol(x.mat),ncol=length(m.vec))
colnames(b.mat)=m.vec

# ridge alpha=0 
fit=glmnet(x=x.mat,y=y.vec,family="gaussian",alpha=0)
b.mat[,"ridge"]=coef(fit)[,length(fit$lambda)]

#lasso 는 alpha=1 
fit=glmnet(x=x.mat,y=y.vec,family="gaussian",alpha=0)
b.mat[,"lasso"]=coef(fit)[,length(fit$lambda)]

# scad 
fit=ncpen(y.vec,x.mat,family="gaussian",penalty="scad")
coef(fit) #lamda 100개 나옴 
b.mat[,"scad"]=coef(fit)[,length(fit$lambda)]


# bridge 
fit=ncpen(y.vec,x.mat,family="gaussian",penalty="mbridge")
b.mat[,"bridge"]=coef(fit)[,length(fit$lambda)]


# lambda 가 크면 페널티 강함 작으면 페널티 작아짐 즉 튜닝 파라미터 0 이면 페널티 안주는거


#### estimation with cross validation 
set.seed(1234)

cv.id= cv.index.fun(y.vec,k.val=10) 

cv.fit=cv.glmnet(x.mat,y.vec,family="gaussian",alpha=0,foldid=cv.id)
names(cv.fit) #cvm은 어떤 measure 사용 했는지 
opt=which.min(cv.fit$cvm)#84 번 람다일때 error 가 젤 작어 
b.mat[,"cv-ridge"]=coef(cv.fit$glmnet.fit)[,opt] # 84번째 회귀 계수 

cv.fit=cv.glmnet(x.mat,y.vec,family="gaussian",alpha=1,foldid=cv.id)
opt=which.min(cv.fit$cvm)#28 번 람다일때 error 가 젤 작어 
b.mat[,"cv-lasso"]=coef(cv.fit$glmnet.fit)[,opt] # 28번째 회귀 계수 

cv.fit=cv.ncpen(y.vec,x.mat,family="gaussian",penalty="scad",fold.id=cv.id)
names(cv.fit)
opt=which.min(cv.fit$rmse)
b.mat[,"cv-scad"]=coef(cv.fit$ncpen.fit)[,opt]

cv.fit=cv.ncpen(y.vec,x.mat,family="gaussian",penalty="mbridge",fold.id=cv.id)
names(cv.fit)
opt=which.min(cv.fit$rmse)
b.mat[,"cv-bridge"]=coef(cv.fit$ncpen.fit)[,opt]  







#####estimation with validation 


# training set 을 2개로 나눠서 validation error 최소일때 lamda 값을 구하는거 
# test set은 최종 모형이 결정되기 전까지는 건드는거아님 

set.seed(1234)
vd.id=rand.index.fun(y.vec,tr.ratio=0.6,s.num=1) # 랜덤하게 index 만들어줌 

#ridge

fit=glmnet(x=x.mat,y=y.vec,family="gaussian",alpha=0)  #전체 데이터 먼저 fit 
fit$lambda #lamda 값중 어떤거 사용할까 
vd.fit=glmnet(x=x.mat[vd.id,],y=y.vec[vd.id],family="gaussian",alpha=0,lamda=fit$lambda) 
ass=glm.ass.fun(y.vec[!vd.id],x.mat[!vd.id,],b.mat=coef(vd.fit),mod="gaussian")$ass
opt=which.min(ass[,"abs.loss"])
b.mat[,"vd-ridge"]=coef(fit)[,opt]




#lasso

fit=glmnet(x=x.mat,y=y.vec,family="gaussian",alpha=1)  #전체 데이터 먼저 fit 
fit$lambda #lamda 값중 어떤거 사용할까 
vd.fit=glmnet(x=x.mat[vd.id,],y=y.vec[vd.id],family="gaussian",alpha=1,lamda=fit$lambda) 
ass=glm.ass.fun(y.vec[!vd.id],x.mat[!vd.id,],b.mat=coef(vd.fit),mod="gaussian")$ass
opt=which.min(ass[,"abs.loss"])
b.mat[,"vd-lasso"]=coef(fit)[,opt]



#scad

fit=ncpen(y.vec,x.mat,family="gaussian",penalty="scad")   #전체 데이터 먼저 fit 
vd.fit=ncpen(y.vec[vd.id],x.mat[vd.id,],family="gaussian",lambda=fit$lambda) 
b.id=vd.fit$lambda<=max(fit$lambda)
ass=glm.ass.fun(y.vec[!vd.id],x.mat[!vd.id,],b.mat=coef(vd.fit)[,b.id],mod="gaussian")$ass
opt=which.min(ass[,"abs.loss"])
b.mat[,"vd-scad"]=coef(fit)[,opt]


#bridge

fit=ncpen(y.vec,x.mat,family="gaussian",penalty="mbridge")   #전체 데이터 먼저 fit 
vd.fit=ncpen(y.vec[vd.id],x.mat[vd.id,],family="gaussian",lambda=fit$lambda) 
b.id=vd.fit$lambda<=max(fit$lambda)
ass=glm.ass.fun(y.vec[!vd.id],x.mat[!vd.id,],b.mat=coef(vd.fit)[,b.id],mod="gaussian")$ass
opt=which.min(ass[,"abs.loss"])
b.mat[,"vd-bridge"]=coef(fit)[,opt]





 # 모형을 lamda 값의 따라 여러개 만들어줌

#fit$lambda에 대응하는 계수를 coef(fit) 이 갖고 있다 



##########
nass=glm.ass.fun(ny.vec,nx.mat,b.mat=b.mat,mod="gaussian")$ass
sq.loss=nass[,"sq.loss"]/sum(nass[,"sq.loss"])
plot(sq.loss)
text(1:length(sq.loss),sq.loss+0.0001,substr(m.vec,1,4))
abs.loss=ass[,"abs.loss"]/sum(ass[,"abs.loss"])
points(abs.loss,col=2)

nass #12가지 방법론중 100개의 여러 측면에서 가장 좋은 값 선택가능





fit=glmnet(x.mat,y.vec,family="gaussian")
ass.lasso=glm.ass.fun(ny.vec,nx.mat,b.mat=coef(fit),mod="gaussian")$ass
plot(ass.lasso[,"sq.loss"],type="l")
min(ass.lasso[,"sq.loss"])

fit=ncpen(y.vec,x.mat,family="gaussian")
ass.scad=glm.ass.fun(ny.vec,nx.mat,b.mat=coef(fit),mod="gaussian")$ass
lines(ass.scad[,"sq.loss"],col=2)
min(ass.scad[,"sq.loss"])

#######로지스틱해보기 

rm(list=ls())

#training samplecv.id= cv.index.fun(y.vec,k.val=10) 

xy.df=read.csv("disease_old_high.csv") #high diimension 한 데이터
dim(xy.df); head(xy.df)[,1:10]
y.vec=dummify(xy.df[,1])[,1]
x.mat=as.matrix(xy.df[,-1])
cv.id= cv.index.fun(y.vec,k.val=10) 

#test sample
nxy.df=read.csv("disease_new_high.csv")
ny.vec=dummify(nxy.df[,1])[,1]
nx.mat=as.matrix(nxy.df[,-1])



# ready
m.vec=c("ridge","lasso","scad","bridge") 
m.vec=c(paste("err-",m.vec,sep=""),paste("dev-",m.vec,sep=""))
b.mat=matrix(NA,nrow=1+ncol(x.mat),ncol=length(m.vec))
colnames(b.mat)=m.vec


#ridge
cv.fit=cv.glmnet(x.mat,y.vec,family="binomial",fold.id=cv.id,alpha=0,type.measure="deviance")
plot(cv.fit) #lambda 에 따른 deviance 그래프 젤작은게 1243
opt=which.min(cv.fit$cvm)
b.mat[,"err-ridge"]=coef(cv.fit$glmnet.fit)[,opt]

cv.fit=cv.glmnet(x.mat,y.vec,family="binomial",fold.id=cv.id,alpha=0,type.measure="class")
plot(cv.fit) 
opt=which.min(cv.fit$cvm)
b.mat[,"dev-ridge"]=coef(cv.fit$glmnet.fit)[,opt]



#lasso
cv.fit=cv.glmnet(x.mat,y.vec,family="binomial",fold.id=cv.id,alpha=1,type.measure="deviance")
plot(cv.fit) 
opt=which.min(cv.fit$cvm)
b.mat[,"err-lasso"]=coef(cv.fit$glmnet.fit)[,opt]

cv.fit=cv.glmnet(x.mat,y.vec,family="binomial",fold.id=cv.id,alpha=1,type.measure="class")
plot(cv.fit) 
opt=which.min(cv.fit$cvm)
b.mat[,"dev-lasso"]=coef(cv.fit$glmnet.fit)[,opt]

#scad
cv.fit=cv.ncpen(y.vec,x.mat,family="binomial",fold.id=cv.id,penalty="scad")
plot(cv.fit$rmse) 
opt=which.min(cv.fit$rmse)
b.mat[,"err-scad"]=coef(cv.fit$ncpen.fit)[,opt]
opt=which.min(cv.fit$like)
b.mat[,"dev-scad"]=coef(cv.fit$ncpen.fit)[,opt]

#bridge


cv.fit=cv.ncpen(y.vec,x.mat,family="binomial",fold.id=cv.id,penalty="mbridge")
plot(cv.fit$rmse) 
opt=which.min(cv.fit$rmse)
b.mat[,"err-bridge"]=coef(cv.fit$ncpen.fit)[,opt]
opt=which.min(cv.fit$like)
b.mat[,"dev-bridge"]=coef(cv.fit$ncpen.fit)[,opt]



###finial assessment based on new sample
ass=glm.ass.fun(ny.vec,nx.mat,b.mat,mod="binomial")$ass

fit=glmnet(x.mat,y.vec,family="binomial")
ass.lasso=glm.ass.fun(ny.vec,nx.mat,coef(fit),mod="binomial")$ass
fit=ncpen(y.vec,x.mat,family="binomial",penalty = "scad",tau=10)
ass.scad=glm.ass.fun(ny.vec,nx.mat,coef(fit),mod="binomial")$ass
c(min(ass.lasso[,"err"]),min(ass.scad[,"err"]))

plot(ass.lasso[,"err"],type="l",);abline(h=ass[,"err"]["err-lasso"])
lines(ass.scad[,"err"],type="l",col=2);abline(h=ass[,"err"]["err-scad"],col=2)

ass.lasso




