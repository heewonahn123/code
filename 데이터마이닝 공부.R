###함수 모음
forward.fun = function(y.vec,x.mat,glm.fam=c("gaussian","binomial","poisson"),
                       inn.cri=c("deviance","predict"),out.cri=c("inform","predict"),
                       var.max=min(dim(x.mat)),out.stop=TRUE,inf.wt=2,trace=TRUE){ 
  # ready 
  inn.cri = match.arg(inn.cri); out.cri = match.arg(out.cri); glm.fam = match.arg(glm.fam);
  n = nrow(x.mat); p = ncol(x.mat); xy.df = data.frame(y.vec,x.mat);  
  var.max = ifelse(out.stop==TRUE,min(n,p),min(var.max,n,p)); mod.num = var.max+1
  coef.mat = matrix(0,p+1,var.max+1); cri.vec = rep(0,var.max+1)
  # null model 
  fit = glm(y.vec~1,data=xy.df,family=glm.fam); 
  if(out.cri=="inform") cri.val = fit$deviance+inf.wt  
  cur.set = NULL; new.set = 1:p; coef.mat[c(1,cur.set+1),1] = coef(fit); cri.vec[1] = cri.val 
  # forward process 
  for(var.id in 1:var.max){
    inn.cri.vec = rep(0,length(new.set))
    for(new.id in 1:length(new.set)){
      fit = glm(y.vec~.,data=xy.df[,c(1,c(cur.set,new.set[new.id])+1)],family=glm.fam) 
      if(inn.cri=="deviance") inn.cri.vec[new.id] = fit$deviance 
    }
    fit = glm(y.vec~.,data=xy.df[,c(1,c(cur.set,new.set[which.min(inn.cri.vec)])+1)],family=glm.fam)
    if(out.cri=="inform") cri.val = fit$deviance+inf.wt*(2+length(cur.set))
    if(out.stop==TRUE){ if(cri.val>cri.vec[var.id]){ var.id = var.id-1; break } }
    cur.set = c(cur.set,new.set[which.min(inn.cri.vec)]); new.set = (1:p)[-cur.set] 
    cri.vec[var.id+1] = cri.val
    coef.mat[c(1,cur.set+1),var.id+1] = coef(fit)
    if(trace) cat("included variables:",cur.set,"\n")
  }
  # summary 
  ret = list(coef.mat=coef.mat[,1:(var.id+1)],cri.vec=cri.vec[1:(var.id+1)])
  return(ret)
}


cv.index.fun=function(y.vec,k.val=10){
  n=length(y.vec); m=k.val*trunc(n/k.val)
  o.vec= order(y.vec,decreasing=T); a.vec=o.vec[1:m]; r.vec=o.vec[-(1:m)]
  o.mat=matrix(a.vec,nrow=k.val); s.mat= apply(o.mat,2,FUN=sample)
  o.vec[s.mat]=row(s.mat);o.vec[r.vec]=sample(1:k.val,length(r.vec))
  return(id.vec=o.vec)
}


rand.index.fun=function(y.vec,tr.ratio=0.7,s.num=100){
  id.mat=matrix(F,length(y.vec),s.num)
  for(s.id in 1:s.num){ id.mat[,s.id]=cv.index.fun(y.vec,k.val=10)<=tr.ratio*10}
  return(id.mat)
}


glm.ass.fun=function(y.vec,x.mat,b.mat,mod=c("gaussian","binomial"),c.val=0.5,wt=2){
  if(is.vector(b.mat)) b.mat=matrix(b.mat,ncol=1)
  xb.mat=cbind(1,x.mat)%*%b.mat;
  if(mod=="gaussian"){
    pred=xb.mat
    sq.loss=colSums((y.vec-xb.mat)^2)
    abs.loss=colSums(abs(y.vec-xb.mat))
    info=log(sq.loss)+wt*colSums(b.mat!=0)
    ass=cbind(length(y.vec),sq.loss,abs.loss,info,colSums(b.mat!=0))
    colnames(ass)=c("n","sq.loss","abs.loss","info","df")
  }  else if(mod=="binomial"){
    exb.mat=exp(xb.mat); exb.mat[exb.mat>1e+10]=1e+10; p.mat=exb.mat/(1+exb.mat)
    pred=1*(p.mat>c.val);loss=colSums(-y.vec*xb.mat+log(1+exb.mat));info=2*loss+wt*colSums(b.mat!=0)
    acc=colSums(y.vec==pred);err=colSums(y.vec!=pred);sen=colSums(pred[y.vec==1,,drop=F]==1);spc=colSums(pred[y.vec==0,,drop=F]==0);
    roc.obj=apply(p.mat,2,FUN=roc.curve,response=y.vec,n.thresholds=5*nrow(x.mat),plotit=F)
    auc=sapply(roc.obj,function(x)x[[2]])
    ass=cbind(c.val,sum(y.vec==1),sum(y.vec==0),loss,info,acc,err,sen,spc,auc,colSums(b.mat!=0))
    colnames(ass)=c("c.val","n1","n0","loss","info","acc","err","sen","spc","auc","df")
  }
  return(list(pred=pred,ass=ass))  
}

#################


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
ass


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

par(mfrow=c(1,3))
boxplot(ass[perf==0,])
boxplot(ass[perf==1,])
boxplot(ass)

rbind(colMeans(ass[perf==0,]),colMeans(ass[perf==1,]),colMeans(ass))
#보통 perfect fit 인 경우는 빼도 된다
 # 전체중  몇 %가 perfect fit 인지 





##########패럴라이즈 추정법(튜닝 파라미터 어떻게 결정하는지 생각)


# cv나 vd는 ways of tuning

# assessment measure(측도) error,deviance, information, AUC
xy.df=read.csv("sensitivity_old_high.csv")
y.vec=as.vector(xy.df$sensitivity)
x.mat=as.matrix(cbind(dummify(xy.df[,2])[,-3],dummify(xy.df[,3])[,-2,drop=F] ,xy.df[,-c(1:3)]))
xy.df=data.frame(y.vec,x.mat)
x.mat=x.mat[,1:10]


nxy.df=read.csv("sensitivity_new_high.csv")
ny.vec=as.vector(nxy.df$sensitivity) #drop F 하면 matrixy 유지
nx.mat=as.matrix(cbind(dummify(nxy.df[,2])[,-3],dummify(nxy.df[,3])[,-2,drop=F] ,nxy.df[,-c(1:3)]))
nxy.df=data.frame(ny.vec,nx.mat)
nx.mat=nx.mat[,1:10]

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


install.packages("glmnet")
library(glmnet)

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
install.packages("ncvreg")
library(ncpen)
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
ass=glm.ass.fun(y.vec[!vd.id],x.mat[!vd.id,],b.mat=coef(vd.fit),mod="gaussian")$ass
opt=which.min(ass[,"abs.loss"])
b.mat[,"vd-scad"]=coef(fit)[,opt]










 # 모형을 lamda 값의 따라 여러개 만들어줌
dim(coef(fit))
# 변수 11개 모형 64개

#fit$lambda에 대응하는 계수를 coef(fit) 이 갖고 있다 

