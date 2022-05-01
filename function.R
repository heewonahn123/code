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









