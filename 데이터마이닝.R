
rm(list=ls())
setwd("C:/Temp/code")
library(pROC)
library(xgboost)
library(Epi)
library(tidyverse)
library(e1071)
source("C:/Temp/code1/function.R")


train=cleancode("C:/Temp/code","train.csv","age_services","sd")[,-c(1,10)]
head(train)

accuracy=rep(NA,3)
gamma=rep(NA,3)
cost=rep(NA,3)


for (i in 1:3) {
  n=nrow(train)
  set=sample(1:n,size=round(n/3),replace=T)  # 2/3를 train  사용 1/3를 test 
  cv.fit=tune.svm(Transported~.,data=train[-set,],gamma=2^c(-4,-1,0,2),cost=2^c(-2,0,2,4),type="C")    
  gamma[i]=cv.fit$best.parameter[1]
  cost[i]=cv.fit$best.parameter[2]
  fit=svm(Transported~.,data=train[-set,],type="C",gamma=gamma[i],cost=cost[i])
  tab=table(train[set,][,12],predict(fit,newdata=train[set,]))
  accuracy[i]=1-sum(diag(tab))/sum(tab) 
}
