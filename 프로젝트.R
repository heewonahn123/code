setwd("C:/Temp/code")


library(tidyverse)
train=read.csv("train.csv")
#PassengerId -  앞에 그룹, 뒤에 그룹에서 번호
#HomePlanet -  출발한 행성
#CryoSleep - 탑승자가 투표로 인해 갑판 안에만 머무룰수 있게 강제되었는지
#Cabin - 탑승자가 머무르는 선실, 갑판, 번호 , 사이드로 나뉘는데 사이드는 포트와 스타보드로 나뉨
#Destination - 도착치
#Age - 나이
#VIP - VIP 인지 아닌지
#RoomService, FoodCourt, ShoppingMall, Spa, VRDeck - 편의시설 얼마나 이용한지
#Name - 이름
#Transported - 다른차원으로 이동성공했는지


# 합쳐져 있는 탑승객 그룹과 그룹넘버 분할후 변수 만듬
split=train$PassengerId %>% str_split("_",simplify = T)
train$groupID=as.integer(split[,1])
train$groupNUM=as.integer(split[,2])

# 합쳐져 있는 선실 선반, 숫자, 위치 분할
split2=train$Cabin %>% str_split("/",simplify = T)
train$CabinDeck=as.factor(split2[,1])
train$CabinNum=as.integer(split2[,2])

split2[,3][split2[,3]=="P"]="Port"
split2[,3][split2[,3]=="S"]="Starboard"
train$CabinSide=as.factor(split2[,3]) 

# 분할후 기존 데이터 삭제
train=train[,-c(1,4)]


# 종속변수 숫자로 바꿈
train$Transported[train$Transported=="True"]=1
train$Transported[train$Transported=="False"]=0
train$Transported=as.integer(train$Transported)

# VIP factor 로 바꿈 
train$VIP=as.factor(train$VIP)

# HomePlanet factor 로 바꿈
train$HomePlanet=train$HomePlanet %>% as.factor()

# CryoSleep factor 로 바꿈 
train$CryoSleep=train$CryoSleep %>% as.factor()

# Homeplanet facotr  로 바꿈
train$HomePlanet=train$Destination %>% as.factor()


# 최종 데이터이지만 NA 와 NULL 둘다 있음 
train

