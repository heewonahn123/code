setwd("C:/temp/code")
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
str(train)


# 데이터 분포 확인
attach(train)
par(mfrow=c(1,1))
names(train)

#1 
table(train$HomePlanet, train$Transported) # 55 cancri e 출신이 비교적 잘 살아남음
#2 
table(train$CryoSleep, train$Transported) # 강제로 머무른애들은 탈출더 잘함 
#3
table(train$Destination, train$Transported) # 55 cancri 목적지인 애들이 많이 살아남음
#4                   # 20대 초반이 제일 많이 살아남음
ggplot(data=train,aes(x=Age, fill=Transported))+geom_histogram(binwidth=2)
#5
table(VIP,Transported)       # 오히려 VIP 인 사람들이 더 탈출 많이 못함 

#6
table(RoomService)[1]
ggplot(data=train, aes(x=RoomService, fill=Transported)) +  geom_histogram()  #한번도 안쓴애 5577
#7
table(FoodCourt)[1]
ggplot(data=train, aes(x=FoodCourt, fill=Transported)) +  geom_histogram()  # 한번도 안쓴사람이 5456 명
#8
table(ShoppingMall)[1]
ggplot(data=train, aes(x=ShoppingMall, fill=Transported)) +  geom_histogram()  # 한번도 안쓴사람이 5587명 
#9
ggplot(data=train, aes(x=Spa, fill=Transported)) +  geom_histogram()  # 한번도 안쓴사람이 5324 명
table(Spa)[1]
#10
table(VRDeck)[1] # 한번도 안쓴사람이 5495 명임 
ggplot(data=train, aes(x=VRDeck, fill=Transported)) +  geom_histogram()  # 대부분 이용안해서 이렇게 나온듯
#11 
ggplot(data=train, aes(x=groupID, fill=Transported)) +  geom_histogram() 
#12
table(groupNUM,Transported)  
#13
table(CabinDeck,Transported)
#14
ggplot(data=train,aes(x=CabinNum, fill=Transported))+geom_histogram(binwidth=5,color="red")  # CabinNum이 작을수록 잘 살아남음
#15
table(CabinSide,Transported) # starboard 사는 사람들이더 잘 살아남음 

