setwd("C:/temp/code")
library(tidyverse)
train=read.csv("train.csv")

# 빈거 NA 로 바꾸자
colSums(is.na(train))
train[train=='' | train==''] <- NA




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
train$Transported=as.factor(train$Transported)

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
allhist=function(x){
par(mfrow=c(3,3))
hist(Age,main = '나이분포')
hist(RoomService, main= '룸서비스 분포')
hist(FoodCourt,main = '푸드코드 이용 분포')
hist(ShoppingMall, main= '쇼핑몰 이용 분포')
hist(Spa,main = '스파 이용 분포')
hist(VRDeck, main= 'VRDeck 이용 분포')
hist(as.numeric(Transported), main= '이동 성공 분포')
hist(groupNUM, main= '그룹 번호 분포')
hist((CabinNum), main= '선반 번호 분포')
}
allhist(1)


# Transported 랑 다른 X 변수 확인해보자

#1 
table(HomePlanet,Transported)
ggplot(na.omit(train),aes(x=HomePlanet,fill=Transported)) + geom_bar(position='fill')

#2 
table(CryoSleep,Transported)
ggplot(na.omit(train),aes(x=CryoSleep,fill=Transported)) + geom_bar(position='fill')

#3
table(Destination,Transported)
ggplot(na.omit(train),aes(x=Destination,fill=Transported)) + geom_bar(position='fill')

#4                   # 20대 초반이 제일 많이 살아남음 젤 많잖아 
ggplot(na.omit(train),aes(x=Age,fill=Transported)) + geom_histogram(position='fill')
  
#5
table(VIP,Transported)
ggplot(na.omit(train),aes(x=VIP,fill=Transported)) + geom_bar(position="fill")

#6

ggplot(na.omit(train), aes(x=RoomService, fill=Transported)) +  geom_histogram(position = "fill")  #한번도 안쓴애 5577
#7
ggplot(na.omit(train), aes(x=FoodCourt, fill=Transported)) +  geom_histogram(position='fill')  # 한번도 안쓴사람이 5456 명
#8
ggplot(na.omit(train), aes(x=ShoppingMall, fill=Transported)) +  geom_histogram(position='fill')  # 한번도 안쓴사람이 5587명 
#9
ggplot(na.omit(train), aes(x=Spa, fill=Transported)) +  geom_histogram(position='fill')  # 한번도 안쓴사람이 5324 명
#10
ggplot(na.omit(train), aes(x=VRDeck, fill=Transported)) +  geom_histogram(position='fill')  # 대부분 이용안해서 이렇게 나온듯
#11 
ggplot(na.omit(train), aes(x=groupID, fill=Transported)) +  geom_histogram(position='fill') 
#12
ggplot(na.omit(train),aes(x=groupNUM,fill=Transported)) + geom_bar(position='fill')
#13
ggplot(na.omit(train),aes(x=CryoSleep,fill=Transported)) + geom_bar(position='fill')
#14
ggplot(na.omit(train),aes(x=CabinNum, fill=Transported))+geom_histogram(position='fill')  # CabinNum이 작을수록 잘 살아남음
#15
ggplot(na.omit(train),aes(x=CabinSide,fill=Transported)) + geom_bar(position='fill')

