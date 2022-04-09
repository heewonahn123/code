setwd("C:/temp/code")



library(tidyverse)
train=read.csv("train.csv");head(train)
test=read.csv("test.csv");head(test)
test$Transported=NA
all=rbind(train,test);head(all)
attach(all)



# 데이터 설명. 


#PassengerId -  앞에 그룹 번호 , 뒤에 그룹내 번호
#HomePlanet -  출발한 행성이름
#CryoSleep - 탑승자가 투표로 인해 갑판 안에만 머무룰수 있게 강제되었는지(감옥)
#Cabin - 탑승자가 머무르는 선실이고 , 갑판, 번호 , 사이드로 나뉘는데 사이드는 포트와 스타보드로 나뉨
#Destination - 도착 행성
#Age - 나이
#VIP - VIP 인지 아닌지
#RoomService, FoodCourt, ShoppingMall, Spa, VRDeck - 편의시설 얼마나 이용한지
#Name - 이름
#Transported - 다른차원으로 이동했는지  (이동하면 안좋은거)




# 빈거 NA 로 바꾸자
colSums(is.na(all))
all[all==''|all=='']=NA
# 그리고 편의 시설 합쳐서 service 라고 놓자 
all=all %>%  mutate(service = (RoomService+FoodCourt+ShoppingMall+Spa+VRDeck) ) 
names(all)





# 합쳐져 있는 탑승객 그룹과 그룹넘버 분할후 변수 만듬
split=all$PassengerId %>% str_split("_",simplify = T)
all$groupID=as.integer(split[,1])
all$groupNUM=as.integer(split[,2])

# 합쳐져 있는 선실 선반, 숫자, 위치 분할
split2=all$Cabin %>% str_split("/",simplify = T)
all$CabinDeck=as.factor(split2[,1])
all$CabinNum=as.integer(split2[,2])

split2[,3][split2[,3]=="P"]="Port"
split2[,3][split2[,3]=="S"]="Starboard"
all$CabinSide=as.factor(split2[,3]) 

# 분할후 기존 데이터 삭제

head(all)

all=all[,-c(1,4,8,9,10,11,12)]


head(all)
# 종속변수 factor로 바꿈
all$Transported[all$Transported=="True"]=1
all$Transported[all$Transported=="False"]=0
all$Transported=as.factor(all$Transported)
# VIP factor 로 바꿈 
all$VIP=as.factor(all$VIP)

# HomePlanet factor 로 바꿈
all$HomePlanet=all$HomePlanet %>% as.factor()

# CryoSleep factor 로 바꿈 
all$CryoSleep=all$CryoSleep %>% as.factor()

# Homeplanet, DEstination  factor  로 바꿈
all$Destination=all$Destination %>% as.factor()
all$HomePlanet=all$HomePlanet %>% as.factor()





# NA인 자료 어떻게 처리할건지 
#이름 일단 no name 으로
all$Name[is.na(all$Name)]="no name"





#내 과제 : 
# 출발-목적지를 그룹으로 묶어서 비교, 각 그룹마다 transport가 다를거같음 ( 그래프 보고 활용방안 생각)



# 총 출발지 3개 : 55 cancri, PSO , TRAPPIST 
# 총 목적지 3개 :Earth, Mars, Europa

#어떤 관계가 있을까?  경우의 수 총 9개 따라서 NA를 제외하고 1부터 9가지 수를 할당해서 그룹을 만듬

head(all)








# 출발지 도착지에 따라 9가지의 group을 만들지
all$Group=ifelse(Destination=="55 Cancri e" & HomePlanet=="Earth","Ear 55",  # Earth 에서 55 Cacancri
            ifelse(Destination=="55 Cancri e" & HomePlanet=="Mars","Mar 55",  # Mars 에서 55 cancri 
            ifelse(Destination=="55 Cancri e" & HomePlanet=="Europa","Euro 55",  # Europa 에서 55 cancri
            ifelse(Destination=="PSO J318.5-22" & HomePlanet=="Earth","Ear pso", # Earth 에서 Pso
            ifelse(Destination=="PSO J318.5-22" & HomePlanet=="Mars","Mar pso",    # Mars 에서 Pso
            ifelse(Destination=="PSO J318.5-22" & HomePlanet=="Europa","Euro pso",    # Europa 에서 Pso
            ifelse(Destination=="TRAPPIST-1e" & HomePlanet=="Earth","Ear TRA",     # Earth 에서 TRA
            ifelse(Destination=="TRAPPIST-1e" & HomePlanet=="Mars","Mar TRA",      # Mars 에서 TRA
            ifelse(Destination=="TRAPPIST-1e" & HomePlanet=="Europa","Euro TRA","NA")))))))))  #Europa 에서 TRA
all$Group=as.factor(all$Group)
attach(all)
names(all)
par(mfrow=c(1,1))




#일단 그룹별 빈도수 확인 Earth 에서 TRA 가는 사람들  제일 많음 
ggplot(na.omit(all),aes(Group)) + geom_bar() 



# 그룹별로 탈출률이 다를까?

ggplot(na.omit(all),aes(Group,fill=Transported)) + 
  geom_bar(position="fill") +
  labs(title="그룹별 탈출")+theme(plot.title = element_text(hjust = 0.5))# Mars 에서 출발한애들, 그다음 Euro에서 출발한애들, Earth 에서 출발한 애들 순으로 많이 없어짐. 이유확인 필요?? 





# 그룹과 groupID 관계 

plot(Group,groupID) # 별 관계없음 euro pso 는 표본의 수가 매우 적어 그래프가 이렇게 낭모


# 그룹과 groupNum 관계
table(Group, groupNUM) # 별 관계없음 모든 그룹다 그룹 넘버가 클수록 사람이 많아 


# 그룹과 VIP 관계
table(all$Group,VIP) #VIP 결측치 중에서 Euro에서  pso  가는사람들, Mar 에서 Pso 가는 사람들 만 제거하고 False 로 놔도 될거같은데?
ggplot(na.omit(all),aes(Group,fill=VIP))+geom_bar(position="fill")+labs(title="그룹별 VIP")+theme(plot.title = element_text(hjust = 0.5))  # Euro 에서 온사람 들이 VIP 많음 Ear 출신과 Mar에서 55 가는 사람들은 VIP 없다고 해도 되지 않을까? 


# 그룹과 cryo sleep 관계
table(Group,all$CryoSleep) # 상관관계없어

# 그룹과 cabinDEck 관계 
table(Group,CabinDeck) # 그룹 별로 나눠짐 earth 출신은 EFG , Euro 출신은 ABCDE ,Mars 출신은 DEF


#그룹과 과 cabinside 관계
table(Group,CabinSide) # 상관관계없어
ggplot(na.omit(all),aes(Group,fill=CabinSide))+geom_bar(position="fill") #상관없음


# 그룹과 age 관계
ggplot(data=na.omit(all), aes(x=Group, y=Age)) + geom_point(shape=15, size=3, colour="blue")  #관계 없음 Euro pso 와 Mar pso 표본이 매우 적어 고른 분포를 보이지 않음


  
  
















# 데이터 분포 확인
attach(all)
allhist=function(x){
par(mfrow=c(3,3))
hist(Age,main = '나이분포')
hist(service, main= '서비스 분포')
hist(as.numeric(Transported), main= '이동 성공 분포')
hist(groupNUM, main= '그룹 번호 분포')
hist((CabinNum), main= '선반 번호 분포')
hist((as.numeric(Group)),main="목적지 출발지별 분포")
}


allhist(1)


# Transported 랑 다른 X 변수 확인해보자


#1

ggplot(na.omit(all),aes(Group,fill=Transported)) + 
  geom_bar(position="fill") +
  labs(title="그룹별 탈출")+theme(plot.title = element_text(hjust = 0.5))# Mars 에서 출발한애들, 그다음 Euro에서 출발한애들, Earth 에서 출발한 애들 순으로 많이 없어짐. 이유확인 필요?? 





#2
table(CryoSleep,Transported)
ggplot(na.omit(all),aes(x=CryoSleep,fill=Transported)) + geom_bar(position='fill')

#3
table(Destination,Transported)
ggplot(na.omit(all),aes(x=Destination,fill=Transported)) + geom_bar(position='fill')

#4                   # 20대 초반이 제일 많이 살아남음 젤 많잖아 
ggplot(na.omit(all),aes(x=Age,fill=Transported)) + geom_histogram(position='fill')
  
#5
table(VIP,Transported)
ggplot(na.omit(all),aes(x=VIP,fill=Transported)) + geom_bar(position="fill")

#6

ggplot(na.omit(all), aes(x=service, fill=Transported)) +  geom_histogram(position='fill')  # 한번도 안쓴사람이 5324 명
#11 
ggplot(na.omit(all), aes(x=groupID, fill=Transported)) +  geom_histogram(position='fill') 
#12
ggplot(na.omit(all),aes(x=groupNUM,fill=Transported)) + geom_bar(position='fill')
#13
ggplot(na.omit(all),aes(x=CryoSleep,fill=Transported)) + geom_bar(position='fill')
#14
ggplot(na.omit(all),aes(x=CabinNum, fill=Transported))+geom_histogram(position='fill')  # CabinNum이 작을수록 잘 살아남음
#15
ggplot(na.omit(all),aes(x=CabinSide,fill=Transported)) + geom_bar(position='fill')

