## 데이터 불러오기

setwd("c:/dm")
ilpd<-read.csv("Indian Liver Patient Dataset (ILPD).csv",header=F)
colnames(ilpd)<-c("age","gender","tb","db","alkphos","sgpt","sgot","tp","alb","ag","class")
str(ilpd)

ilpd$class<-as.factor(ilpd$class)
str(ilpd)

##########################################################################################

## 데이터 탐색

# 변수 별 분포
hist(ilpd$age) # 나이
barplot(table(ilpd$gender)) # 성별
hist(ilpd$tb,xlim=c(0,40),breaks=300) # 총 빌리루빈
hist(ilpd$db,xlim=c(0,20),breaks=200) # 직접 빌리루빈
hist(ilpd$alkphos,xlim=c(0,1200),breaks=400) # ALP
hist(ilpd$sgpt,xlim=c(0,700),breaks=400) # ALT
hist(ilpd$sgot,xlim=c(0,1000),breaks=400) # AST
hist(ilpd$tp) # 혈청 총 단백
hist(ilpd$alb) # 알부민
hist(ilpd$ag) # 알부민/글로루빈 비율
barplot(table(ilpd$class)) # Class

# 만약 파생변수를 만든다면

hist(ilpd$tb-ilpd$db)
# 간접 빌리루빈 : 총 빌리루빈 - 직접 빌리루빈
# 일부 0 이하의 값이 존재하는데 이는 곧 직접 빌리루빈>총 빌리루빈이므로 문제 있는 데이터라 생각됨

ilpd$ib<-ilpd$tb-ilpd$db

ilpd<-ilpd[ilpd$ib>0,]

q<-ilpd[is.na(ilpd$ag),]
ilpd[is.na(ilpd$ag),]$ag<-ilpd[is.na(ilpd$ag),]$tp-ilpd[is.na(ilpd$ag),]$alb
ilpd<-ilpd[,-c(3,9)]

head(ilpd)

hist(ilpd$tp-ilpd$alb) 
# 글로불린 추정(1) : 혈청 총 단백 - 알부민 #정확한 값이 아니지 않나..?
hist(1/(ilpd$ag/ilpd$alb)) 
# 글로불린 추정(2) : 1 / (알부민/글로루빈 비율 / 알부민)

ilpd$tp-ilpd$alb==1/(ilpd$ag/ilpd$alb)

# 참고1 : 혈청 단백은 주로 알부민과 글로불린으로 구성되어 있습니다.
# 참고2 : 알부민과 글로부린의 비율은 건강한 사람에서 알부민이 약 67%, 글로불린은 약 33% 정도입니다. 


# 결측값 확인
sum(is.na(ilpd$ag))
# 결측값 4개 존재, 알부민이 엄청 크거나 글로루빈이 0이거나 정말 결측치거나

# 변수별 분포 2

library(ggplot2)
ilpd$class<-factor(ilpd$class,labels=c('Yes','No'))

ggplot(data=ilpd,aes(x=age,colour=class,fill=class))+geom_histogram(position="identity",alpha=0.5)+
  labs(x="Age",y="Frequency",fill="Class",colour="Class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))

ggplot(data=ilpd,aes(x=gender,colour=class,fill=class))+geom_bar(position="dodge",alpha=0.5)+
  labs(x="Gender",y="Frequency",fill="Class",colour="Class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))

ggplot(data=ilpd,aes(x=ib,colour=class,fill=class))+geom_histogram(position="dodge",alpha=0.5)+
  labs(x="ib",y="Frequency",fill="Class",colour="Class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))


ggplot(data=ilpd,aes(x=tb,colour=class,fill=class))+geom_histogram(position="identity",alpha=0.5)+
  labs(x="Total Bilirubin",y="Frequency",fill="Class",colour="Class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))

ggplot(data=ilpd,aes(x=db,fill=class))+geom_histogram(position="identity",alpha=0.5)+
  labs(x="Direct Bilirubin",y="Frequency",fill="Class",colour="Class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))

ggplot(data=ilpd,aes(x=alkphos,colour=class,fill=class))+geom_histogram(position="identity",alpha=0.5)+
  labs(x="ALP",y="Frequency",fill="Class",colour="Class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))

ggplot(data=ilpd,aes(x=sgpt,colour=class,fill=class))+geom_histogram(position="identity",alpha=0.5)+
  labs(x="ALT",y="Frequency",fill="Class",colour="Class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))+
  xlim(0,100)

ggplot(data=ilpd,aes(x=sgot,colour=class,fill=class))+geom_histogram(position="identity",alpha=0.5)+
  labs(x="AST",y="Frequency",fill="Class",colour="Class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))

ggplot(data=ilpd,aes(x=tp,colour=class,fill=class))+geom_histogram(position="identity",alpha=0.5)+
  labs(x="Total Protien",y="Frequency",fill="class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))

ggplot(data=ilpd,aes(x=alb,colour=class,fill=class))+geom_histogram(position="identity",alpha=0.5)+
  labs(x="Albumin",y="Frequency",fill="class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))

ggplot(data=ilpd,aes(x=ag,colour=class,fill=class))+geom_histogram(position="identity",alpha=0.5)+
  labs(x="Albumin/Globulin Ratio",y="Frequency",fill="class")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))

ilpd$class<-factor(ilpd$class,labels=c(1,0)) # KNN
ilpd$gender<-as.factor(ifelse(ilpd$gender=="Male",0,1)) # KNN


# 상관계수 확인
ilpd<-na.omit(ilpd) # 일단 코드를 돌려보기 위해 결측 제거함
plot(ilpd[,-c(2,11)])
cor(ilpd[,-c(2,11)]) # 일부 상관계수가 높은 변수들이 존재
q<-ilpd[is.na(ilpd$ag),]
ilpd[is.na(ilpd$ag),]$ag<-ilpd[is.na(ilpd$ag),]$tp-ilpd[is.na(ilpd$ag),]$alb
ilpd<-ilpd[,-c(3,9)]

cor(ilpd[,3],ilpd[,4])
# 총 빌리루빈과 직접 빌리루빈의 상관계수가 높음

cor(ilpd[,3]-ilpd[,4],ilpd[,3]) # 0.9400199
cor(ilpd[,3]-ilpd[,4],ilpd[,4]) # 0.6565661
# 총 빌리루빈과 간접 빌리루빈의 상관계수는 상당히 높음.
# 직접 빌리루빈과 간접 빌리루빈의 상관계수는 상대적으로 낮음.
# 분석을 해 봐야 알겠지만 총 빌리루빈을 없애고 직접 빌리루빈과 간접 빌리루빈을 사용하는 게 더 나을수도 있음
# 만약 간접 빌리루빈을 넣어도 결과가 안 좋다면 총 빌리루빈이나 직접 빌리루빈만 남겨서 분석도 시도

cor(ilpd[,8],ilpd[,9]) # 0.7831122 tp alb
cor(ilpd[,9],ilpd[,10]) # 0.6896323 alb ag
cor(ilpd[,8],ilpd[,10]) # 0.2348872 tp ag
# 알부민과 혈청 총 단백, A/G 비율과의 상관계수가 높음
# 혈청 총 단백과 A/G 비율의 상관계수는 낮음

cor(ilpd[,8]-ilpd[,9],ilpd[,8])
cor(ilpd[,8]-ilpd[,9],ilpd[,9])
cor(ilpd[,8]-ilpd[,9],ilpd[,10])

cor(1/(ilpd[,10]/ilpd[,9]),ilpd[,8])
cor(1/(ilpd[,10]/ilpd[,9]),ilpd[,9])
cor(1/(ilpd[,10]/ilpd[,9]),ilpd[,10])
# 글로불린과 혈청 총 단백,글로불린과 A/G 비율과의 상관계수는 어느 정도 존재함
# 글로불린과 알부민의 상관관계는 거의 없음
# 혈청 총 단백과 A/G 비율만 살리거나, 알부민과 글로불린만 살리거나

##########################################################################################

## 데이터 분할
ilpd$gender<-ifelse(ilpd$gender=='Male',0,1)
ilpd$gender<-as.factor(ilpd$gender)
str(ilpd)
set.seed(3) # 시드는 임의(대학원 학번)로 지정하였기에 나중에 바꿔야 함
size<-floor(0.7*nrow(ilpd)) # train : 70%, test : 30%
train=sample(1:nrow(ilpd),size)
ilpd.tr<-ilpd[train,] # train set
ilpd.te<-ilpd[-train,] # test set

##########################################################################################

## 데이터 분석

# 1. KNN

library(class)
err.tr=rep(0,174)
err.te=rep(0,174)
# 405 : train set의 개수

for (i in 1:174){
  set.seed(3)
  pred.tr=knn(ilpd.tr,ilpd.tr,ilpd.tr$class,i)
  pred.te=knn(ilpd.tr,ilpd.te,ilpd.tr$class,i) 
  err.tr[i]=mean(ilpd.tr$class!=pred.tr)
  err.te[i]=mean(ilpd.te$class!=pred.te)
}

knn.tr<-data.frame(k=1:174,err=err.tr[1:174],type=rep("Train",174))
knn.te<-data.frame(k=1:174,err=err.te[1:174],type=rep("Test",174))
ilpd.knn<-rbind(knn.tr,knn.te) 

ggplot(ilpd.knn,aes(x=k,y=err,colour=type))+geom_line()+geom_point()+
  labs(x="K",y="Error Rate",colour="Error Rate")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))
which.min(err.te)

set.seed(3)
yhat.knn<-knn(ilpd.tr,ilpd.te,ilpd.tr$class,127)

library(caret)
confusionMatrix(yhat.knn,ilpd.te$class)


## 2. Tree

library(tree)
fit.tree<-tree(class~.,data=ilpd.tr,method="misclass") # 분류 문제이므로 misclass
# maximum depth reached
plot(fit.tree);text(fit.tree)

set.seed(3)
plot(cv.tree(fit.tree,FUN=prune.misclass))
# 발표할 때 교차검증 설명 필요할 듯 #교차검증??

fit.tree2<-prune.misclass(fit.tree,best=4,newdata=ilpd.te) #왜 best=13 ??
plot(fit.tree2); text(fit.tree2)
yhat.tree<-predict(fit.tree2,ilpd.te,type="class")

confusionMatrix(yhat.tree,ilpd.te$class)


## 3. Neural Net


## 신경망의 성능을 좋게 하는 조건

# 1. 심하게 비대칭인 변수들을 변환

hist(ilpd$age)
hist(ilpd$tb)
hist(ilpd$db)
hist(ilpd$alkphos)
hist(ilpd$sgpt)
hist(ilpd$sgot)
hist(ilpd$tp)
hist(ilpd$alb)
hist(ilpd$ag)
# 왼쪽으로 치우친 변수들(tb, db, alkphohs, sgpt, sgot, ag)에 대해 log변환
# 해당 데이터에 0이 많아 1을 더해준 후 log 변환


hist(log(1+ilpd$tb))
hist(log(1+ilpd$db))
# 총 빌리루빈과 직접 빌리루빈의 경우 여전히 왼쪽으로 치우침
# 이상치를 판단하여 제거할지, 그냥 이대로 갈지 고민 필요

hist(log(1+ilpd$alkphos))
hist(log(1+ilpd$sgpt))
hist(log(1+ilpd$sgot))
hist(log(1+ilpd$ag))
# 위 4개는 상대적으로 훨씬 안정적인 모습으로 바뀜. 변환하는게 좋을지 고민 필요
#로그변환
hist(ip$db)
hist(log(1+ilpd$db))
ilpd$db<-log(1+ilpd$db)
hist(ilpd$alkphos)
hist(log(1+ilpd$alkphos))
ilpd$alkphos<-log(1+ilpd$alkphos)
hist(ilpd$sgpt)
hist(log(1+ilpd$sgpt))
ilpd$sgpt<-log(1+ilpd$sgpt)
hist(log(1+ilpd$sgot))
ilpd$sgot<-log(1+ilpd$sgot)
hist(log(1+ilpd$tp))
ilpd$tp<-log(1+ilpd$tp)

str(ilpd)
ilpd.nn<-ilpd # 원래 데이터에서 3 9 13 제외 
nn.scale<-function(x){(x-min(x))/(max(x)-min(x))}
ilpd.nn[,-c(2,9)]<-data.frame(lapply(ilpd.nn[,-c(2,9)],nn.scale))

library(nnet) # class.ind(가변수 만드는 함수) 사용을 위한 패키지
ilpd.nn<-cbind(ilpd.nn[,1],class.ind(ilpd.nn[,2]),ilpd.nn[,c(3:8,10)],class.ind(ilpd.nn[,9]))
ilpd.name<-c("age","gender_male","gender_female",colnames(ilpd.nn[,c(4:9,10)]),"class_y","class_n")
names(ilpd.nn)<-ilpd.name
str(ilpd.nn)

## 변환한 데이터로 다시 데이터 분할 

ilpd.tr2<-ilpd.nn[train,]
ilpd.te2<-ilpd.nn[-train,]
# 앞서 데이터 분할 시 사용한 번호(train) 다시 사용


full<-as.formula(paste("class_y + class_n ~",paste(ilpd.name[!ilpd.name %in% c("class_y","class_n")],collapse=" + ")))
full # neuralnet 식 쉽게 작성하는 코드

library(neuralnet)
test.err=function(hidden.size)
{
  set.seed(3)
  fit.nn<-neuralnet(full,data=ilpd.tr2,hidden=hidden.size,threshold=0.01,stepmax=1e9,
                    act.fct="logistic",linear.output=F,lifesign="minimal")
  # 예측변수와 반응변수가 [0,1]상의 값일 때 신경망이 최상으로 작동하기 위해서는
  # 활성함수(act.fct)를 로지스틱("logistic")으로 사용해야 한다.
  # linear.output=F : 활성 함수를 출력노드에 적용하려면 F로 해야 함
  
  compute.nn<-compute(fit.nn,ilpd.te2[,1:10])
  pred.nn<-compute.nn$net.result 
  
  yhat.nn<-max.col(pred.nn)  # max.col : 예측확률이 더 큰 쪽으로 예측값을 할당
  class.nn<-max.col(ilpd.te2[,11:12])
  
  err<-mean(class.nn!=yhat.nn)
  c(hidden.size,err)
}

# 참고 : 파생 변수들에 대한 확인

hist((ilpd$tb-ilpd$db)[(ilpd$tb-ilpd$db)>=0])
hist(log(1+(ilpd$tb-ilpd$db)[(ilpd$tb-ilpd$db)>=0]))
# 간접 빌리루빈의 경우 총 빌리루빈, 직접 빌리루빈과 같이 왼쪽으로 치우침

hist(ilpd$tp-ilpd$alb) 
hist(1/(ilpd$ag/ilpd$alb)) 
# 글로불린에 대한 2가지 추정값 모두 정규분포에 가까운 안정적인 분포가 나타남


# 2. 입력변수들간의 상관관계를 작게 한다.
# 데이터 탐색 과정에서 확인하였음


# 3. 입력변수들이 [0,1]상의 값일 때 최상으로 작동

ilpd.nn<-ilpd
nn.scale<-function(x){(x-min(x))/(max(x)-min(x))}
ilpd.nn[,-c(2,11)]<-data.frame(lapply(ilpd.nn[,-c(2,11)],nn.scale))

library(nnet) # class.ind(가변수 만드는 함수) 사용을 위한 패키지
ilpd.nn<-cbind(ilpd.nn[,1],class.ind(ilpd.nn[,2]),ilpd.nn[,3:10],class.ind(ilpd.nn[,11]))
ilpd.name<-c("age","gender_male","gender_female",colnames(ilpd.nn[,4:11]),"class_y","class_n")
names(ilpd.nn)<-ilpd.name
str(ilpd.nn)


## 변환한 데이터로 다시 데이터 분할

ilpd.tr2<-ilpd.nn[train,]
ilpd.te2<-ilpd.nn[-train,]
# 앞서 데이터 분할 시 사용한 번호(train) 다시 사용


full<-as.formula(paste("class_y + class_n ~",paste(ilpd.name[!ilpd.name %in% c("class_y","class_n")],collapse=" + ")))
full # neuralnet 식 쉽게 작성하는 코드

library(neuralnet)
test.err=function(hidden.size)
{
  set.seed(340)
  fit.nn<-neuralnet(full,data=ilpd.tr2,hidden=hidden.size,threshold=0.01,stepmax=1e9,
                    act.fct="logistic",linear.output=F,lifesign="minimal")
  # 예측변수와 반응변수가 [0,1]상의 값일 때 신경망이 최상으로 작동하기 위해서는
  # 활성함수(act.fct)를 로지스틱("logistic")으로 사용해야 한다.
  # linear.output=F : 활성 함수를 출력노드에 적용하려면 F로 해야 함
  
  compute.nn<-compute(fit.nn,ilpd.te2[,1:11])
  pred.nn<-compute.nn$net.result 
  
  yhat.nn<-max.col(pred.nn) # max.col : 예측확률이 더 큰 쪽으로 예측값을 할당
  class.nn<-max.col(ilpd.te2[,12:13])
  
  err<-mean(class.nn!=yhat.nn)
  c(hidden.size,err)
}

out1<-t(sapply(1:10,FUN=test.err))
out2<-t(sapply(11:20,FUN=test.err))
out3<-t(sapply(21:30,FUN=test.err))
out4<-t(sapply(31:40,FUN=test.err))
out5<-t(sapply(41:50,FUN=test.err))
out6<-t(sapply(51:60,FUN=test.err))
out7<-t(sapply(61:70,FUN=test.err))
out8<-t(sapply(71:80,FUN=test.err))
out9<-t(sapply(81:90,FUN=test.err))
out10<-t(sapply(91:100,FUN=test.err))
out<-rbind(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10)

colnames(out)<-c('hidden.size','err')
out<-as.data.frame(out)

ggplot(out,aes(x=hidden.size,y=err))+geom_line(colour='#3366FF')+geom_point(colour='#3366FF',size=2.5)+
  labs(x="Hidden Size",y="Error Rate")+
  theme(axis.title=element_text(color="#666666",face="bold",size=12))+
  theme(legend.title=element_text(color="#666666",face="bold",size=10))


set.seed(354)
fit.nn<-neuralnet(full,data=ilpd.tr2,hidden=3,threshold=0.01,stepmax=1e9,
                  act.fct="logistic",linear.output=F,lifesign="minimal")
plot(fit.nn)

compute.nn<-compute(fit.nn,ilpd.te2[,1:11])
pred.nn<-compute.nn$net.result 

yhat.nn<-as.factor(max.col(pred.nn))
class.nn<-as.factor(max.col(ilpd.te2[,12:13]))

confusionMatrix(yhat.nn,class.nn)