library(car)
library(plyr)
test<-read.csv("/home/alex/R/test.csv",header=TRUE,sep=",")
train<-read.csv("/home/alex/R/train.csv",header=TRUE,sep=",")
prop.table(table(train$Survived))
a<-table(train$Sex,train$Survived)
mosaicplot(a)
prop.table(a,1)

#####results of each model
#gender model 0.76555
#frequency model 0.689
#random forest 0.78469
#logistic 0.72727

#making first predictions

Survived<-rep(0,length(test[,1]))

test<-cbind(test,Survived)
test$Survived[test$Sex=="female"]<-1

PassengerID<-test$PassengerId
Survived<-test$Survived
First_Predictions<-cbind(PassengerID,Survived)

write.csv(First_Predictions,file="First_Predictions.csv",row.names=FALSE)


##### plucking age and pclass on gender model
train<-read.csv("/home/alex/R/train.csv",header=TRUE,sep=",")
mydata<-data.frame(Sex=train$Sex,Pclass=train$Pclass,Age=train$Age,Survived=train$Survived)
mydata<-mydata[complete.cases(mydata),]
#recoding age
mydata$Age[mydata$Age<=20]<-"young"
mydata$Age[mydata$Age>20&mydata$Age<=40]<-"amateur"
mydata$Age[mydata$Age>40 & mydata$Age<=60]<-"pro"
mydata$Age[mydata$Age>60 & mydata$Age<=80]<-"fossil"
mydata$Age<-as.factor(mydata$Age)
tablature<-prop.table(table(mydata$Pclass,mydata$Age,mydata$Sex,mydata$Survived))
#mosaicplot(tablature)

##percentages

aux2<-count(mydata)
aux<-ddply(aux2,~Sex+Pclass+Age,summarise,sums=sum(freq))
prob<-rep(0,length(aux2[,1]))
sums<-rep(0,length(aux2[,1]))
mydata<-cbind(aux2,sums,prob)
mydata$sums[1:2]<-45
mydata$sums[3]<-2
mydata$sums[4:5]<-24
mydata$sums[6:7]<-14
mydata$sums[8:9]<-45
mydata$sums[10:11]<-13
mydata$sums[12]<-16
mydata$sums[13:14]<-46
mydata$sums[15]<-1
mydata$sums[16]<-8
mydata$sums[17:18]<-47
mydata$sums[19:20]<-44
mydata$sums[21:22]<-12
mydata$sums[23:24]<-38
mydata$sums[25:26]<-7
mydata$sums[27:28]<-59
mydata$sums[29:30]<-3
mydata$sums[31:32]<-18
mydata$sums[33:34]<-19
mydata$sums[35:36]<-146
mydata$sums[37]<-4
mydata$sums[38:39]<-27
mydata$sums[30:41]<-76

mydata$prob<-mydata$freq/mydata$sums
#predictions
test1<-read.csv("/home/alex/R/test.csv",header=TRUE,sep=",")
test<-test1[,c(1,2,4,5)]
test<-test[complete.cases(test),]
test$Age[test$Age<=20]<-"young"
test$Age[test$Age<=40 & test$Age>20]<-"amateur"
test$Age[test$Age>40 & test$Age<=60]<-"pro"
test$Age[test$Age>60 & test$Age<=80]<-"fossil"
test$Age<-as.factor(test$Age)
aux4<-rep(0,length(test[,1]))
test<-cbind(test,Survived=aux4)

for(i in 1:length(test[,1])){
  probability<-mydata$prob[mydata$Sex==test$Sex[i]& mydata$Pclass==test$Pclass[i]&mydata$Age==test$Age[i]&mydata$Survived==1]
  print (probability)
  if(length(probability)>0){
    if(runif(1,0,1)<probability){
      test$Survived[i]<-1
    }
    else{
      test$Survived[i]<-0
    }}}
#copying
asd<-rep(0,length(test1[,1]))
test1<-cbind(test1,Survived=asd)
for ( i in 1:length(test1[,1])){
  if(test1$PassengerId[i]%in%test$PassengerId){
    test1$Survived[i]<-test$Survived[test1$PassengerId[i]==test$PassengerId]
  }
  else{
    if(test1$Sex[i]=="female"){
      test1$Survived[i]<-1
    }
    else{
      test1$Survived[i]<-0
    }
  }
}

predictions2<-test1[,c(1,12)]

test1$PassengerId==test$PassengerId
write.csv(predictions2,file="predictions2.csv",row.names=FALSE)





####random forest
library(mice)
library(randomForest)

train<-read.csv("/home/alex/R/train.csv",na.strings=c('NA',''),stringsAsFactors=F)
test<-read.csv("/home/alex/R/test.csv",na.strings=c('NA',''),stringsAsFactors=F)

check.missing<-function(x) return(paste0(round(sum(is.na(x))/length(x),4)*100,'%'))
data.frame(sapply(train,check.missing))
data.frame(sapply(test,check.missing))

#combine train/test data for pre-processing
train$Cat<-'train'
test$Cat<-'test'
test$Survived<-NA
full<-rbind(train,test)

#Embarked
full$Embarked[is.na(full$Embarked)]<-'S'

#Extract Title from Name
full$Title = sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][2])
full$Title<-gsub(' ','',full$Title)
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#Adding FamilySize
full$FamilySize<-full$Parch+full$SibSp+1

#Perform Imputation to remove NAs
set.seed(144)
vars.for.imputation = setdiff(names(full), "Survived")
imputed = complete(mice(full[vars.for.imputation]))
full[vars.for.imputation] = imputed

#Adding Mother
full$Mother<-0
full$Mother[full$Sex=='female' & full$Parch>0 & full$Age>18 & full$Title!='Miss']<-1
#Adding Child
full$Child<-0
full$Child[full$Parch>0 & full$Age<=18]<- 1

#FamilyId2
Surname<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][1])
FamilyId<-paste0(full$FamilySize,Surname)
full$FamilyId<-factor(FamilyId)
Family<-data.frame(table(FamilyId))
SmallFamily<-Family$FamilyId[Family$Freq<=2]
FamilyId[FamilyId %in% SmallFamily]<-'Small'
full$FamilyId2<-factor(FamilyId)

#Exact Deck from Cabin number
full$Deck<-sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1])

#Excat Position from Cabin number
full$CabinNum<-sapply(full$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
full$num<-as.numeric(full$CabinNum)
num<-full$num[!is.na(full$num)]
Pos<-kmeans(num,3)
full$CabinPos[!is.na(full$num)]<-Pos$cluster
full$CabinPos<-factor(full$CabinPos)
levels(full$CabinPos)<-c('Front','End','Middle')
full$num<-NULL

full<-transform(full,
                Pclass=factor(Pclass),
                Sex=factor(Sex),
                Embarked=factor(Embarked),
                Title=factor(Title),
                Mother=factor(Mother),
                Child=factor(Child),
                FamilyId2=factor(FamilyId2),
                Deck=factor(Deck)
)

#split train/test data
train<-full[full$Cat=='train',]
test<-full[full$Cat=='test',]
train$Survived<-factor(train$Survived)

rf.fit = randomForest(Survived ~ Pclass + Age + Sex + Title + Mother + Child + Fare, data=train, ntree = 100, nodesize = 25)
test$Survived = predict(rf.fit, test)

#library(party)
#cf.fit<-cforest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child,data=train,controls=cforest_unbiased(ntree=2000, mtry=3))
#cf.fit<-cforest(train$Survived~FamilyId2+CabinPos+Deck+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child+Deck,data=train,controls=cforest_unbiased(ntree=2000, mtry=3))
#test$Survived = predict(cf.fit, test, OOB=TRUE,type='response')

submission<-test[,1:2]
write.csv(submission,'submission.csv',row.names=F)


########## logistic regression
test<-read.csv("/home/alex/R/test.csv",header=TRUE,sep=",")
train<-read.csv("/home/alex/R/train.csv",header=TRUE,sep=",")
train$Age[train$Age<=20]<-"young"
train$Age[train$Age>20&train$Age<=40]<-"amateur"
train$Age[train$Age>40 & train$Age<=60]<-"pro"
train$Age[train$Age>60 & train$Age<=80]<-"fossil"
train<-train[complete.cases(train),]
mylogit<-glm(Survived ~Sex+ Pclass+Age ,data=train,family="binomial")
exp(coef(mylogit))
#preparing test
test$Age[test$Age<=20]<-"young"
test$Age[test$Age>20 & test$Age<=40]<-"amateur"
test$Age[test$Age>40 & test$Age<=60]<-"pro"
test$Age[test$Age>60 & test$Age<=80]<-"fossil"
test$Age<-as.factor(test$Age)
test$Survived<-0
test$Survived<-predict(mylogit,newdata=test,type="response")
for(i in 1:length(test$Survived)){
  if (is.na(test$Survived[i])){
    if (test$Sex[i]=="female"){
      test$Survived[i]<-1
    }
    else {
      test$Survived[i]<-0
      }}
  else{
    if (test$Survived[i]>0.5){
      test$Survived[i]<-1
    }
    else {
      test$Survived[i]<-0
  }  
  }
}
test<-test[,c(1,12)]
write.csv(test,'submission2',row.names = F)