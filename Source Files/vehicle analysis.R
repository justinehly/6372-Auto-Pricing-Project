library(class)
library(caret)
library(e1071)
library(magrittr)
library(XML) 
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) 
library(ggplot2)
library(gridExtra)
library(naniar)
library(glmnet)
library(car)
library(tibble)
library(VIM)
library(FactoMineR)
library(purrr)
library(leaps)
library(tree)

setwd("C:/Users/wrf0/Documents/SMU Data Science Program/Applied Statistics/Project 1")

autos <- read.csv("data1.csv")
head(autos)

##### Look for any NAs in the data set ####
sapply(autos, function(x) sum(x %in% common_na_strings)) # missing values using other than NA
sapply(autos, function(x) sum(x %in% common_na_numbers)) # missing values using other than NA

res<-summary(aggr(autos)) # visual graph of missing values
res #summary of missing values by variable name

# Missing 69 values for HP, 30 for Engine Cylinders, 6 for # of doors
missingHP <- which (is.na(autos$Engine.HP))
write.csv(autos[missingHP,],"MissingHP.csv") #export missing HP to csv for excel manipulation

#Fiat 500e - electric so no cylinders
autos <- autos %>% mutate(Engine.HP = replace(Engine.HP, Model == "500e",111)) 
#2017 Continental
autos[c(2906:2909),]
autos$Engine.HP[c(2906:2909)] <- 400 

#2015 Impala duel-fuel
autos[c(5826,5831, 5832, 5834, 5840, 5841),]
autos$Engine.HP[c(5826,5831,5832,5834,5840, 5841)] <- "260(gas)/ 230(CNG) "

#2017 Escape
autos[c(4204:4207),]
autos$Engine.HP[c(4204:4207)] <- 179 

#2013-2014 Fit EV
autos[c(4706,4707),]
autos$Engine.HP[c(4706,4707)] <- 123

#2015 Ford Focus EV 143hp
autos[c(4786,4790,4799),]
autos$Engine.HP[c(4786,4790,4799)] <- 143

#2005 Ford Freestar only vans above $29k have 201 HP
autos[c(4915:4928),] %>% select(Engine.HP,highway.MPG, city.mpg, MSRP) %>% arrange(MSRP)
autos[c(4915:4928),] %>% select(Engine.HP,highway.MPG, city.mpg, MSRP) %>% arrange(highway.MPG)
autos$Engine.HP[c(4915:4918)] <- 193
autos$Engine.HP[c(4919:4920)] <- 201

# 2014 Mitsubishi i-MiEV
autos$Engine.HP[5779] <- 66
# 2015-2016 Kia Soul EV
autos$Engine.HP[c(9851:9855)] <- 109
#2013-2014 Toyota Rav4 EV
autos$Engine.HP[c(8375:8376)] <- 154
#Telsa Model S missing values
autos %>% filter(Make == "Tesla")

autos %>% filter(Make == "Tesla") #export missing HP to csv for excel manipulation
tesla <- read.csv("tesla.csv")

# mass replace tesla missing values since they were mostly all independent
for(i in 6922:6939){autos$Engine.HP[i] <- tesla$Engine.HP[i-(6921)]}
for(i in 6922:6939){autos$Number.of.Doors[i] <- tesla$Number.of.Doors[i-(6921)]}

# 2017 Lincoln MKZ - all FWD have 240hp
autos$Engine.HP[c(6909,6911,6917,6919)] <- 240
# 2015 Mercedes M-Class Diesel
autos$Engine.HP[6579] <- 200
#2014-2016 Nissan Leaf - all 107 hp
autos$Engine.HP[c(6386:6395)] <- 107

#### Work on missing cylinders 
missingCyl <- which (is.na(autos$Engine.Cylinders))
#write.csv(autos[missingCyl,],"MissingCyl.csv")
#view(autos[missingCyl,])
#change electric cars to 'e' for cylinders since they don't have any
autos$Engine.Cylinders <- ifelse(autos$Engine.Fuel.Type == 'electric','e',autos$Engine.Cylinders)
#change the mazda RX cars to 'r' for rotary engine since they don't have cylinders
autos$Engine.Cylinders[c(8696:8715)] <- 'r'
# any remaining missing values?
sapply(autos, function(x) sum(x %in% common_na_strings))
sapply(autos, function(x) sum(x %in% common_na_numbers))
# the software seems to think there is 1 value missing for number.of.doors
autos[which(is.na(autos$Number.of.Doors)),]
# 2- door ferrari ff
autos$Number.of.Doors[which(is.na(autos$Number.of.Doors))] <-2
summary(autos)
str(autos)
# replace all chr with factors
autos[sapply(autos,is.character)] <- lapply(autos[sapply(autos,is.character)], as.factor)
autos$Year <- as.factor(autos$Year) # make Year into a factor
str(autos)
sapply(autos, function(x) sum(is.na(x)))
# Engine.Fuel.Type - suzuki is missing
autos$Engine.Fuel.Type[c(11322-11324)] <- 'regular unleaded'

autos$Engine.HP = as.numeric(autos$Engine.HP)
autos$Popularity =as.numeric(autos$Popularity)

str(autos)

attach(autos)

#Before we begin to work on EDA, we have to check outliers, multicollinearity and categorical variable factor levels.


#Check Car make and popularity plot. Ford is the in the first place

autos %>% ggplot(aes(x=Make,y=Popularity)) + geom_bar(stat='identity')

#Check horsepower and MSRP, there some outliers. 
autos %>% ggplot(aes(x=Engine.HP,y=MSRP)) +geom_point()+geom_smooth(method='loess')

#Remove MSRP outliers
autos = autos %>% filter(MSRP<=1000000)

autos %>% ggplot(aes(x=Popularity,y=MSRP)) +geom_point()


#Check VIF for full model, we have to remove aliased coefficients, Make, Model,popularity and Market category. 
autos1<-autos[,-c(1,2,15,10)]  
full.model<-lm(MSRP~.,data=autos1) 
vif(full.model)[,3]^2
#Remove city MPG and number of doors as VIF is larger than 5.
autos2<-autos[,-c(1,2,10,9,14,15)]  
full.model2<-lm(MSRP~.,data=autos2) 
vif(full.model2)[,3]^2

#We can safely remove city MPG and number of doors in our model, but we have to combine some factor levels in some
#categorical variables.

#Let's separate vehicle market category column first.
autos3=autos %>% mutate(FactoryTuner=ifelse(grepl('Factory Tuner',Market.Category),'Yes','No')) %>%
  mutate(Luxury=ifelse(grepl('Luxury',Market.Category),'Yes','No')) %>%
  mutate(FlexFuel=ifelse(grepl('Flex Fuel',Market.Category),'Yes','No')) %>%
  mutate(Hatchback=ifelse(grepl('Hatchback',Market.Category),'Yes','No')) %>%
  mutate(Diesel=ifelse(grepl('Diesel',Market.Category),'Yes','No')) %>%
  mutate(Hybrid=ifelse(grepl('Hybrid',Market.Category),'Yes','No')) %>%
  mutate(Exotic=ifelse(grepl('Exotic',Market.Category),'Yes','No')) %>%
  mutate(Crossover=ifelse(grepl('Crossover',Market.Category),'Yes','No')) %>%
  mutate(Performance=ifelse(grepl('\\b,Performance\\b|^Performance',Market.Category),'Yes','No')) %>%
  mutate(HighPerformance=ifelse(grepl('High-Performance',Market.Category),'Yes','No'))

#Due to the limit quantity of each model level with upto 900 factor levels, we will delete model column.
#Before we start to use model selection method, let's run linear regression to find out insignificant factor levels.
autos3 = autos3[,-c(2,9,10,14)] #De-selected model, number of door, city MPG and old market category columns.
autos3$FactoryTuner = as.factor(autos3$FactoryTuner)
autos3$Luxury = as.factor(autos3$Luxury)
autos3$FlexFuel = as.factor(autos3$FlexFuel)
autos3$Hatchback = as.factor(autos3$Hatchback)
autos3$Diesel = as.factor(autos3$Diesel)
autos3$Hybrid = as.factor(autos3$Hybrid)
autos3$Exotic = as.factor(autos3$Exotic)
autos3$Crossover = as.factor(autos3$Crossover)
autos3$Performance = as.factor(autos3$Performance)
autos3$HighPerformance = as.factor(autos3$HighPerformance)



EDA.model<-lm(MSRP~.,data=autos3)
summary(EDA.model)
#Still too many levels in other categorical variables. Let's combine insignificant levels.

Make_new <- c('Aston Martin','Bentley','Audi', 'BMW', 'Ferrari', 'Cadillac','Dodge',
            'FIAT', 'Hyundai', 'Kia','Lexus','Lotus',
            'Lamborghini', 'Land Rover', 'Maserati','Maybach','McLaren','Mercedes-Benz',
            'Porsche','Rolls-Royce','Spyker', 'Tesla')
for(i in 1:length(autos3$Make)){
  ifelse(autos3$Make[i] %in% Make_new,autos3$Make_new[i] <- paste(autos3$Make[i]), autos3$Make_new[i] <- 'Insignificant Make')
}
autos3$Make_new = as.factor(autos3$Make_new)

Year_new <- c('2001','2002', '2003', '2004', 
              '2005', '2006', '2007', '2008',
              '2009', '2010', '2011','2012','2013','2014','2015','2016','2017')
for(i in 1:length(autos3$Year)){
  ifelse(autos3$Year[i] %in% Year_new,autos3$Year_new[i] <- paste(autos3$Year[i]), autos3$Year_new[i] <- 'Insignificant Year')
}
autos3$Year_new = as.factor(autos3$Year_new)


Vehicle.Style_new <- c('2dr SUV','4dr SUV', 'Convertible ', 'Sedan') 
          
for(i in 1:length(autos3$Vehicle.Style)){
  ifelse(autos3$Vehicle.Style[i] %in% Vehicle.Style_new,autos3$Vehicle.Style_new[i] <- paste(autos3$Vehicle.Style[i]), autos3$Vehicle.Style_new[i] <- 'Insignificant Style')
}
autos3$Vehicle.Style_new = as.factor(autos3$Vehicle.Style_new)

Cylinders_new <- c('12','3', '8 ') 

for(i in 1:length(autos3$Engine.Cylinders)){
  ifelse(autos3$Engine.Cylinders[i] %in% Cylinders_new,autos3$Cylinders_new[i] <- paste(autos3$Engine.Cylinders[i]), autos3$Cylinders_new[i] <- 'Insignificant Style')
}
autos3$Cylinders_new = as.factor(autos3$Cylinders_new)

autos3=autos3[,-c(1,2,5,9)] #Remove old Make, Year, engine cylinders and style columns.
EDA.model<-lm(MSRP~.,data=autos3) 
summary(EDA.model)
vif(EDA.model)[,3]^2 #Remove Exotic column as its VIF is larger than 10.
autos3=autos3[,-c(15)]

str(autos3)
summary(autos3)

#Now we can start to do EDA and fit a model.








par(mfrow=c(1,3))
plot(autos$Engine.HP,autos$MSRP, xlab="Engine Horsepower",ylab="MSRP")
new5<-data.frame(Engine.HP=seq(0,100,1))
EDA.model<-lm(MSRP~Engine.HP+I(Engine.HP^2))
lines(seq(0,100,1),predict(EDA.model,newdata=new5),col="red",lwd=4)
plot(EDA.model$fitted.values,EDA.model$residuals,xlab="Fitted Values",ylab="Residuals")
plot(Engine.HP,EDA.model$residuals,xlab="Engine Horsepower",ylab="Residuals")

#Added log transform to MSRP
par(mfrow=c(1,3))
plot(autos$Engine.HP,autos$MSRP, xlab="Engine Horsepower",ylab="MSRP")
new5<-data.frame(Engine.HP=seq(0,100,1))
EDA.model_3<-lm(log1p(MSRP)~Engine.HP+I(Engine.HP^2))
lines(seq(0,100,1),predict(EDA.model_3,newdata=new5),col="red",lwd=4)
plot(EDA.model_3$fitted.values,EDA.model_3$residuals,xlab="Fitted Values",ylab="Residuals")
plot(Engine.HP,EDA.model_3$residuals,xlab="Engine Horsepower",ylab="Residuals")

par(mfrow=c(2,2))
plot(EDA.model_2)

par(mfrow=c(2,2))
plot(EDA.model_1)

summary(EDA.model_2)

#Let's try to use stepwise method to build a model. First we split the data.
set.seed(1)
autos_model = autos[,-c(9,14)]
trainIndices = sample(1:dim(autos_model)[1],round(0.8 * dim(autos_model)[1]),replace=F)
testIndices = sample(1:dim(autos_model)[1],round(0.1 * dim(autos_model)[1]),replace=F)
train=autos_model[trainIndices,]
test=autos_model[testIndices,]

reg.fwd=regsubsets(MSRP~.,data=train,method="forward",nvmax=20)

predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
testASE<-c()

for (i in 1:20){
  predictions<-predict.regsubsets(object=reg.fwd,newdata=test,id=i) 
  testASE[i]<-mean((test$MSRP-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:20,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE",ylim=c(0.3,0.8))
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(reg.fwd)$rss
lines(1:21,rss/nrow(train),lty=3,col="blue") 
summary(reg.fwd)

#Let's try to use LASSO method to build a model.
x=model.matrix(MSRP~.,train)[,-1]
y=train$MSRP

xtest=model.matrix(MSRP~.,test)[,-1]
ytest=test$MSRP


lasso.mod=glmnet(x,y,alpha=1,lambda=10^seq(10,-2,length=100))

cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlambda=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlambda,newx=xtest)
testMSE_LASSO=mean((ytest-lasso.pred)^2)
testMSE_LASSO

coef(lasso.mod,s=bestlambda)

#lm(y~.,data=somedata)

#rpart

