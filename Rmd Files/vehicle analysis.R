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
library(ggcorrplot)
library(olsrr)
library(plotly)
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

#Change transmission column to automatic and manual only.
autos3=autos3 %>% mutate(Transmission.Type=ifelse(Transmission.Type=='AUTOMATIC','AUTOMATIC','MANUAL')) 
autos3$Transmission.Type = as.factor(autos3$Transmission.Type)

autos3=autos3[,-c(1,2,5,9)] #Remove old Make, Year, engine cylinders and style columns.
EDA.model<-lm(MSRP~.,data=autos3) 
summary(EDA.model)
vif(EDA.model)[,3]^2 #Remove Exotic column as its VIF is larger than 10.
autos3=autos3[,-c(15)]

str(autos3)
summary(autos3)


#Now we can start to do EDA and fit a model.

#--------EDA---------------
outlierTest(EDA.model) 

qqPlot(EDA.model, main="QQ Plot") 

leveragePlots(EDA.model)

plot(EDA.model)

ols_plot_comp_plus_resid(EDA.model)

cutoff = 4/((nrow(MSRP)-length(EDA.model$coefficients)-2))
plot(EDA.model, which=4, cook.levels=cutoff)

# Influence Plot
influencePlot(EDA.model, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#Checked the data 1120,4025,9681 and 9682, only 1120 shows its MPG is 354 which is an data entry error. 
#Removed this data.
autos3 = autos3 %>% filter(highway.MPG!=354)

#Correlation between MSRP and all other numeric variables.
cor_autos=cor(autos3[,c('MSRP','Popularity','Engine.HP','highway.MPG')],use='complete.obs')
MSRP.cor <- data.frame(var = rownames(cor_autos)[-1], correlation = cor_autos[-1,1])
MSRP.cor %>% ggplot(aes(x = var, y = correlation)) + geom_col() + coord_flip() + ylab('Correlation to MSRP')
corr <- round(cor(cor_autos), 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3, method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           title = "Correlations of all relevant variables",
           ggtheme = theme_bw())

#--------Try to log transform MSRP
log.model<-lm(log(MSRP)~.,data=autos3) 
summary(log.model)
outlierTest(log.model) 

qqPlot(log.model, main="QQ Plot") 

leveragePlots(log.model)

plot(log.model)

#--------Check plot shape between MSRP and engine HP.
par(mfrow=c(1,2))
plot(Engine.HP,log1p(MSRP), xlab="Engine.HP",ylab="log.MSRP")
new<-data.frame(Engine.HP=seq(0,1000,1))
model.engine<-lm(log1p(MSRP)~Engine.HP, data=autos3)
lines(seq(0,1000,1),predict(model.engine,newdata=new),col="red",lwd=4)
plot(model.engine$fitted.values,model.engine$residuals,xlab="Fitted Values",ylab="Residuals")

par(mfrow=c(1,2))
plot(Engine.HP,log1p(MSRP), xlab="Engine.HP",ylab="log.MSRP")
new<-data.frame(Engine.HP=seq(0,1000,1))
model.engine.square<-lm(log1p(MSRP)~Engine.HP+I(Engine.HP^2), data=autos3)
lines(seq(0,1000,1),predict(model.engine.square,newdata=new),col="red",lwd=4)
plot(model.engine.square$fitted.values,model.engine.square$residuals,xlab="Fitted Values",ylab="Residuals")

par(mfrow=c(1,2))
plot(Engine.HP,log1p(MSRP), xlab="Engine.HP",ylab="log.MSRP")
new<-data.frame(Engine.HP=seq(0,1000,1))
model.engine.cubic<-lm(log1p(MSRP)~Engine.HP+I(Engine.HP^2)+I(Engine.HP^3), data=autos3)
lines(seq(0,1000,1),predict(model.engine.cubic,newdata=new),col="red",lwd=4)
plot(model.engine.cubic$fitted.values,model.engine.cubic$residuals,xlab="Fitted Values",ylab="Residuals")

#--------Check plot shape between MSRP and Popularity.
par(mfrow=c(1,2))
plot(Popularity,log1p(MSRP), xlab="Popularity",ylab="log.MSRP")
new<-data.frame(Popularity=seq(0,10000,1))
model.Popularity<-lm(log1p(MSRP)~Popularity, data=autos3)
lines(seq(0,10000,1),predict(model.Popularity,newdata=new),col="red",lwd=4)
plot(model.Popularity$fitted.values,model.Popularity$residuals,xlab="Fitted Values",ylab="Residuals")

#--------Check plot shape between MSRP and highwany MPG.
par(mfrow=c(1,2))
plot(highway.MPG,log1p(MSRP), xlab="MPG",ylab="log.MSRP")
new<-data.frame(highway.MPG=seq(0,100,1))
model.MPG<-lm(log1p(MSRP)~highway.MPG, data=autos3)
lines(seq(0,100,1),predict(model.MPG,newdata=new),col="red",lwd=4)
plot(model.MPG$fitted.values,model.MPG$residuals,xlab="Fitted Values",ylab="Residuals")


#---------Forward--------
reg.fwd=regsubsets(log(MSRP)~.,data=autos3,method="forward",nvmax=20)
summary(reg.fwd)$adjr2
summary(reg.fwd)$rss
summary(reg.fwd)$bic

#Split the data. And try to plot ASE plot
set.seed(1234)

trainIndices = sample(1:dim(autos3)[1],round(0.8 * dim(autos3)[1]),replace=F)
testIndices = sample(1:dim(autos3)[1],round(0.1 * dim(autos3)[1]),replace=F)
train=autos3[trainIndices,]
test=autos3[testIndices,]

reg.fwd=regsubsets(log1p(MSRP)~.,data=train,method="forward",nvmax=20)

bics<-summary(reg.fwd)$bic
plot(bics,type="l",ylab="BIC",xlab="# of predictors")
index<-which(bics==min(bics))
points(index,bics[index],col="red",pch=10)
print("Min Bics is:")
which(bics==min(bics))

# Adjr2
adjr2<-summary(reg.fwd)$adjr2
plot(adjr2,type="l",ylab="Adjusted R-squared",xlab="# of predictors")
index<-which(adjr2==max(adjr2))
points(index,adjr2[index],col="red",pch=10)
print("Max Adj R2 is:")
which(adjr2==max(adjr2))

MallowCP <- summary(reg.fwd)$cp
plot(MallowCP,type="l",ylab="Mallow's CP",xlab="# of predictors")
index<-which(MallowCP==min(MallowCP))
points(index,MallowCP[index],col="red",pch=10)
print("Min Mallow CP is:")
which(MallowCP==min(MallowCP))




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
  testASE[i]<-mean((log1p(test$MSRP)-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:20,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(reg.fwd)$rss
lines(1:20,rss/nrow(train),lty=3,col="blue") 
summary(reg.fwd)

#Try stepwise
reg.stp=regsubsets(log1p(MSRP)~.,data=train,method="seqrep",nvmax=20)
testASE<-c()

for (i in 1:20){
  predictions<-predict.regsubsets(object=reg.stp,newdata=test,id=i) 
  testASE[i]<-mean((log1p(test$MSRP)-predictions)^2)
}
par(mfrow=c(1,1))
plot(testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(reg.stp)$rss
lines(rss/nrow(train),lty=3,col="blue") 
summary(reg.stp)
#Looks like stepwise method gave a better result, but still need some improvement.
#Let's use the predictors in stepwise method to build our own model.
#The predictors we use are Engine.HP, Transmission type, Driven wheels, Vehicle size, highway MPG,
#Popularity, factory Tune, Luxury, Flex Fuel, Hatchback, Make_new, Year_new, Vehicle style and cylinders new
model_cust=lm(log1p(MSRP)~Engine.HP+Transmission.Type+Driven_Wheels+Vehicle.Size+highway.MPG+Popularity+FactoryTuner
              +Luxury+FlexFuel+Hatchback+Make_new+Year_new+Vehicle.Style_new+Cylinders_new, data=train)
summary(model_cust)
vif(model_cust)[,3]^2
ols_plot_resid_fit(model_cust)
ols_plot_resid_lev(model_cust)
ols_plot_resid_qq(model_cust)
ols_plot_resid_hist(model_cust)
ols_plot_cooksd_bar(model_cust)

#Let's try a complex model.
model_complex=lm(log1p(MSRP)~poly(Engine.HP,6)+Transmission.Type+Driven_Wheels+Vehicle.Size+I(highway.MPG^5)
                 +I(highway.MPG^4)+I(highway.MPG^3)+I(highway.MPG^2)+Popularity+FactoryTuner
              +Luxury+FlexFuel+Hatchback+Make_new+Year_new+Vehicle.Style_new+Cylinders_new, data=train)
summary(model_complex)
vif(model_complex)[,3]^2
ols_plot_resid_fit(model_complex)
ols_plot_resid_lev(model_complex)
ols_plot_resid_qq(model_complex)
ols_plot_resid_hist(model_complex)

complex.pred<-predict(model_complex,test)

complex.RMSE<-sqrt(mean((test$MSRP-expm1(complex.pred))^2))
plot(expm1(complex.pred),test$MSRP)
lines(0:200000,0:200000)
complex.RMSE



#Let's try to use LASSO method to build a model.
fitControl<-trainControl(method="repeatedcv",number=10,repeats=10)
glmnet.fit<-train(log1p(MSRP)~.,
                  data=train,
                  method="glmnet",
                  trControl=fitControl)

#Lets look at the results of what this model has decided on
glmnet.fit
#Here we can see exactly what the estimated f(x) looks like.
coef(glmnet.fit$finalModel,glmnet.fit$finalModel$lambdaOpt)

glmnet.pred<-predict(glmnet.fit,test)

glmnet.RMSE<-sqrt(mean((test$MSRP-expm1(glmnet.pred))^2))
plot(expm1(glmnet.pred),test$MSRP)
lines(0:200000,0:200000)
glmnet.RMSE

#Here is a more natural tool to compute RMSE as well as some additional metrics
glmnet.test<-postResample(pred = expm1(glmnet.pred), obs = test$MSRP)                
glmnet.test

#Ranking of the predictors
varImp(glmnet.fit)
plot(varImp(glmnet.fit))

#Based on the ranking of the predictors, we have to remove some variables.
#High way MPG, engine fuel type, engine HP,Popularity can be removed.
#Then we run linear regression model again.
model_afterlasso=lm(log1p(MSRP)~Transmission.Type+Driven_Wheels+Vehicle.Size+FactoryTuner+Diesel+Hybrid+Crossover
              +Luxury+FlexFuel+Hatchback+Performance+HighPerformance+Make_new+Year_new+Vehicle.Style_new+Cylinders_new, data=train)
summary(model_afterlasso)
vif(model_afterlasso)[,3]^2
ols_plot_resid_fit(model_afterlasso)
ols_plot_resid_lev(model_afterlasso)
ols_plot_resid_qq(model_afterlasso)
ols_plot_resid_hist(model_afterlasso)
ols_plot_cooksd_bar(model_afterlasso)

lasso_selected.pred<-predict(model_afterlasso,test)

glmnet.RMSE2<-sqrt(mean((test$MSRP-expm1(lasso_selected.pred))^2))
plot(expm1(lasso_selected.pred),test$MSRP)
lines(0:200000,0:200000)
glmnet.RMSE2


#------------------Let's try KNN--------------
autos4= autos3 %>% mutate(FactoryTuner=ifelse(FactoryTuner=='Yes',1,0)) %>%
  mutate(Luxury=ifelse(Luxury=='Yes',1,0)) %>% 
  mutate(FlexFuel=ifelse(FlexFuel=='Yes',1,0)) %>% 
  mutate(Diesel=ifelse(Diesel=='Yes',1,0)) %>% 
  mutate(Hybrid=ifelse(Hybrid=='Yes',1,0)) %>% 
  mutate(Crossover=ifelse(Crossover=='Yes',1,0)) %>% 
  mutate(Performance=ifelse(Performance=='Yes',1,0)) %>% 
  mutate(HighPerformance=ifelse(HighPerformance=='Yes',1,0)) %>% 
  mutate(Transmission.Type=ifelse(Transmission.Type=='AUTOMATIC',1,0)) %>%
  mutate(Hatchback=ifelse(Hatchback=='Yes',1,0))

autos4$Transmission.Type = as.factor(autos4$Transmission.Type)
autos4$Luxury = as.factor(autos4$Luxury)
autos4$FlexFuel = as.factor(autos4$FlexFuel)
autos4$Diesel = as.factor(autos4$Diesel)
autos4$Hybrid = as.factor(autos4$Hybrid)
autos4$Crossover = as.factor(autos4$Crossover)
autos4$Performance = as.factor(autos4$Performance)
autos4$HighPerformance = as.factor(autos4$HighPerformance)
autos4$FactoryTuner = as.factor(autos4$FactoryTuner)
autos4$Hatchback = as.factor(autos4$Hatchback)
autos4$highway.MPG=as.numeric(autos4$highway.MPG)
autos4$MSRP=as.numeric(autos4$MSRP)

#Use numeric variables to run KNN.
autos4 = autos4[,c('Engine.HP','highway.MPG','Popularity','MSRP')]


set.seed(1234)

trainIndices = sample(1:dim(autos4)[1],round(0.8 * dim(autos4)[1]),replace=F)
testIndices = sample(1:dim(autos4)[1],round(0.1 * dim(autos4)[1]),replace=F)
train=autos4[trainIndices,]
test=autos4[testIndices,]

knn.fit<-train(MSRP~.,
               data=train,
               method="knn",preProcess = c("center","scale"),
               trControl=fitControl,
               tuneGrid=data.frame(k=c(1:10,15,20,25,30))
)

#Lets look at the CV result
knn.fit


plot(knn.fit)
#Making predictions on the validation set
knn.pred<-predict(knn.fit,test)

#Computing Error Metrics
knn.test<-postResample(pred=knn.pred,obs=test$MSRP)
knn.test

plot(knn.pred,test$MSRP)
lines(0:2000,0:2000)

#Ranking predictors
varImp(knn.fit)
plot(varImp(knn.fit))

#------------Tree model--------------
tree.fit<-train(MSRP~.,
                data=train,
                method="rpart",minsplit=5,
                trControl=fitControl,
                tuneGrid=data.frame(cp=c(.005,.0008,.01,.015,.02,.025,.03,.035,.04,.05,.06,.07,.08,.09,.25,.4))
)

#Lets look at the CV result
tree.fit

#If we want the final model tree
plot(tree.fit$finalModel)
text(tree.fit$finalModel)

#prettier tree
fancyRpartPlot(tree.fit$finalModel)


#Making predictions on the validation set
tree.pred<-predict(tree.fit,test)

#Computing Error Metrics
tree.test<-postResample(pred=tree.pred,obs=test$MSRP)
tree.test

plot(tree.pred,test$MSRP)
lines(0:2000,0:2000)

#Ranking predictors
varImp(tree.fit)
plot(varImp(tree.fit))

#------KNN model looks better-----------