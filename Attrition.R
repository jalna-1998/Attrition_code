
hrdata<- read.csv(choose.files())
View(hrdata)

####### Checking structure of dataset #######################

str(hrdata)

########## Checking missing values ##########################

sum(is.na(hrdata))

####### Converting to categories ############################

hrdata$EducationField<- as.factor(hrdata$EducationField)
hrdata$EnvironmentSatisfaction<- as.factor(hrdata$EnvironmentSatisfaction)
hrdata$JobInvolvement <-as.factor(hrdata$JobInvolvement)
hrdata$JobLevel <- as.factor(hrdata$JobLevel)
hrdata$JobSatisfaction <- as.factor(hrdata$JobSatisfaction)
hrdata$PerformanceRating<- as.factor(hrdata$PerformanceRating)
hrdata$RelationshipSatisfaction <- as.factor(hrdata$RelationshipSatisfaction)
hrdata$StockOptionLevel <- as.factor(hrdata$StockOptionLevel)
hrdata$WorkLifeBalance <- as.factor(hrdata$WorkLifeBalance)
hrdata$NumCompaniesWorked <- as.factor(hrdata$NumCompaniesWorked)
hrdata$Attrition <- as.factor(hrdata$Attrition)

########## Checking summary #################################

summary(hrdata)
library(psych)
describe(hrdata)

############ Removing Constant value columns #####################

library(dplyr)
hrdata<-select(hrdata,-c('EmployeeCount','EmployeeNumber','Over18','StandardHours'))
str(hrdata)


################### Selecting only factor columns and checking for dependence ###################

library(dplyr)
Factors<-sapply(hrdata,is.factor)
df_factors<-hrdata[,Factors]
colnames(df_factors)
df_factors %>% 
  summarise_each(funs(chisq.test(.,df_factors$Attrition)$p.value), -one_of("Attrition"))

################# Train Test split ###################################

set.seed(340)
index<- sample(nrow(hrdata),0.65*nrow(hrdata))
trainhrdata<-hrdata[index,]
testhrdata<- hrdata[-index,]
table(trainhrdata$Attrition)
table(testhrdata$Attrition)

################ Logistic Model ##################################

library(caret)
logicmodel<-glm(Attrition~.,data=trainhrdata,family="binomial")
summary(logicmodel)
pred<- predict(logicmodel,testhrdata,type="response")
summary(pred)
targ<-ifelse (pred>0.15,"Yes","No")
cm1<- confusionMatrix(as.factor(targ),testhrdata$Attrition)
cm1

################### Removing PerformanceRating ##########################
########## Column  not shown as significant both in CHi squared and Logistic Regression ####

hrdata<-select(hrdata,-c('PerformanceRating'))


############### DownSampling #########################################

set.seed(111)
trainhrdown<-downSample(x=trainhrdata[,-2],
                      y=trainhrdata$Attrition)

################ Logistic Model ##################################

library(caret)
logicmodeldown<-glm(Class~.,data=trainhrdown,family="binomial")
summary(logicmodeldown)
pred<- predict(logicmodeldown,testhrdata,type="response")
summary(pred)
targ<-ifelse (pred>0.2,"Yes","No")
cm1<- confusionMatrix(as.factor(targ),testhrdata$Attrition)
cm1

############### UpSampling #########################################

set.seed(111)
trainhrup<-upSample(x=trainhrdata[,-2],
                        y=trainhrdata$Attrition)

################ Logistic Model ##################################

library(caret)
logicmodelup<-glm(Class~.,data=trainhrup,family="binomial")
summary(logicmodelup)
pred<- predict(logicmodelup,testhrdata,type="response")
summary(pred)
targ<-ifelse (pred>0.2,"Yes","No")
cm1<- confusionMatrix(as.factor(targ),testhrdata$Attrition)
cm1

############ Decision Tree Model ################################

library(rpart)
treemodel<-rpart(Attrition~.,trainhrdata,method='class')
predhr<-predict(treemodel,testhrdata,type="class")
cm<-confusionMatrix(factor(predhr,levels=levels(testhrdata$Attrition)),testhrdata$Attrition)
cm

############ Random Forest ################################

library(randomForest)
rfmodel <- randomForest(Attrition~.,data=trainhrdata,ntree=400)
pred<- predict(rfmodel,testhrdata,type='class')
cm<-confusionMatrix(factor(pred,levels=levels(testhrdata$Attrition)),testhrdata$Attrition)
cm

############# Final Model Logistic Model without Up or Down Sampling#################################################

library(caret)
logicmodel<-glm(Attrition~.,data=trainhrdata,family="binomial")
summary(logicmodel)
pred<- predict(logicmodel,testhrdata,type="response")
summary(pred)
targ<-ifelse (pred>0.15,"Yes","No")
cm1<- confusionMatrix(as.factor(targ),testhrdata$Attrition)
cm1


