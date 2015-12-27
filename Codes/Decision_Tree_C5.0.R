str(existing.customer)
table(existing.customer$Income)
prop.table(table(existing.customer$Income))
nrow(existing.customer)
library(caret)
existing.customer.fact<- existing.customer
existing.customer.fact$Sex<- as.factor(existing.customer.fact$Sex)
existing.customer.fact$MaritalStatus<- as.factor(existing.customer.fact$MaritalStatus)
existing.customer.fact$Age<- as.factor(existing.customer.fact$Age)
existing.customer.fact$Education<- as.factor(existing.customer.fact$Education)
existing.customer.fact$Occupation<- as.factor(existing.customer.fact$Occupation)
existing.customer.fact$YearsInSf<- as.factor(existing.customer.fact$YearsInSf)
existing.customer.fact$DualIncome<- as.factor(existing.customer.fact$DualIncome)
existing.customer.fact$HouseholdMembers<- as.factor(existing.customer.fact$HouseholdMembers)
existing.customer.fact$Under18<- as.factor(existing.customer.fact$Under18)
existing.customer.fact$HouseholdStatus<- as.factor(existing.customer.fact$HouseholdStatus)
existing.customer.fact$TypeOfHome<- as.factor(existing.customer.fact$TypeOfHome)
existing.customer.fact$EthnicClass<- as.factor(existing.customer.fact$EthnicClass)
existing.customer.fact$Language<- as.factor(existing.customer.fact$Language)


existing.customer.fact$Income[existing.customer.fact$Income==0]<- "Low"
existing.customer.fact$Income[existing.customer.fact$Income==1]<- "Low"
existing.customer.fact$Income[existing.customer.fact$Income==2]<- "Low"
existing.customer.fact$Income[existing.customer.fact$Income==3]<- "Low"
existing.customer.fact$Income[existing.customer.fact$Income==4]<- "Medium"
existing.customer.fact$Income[existing.customer.fact$Income==5]<- "Medium"
existing.customer.fact$Income[existing.customer.fact$Income==6]<- "Medium"
existing.customer.fact$Income[existing.customer.fact$Income==7]<- "High"
existing.customer.fact$Income[existing.customer.fact$Income==8]<- "High"
existing.customer.fact$Income<- as.factor(existing.customer.fact$Income)

str(existing.customer.fact)
prop.table(table(is.na(existing.customer.fact$MaritalStatus)))
mode(existing.customer.fact$MaritalStatus)

#create train and test data
nrow(existing.customer.fact)
#8493*.7

train<- existing.customer.fact[1:7000,]
test<- existing.customer.fact[7001:8493,]

prop.table(table(train$Income))
prop.table(table(test$Income))

#building model
library(C50)
ncol(train)
model<- C5.0(train[-14], train$Income)
model
summary(model)

#apply the model on test data
test.pred<- predict(model, test)

#validate:
library(gmodels)
CrossTable(test$Income, test.pred, prop.chisq = FALSE,prop.r = FALSE,
           prop.c = FALSE, dnn=c("Actual Class", "Predicted Class"))
confusionMatrix(test$Income,test.pred)

op<-data.frame(test$Income,test.pred)

#Boosting model performance by adding trials
model.boost10<- C5.0(train[-14], train$Income, trials= 10)
model.boost10
summary(model.boost10)

test.pred.boost10<- predict(model.boost10, test)
CrossTable(test$Income, test.pred.boost10, prop.chisq = FALSE,prop.r = FALSE,
           prop.c = FALSE, dnn=c("Actual Class", "Predicted Class"))

op1<-data.frame(test$Income,test.pred.boost10)
confusionMatrix(test$Income,test.pred.boost10)


#predict the test data
new.customer.fact<- new.customer
new.customer.fact$Sex<- as.factor(new.customer.fact$Sex)
new.customer.fact$MaritalStatus<- as.factor(new.customer.fact$MaritalStatus)
new.customer.fact$Age<- as.factor(new.customer.fact$Age)
new.customer.fact$Education<- as.factor(new.customer.fact$Education)
new.customer.fact$Occupation<- as.factor(new.customer.fact$Occupation)
new.customer.fact$YearsInSf<- as.factor(new.customer.fact$YearsInSf)
new.customer.fact$DualIncome<- as.factor(new.customer.fact$DualIncome)
new.customer.fact$HouseholdMembers<- as.factor(new.customer.fact$HouseholdMembers)
new.customer.fact$Under18<- as.factor(new.customer.fact$Under18)
new.customer.fact$HouseholdStatus<- as.factor(new.customer.fact$HouseholdStatus)
new.customer.fact$TypeOfHome<- as.factor(new.customer.fact$TypeOfHome)
new.customer.fact$EthnicClass<- as.factor(new.customer.fact$EthnicClass)
new.customer.fact$Language<- as.factor(new.customer.fact$Language)

new.customer.predicted<-predict(model.boost10,new.customer.fact)

new.customer.predicted<- as.data.frame(new.customer.predicted)

colnames(new.customer.predicted)[1]<-"Income"

write.csv(new.customer.predicted,file = "E:\\income.csv",row.names = FALSE)
#Revenue


#no. of customers for different income levels
#high=108
#low=246
#med=146
#108+246+146= 500
  

#for low income
rev1<- (246*16000*0.9)+(246*0.07*23000)+(246*0.03*36000)
rev1
#for medium
rev2<-(146*0.43*16000)+(146*0.33*23000)+(146*0.23*36000)+(146*0.01*62000)
rev2
#for high
rev3<-(108*0.03*23000)+(108*0.22*36000)+(108*0.24*62000)+(108*0.51*90000)
rev3

total.revenue<- rev1+rev2+rev3
total.revenue


