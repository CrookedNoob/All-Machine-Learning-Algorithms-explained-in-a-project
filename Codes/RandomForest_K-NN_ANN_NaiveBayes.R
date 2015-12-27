str(existing.customer)
table(existing.customer$Income)
prop.table(table(existing.customer$Income))
nrow(existing.customer)

existing.customer.fact<- existing.customer

#Convert Income in bins High Low and Medium
existing.customer.fact$Income[existing.customer.fact$Income==0]<- "1"
existing.customer.fact$Income[existing.customer.fact$Income==1]<- "1"
existing.customer.fact$Income[existing.customer.fact$Income==2]<- "1"
existing.customer.fact$Income[existing.customer.fact$Income==3]<- "1"
existing.customer.fact$Income[existing.customer.fact$Income==4]<- "2"
existing.customer.fact$Income[existing.customer.fact$Income==5]<- "2"
existing.customer.fact$Income[existing.customer.fact$Income==6]<- "2"
existing.customer.fact$Income[existing.customer.fact$Income==7]<- "3"
existing.customer.fact$Income[existing.customer.fact$Income==8]<- "3"

#convert all the columns to factor level
existing.customer.fact[]<-lapply(existing.customer.fact, factor)


#replace missing values with mode
library(prettyR) #Use prettyR library to use the Mode function
table(is.na(existing.customer.fact$Income))

existing.customer.fact$Sex[is.na(existing.customer.fact$Sex)]<-Mode(existing.customer.fact$Sex)
existing.customer.fact$MaritalStatus[is.na(existing.customer.fact$MaritalStatus)]<-Mode(existing.customer.fact$MaritalStatus)
existing.customer.fact$Age[is.na(existing.customer.fact$Age)]<-Mode(existing.customer.fact$Age)
existing.customer.fact$Education[is.na(existing.customer.fact$Education)]<-Mode(existing.customer.fact$Education)
existing.customer.fact$Occupation[is.na(existing.customer.fact$Occupation)]<-Mode(existing.customer.fact$Occupation)
existing.customer.fact$YearsInSf[is.na(existing.customer.fact$YearsInSf)]<-Mode(existing.customer.fact$YearsInSf)
existing.customer.fact$DualIncome[is.na(existing.customer.fact$DualIncome)]<-Mode(existing.customer.fact$DualIncome)
existing.customer.fact$HouseholdMembers[is.na(existing.customer.fact$HouseholdMembers)]<-Mode(existing.customer.fact$HouseholdMembers)
existing.customer.fact$Under18[is.na(existing.customer.fact$Under18)]<-Mode(existing.customer.fact$Under18)
existing.customer.fact$HouseholdStatus[is.na(existing.customer.fact$HouseholdStatus)]<-Mode(existing.customer.fact$HouseholdStatus)
existing.customer.fact$TypeOfHome[is.na(existing.customer.fact$TypeOfHome)]<-Mode(existing.customer.fact$TypeOfHome)
existing.customer.fact$EthnicClass[is.na(existing.customer.fact$EthnicClass)]<-Mode(existing.customer.fact$EthnicClass)
existing.customer.fact$Language[is.na(existing.customer.fact$Language)]<-Mode(existing.customer.fact$Language)

#Check if the variables are still or not
str(existing.customer.fact)

#Check if any NA exists or not--- FALSE shows no NA
table(is.na(existing.customer.fact))

#Build train and test datasets
8493*.7
prop.table(table(existing.customer.fact$Income))

train.rf1<- existing.customer.fact[1:7000,]
test.rf1<- existing.customer.fact[7001:8493,]

#using Caret package
library(caret)
#define the control using RANDOM FOREST selection Function
#control<- rfeControl(method = "repeatedcv", number = 3, repeats=3)
#run RFE algorithm
#result<- rfe(train.rf1[,1:13], train.rf1[,14], rfeControl = control, metric = "Kappa")

#grid.rf<- expand.grid(.mtry=c(2,4,8,16))
#set.seed(300)
#model.rf<- train(Income~., data=train.rf1, method = "rf", metric= "Kappa",
                                  #trControl = control, tuneGrid=grid.rf)

rfecentrl=rfeControl(functions=rfFuncs,method="cv",number=5)
z=rfe(train.rf1[,-13],train.rf1[,14],rfeControl = rfecentrl,metric = "Kappa")
z
names(z)
z$variables


#Using all the variables gives accuracy of 63.9%
train.cntrl<-trainControl(method = "cv",number = 5)
model.rf=train.default(x=train.rf1[,c(1:13)],
                       y=train.rf1[,c(14)],trControl = train.cntrl,
                       tuneGrid = data.frame(mtry=c(2:4)))
model.rf
names(model.rf)

pred<-predict(model.rf,test.rf1)
confusionMatrix(test.rf1$Income,pred)
plot(model.rf)

#using 3 variables gives accuracy of 52.7%
train.cntrl<-trainControl(method = "cv",number = 5)
model.rf.3var=train.default(x=train.rf1[,c(4,8,13)],
                       y=train.rf1[,c(14)],trControl = train.cntrl,
                       tuneGrid = data.frame(mtry=c(2:4)))
model.rf.3var
names(model.rf.3var)

pred2<-predict(model.rf.3var,test.rf1)
confusionMatrix(test.rf1$Income,pred2)
plot(model.rf.3var)

#compare the original test data with the 2 predicted outputs -- pred and pred2
validate.op<- data.frame(test.rf1$Income, pred, pred2)

library(ggplot2)
library(gmodels)
colnames(validate.op)[1]<-"Income"

#rename factor labels as per the requirement
levels(validate.op$Income)[levels(validate.op$Income)=="1"]<- "Low"
levels(validate.op$pred)[levels(validate.op$pred)=="1"]<- "Low"
levels(validate.op$pred2)[levels(validate.op$pred2)=="1"]<- "Low"
levels(validate.op$pred)[levels(validate.op$pred)=="2"]<- "Medium"
levels(validate.op$pred2)[levels(validate.op$pred2)=="2"]<- "Medium"
levels(validate.op$Income)[levels(validate.op$Income)=="2"]<- "Medium"
levels(validate.op$Income)[levels(validate.op$Income)=="3"]<- "High"
levels(validate.op$pred)[levels(validate.op$pred)=="3"]<- "High"
levels(validate.op$pred2)[levels(validate.op$pred2)=="3"]<- "High"

#using 7000 observations in train data and the rest in test data increases the acuracy to 65.97%
#Using RFE and 3 predictors the accuracy is 54.79%



#cross table of test income with pred
CrossTable(validate.op$Income, validate.op$pred, prop.chisq = FALSE,prop.r = FALSE,
           prop.c = FALSE, dnn=c("Actual Class", "Predicted Class"))

#cross table of test income with pred2
CrossTable(validate.op$Income, validate.op$pred2, prop.chisq = FALSE,prop.r = FALSE,
           prop.c = FALSE, dnn=c("Actual Class", "Predicted Class"))

#cross table of pred vs pred2 --- 63.4% similarity between the 2 predicted outcomes
CrossTable(validate.op$pred, validate.op$pred2, prop.chisq = FALSE,prop.r = FALSE,
           prop.c = FALSE, dnn=c("Actual Class", "Predicted Class"))



#-------------------------------------------------------------------------------------------#

#using KNN
library(class)

#build train and test data
train.knn<- existing.customer[1:7000,]
test.knn<- existing.customer[7001:8493,]


library(prettyR)
#replace NA with Mode in train data
train.knn$Sex[is.na(train.knn$Sex)]<-Mode(train.knn$Sex)
train.knn$MaritalStatus[is.na(train.knn$MaritalStatus)]<-Mode(train.knn$MaritalStatus)
train.knn[,3][is.na(train.knn[,3])]<-Mode(train.knn[,3])
train.knn[,4][is.na(train.knn[,4])]<-Mode(train.knn[,4])
train.knn[,5][is.na(train.knn[,5])]<-Mode(train.knn[,5])
train.knn[,6][is.na(train.knn[,6])]<-Mode(train.knn[,6])
train.knn[,7][is.na(train.knn[,7])]<-Mode(train.knn[,7])
train.knn[,8][is.na(train.knn[,8])]<-Mode(train.knn[,8])
train.knn[,9][is.na(train.knn[,9])]<-Mode(train.knn[,9])
train.knn[,10][is.na(train.knn[,10])]<-Mode(train.knn[,10])
train.knn[,11][is.na(train.knn[,11])]<-Mode(train.knn[,11])
train.knn[,12][is.na(train.knn[,12])]<-Mode(train.knn[,12])
train.knn[,13][is.na(train.knn[,13])]<-Mode(train.knn[,13])

#replace NA with Mode in test data
test.knn$Sex[is.na(test.knn$Sex)]<-Mode(test.knn$Sex)
test.knn$MaritalStatus[is.na(test.knn$MaritalStatus)]<-Mode(train.knn$MaritalStatus)
test.knn[,3][is.na(test.knn[,3])]<-Mode(test.knn[,3])
test.knn[,4][is.na(test.knn[,4])]<-Mode(test.knn[,4])
test.knn[,5][is.na(test.knn[,5])]<-Mode(test.knn[,5])
test.knn[,6][is.na(test.knn[,6])]<-Mode(test.knn[,6])
test.knn[,7][is.na(test.knn[,7])]<-Mode(test.knn[,7])
test.knn[,8][is.na(test.knn[,8])]<-Mode(test.knn[,8])
test.knn[,9][is.na(test.knn[,9])]<-Mode(test.knn[,9])
test.knn[,10][is.na(test.knn[,10])]<-Mode(test.knn[,10])
test.knn[,11][is.na(test.knn[,11])]<-Mode(test.knn[,11])
test.knn[,12][is.na(test.knn[,12])]<-Mode(test.knn[,12])
test.knn[,13][is.na(test.knn[,13])]<-Mode(test.knn[,13])

#creating output class for Income in train model
train.class.knn<- NULL
train.class.knn$Income<- train.knn$Income

#making the output class factors in train data
train.class.knn$Income[train.class.knn$Income==1]<- "Low"
train.class.knn$Income[train.class.knn$Income==2]<- "Low"
train.class.knn$Income[train.class.knn$Income==0]<- "Low"
train.class.knn$Income[train.class.knn$Income==3]<- "Low"
train.class.knn$Income[train.class.knn$Income==4]<- "Medium"
train.class.knn$Income[train.class.knn$Income==5]<- "Medium"
train.class.knn$Income[train.class.knn$Income==6]<- "Medium"
train.class.knn$Income[train.class.knn$Income==7]<- "High"
train.class.knn$Income[train.class.knn$Income==8]<- "High"

#making output class factors in test data
test.knn$Income[test.knn$Income==1]<- "Low"
test.knn$Income[test.knn$Income==2]<- "Low"
test.knn$Income[test.knn$Income==0]<- "Low"
test.knn$Income[test.knn$Income==3]<- "Low"
test.knn$Income[test.knn$Income==4]<- "Medium"
test.knn$Income[test.knn$Income==5]<- "Medium"
test.knn$Income[test.knn$Income==6]<- "Medium"
test.knn$Income[test.knn$Income==7]<- "High"
test.knn$Income[test.knn$Income==8]<- "High"

#Creating output class Income for the test data
test.class.knn$Income<- test.class.knn$Income

#Converting both the train and test class INCOME into factors
str(train.class.knn)
train.class.knn$Income<-as.factor(train.class.knn$Income)
test.class.knn$Income<-as.factor(test.class.knn$Income)

#Applying K-NN model 
library(class)
predict.knn<-knn(train = train.knn[,1:13], test= test.knn[,1:13],cl=train.class.knn[,1], k=3)
predict.knn<-as.data.frame(predict.knn)
colnames(predict.knn)[1]<-"Income"

#Renaming the class labels as High, Medium  and Low as mentioned
library(plyr)
predict.knn$predict<-revalue(predict.knn$Income, c("1"="Low", "2"="Low","0"="Low","3"="Low","4"="Medium","5"="Medium",
                              "6"="Medium","7"="High","8"="High"))


#Check the accuracy and Kappa value of the model
library(caret)
confusionMatrix(test.class.knn$Income, predict.knn$predict)

#Also created Cross table for the model
library(gmodels)
CrossTable(x=test.class.knn$Income,y=predict.knn$predict, prop.chisq = FALSE, prop.r = FALSE,
           prop.c = FALSE)
#This K-NN model gives 58.16% accuracy
#K-NN gives same accuracy for increased sample siz of 7000 observations in train data


#--------------------------------------------------------------------------------------------#


#Using Artificial Neural Network(ANN)
exist.cus.ann<- existing.customer


#Replace NA with mode
exist.cus.ann$Sex[is.na(exist.cus.ann$Sex)]<-Mode(exist.cus.ann$Sex)
exist.cus.ann$MaritalStatus[is.na(exist.cus.ann$MaritalStatus)]<-Mode(exist.cus.ann$MaritalStatus)
exist.cus.ann[,3][is.na(exist.cus.ann[,3])]<-Mode(exist.cus.ann[,3])
exist.cus.ann[,4][is.na(exist.cus.ann[,4])]<-Mode(exist.cus.ann[,4])
exist.cus.ann[,5][is.na(exist.cus.ann[,5])]<-Mode(exist.cus.ann[,5])
exist.cus.ann[,6][is.na(exist.cus.ann[,6])]<-Mode(exist.cus.ann[,6])
exist.cus.ann[,7][is.na(exist.cus.ann[,7])]<-Mode(exist.cus.ann[,7])
exist.cus.ann[,8][is.na(exist.cus.ann[,8])]<-Mode(exist.cus.ann[,8])
exist.cus.ann[,9][is.na(exist.cus.ann[,9])]<-Mode(exist.cus.ann[,9])
exist.cus.ann[,10][is.na(exist.cus.ann[,10])]<-Mode(exist.cus.ann[,10])
exist.cus.ann[,11][is.na(exist.cus.ann[,11])]<-Mode(exist.cus.ann[,11])
exist.cus.ann[,12][is.na(exist.cus.ann[,12])]<-Mode(exist.cus.ann[,12])
exist.cus.ann$Language[is.na(exist.cus.ann$Language)]<-Mode(exist.cus.ann$Language)

#To check if NA values exist or not
table(is.na(exist.cus.ann$Language))

#Conver character format to numeric after replacing NA with Mode 
exist.cus.ann<- as.data.frame(lapply(exist.cus.ann, as.numeric))#have to convert it to data frame

#Check the variable type
str(exist.cus.ann)

summary(exist.cus.ann)
train.ann1<- exist.cus.ann[1:7000,]
test.ann1<- exist.cus.ann[7001:8493,]

cor(exist.cus.ann)


#Applying ANN model
library(caret)
my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
ann.model <- train(Income ~., data = train.ann1,
                   method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1) 
plot(ann.model)

#Predicting the test data
predict.ann1<-predict(ann.model, newdata = test.ann1[1:13])
predict.ann1

#Rounding off the predicted values of Income(response)
pred.ann2<-round(data.frame(predict.ann1))
colnames(pred.ann2)[]<-"Income"
pred.ann2$Income

str(pred.ann2)

#Change predicted Income Class labels as required
pred.ann2$Income[pred.ann2$Income==-2]<- "Low"
pred.ann2$Income[pred.ann2$Income==-1]<- "Low"
pred.ann2$Income[pred.ann2$Income==0]<- "Low"
pred.ann2$Income[pred.ann2$Income==1]<- "Low"
pred.ann2$Income[pred.ann2$Income==2]<- "Low"
pred.ann2$Income[pred.ann2$Income==3]<- "Low"
pred.ann2$Income[pred.ann2$Income==4]<- "Medium"
pred.ann2$Income[pred.ann2$Income==5]<- "Medium"
pred.ann2$Income[pred.ann2$Income==6]<- "Medium"
pred.ann2$Income[pred.ann2$Income==7]<- "High"
pred.ann2$Income[pred.ann2$Income==8]<- "High"

#Change Test data Income Labels as required
test.ann1$Income[test.ann1$Income==0]<- "Low"
test.ann1$Income[test.ann1$Income==1]<- "Low"
test.ann1$Income[test.ann1$Income==2]<- "Low"
test.ann1$Income[test.ann1$Income==3]<- "Low"
test.ann1$Income[test.ann1$Income==4]<- "Medium"
test.ann1$Income[test.ann1$Income==5]<- "Medium"
test.ann1$Income[test.ann1$Income==6]<- "Medium"
test.ann1$Income[test.ann1$Income==7]<- "High"
test.ann1$Income[test.ann1$Income==8]<- "High"

#Building Confusion Matrix
confusionMatrix(test.ann1$Income, pred.ann2$Income)

#validate using CrossTable
CrossTable(x=test.ann1$Income,y=pred.ann2$Income, prop.chisq = FALSE, prop.r = FALSE,
           prop.c = FALSE)
#61.87% accuracy using ANN for 5495 train data observations
#63.63% accuracy using ANN for 7000 train data

#---------------------------------------------------------------------------------------#

#Using Naive- Bayes
data.nb<- existing.customer

#Converting from Numeric to Factor
data.nb<- data.frame(lapply(data.nb, factor))
str(data.nb)

#labelling the Income as per the required classes
data.nb$Income<-revalue(data.nb$Income, c("1"="Low", "2"="Low","0"="Low","3"="Low","4"="Medium","5"="Medium",
                                                   "6"="Medium","7"="High","8"="High"))
#For verification
str(data.nb)

#Treating NA values by replacing with Mode
data.nb[,1][is.na(data.nb[,1])]<-Mode(data.nb[,1])
data.nb[,2][is.na(data.nb[,2])]<-Mode(data.nb[,2])
data.nb[,3][is.na(data.nb[,3])]<-Mode(data.nb[,3])
data.nb[,4][is.na(data.nb[,4])]<-Mode(data.nb[,4])
data.nb[,5][is.na(data.nb[,5])]<-Mode(data.nb[,5])
data.nb[,6][is.na(data.nb[,6])]<-Mode(data.nb[,6])
data.nb[,7][is.na(data.nb[,7])]<-Mode(data.nb[,7])
data.nb[,8][is.na(data.nb[,8])]<-Mode(data.nb[,8])
data.nb[,9][is.na(data.nb[,9])]<-Mode(data.nb[,9])
data.nb[,10][is.na(data.nb[,10])]<-Mode(data.nb[,10])
data.nb[,11][is.na(data.nb[,11])]<-Mode(data.nb[,11])
data.nb[,12][is.na(data.nb[,12])]<-Mode(data.nb[,12])
data.nb[,13][is.na(data.nb[,13])]<-Mode(data.nb[,13])


#Create Train and Test Data
train.data.nb<-data.nb[1:5495,]
test.data.nb<- data.nb[5496:8493,]

#Check if the ratio of class is same in train and test data as in the original data 
prop.table(table(data.nb$Income))
prop.table(table(train.data.nb$Income))
prop.table(table(test.data.nb$Income))
#Almost same so we can continue with the model building

#Building Model
library(e1071)
model.nb<- naiveBayes(Income~., data= train.data.nb, laplace = 1)
model.nb

pred.nb<- predict(model.nb, test.data.nb[1:13])
pred.nb<- as.data.frame(pred.nb)
str(pred.nb)

#Check accuracy
library(caret)
confusionMatrix(test.data.nb$Income,pred.nb$pred.nb)


library(gmodels)
CrossTable(test.data.nb$Income, pred.nb$pred.nb, chisq =T,prop.r = F,
           prop.chisq = F, prop.c = F, prop.t = T )

#Accuracy is 63.63% with 7000 observations in train data
#Accuracy is 63.14% with 5495 observations in train data (70% of the sample dataset)