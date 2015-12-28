#using SVM

data.svm<- existing.customer

#Converting from Numeric to Factor
data.svm<- data.frame(lapply(data.svm, factor))
str(data.svm)

#labelling the Income as per the required classes
library(plyr)
data.svm$Income<-revalue(data.svm$Income, c("1"="Low", "2"="Low","0"="Low","3"="Low","4"="Medium","5"="Medium",
                                            "6"="Medium","7"="High","8"="High"))
#For verification
str(data.svm)

#Treating NA values by replacing with Mode
library(prettyR)
data.svm[,1][is.na(data.svm[,1])]<-Mode(data.svm[,1])
data.svm[,2][is.na(data.svm[,2])]<-Mode(data.svm[,2])
data.svm[,3][is.na(data.svm[,3])]<-Mode(data.svm[,3])
data.svm[,4][is.na(data.svm[,4])]<-Mode(data.svm[,4])
data.svm[,5][is.na(data.svm[,5])]<-Mode(data.svm[,5])
data.svm[,6][is.na(data.svm[,6])]<-Mode(data.svm[,6])
data.svm[,7][is.na(data.svm[,7])]<-Mode(data.svm[,7])
data.svm[,8][is.na(data.svm[,8])]<-Mode(data.svm[,8])
data.svm[,9][is.na(data.svm[,9])]<-Mode(data.svm[,9])
data.svm[,10][is.na(data.svm[,10])]<-Mode(data.svm[,10])
data.svm[,11][is.na(data.svm[,11])]<-Mode(data.svm[,11])
data.svm[,12][is.na(data.svm[,12])]<-Mode(data.svm[,12])
data.svm[,13][is.na(data.svm[,13])]<-Mode(data.svm[,13])

#Train and Test data
train.data.svm<-data.svm[1:5495,]
test.data.svm<- data.svm[5496:8493,]

#Building SVM Model
library(caret)
#for k-fold CV
control <- trainControl(method = "repeatedcv", repeats = 5)
set.seed(1500)
model.svm<- train(Income~., data=train.data.svm, method="svmLinear", trControl=control)
model.svm
names(model.svm)
pred.svm<- predict(model.svm, test.data.svm[1:13])
pred.svm

#Validating the predicted output
confusionMatrix(test.data.svm$Income, pred.svm)

library(gmodels)
CrossTable(test.data.svm$Income, pred.svm, prop.c = F, prop.r=F, prop.t = T, prop.chisq = F)
#SVM gives an accuracy of 64.8% for 5495 observations in train data