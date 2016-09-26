getwd()
setwd("G:\\projects\\analytics vidhya\\loan_prediction")

train<-read.csv("train.csv",header = T, na.strings = "")
sum(is.na(train))

test<-read.csv("test.csv",header = T,na.strings = "")
sum(is.na(test))

str(train)


train$total_income<-train$ApplicantIncome+train$CoapplicantIncome
test$total_income<-test$ApplicantIncome+test$CoapplicantIncome

train$Loan_Amount_Term<-train$Loan_Amount_Term/12
train$feature1<-train$LoanAmount/train$total_income
train$feature1<-round(train$feature1,digits = 3)


#train[is.na(train)]<--1
#KnnImputation 
library(DMwR)
train1<-knnImputation(train)
sum(is.na(train1))

test<-knnImputation(test)
sum(is.na(test))

str(train1)
train$LoanAmount<-as.integer(train$LoanAmount)
train$Loan_Amount_Term<-as.integer(train$Loan_Amount_Term)
train$Credit_History<-as.integer(train$Credit_History)
train$total_income<-as.integer(train$total_income)

test$LoanAmount<-as.integer(test$LoanAmount)
test$Loan_Amount_Term<-as.integer(test$Loan_Amount_Term)
test$Credit_History<-as.integer(test$Credit_History)
test$total_income<-as.integer(test$total_income)

#train and test set
names(train1)
train2<-train1[,c(2:6,9,10,11,12,14,13)]
names(train2)
names(test)
test2<-test[,c(2:6,9:13)]
str(train2)
str(test2)
#glm
linear<-glm(Loan_Status~.,data=train2,family=binomial())
summary(linear)
predicted<-predict.glm(linear,test2[],type = "response")
x<-rep(0,367)
x[predicted>0.5]<-1
submit<- data.frame(test[,1],x)
colnames(submit)<- c("Loan_ID", "Loan_Status")

write.csv(submit,file = "submission_glm.csv",row.names = F)

#model building
#C5.0
model <- C5.0(Loan_Status ~., train2, rules = T)
summary(model)
CM.trees.predict <- predict.C5.0(model, test2)
submit<-data.frame(test$Loan_ID)
submit$Loan_Status<-CM.trees.predict
colnames(submit)<- c("Loan_ID", "Loan_Status")
write.csv(submit,file = "submission1.csv",row.names = F)

#rpart
model<-rpart(Loan_Status~.,data=train2)
summary(model)
predicted<-predict(model,test2)

submit<- data.frame(test[,1],predicted)

colnames(submit)<- c("t_id", "probability")
write.csv(submit,"Prediction2.csv",row.names=F)

#glm
linear<-glm(Loan_Status~.,data=train2,family=binomial())
summary(linear)
predicted<-predict.glm(linear,test2[],type = "response")


submit<- data.frame(test[,1],predicted)

for(i in 1:367){
  ifelse(submit[i,2]>0.05,submit[i,2]<-1,submit[i,2]<-0)
}
  
colnames(submit)<- c("Loan_ID", "Loan_Status")
write.csv(submit,"Prediction2.csv",row.names=F)

#####XGBoost
str(train2)
# train2$Gender<-ifelse(train2$Gender=="Male",1,0)
# train2$Gender<-as.integer(train2$Gender)
# train2$Married<-ifelse(train2$Married=="Yes",1,0)
# train2$Married<-as.integer(train2$Married)
# train2$Education<-ifelse(train2$Education=="Graduate",1,0)
# train2$Education<-as.integer(train2$Education)
# train2$Self_Employed<-ifelse(train2$Self_Employed=="Yes",1,0)
# train2$Self_Employed<-as.integer(train2$Self_Employed)
# train2$Property_Area<-ifelse(train2$Property_Area=="Rural",0,ifelse(train2$Property_Area=="Semiurban",1,2))
# train2$Property_Area<-as.integer(train2$Property_Area)
# train2$Dependents<-ifelse(train2$Dependents=="0",0,
# ifelse(train2$Dependents=="1",1,ifelse(train2$Dependents=="2",2,3)))
# train2$Dependents<-as.integer(train2$Dependents)
str(train2)
require(dummies)
names(train2)
train3<-train2[,-c(11)]
train3<-dummy.data.frame(train3)

names(train3)
str(train3)
train4<-train3[,c(1:19)]

train3[] <- lapply(train3, as.numeric)
test2[] <- lapply(test2, as.numeric)

require(xgboost)
x<-as.matrix(train3)
model_xgb<-xgboost(x,label = as.matrix(train2$Loan_Status),
             obective="binary:logistic",nrounds = 25)
summary(model_xgb)

y<-data.matrix(test2)
pred <- predict(model_xgb,y)

submit <- data.frame("Loan_ID"=test$Loan_ID, "Loan_Status"=pred)

write.csv(submit,file = "submission_xgboost.csv",row.names = F)

