library(caTools)
library(Amelia)
library(dplyr)
library(stats)
library(tree)
library(randomForest)
library(party)
library(ROCR)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

setwd("E:\\Data Creation\\Telecom")
telecomdata<-read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header = T)
head(telecomdata)
which(is.na(telecomdata$tenure))
telecomdata[!complete.cases(telecomdata), ]

table(telecomdata$Churn)
any(is.na(telecomdata))
#Create new column tenure_interval from the tenure column
group_tenure <- function(tenure){
  if (tenure >= 0 && tenure <= 6){
    return('0-6 Month')
  }else if(tenure > 6 && tenure <= 12){
    return('6-12 Month')
  }else if (tenure > 12 && tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 && tenure <=36){
    return('24-36 Month')
  }else if (tenure > 36 && tenure <=48){
    return('36-48 Month')
  }else if (tenure > 48 && tenure <= 62){
    return('48-62 Month')
  }else if (tenure > 62){
    return('> 62 Month')
  }
}

# apply group_tenure function on each row of dataframe
telecomdata$tenure_interval <- sapply(telecomdata$tenure,group_tenure)
telecomdata$tenure_interval <- as.factor(telecomdata$tenure_interval)

# Ignore the variables with more levels while predicting the model
# Columns "customerID" and "tenure" having more levels

telecomdata <- select(telecomdata,-customerID,-tenure)
telecomdata$MultipleLines <- as.character(telecomdata$MultipleLines)
telecomdata$OnlineSecurity <- as.character(telecomdata$OnlineSecurity)
telecomdata$OnlineBackup <- as.character(telecomdata$OnlineBackup)
telecomdata$DeviceProtection <- as.character(telecomdata$DeviceProtection)
telecomdata$TechSupport <- as.character(telecomdata$TechSupport)
telecomdata$StreamingTV <- as.character(telecomdata$StreamingTV)
telecomdata$StreamingMovies <- as.character(telecomdata$StreamingMovies)


# convert factor variables into character variables before changing the values
telecomdata$MultipleLines[telecomdata$MultipleLines=="No phone service"] <- "No"
telecomdata$OnlineSecurity[telecomdata$OnlineSecurity=="No internet service"] <- "No"
telecomdata$OnlineBackup[telecomdata$OnlineBackup=="No internet service"] <- "No"
telecomdata$DeviceProtection[telecomdata$DeviceProtection=="No internet service"] <- "No"
telecomdata$TechSupport[telecomdata$TechSupport=="No internet service"] <- "No"
telecomdata$StreamingTV[telecomdata$StreamingTV=="No internet service"] <- "No"
telecomdata$StreamingMovies[telecomdata$StreamingMovies=="No internet service"] <- "No"

# converting character variables into factor variables
telecomdata$MultipleLines <- as.factor(telecomdata$MultipleLines)
telecomdata$OnlineSecurity <- as.factor(telecomdata$OnlineSecurity)
telecomdata$OnlineBackup <- as.factor(telecomdata$OnlineBackup)
telecomdata$DeviceProtection <- as.factor(telecomdata$DeviceProtection)
telecomdata$TechSupport <- as.factor(telecomdata$TechSupport)
telecomdata$StreamingTV <- as.factor(telecomdata$StreamingTV)
telecomdata$StreamingMovies <- as.factor(telecomdata$StreamingMovies)


# check the number of NA rows if it is relatively small in number then ignore those rows from the analysis
telecomdata <- na.omit(telecomdata)


# set the seed it will output same output when ever the model is executed
set.seed(123)

# sample the input data with 70% for training and 30% for testing
sample <- sample.split(telecomdata$Churn,SplitRatio=0.70)
trainData <- subset(telecomdata,sample==TRUE)
testData <- subset(telecomdata,sample==FALSE)
#Stepwise Regression
telecomModelstep <- step(glm(Churn ~ .,family=binomial(link="logit"),data=trainData),direction = "backward")
telecomModelstep_1<-glm(Churn ~ PaymentMethod+OnlineSecurity+MonthlyCharges+StreamingMovies+PaperlessBilling+StreamingTV+InternetService+Contract+tenure_interval+MultipleLines+SeniorCitizen,data=trainData,family=binomial(link="logit"))
plot(telecomModelstep_1)
print(summary(telecomModelstep_1))
# test the model with test dataset
test.predictionsstep_response <- predict(telecomModelstep_1,newdata=testData,type="response")
test.predictionsstep_link <- predict(telecomModelstep_1,newdata=testData,type="link")
# if the prediction probability is greater than 0.5 then those 
# customers are classified as churned customer less than 0.5 are classified as not churning customer
fitted.results <- ifelse(test.predictionsstep_response > 0.5,1,0)
testData$Churn <- as.character(testData$Churn)
testData$Churn[testData$Churn=="No"] <- "0"
testData$Churn[testData$Churn=="Yes"] <- "1"
# calculating the misclassfication rate
misClasificationError <- mean(fitted.results!=testData$Churn)
print(misClasificationError)
# calculating the accuracy rate
accuracyRate <- 1-misClasificationError
print(accuracyRate)
# confusion matrix
table(testData$Churn,test.predictionsstep_response > 0.5)
(1392+294)/nrow(testData)

score_data <- data.frame(link=test.predictionsstep_link, 
                         response=test.predictionsstep_response,
                         churn_test=testData$Churn,
                         stringsAsFactors=FALSE)

score_data %>% 
  ggplot(aes(x=link, y=response, col=churn_test)) + 
  scale_color_manual(values=c("black", "red")) + 
  geom_point() + 
  geom_rug() + 
  ggtitle("Both link and response scores put cases in the same order")

plot(roc(testData$Churn, test.predictionsstep_response, direction="<"),
     col="yellow", lwd=3, main="ROC Curve")
# cbinding actual results with the predicted results
results <- cbind(fitted.results,testData$Churn)
colnames(results) <- c("predicted","actual")
results <- as.data.frame(results)
print(results)

# logistic regression model on  without step on training the data
tele<-glm(Churn ~PaymentMethod+OnlineSecurity+MonthlyCharges+StreamingMovies+PaperlessBilling+StreamingTV+InternetService+Contract+tenure_interval+MultipleLines+SeniorCitizen,family=binomial(link="logit"),data=telecomdata)
preddicted_val<-predict(tele,type = "response")
table(telecomdata$Churn,preddicted_val>0.5)
#Actual ROC Curve
prediction_object<-prediction(preddicted_val,telecomdata$Churn)
perf_1<-performance(prediction_object,measure = "tpr",x.measure = "fpr")
plot(perf_1)
print(summary(tele))

#Random Forest with selection variables
set.seed(415)
mytree_sel<-randomForest(Churn ~ PaymentMethod+OnlineSecurity+MonthlyCharges+StreamingMovies+PaperlessBilling+StreamingTV+InternetService+Contract+tenure_interval+MultipleLines+SeniorCitizen,data=trainData,importance = T)
summary(mytree_sel)
varImpPlot(mytree_sel)
print(mytree_sel)
pred_sel<-predict(mytree_sel, newdata =testData)
table(pred_sel,testData$Churn)
(1353 + 300)/nrow(testData)
mytree_sel_cf<-cforest(Churn ~ PaymentMethod+OnlineSecurity+MonthlyCharges+StreamingMovies+PaperlessBilling+StreamingTV+InternetService+Contract+tenure_interval+MultipleLines+SeniorCitizen,data=trainData,controls=cforest_unbiased(ntree=200, mtry=3))
Prediction_c <- predict(mytree_sel_cf, testData, OOB=TRUE, type = "response")
table(Prediction_c,testData$Churn)
(1359+306)/nrow(testData)
#Random Forest Plotting
mytree_sel<-rpart(Churn ~ PaymentMethod+OnlineSecurity+MonthlyCharges+StreamingMovies+PaperlessBilling+StreamingTV+InternetService+Contract+tenure_interval+MultipleLines+SeniorCitizen,data=trainData,method ="class")
mytree_sel<-rpart(Churn ~ PaymentMethod+OnlineSecurity+MonthlyCharges+StreamingMovies+PaperlessBilling+StreamingTV+InternetService+Contract+tenure_interval+MultipleLines+SeniorCitizen,data=trainData,method ="class",control=rpart.control(minsplit=2, cp=0.005))
new.fit <- prp(mytree_sel,snip=TRUE)$obj
fancyRpartPlot(mytree)
fancyRpartPlot(mytree_sel)
mytree<-rpart(Churn ~ .,data=trainData,method = "class")
plot(mytree)
text(mytree)
#Random Forest
set.seed(415)
mytree<-randomForest(Churn ~ .,data=trainData,importance = T)
summary(mytree)
varImpPlot(mytree)
print(mytree)
pred<-predict(mytree, newdata =testData)
submit<-data.frame(Churn=testData$Churn,Churn = pred)
write.csv(submit,file = "firstforest.csv",row.names = F)
table(pred,testData$Churn)
(1387 + 277)/nrow(testData)
set.seed(415)
mytree1<-cforest(Churn ~ .,data=trainData,controls=cforest_unbiased(ntree=200, mtry=3))
Prediction <- predict(mytree1, testData, OOB=TRUE, type = "response")
table(Prediction,testData$Churn)
(1397+288)/nrow(testData)
# test the model with test dataset
test.predictions <- predict(telecomModel,newdata=testData,type="response")

