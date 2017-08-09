#setwd("/var/lib/jenkins/workspace/PredictiveAnalytics_R_Churn/")
setwd("E:\\Confidential\\Case Studies\\Predictive Analytics\\POC on Telecom Churn")
library(dplyr)
args<-commandArgs(TRUE)
telecomdata<-read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header = T)

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


# convert factor variables into character variables before changing the values
telecomdata$MultipleLines <- as.character(telecomdata$MultipleLines)
telecomdata$OnlineSecurity <- as.character(telecomdata$OnlineSecurity)
telecomdata$OnlineBackup <- as.character(telecomdata$OnlineBackup)
telecomdata$DeviceProtection <- as.character(telecomdata$DeviceProtection)
telecomdata$TechSupport <- as.character(telecomdata$TechSupport)
telecomdata$StreamingTV <- as.character(telecomdata$StreamingTV)
telecomdata$StreamingMovies <- as.character(telecomdata$StreamingMovies)


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

#Churn Model using Logistic Regression
telecomModelstep_1<-glm(Churn ~ PaymentMethod+OnlineSecurity+MonthlyCharges+StreamingMovies+PaperlessBilling+StreamingTV+InternetService+Contract+tenure_interval+MultipleLines+SeniorCitizen,data=telecomdata,family=binomial(link="logit"))

# testData Preparation with input arguments from Java
#testData<-data.frame(PaymentMethod=factor(),OnlineSecurity=factor(),MonthlyCharges=double(),StreamingMovies=factor(),PaperlessBilling=factor(),StreamingTV=factor(),InternetService=factor(),Contract=factor(),tenure_interval=factor(),MultipleLines=factor(),SeniorCitizen=integer());
tenure_intr <- sapply(args[9],group_tenure)
tenure_intr <- as.factor(tenure_intr)
#args[1]<-substr(args[1],2,nchar(args[1])-1)
#args[7]<-substr(args[7],2,nchar(args[7])-1)
#args[8]<-substr(args[8],2,nchar(args[8])-1)
#x<-data.frame(args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],tenure_intr,args[10],args[11])
#names(x)<-c("PaymentMethod","OnlineSecurity","MonthlyCharges","StreamingMovies","PaperlessBilling","StreamingTV","InternetService","Contract","tenure_interval","MultipleLines","SeniorCitizen")
#testData<-rbind(testData,x)
testData<-data.frame(PaymentMethod=as.factor(args[1]),OnlineSecurity=as.factor(args[2]),MonthlyCharges=as.double(args[3]),StreamingMovies=as.factor(args[4]),PaperlessBilling=as.factor(args[5]),StreamingTV=as.factor(args[6]),InternetService=as.factor(args[7]),Contract=as.factor(args[8]),tenure_interval=as.factor(tenure_intr),MultipleLines=as.factor(args[10]),SeniorCitizen=as.integer(args[11]));
# Predict with the model and input args. If the prediction probability is greater than 0.5 then those 
# customers are classified as churned customer less than 0.5 are classified as not churning customer
test.predictionsstep_response <- predict(telecomModelstep_1,newdata=testData,type="response")
fitted_result <- ifelse(test.predictionsstep_response > 0.5,'Yes','No')
if(fitted_result == 'Yes'){
print(paste0(args[12]," is predicted to churn out of Verizon"))
}else{
print(paste0(args[12]," is predicted to stay with Verizon"))
}

#Random Forest with selection variables
#set.seed(415)
#mytree_sel<-randomForest(Churn ~ PaymentMethod+OnlineSecurity+MonthlyCharges+StreamingMovies+PaperlessBilling+StreamingTV+InternetService+Contract+tenure_interval+MultipleLines+SeniorCitizen,data=telecomdata,importance = T)
#pred_sel<-predict(mytree_sel, newdata =testData,type="response")
#fitted_result_2 <- ifelse(pred_sel > 0.5,'Yes','No')
#if(fitted_result_2 == 'Yes'){
#print("Customer is predicted to churn out of Verizon based on Model 2")
#}else{
#print("Customer is predicted to stay with Verizon based on Model 2")
#}
