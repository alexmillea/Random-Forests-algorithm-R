#Random Forest is created by aggregating trees
#Can be used for classification or regression
#Can deal with large number of features
#Avoids overfitting
#It deals with only parameters

# Download Data
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/Semester 2/Web Mining/CA2/BS-updated")

BikeDayCleaned<-read.csv("BikeDayCleaned.csv")
BikeHoursCleaned<-read.csv("BikeHourCleaned.csv")

#checking data structure
str(BikeDayCleaned)

#data partition
set.seed(123)
ind<-sample(2, nrow(BikeDayCleaned), replace=TRUE, prob=c(0.7,0.3))
train<-BikeDayCleaned[ind==1,]
test<-BikeDayCleaned[ind==2,]

#random forest
library(randomForest)
set.seed(222)
rf<-randomForest(count ~ season + year+ month+holiday + weekday+workingday+ temp +weather + atemp + humidity+windspeed, data=train)
print(rf)

#checking rf attributes
attributes(rf)

#prediction and actual data
library(caret)

prediction1<-predict(rf,train)
head(prediction1)
head(train$count)

#Graph
plot(prediction1)

#Get Percentages 
RMSE(prediction1, train$count) # Mean Squared Error (MSE)

#load library to use MAPE() function
library(MLmetrics)
MAPE(prediction1, train$count) #Mean Absolute Percentage Error

MAE(prediction1, train$count) #Mean Average Error

#prediction with test data
prediction2<-predict(rf,test)

#error rate of random forest
plot(rf)

#tune mtry
train2<-train[,2:15]

tuned<-tuneRF(train2[,-8], train2[,8],
       stepFactor = 0.5,
       plot=TRUE,
       ntreeTry = 300,
       trace=TRUE,
       improve=0.05)

