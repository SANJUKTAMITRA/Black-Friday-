#import the training and test data 

bf_train <- read.csv("blackfriday_train.csv",stringsAsFactors = FALSE,header = TRUE)
bf_test <- read.csv("blackfriday_test.csv",stringsAsFactors = FALSE,header = TRUE)

#data understanding and preparation

#check for duplicates

sum(duplicated(bf_train))

sum(duplicated(bf_test))

#no duplicate record exist

#check for missing values in training data

sum(is.na(bf_train))


sum(is.na(bf_train$Product_ID))
sum(is.na(bf_train$Gender))
sum(is.na(bf_train$Age))
sum(is.na(bf_train$Occupation))
sum(is.na(bf_train$City_Category))
sum(is.na(bf_train$Stay_In_Current_City_Years))
sum(is.na(bf_train$Marital_Status))
sum(is.na(bf_train$Product_Category_1))
sum(is.na(bf_train$Product_Category_2))
sum(is.na(bf_train$Product_Category_3))

#convert all NAs in th column Product_Category_1,Product_Category_2 and Product_Category_3 to zeros 

bf_train[which(is.na(bf_train$Product_Category_2)),10] <- 0
bf_train[which(is.na(bf_train$Product_Category_3)),11] <- 0

#check for missing values in training data
sum(is.na(bf_test$Product_ID))
sum(is.na(bf_test$Gender))
sum(is.na(bf_test$Age))
sum(is.na(bf_test$Occupation))
sum(is.na(bf_test$City_Category))
sum(is.na(bf_test$Stay_In_Current_City_Years))
sum(is.na(bf_test$Marital_Status))
sum(is.na(bf_test$Product_Category_1))
sum(is.na(bf_test$Product_Category_2))
sum(is.na(bf_test$Product_Category_3))


#convert all NAs in th column Product_Category_1,Product_Category_2 and Product_Category_3 to zeros 
bf_test[which(is.na(bf_test$Product_Category_2)),10] <- 0
bf_test[which(is.na(bf_test$Product_Category_3)),11] <- 0

#recheck for missing values
sum(is.na(bf_train))
#check for dimensions and struture 

dim(bf_train)
dim(bf_test)

str(bf_test)
str(bf_train)

#change the required variables to factor

bf_train$Gender <- as.factor(bf_train$Gender)
bf_test$Gender <- as.factor(bf_test$Gender)

levels((as.factor(bf_train$Age)))
levels((as.factor(bf_test$Age)))

bf_train$Age <- as.factor(bf_train$Age)
bf_test$Age <- as.factor(bf_test$Age)

levels(as.factor(bf_train$Occupation))
levels(as.factor(bf_test$Occupation))

bf_train$Occupation <- as.factor(bf_train$Occupation)
bf_train$City_Category <- as.factor(bf_train$City_Category)


bf_test$Occupation <- as.factor(bf_test$Occupation)
bf_test$City_Category <- as.factor(bf_test$City_Category)


levels(as.factor(bf_train$Stay_In_Current_City_Years))
levels(as.factor(bf_test$Stay_In_Current_City_Years))
bf_train$Stay_In_Current_City_Years <- as.factor(bf_train$Stay_In_Current_City_Years)
bf_test$Stay_In_Current_City_Years <- as.factor(bf_test$Stay_In_Current_City_Years)

bf_train$Marital_Status <- as.factor(bf_train$Marital_Status)
bf_test$Marital_Status <- as.factor(bf_test$Marital_Status)

#recheck the structure of bf_train and bf_test

str(bf_train)
str(bf_test)

#check if the User_ID + Product_ID column contain duplicate value
x <- bf_train[,c(1:2)]
sum(duplicated(x))
rm(x)
x <- bf_test[,c(1:2)]
sum(duplicated(x))

#so all the values of User_ID + Product_ID is unique and no duplicate value exist

#convert the purchase value to numeric

bf_train$Purchase <- as.numeric(bf_train$Purchase)

str(bf_train)
#create the model

library(randomForest)

#create train and test data
shuffledata <- bf_train[sample(nrow(bf_train)),]

indices <- as.integer(nrow(shuffledata)*0.03)
traindata <- shuffledata[1:indices,]
testdata <- shuffledata[((indices+1):50000),]
# Build the random forest
set.seed(121)

model.rf <- randomForest(formula=Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + Marital_Status+
                           Product_Category_1 + Product_Category_2 + Product_Category_3,data=traindata,mtry=7,proximity=FALSE,ntree=300,na.action = na.omit,do.trace=3)

model.rf
testPred <- predict(model.rf, newdata=testdata)
cor(testPred,testdata$Purchase)
cor(testPred,testdata$Purchase) ^ 2
#R2 value 62%

#RMSE value 

install.packages("Metrics")
library(Metrics)
rmse(testPred,testdata$Purchase)
