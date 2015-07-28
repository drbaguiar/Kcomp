# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.
# Load functions
source('C:/Users/bryan_000/Documents/GitHub/MyWork/functions.R')
# Set Directory
setwd("C:/Users/bryan_000/Documents/GitHub/Kcomp")


# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer 

# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields

dfTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
dfTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

# count blanks remove blanks
barplot(colSums(!is.na(dfTrain)))
colSums(!is.na(dfTrain))
#df <- na.omit(df)

#SPlit training on 
dfTrain1 <-subset(dfTrain,biddable==1)
dfTrain2 <-subset(dfTrain,biddable==0)

# Subset test on biddable
dfTest1 <-subset(dfTest,biddable==1)
dfTest2 <-subset(dfTest,biddable==0)

# Subset training on celuar
dfTrain3 <-subset(dfTrain1,cellular==1)
dfTrain4 <-subset(dfTrain2,cellular==1)
dfTrain5 <-subset(dfTrain1,cellular==0)
dfTrain6 <-subset(dfTrain2,cellular==0)
dfTrain7 <-subset(dfTrain1,cellular=="Unknown")
dfTrain8 <-subset(dfTrain2,cellular=="Unknown")

# Subset test on cellular
dfTest3 <-subset(dfTest1,cellular==1)
dfTest4 <-subset(dfTest2,cellular==1)
dfTest5 <-subset(dfTest1,cellular==0)
dfTest6 <-subset(dfTest2,cellular==0)
dfTest7 <-subset(dfTest1,cellular=="Unknown")
dfTest8 <-subset(dfTest2,cellular=="Unknown")

# Baseline on Training data 
# Determine the Majority
bl <-table(dfTrain$sold)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority
predictTrainBase <-rep(majority,nrow(dfTrain))
#Compare
cm <- table(dfTrain$sold,predictTrainBase, exclude=NULL)
addmargins(cm)
getstats(cm)

# Dep and Independent Vars define columns we will be working with
depvar <- 'sold'
indepvars1 <-c('biddable','startprice', 'cellular','condition', 'storage','productline','carrier')
indepvars2 <-c('startprice', 'cellular','condition', 'storage','productline','carrier')
indepvars3 <-c('startprice', 'condition', 'storage','productline','carrier')
indepvars4 <-c('startprice', 'condition', 'storage','productline')
exclude <- c('UniqueID', 'biddable','cellular','carrier') # numerical variables to exclude from using all

f1 <- paste(depvar,paste(indepvars1,collapse=' + '),sep=' ~ ')
f2 <- paste(depvar,paste(indepvars2,collapse=' + '),sep=' ~ ')
f3 <- paste(depvar,paste(indepvars3,collapse=' + '),sep=' ~ ')
f4 <- paste(depvar,paste(indepvars4,collapse=' + '),sep=' ~ ')

#f2 <- paste(f1,paste(exclude,collapse=' - '),sep=' - ')

# We will just create a simple logistic regression model, to predict Sold using Price:
SimpleMod <- step(glm(f1, data=dfTrain, family=binomial))
model1 <- glm(f2, family=binomial, data=dfTrain1)
model2 <- glm(f2, family=binomial, data=dfTrain2)
model3 <- glm(f3, family=binomial, data=dfTrain3)
model4 <- glm(f3, family=binomial, data=dfTrain4)
model5 <- glm(f4, family=binomial, data=dfTrain5)
model6 <- glm(f4, family=binomial, data=dfTrain6)
model7 <- glm(f4, family=binomial, data=dfTrain7)
model8 <- glm(f4, family=binomial, data=dfTrain8)

# Predict on Training
thres = .5
predictTrain <- predict(SimpleMod, type="response")
cm <- table(dfTrain$sold,predictTrain>thres)
addmargins(cm)
getstats(cm)

#Predict separate for biddable
thres = .5
predictTrain1 <- predict(model1, type="response")
cm1 <- table(dfTrain1$sold,predictTrain1>thres)
addmargins(cm1)
getstats(cm1)

thres = .5
predictTrain2 <- predict(model2, type="response")
cm2 <- table(dfTrain2$sold,predictTrain2>thres)
addmargins(cm2)
getstats(cm2)

#Predict separate for cellular
thres = .5
predictTrain3 <- predict(model3, type="response")
cm3 <- table(dfTrain3$sold,predictTrain3>thres)
addmargins(cm3)
getstats(cm3)

thres = .5
predictTrain4 <- predict(model4, type="response")
cm4 <- table(dfTrain4$sold,predictTrain4>thres)
addmargins(cm4)
getstats(cm4)

thres = .5
predictTrain5 <- predict(model5, type="response")
cm5 <- table(dfTrain5$sold,predictTrain5>thres)
addmargins(cm5)
getstats(cm5)

thres = .5
predictTrain6 <- predict(model6, type="response")
cm6 <- table(dfTrain6$sold,predictTrain6>thres)
addmargins(cm6)
getstats(cm6)

thres = .5
predictTrain7 <- predict(model7, type="response")
cm7 <- table(dfTrain7$sold,predictTrain7>thres)
addmargins(cm7)
getstats(cm7)

thres = .5
predictTrain8 <- predict(model8, type="response")
cm8 <- table(dfTrain8$sold,predictTrain8>thres)
addmargins(cm8)
getstats(cm8)

dfTest4$storage <- replace(dfTest4$storage, dfTest4$storage == "Unknown",16) 
dfTest4$carrier <- replace(dfTest4$carrier, dfTest4$carrier == "None","Unknown") 

# And then make predictions on the test set:
PredTest = predict(SimpleMod, newdata=dfTest, type="response")
model1pred <- predict(model1, newdata=dfTest1, type="response")
model2pred <- predict(model2, newdata=dfTest2, type="response")
model3pred <- predict(model3, newdata=dfTest3, type="response")
model4pred <- predict(model4, newdata=dfTest4, type="response")
model5pred <- predict(model5, newdata=dfTest5, type="response")
model6pred <- predict(model6, newdata=dfTest6, type="response")
model7pred <- predict(model7, newdata=dfTest7, type="response")
model8pred <- predict(model8, newdata=dfTest8, type="response")

# We can't compute the accuracy or AUC on the test set ourselves, since we don't have the dependent variable on the test set (you can compute it on the training set though!). 
# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):
MySubmission1 = data.frame(UniqueID = dfTest1$UniqueID, Probability1 = model1pred)
write.csv(MySubmission1, "SubmissionSimpleLog1.csv", row.names=FALSE)

MySubmission2 = data.frame(UniqueID = dfTest2$UniqueID, Probability1 = model2pred)
write.csv(MySubmission2, "SubmissionSimpleLog2.csv", row.names=FALSE)

MySubmission3 = data.frame(UniqueID = dfTest3$UniqueID, Probability1 = model3pred)
write.csv(MySubmission3, "SubmissionSimpleLog3.csv", row.names=FALSE)

MySubmission4 = data.frame(UniqueID = dfTest4$UniqueID, Probability1 = model4pred)
write.csv(MySubmission4, "SubmissionSimpleLog4.csv", row.names=FALSE)

MySubmission5 = data.frame(UniqueID = dfTest5$UniqueID, Probability1 = model5pred)
write.csv(MySubmission5, "SubmissionSimpleLog5.csv", row.names=FALSE)

MySubmission6 = data.frame(UniqueID = dfTest6$UniqueID, Probability1 = model6pred)
write.csv(MySubmission6, "SubmissionSimpleLog6.csv", row.names=FALSE)

MySubmission7 = data.frame(UniqueID = dfTest7$UniqueID, Probability1 = model7pred)
write.csv(MySubmission7, "SubmissionSimpleLog7.csv", row.names=FALSE)

MySubmission8 = data.frame(UniqueID = dfTest8$UniqueID, Probability1 = model8pred)
write.csv(MySubmission8, "SubmissionSimpleLog8.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition
# This model was just designed to help you get started - to do well in the competition, you will need to build better models!


# Load CART packages
library(rpart)
library(rpart.plot)

#treeFit <-rpart(isB ~ . - letter, data=dfTrain, method="class")
treeFit1 <-rpart(f2, data=dfTrain1, method="class")
treeFit2 <-rpart(f2, data=dfTrain2, method="class")
treeFit3 <-rpart(f3, data=dfTrain3, method="class")
treeFit4 <-rpart(f3, data=dfTrain4, method="class")
treeFit5 <-rpart(f4, data=dfTrain5, method="class")
treeFit6 <-rpart(f4, data=dfTrain6, method="class")
treeFit7 <-rpart(f4, data=dfTrain7, method="class")
treeFit8 <-rpart(f4, data=dfTrain8, method="class")

#Predict on Training
predictTrain1 = predict(treeFit1, type = "class")
cm1 <-table(dfTrain1$sold, predictTrain1)
addmargins(cm1)
getstats(cm1)

#Predict on Traing
predictTrain2 = predict(treeFit2, type = "class")
cm2 <-table(dfTrain2$sold, predictTrain2)
addmargins(cm2)
getstats(cm2)

#Predict on Traing
predictTrain3 = predict(treeFit3, type = "class")
cm3 <-table(dfTrain3$sold, predictTrain3)
addmargins(cm3)
getstats(cm3)

#Predict on Traing
predictTrain4 = predict(treeFit4, type = "class")
cm4 <-table(dfTrain4$sold, predictTrain4)
addmargins(cm4)
getstats(cm4)

#Predict on Traing
predictTrain5 = predict(treeFit5, type = "class")
cm5 <-table(dfTrain5$sold, predictTrain5)
addmargins(cm5)
getstats(cm5)

#Predict on Traing
predictTrain6 = predict(treeFit6, type = "class")
cm6 <-table(dfTrain6$sold, predictTrain6)
addmargins(cm6)
getstats(cm6)

#Predict on Traing
predictTrain7 = predict(treeFit7, type = "class")
cm7 <-table(dfTrain7$sold, predictTrain7)
addmargins(cm7)
getstats(cm7)

#Predict on Traing
predictTrain8 = predict(treeFit8, type = "class")
cm8 <-table(dfTrain8$sold, predictTrain8)
addmargins(cm8)
getstats(cm8)

# And then make predictions on the test set:
PredTest1 = predict(treeFit1, newdata=dfTest1, type="prob")
PredTest2 = predict(treeFit2, newdata=dfTest2, type="prob")
PredTest3 = predict(treeFit3, newdata=dfTest3, type="prob")
PredTest4 = predict(treeFit4, newdata=dfTest4, type="prob")
PredTest5 = predict(treeFit5, newdata=dfTest5, type="prob")
PredTest6 = predict(treeFit6, newdata=dfTest6, type="prob")
PredTest7 = predict(treeFit7, newdata=dfTest7, type="prob")
PredTest8 = predict(treeFit8, newdata=dfTest8, type="prob")

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):
MySubmission1 = data.frame(UniqueID = dfTest1$UniqueID, Probability1 = PredTest1[,2])
write.csv(MySubmission1, "SubmissionSimpleRF1.csv", row.names=FALSE)

MySubmission2 = data.frame(UniqueID = dfTest2$UniqueID, Probability1 = PredTest2[,2])
write.csv(MySubmission2, "SubmissionSimpleRF2.csv", row.names=FALSE)

MySubmission3 = data.frame(UniqueID = dfTest3$UniqueID, Probability1 = PredTest3[,2])
write.csv(MySubmission3, "SubmissionSimpleRF3.csv", row.names=FALSE)

MySubmission4 = data.frame(UniqueID = dfTest4$UniqueID, Probability1 = PredTest4[,2])
write.csv(MySubmission4, "SubmissionSimpleRF4.csv", row.names=FALSE)

MySubmission5 = data.frame(UniqueID = dfTest5$UniqueID, Probability1 = PredTest5[,2])
write.csv(MySubmission5, "SubmissionSimpleRF5.csv", row.names=FALSE)

MySubmission6 = data.frame(UniqueID = dfTest6$UniqueID, Probability1 = PredTest6[,2])
write.csv(MySubmission6, "SubmissionSimpleRF6.csv", row.names=FALSE)

MySubmission7 = data.frame(UniqueID = dfTest7$UniqueID, Probability1 = PredTest7[,2])
write.csv(MySubmission7, "SubmissionSimpleRF7.csv", row.names=FALSE)

MySubmission8 = data.frame(UniqueID = dfTest8$UniqueID, Probability1 = PredTest8[,2])
write.csv(MySubmission8, "SubmissionSimpleRF8.csv", row.names=FALSE)

