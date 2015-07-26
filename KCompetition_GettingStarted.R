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
indepvars <-c('biddable','startprice', 'condition', 'cellular', 'storage','productline','carrier')
exclude <- c('UniqueID') # numerical variables to exclude from using all

f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
f2 <- paste(f1,paste(exclude,collapse=' - '),sep=' - ')

# We will just create a simple logistic regression model, to predict Sold using Price:
SimpleMod <- step(glm(f1, data=dfTrain, family=binomial))
model1 <- glm(f1, family=binomial, data=dfTrain1)
model2 <- glm(f1, family=binomial, data=dfTrain2)

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


# And then make predictions on the test set:
PredTest = predict(SimpleMod, newdata=dfTest, type="response")
model1pred <- predict(model1, newdata=dfTest1, type="response")
model2pred <- predict(model2, newdata=dfTest2, type="response")

# We can't compute the accuracy or AUC on the test set ourselves, since we don't have the dependent variable on the test set (you can compute it on the training set though!). 
# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):
MySubmission = data.frame(UniqueID = dfTest1$UniqueID, Probability1 = model1pred)
write.csv(MySubmission, "SubmissionSimpleLog1.csv", row.names=FALSE)
MySubmission = data.frame(UniqueID = dfTest2$UniqueID, Probability1 = model2pred)
write.csv(MySubmission, "SubmissionSimpleLog2.csv", row.names=FALSE)



# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition
# This model was just designed to help you get started - to do well in the competition, you will need to build better models!


# Load CART packages
library(rpart)
library(rpart.plot)

#treeFit <-rpart(isB ~ . - letter, data=dfTrain, method="class")
treeFit1 <-rpart(f1, data=dfTrain1, method="class")
treeFit2 <-rpart(f1, data=dfTrain2, method="class")

#Predict on Training
predictTrain1 = predict(treeFit1, type = "class")
cm <-table(dfTrain1$sold, predictTrain1)
addmargins(cm)
getstats(cm)

#redict on Traing
predictTrain2 = predict(treeFit2, type = "class")
cm <-table(dfTrain2$sold, predictTrain2)
addmargins(cm)
getstats(cm)

# And then make predictions on the test set:
PredTest1 = predict(treeFit1, newdata=dfTest1, type="prob")
PredTest2 = predict(treeFit2, newdata=dfTest2, type="prob")

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):
MySubmission = data.frame(UniqueID = dfTest1$UniqueID, Probability1 = PredTest1)
write.csv(MySubmission, "SubmissionSimpleRF1.csv", row.names=FALSE)
MySubmission = data.frame(UniqueID = dfTest2$UniqueID, Probability1 = PredTest2)
write.csv(MySubmission, "SubmissionSimpleRF2.csv", row.names=FALSE)
