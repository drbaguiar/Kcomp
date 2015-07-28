# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

# This script file is intended to help you deal with the text data provided in the competition data files

# If you haven't already, start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer 

# Load functions
source('C:/Users/bryan_000/Documents/GitHub/MyWork/functions.R')

# Set Directory
setwd("C:/Users/bryan_000/Documents/GitHub/Kcomp")

# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields
dfTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
dfTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

# Now, let's load the "tm" package.
library(tm)

# Then create a corpus from the description variable. You can use other variables in the dataset for text analytics, but we will just show you how to use this particular variable. 
# Note that we are creating a corpus out of the training and testing data.
CorpusDescription = Corpus(VectorSource(c(dfTrain$description, dfTest$description)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)

# Remember this extra line is needed after running the tolower step:
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!
dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.99)
DescriptionWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:
colnames(DescriptionWords) = make.names(colnames(DescriptionWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of DescriptionWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(eBayTrain) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTrain"
DescriptionWordsTrain = head(DescriptionWords, nrow(dfTrain))

# The tail function takes the last "n" rows of DescriptionWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(eBayTest) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTest"
DescriptionWordsTest = tail(DescriptionWords, nrow(dfTest))

# Note that this split of DescriptionWords works to properly put the observations back into the training and testing sets, because of how we combined them together when we first made our corpus.

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!
DescriptionWordsTrain$sold = dfTrain$sold
DescriptionWordsTrain$WordCount = dfTrain$WordCount
DescriptionWordsTest$WordCount =dfTest$WordCount

# Added Back as Well
DescriptionWordsTrain$biddable <-dfTrain$biddable
DescriptionWordsTest$biddable <-dfTest$biddable
DescriptionWordsTrain$startprice <-dfTrain$startprice
DescriptionWordsTest$startprice <- dfTest$startprice
DescriptionWordsTrain$cellular <- dfTrain$cellular
DescriptionWordsTest$cellular <- dfTest$cellular
DescriptionWordsTrain$storage <- dfTrain$storage
DescriptionWordsTest$storage <- dfTest$storage
DescriptionWordsTest$UniqueID <- dfTest$UniqueID
DescriptionWordsTrain$carrier <- dfTrain$carrier
DescriptionWordsTest$carrier <- dfTest$carrier
DescriptionWordsTrain$condition <- dfTrain$condition
DescriptionWordsTest$condition <- dfTest$condition
DescriptionWordsTrain$productline <- dfTrain$productline
DescriptionWordsTest$productline <- dfTest$productline

#SPlit training on biddable
dfTrain1 <-subset(DescriptionWordsTrain,biddable==1)
dfTrain2 <-subset(DescriptionWordsTrain,biddable==0)

# Subset test on biddable
dfTest1 <-subset(DescriptionWordsTest,biddable==1)
dfTest2 <-subset(DescriptionWordsTest,biddable==0)

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

# Remember that you can always look at the structure of these data frames to understand what we have created
# Baseline on Training data 
# Determine the Majority
bl <-table(DescriptionWordsTrain$sold)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority
predictTrainBase <-rep(majority,nrow(DescriptionWordsTrain))
#Compare
cm <- table(DescriptionWordsTrain$sold,predictTrainBase, exclude=NULL)
addmargins(cm)
getstats(cm)


# Dep and Independent Vars define columns we will be working with
depvar <- 'sold'
indepvars1 <-c('.')
exclude1 <- c('biddable') # numerical variables to exclude from using all
exclude2 <- c('biddable','cellular') # numerical variables to exclude from using all
exclude3 <- c('biddable','cellular', 'carrier') # numerical variables to exclude from using all

f1 <- paste(depvar,paste(indepvars1,collapse=' + '),sep=' ~ ')
f2 <- paste(f1,paste(exclude1,collapse=' - '),sep=' - ')
f3 <- paste(f1,paste(exclude2,collapse=' - '),sep=' - ')
f4 <- paste(f1,paste(exclude3,collapse=' - '),sep=' - ')

# Now let's create a logistic regression model:
Log  = glm(f1, data=DescriptionWordsTrain, family=binomial)
Log1 = glm(f2, data=dfTrain1, family=binomial)
Log2 = glm(f2, data=dfTrain2, family=binomial)
Log3 = glm(f3, data=dfTrain3, family=binomial)
Log4 = glm(f3, data=dfTrain4, family=binomial)
Log5 = glm(f4, data=dfTrain5, family=binomial)
Log6 = glm(f4, data=dfTrain6, family=binomial)
Log7 = glm(f4, data=dfTrain7, family=binomial)
Log8 = glm(f4, data=dfTrain8, family=binomial)

# Predict on Training
thres = .5
predictTrain <- predict(Log, type="response")
cm <- table(DescriptionWordsTrain$sold,predictTrain>thres)
addmargins(cm)
getstats(cm)

thres = .5
predictTrain1 <- predict(Log1, type="response")
cm1 <- table(dfTrain1$sold,predictTrain1>thres)
addmargins(cm1)
getstats(cm1)

thres = .5
predictTrain2 <- predict(Log2, type="response")
cm2 <- table(dfTrain2$sold,predictTrain2>thres)
addmargins(cm2)
getstats(cm2)

thres = .5
predictTrain3 <- predict(Log3, type="response")
cm3 <- table(dfTrain3$sold,predictTrain3>thres)
addmargins(cm3)
getstats(cm3)

thres = .5
predictTrain4 <- predict(Log4, type="response")
cm4 <- table(dfTrain4$sold,predictTrain4>thres)
addmargins(cm4)
getstats(cm4)

thres = .5
predictTrain5 <- predict(Log5, type="response")
cm5 <- table(dfTrain5$sold,predictTrain5>thres)
addmargins(cm5)
getstats(cm5)

thres = .5
predictTrain6<- predict(Log6, type="response")
cm6 <- table(dfTrain6$sold,predictTrain6>thres)
addmargins(cm6)
getstats(cm6)

thres = .5
predictTrain7 <- predict(Log7, type="response")
cm7 <- table(dfTrain7$sold,predictTrain7>thres)
addmargins(cm7)
getstats(cm7)

thres = .5
predictTrain8 <- predict(Log8, type="response")
cm8 <- table(dfTrain8$sold,predictTrain8>thres)
addmargins(cm8)
getstats(cm8)

# And make predictions on our test set:
PredTest = predict(Log, newdata=DescriptionWordsTest, type="response")
PredTest1 = predict(Log1, newdata=dfTest1, type="response")
PredTest2 = predict(Log2, newdata=dfTest2, type="response")
PredTest3 = predict(Log3, newdata=dfTest3, type="response")
PredTest4 = predict(Log4, newdata=dfTest4, type="response")
PredTest5 = predict(Log5, newdata=dfTest5, type="response")
PredTest6 = predict(Log6, newdata=dfTest6, type="response")
PredTest7 = predict(Log7, newdata=dfTest7, type="response")
PredTest8 = predict(Log8, newdata=dfTest8, type="response")

# Now we can prepare our submission file for Kaggle:
MySubmission = data.frame(UniqueID = DescriptionWordsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionDescriptionLogLetter.csv", row.names=FALSE)

MySubmission1 = data.frame(UniqueID = dfTest1$UniqueID, Probability1 = PredTest1)
write.csv(MySubmission1, "SubmissionDescriptionLogLetter1.csv", row.names=FALSE)

MySubmission2 = data.frame(UniqueID = dfTest2$UniqueID, Probability1 = PredTest2)
write.csv(MySubmission2, "SubmissionDescriptionLogLetter2.csv", row.names=FALSE)

MySubmission3 = data.frame(UniqueID = dfTest3$UniqueID, Probability1 = PredTest3)
write.csv(MySubmission3, "SubmissionDescriptionLogLetter3.csv", row.names=FALSE)

MySubmission4 = data.frame(UniqueID = dfTest4$UniqueID, Probability1 = PredTest4)
write.csv(MySubmission4, "SubmissionDescriptionLogLetter4.csv", row.names=FALSE)

MySubmission5 = data.frame(UniqueID = dfTest5$UniqueID, Probability1 = PredTest5)
write.csv(MySubmission5, "SubmissionDescriptionLogLetter5.csv", row.names=FALSE)

MySubmission6 = data.frame(UniqueID = dfTest6$UniqueID, Probability1 = PredTest6)
write.csv(MySubmission6, "SubmissionDescriptionLogLetter6.csv", row.names=FALSE)

MySubmission7 = data.frame(UniqueID = dfTest7$UniqueID, Probability1 = PredTest7)
write.csv(MySubmission7, "SubmissionDescriptionLogLetter7.csv", row.names=FALSE)

MySubmission8 = data.frame(UniqueID = dfTest8$UniqueID, Probability1 = PredTest8)
write.csv(MySubmission8, "SubmissionDescriptionLogLetter8.csv", row.names=FALSE)


# Stepwise
Step = step(glm(f1, data=DescriptionWordsTrain, family=binomial))
Step1 = step(glm(f2, data=dfTrain1, family=binomial))
Step2 = step(glm(f2, data=dfTrain2, family=binomial))
Step3 = step(glm(f3, data=dfTrain3, family=binomial))
Step4 = step(glm(f3, data=dfTrain4, family=binomial))
Step5 = step(glm(f4, data=dfTrain5, family=binomial))
Step6 = step(glm(f4, data=dfTrain6, family=binomial))
Step7 = step(glm(f4, data=dfTrain7, family=binomial))
Step8 = step(glm(f4, data=dfTrain8, family=binomial))

thres = .5
predictTrainStep <- predict(Step4, type="response")
cm <- table(dfTrain4$sold,predictTrainStep>thres)
addmargins(cm)
getstats(cm)


# And make predictions on our test set:
PredTest = predict(DescriptionWordsStep, newdata=DescriptionWordsTest, type="response")

# Now we can prepare our submission file for Kaggle:
MySubmission = data.frame(UniqueID = DescriptionWordsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionDescriptionStep.csv", row.names=FALSE)

# This script file was just designed to help you get started - to do well in the competition, you will need to build better models!
# Load CART packages
library(rpart)
library(rpart.plot)

#treeFit <-rpart(isB ~ . - letter, data=dfTrain, method="class")
treeFit <-rpart(f4, data=dfTrain8, method="class")

thres<-.5
predictTrain = predict(treeFit, type = "prob")
cm <-table(dfTrain8$sold, predictTrain[,2]>thres)
addmargins(cm)
getstats(cm)

# And then make predictions on the test set:
PredTest = predict(treeFit, newdata=DescriptionWordsTest, type="prob")

# Now we can prepare our submission file for Kaggle:
MySubmission = data.frame(UniqueID = DescriptionWordsTest$UniqueID, Probability1 = PredTest[,2])
write.csv(MySubmission, "SubmissionDescriptionTree.csv", row.names=FALSE)

# Random Forest
library(caret)
ctrl <- trainControl(method = "repeatedcv", repeats = 3)

rf_model<-train(sold ~ X100 + box + hous + includ  + may + mint + perfect + sign + still+ wear + wifi + biddable + startprice + cellular + storage + groups.1,data=DescriptionWordsTrain,method="rf",trControl=ctrl,prox=TRUE,allowParallel=TRUE)
rf_model$finalModel

predictTrain = predict(rf_model$finalModel)
cm <-table(dfTrain$sold, predictTrain>thres)
addmargins(cm)
getstats(cm)

# And then make predictions on the test set:
PredTest = predict(rf_model$finalModel, newdata=DescriptionWordsTest)

# Now we can prepare our submission file for Kaggle:
MySubmission = data.frame(UniqueID = DescriptionWordsTest$UniqueID, Probability1 = PredTest[,2])
write.csv(MySubmission, "SubmissionDescriptionTree.csv", row.names=FALSE)




# K-Means Cluster Analysis
fit <- kmeans(DescriptionWordsTrain[-73], 5) # 5 cluster solution
fit# get cluster means 
aggregate(mydata[,-'sold'],by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

# append cluster assignment
DescriptionWordsTrain <- data.frame(DescriptionWordsTrain, groups)

fit2 <- glm(sold ~ X100 + box + hous + includ  + may + mint + perfect + sign + still+ wear + wifi + biddable + startprice + cellular + storage + groups.1,data=DescriptionWordsTrain, family=binomial)
# Predict on Training
thres = .5
predictTrainStep <- predict(fit2, type="response")
cm <- table(DescriptionWordsTrain$sold,predictTrainStep>thres)
addmargins(cm)
getstats(cm)
summary(fit2)
