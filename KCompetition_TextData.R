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
DescriptionWordsTrain1 <-subset(DescriptionWordsTrain,biddable==1)
DescriptionWordsTrain2 <-subset(DescriptionWordsTrain,biddable==0)

# Subset test on biddable
DescriptionWordsTest1 <-subset(DescriptionWordsTest,biddable==1)
DescriptionWordsTest2 <-subset(DescriptionWordsTest,biddable==0)

# Remember that you can always look at the structure of these data frames to understand what we have created
# Baseline on Training data 
# Determine the Majority
bl <-table(DescriptionWordsTrain1$sold)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority
predictTrainBase <-rep(majority,nrow(DescriptionWordsTrain1))
#Compare
cm <- table(DescriptionWordsTrain1$sold,predictTrainBase, exclude=NULL)
addmargins(cm)
getstats(cm)

# Now let's create a logistic regression model using all of the variables:
DescriptionWordsLog1 = glm(sold ~ ., data=DescriptionWordsTrain1, family=binomial)
DescriptionWordsLog2 = glm(sold ~ ., data=DescriptionWordsTrain2, family=binomial)

# Predict on Training
thres = .5
predictTrain1 <- predict(DescriptionWordsLog1, type="response")
cm <- table(DescriptionWordsTrain1$sold,predictTrain1>thres)
addmargins(cm)
getstats(cm)

thres = .5
predictTrain2 <- predict(DescriptionWordsLog2, type="response")
cm <- table(DescriptionWordsTrain2$sold,predictTrain2>thres)
addmargins(cm)
getstats(cm)


# And make predictions on our test set:
PredTest1 = predict(DescriptionWordsLog1, newdata=DescriptionWordsTest1, type="response")
PredTest2 = predict(DescriptionWordsLog2, newdata=DescriptionWordsTest2, type="response")

# Now we can prepare our submission file for Kaggle:
MySubmission = data.frame(UniqueID = DescriptionWordsTest1$UniqueID, Probability1 = PredTest1)
write.csv(MySubmission, "SubmissionDescriptionLogLetter1.csv", row.names=FALSE)
MySubmission = data.frame(UniqueID = DescriptionWordsTest2$UniqueID, Probability1 = PredTest2)
write.csv(MySubmission, "SubmissionDescriptionLogLetter2.csv", row.names=FALSE)



# Stepwise
DescriptionWordsStep = step(glm(sold ~ ., data=DescriptionWordsTrain, family=binomial))

# Predict on Training
thres = .5
predictTrainStep <- predict(DescriptionWordsStep, type="response")
cm <- table(DescriptionWordsTrain$sold,predictTrainStep>thres)
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
treeFit <-rpart(sold ~ X100 + box + hous + includ  + may + mint + perfect + sign + still+ wear + wifi + biddable + startprice + cellular + storage + groups.1, data=DescriptionWordsTrain, method="class")

predictTrain = predict(treeFit, type = "prob")
cm <-table(dfTrain$sold, predictTrain[,2]>thres)
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
