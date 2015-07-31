set.seed(7)
# load the library
library(mlbench)
library(caret)

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

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(sold~., data=DescriptionWordsTrain, method="rf", preProcess="scale", trControl=control)
# estimate variable importance
resultsTrain<- predict(model, newdata = DescriptionWordsTrain, type = "raw")

# Predict on Training
thres = .5
cm <- table(DescriptionWordsTrain$sold,resultsTrain>thres)
addmargins(cm)
getstats(cm)

resultsTest<- predict(model, newdata = DescriptionWordsTest, type = "raw")
MySubmission = data.frame(UniqueID = DescriptionWordsTest$UniqueID, Probability1 = resultsTest)
write.csv(MySubmission, "SubmissionDescriptionRFLetter.csv", row.names=FALSE)

