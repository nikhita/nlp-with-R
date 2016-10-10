# Load the data from the csv file
setwd("/home/prakash/Dropbox/myShare/Courses/RCourse/TextAnalytics/code/")
data <- read.table('./smsspamcollection/SMSSpamCollection.csv', sep="\t", header = T)
head(data)
# Create the document term matrix
require(RTextTools)
data <- data[sample(nrow(data)),]
# training Data
traindata <- data[1:20,]
traindata$Class
# testing data
testdata <- data[21:25,]

dtMatrix <- create_matrix(traindata["Text"])
dtMatrix

# Configure the training data
container <- create_container(dtMatrix, data$Class, trainSize=1:20, virgin=FALSE)

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)

# test data
predictionData <- testdata$Text
predMatrix <- create_matrix(predictionData, originalMatrix = dtMatrix)
predMatrix
# create a prediction document term matrix
predMatrix <- create_matrix(predictionData, originalMatrix = dtMatrix)
predMatrix

# create the corresponding container
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

# predict
results <- classify_model(predictionContainer, model)
results
testdata$Class
table(testdata$Class,results$SVM_LABEL)
