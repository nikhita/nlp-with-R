library(e1071)
data(iris)
model <- naiveBayes(Species ~ ., data = iris)
#model
#class(iris)
pData <- iris[sample(nrow(iris),),]
#pData
pData <- pData[10:20,]
#pData$Species
preds <- predict(model, pData)
table(pData$Species, preds)
