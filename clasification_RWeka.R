library(RWeka)
#summary(iris)
# trainig with complete data
iris_j48 <- J48(Species ~ ., data = iris)

#iris_j48
summary(iris_j48)

# cross validation
eval_j48 <- evaluate_Weka_classifier(iris_j48, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
eval_j48
#WOW(J48)

# With Numbers
a <- 1:100
b <- a+2
data1 <- data.frame("No"=a, "Sq"=b)
#data1

#randomize rows befire training
data1 = data1[sample(nrow(data1), ), ]
#data1

# train
modelLR <- LinearRegression(data1$Sq ~ ., data = data1)
modelLR

# new variables for prediction
newdata1 <- data.frame("No"=c(11:15))
#newdata1

#predict
predictions <- predict(modelLR, newdata = newdata1)
summary(predictions)
