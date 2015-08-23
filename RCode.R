library('caret')
setwd('D:/Liang Tang/Scalable Machine Learning')
# import data
data = read.csv('pml-training.csv')
true_testing = read.csv('pml-testing.csv')
# Filtering the variables
data = data[,-1]
dataFilter = data[,c(7:9,36:48,59:67,83:85,112:123,150:159)]
dataFilter = dataFilter[, colSums(is.na(dataFilter)) == 0 ]
sum(complete.cases(dataFilter))

# Create training and testing data set
inTrain = createDataPartition(y = dataFilter$classe, p = 0.7, list = FALSE)
training = dataFilter[inTrain,]
testing = dataFilter[-inTrain,]

# Create random forest model based on training data
rf1 <- randomForest(classe~., data=training, mtry=7, importance = TRUE)

# Calculate training error
prediction_train = predict(rf1, training)
table(training$classe,prediction_train)

# Calculate out of sample error
prediction_test = predict(rf1, testing)
table(testing$classe,prediction_test)

# Create random forest model using all training data
rf <- randomForest(classe~., data=training, mtry=7, importance = TRUE)

# predict for the 20 samples
answers = predict(rf, true_testing)

# Create files for each of teh 20 samples
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
