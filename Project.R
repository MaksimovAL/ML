library(abind)
library(arm)
library(caret)
library(kernlab)
library(klaR)
library(rattle)
library(randomForest)
library(rpart)


set.seed(1234)

##  Creation of the data.frame

# Setting train and testing datasets' urls
urlTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTest <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"


#File retrieval and reading:

csvTrain <- "pml-training.csv"

if (file.exists(csvTrain)) {
  train <- read.csv(csvTrain, na.strings=c("NA","#DIV/0!",""))
} else { 
  download.file(urlTrain,csvTrain)
  train <- read.csv(csvTrain, na.strings=c("NA","#DIV/0!",""))
}                           

csvTest <-  "pml-testing.csv"

if (file.exists(csvTest)) {
  test <- read.csv(csvTest, na.strings=c("NA","#DIV/0!",""))
} else { 
  download.file(urlTest,csvTest)
  test <- read.csv(csvTest, na.strings=c("NA","#DIV/0!",""))
}   

#Names's Coherence Check
all.equal(colnames(test)[1:length(colnames(test))-1], colnames(train)[1:length(colnames(train))-1])

#Varibles with Near Zero Variance are removed
nearzero <- nearZeroVar(train, saveMetrics = TRUE)
train <- train[, !nearzero$nzv]

#Variables with more than 80% missing values are removed
toberem <- sapply(colnames(train), function(x) if(sum(is.na(train[, x])) > 0.8*nrow(train)){return(TRUE)}else{return(FALSE)})
train <- train[, !toberem]

#Variables related with data acquisition ( like timestamps and individual
# names) are not suitable to be used in prediction and are removed

train <- train[, -(1:6)]

#Exploratory correlation analysis
corr <- abs(sapply(colnames(train[, -ncol(train)]), function(x) cor(as.numeric(train[, x]), as.numeric(train$classe), method = "spearman")))
Hcorr<- caret::findCorrelation(cor(train[, 1:52]), cutoff=0.8)
names(train[Hcorr])

#Many variables are highly correlated. PCA will be used in the pre-processing.
#After the data cleaning the variabe selected to specify the model are:

names(train)

# In order to avoid overfitting and to reduce out of sample errors, TrainControl is used
# to perform 10-fold cross validation.

tc <- trainControl(method = "cv", number = 10,  allowParallel = TRUE, verboseIter = TRUE)

#five models are estimated: Random forest, Support Vector Machine (both radial and linear), a Neural net and a Bayes Generalized linear model

rf <- train(classe ~ ., data = train, method = "rf", preProcess="pca", trControl = tc)
svmr <- train(classe ~ ., data = train, method = "svmRadial", preProcess="pca", trControl = tc)
NN <- train(classe ~ ., data = train, method = "nnet", preProcess="pca", trControl = tc)
svml <- train(classe ~ ., data = train, method = "svmLinear", preProcess="pca", trControl = tc)
bayesglm <- train(classe ~ ., data = train, method = "bayesglm", preProcess="pca")

#Accuracy comparision

model <- c("Random Forest", "SVM (radial)","Neural Net" ,"SVM (linear)", "Bayes GLM")
Accuracy <- c(max(rf$results$Accuracy),
              max(svmr$results$Accuracy),
              max(NN$results$Accuracy),
              max(svml$results$Accuracy),
              max(bayesglm$results$Accuracy))

Kappa <- c(max(rf$results$Kappa),
           max(svmr$results$Kappa),
           max(NN$results$Kappa),
           max(svml$results$Kappa),
           max(bayesglm$results$Kappa))


performance <- rbind(model,Accuracy,Kappa)

performance

#Random forest and SVM(radial) provide the best results and will provide the predictions
#for the submission

#Predicting variable classe for the test set

rfPred <- predict(rf, test)
svmrPred <- predict(svmr, test)
#NNPred <- predict(NN, test)

#Checking if the models give same predictions

prediction <- data.frame(cbind(rfPred, svmrPred))
prediction$same <- with(prediction, rfPred == svmrPred)
colnames(prediction) <- c("Random Forest", "SVM (radial)", "Same Prediction")
prediction



#Generation of the files to be submitted is made through the provided function
```{r}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(rfPred)
pml_write_files(svmrPred)
```