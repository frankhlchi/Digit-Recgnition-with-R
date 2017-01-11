library(kknn)
library(caret)
#import dataset
train <- read.csv("C:/Users/lenovo/Desktop/trainknn.csv", header=F)
test <- read.csv("C:/Users/lenovo/Desktop/testknn.csv", header=F)
#reduce column with near zero variance 
badCols <- nearZeroVar(train) 
print(paste("Fraction of nearZeroVar columns:", round(length(badCols)/length(train),4))) 
train <- train[, -badCols] 
test <- test[, -badCols] 
#save the label in training set and testing set as values 
label= train[,1]
out= test[1:2000,1]
#remove the label in training set and testing set 
train=train[,-1]
test=test[,-1]
# train the weighted knn 
error= c(1:27)
for(i in 1:27){
model <- kknn(as.factor(label) ~ ., train, test, k=i, kernel="triangular")
results <- model$fitted.values
tab= table(out, results)
#IF withou the loop, this command can give you the confusion matrix 
tab
#contrain the decimal place of error rate to four 
options(digit=4)
error[i]= 1-sum(diag(tab))/sum(tab)
}
print(error)
plot(error, type="o", col="red", main ="misclassification curve", xlab="k", ylab="testerror rate")

