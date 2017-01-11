#this R code is written by Hongliang Chi SID:4300283388
#this is for the part of Random Forest and Support Vector Machine of project handwritten recognition
#if have any questions please email me email:hchi3573@uni.sydney.edu.au
#the Programmer of this code have adequate experience on programming 
#if you want a RA with strong programming skill,  please consider me   

#PART ONE of RF

#build up model with different number of trees
library(randomForest)
trains <- read.csv("~/Desktop/qbus3820/train.csv") #please convert the address to your training set's address 
labels <- as.factor(trains[1:1000,1])
train <- trains[1:1000,-1]
test=trains[1001:1600,-1]
outcome = trains[1001:1600,1]
error= c(1:1000)
totalerror =c(1:20)
#the following for loop will print out 20 confusion matrices of model with increasing trees
for(n in seq(50, 1000, 50) ){ #increase number of trees from 50 to 1000 by 50
  rf <- randomForest(train, labels, xtest=test, ntree=n)
  predictions <- levels(labels)[rf$test$predicted]
  error[n]=1-(sum(diag(table(predictions,outcome)))/600) #put error rate into a vector
  print(table(predictions,outcome))#print out confusion matrices
}
# convert error vector into a unit error vector 
m=1
for(n in seq(50, 1000, 50) ){ 
  totalerror[m]= error[n]
  print(error[n])
  m=m+1
}
#plot the comparison plot of error rate about different trees
plot(totalerror,xlab = "Number of trees (*50)",ylab="Error rate",pch=19,col=c("red"),main="Error rate comparison among models with increasing trees")
lines(totalerror)

#PART TWO of RF
#build up model with different number of variable used to split the node
for(n in seq(50, 700, 50) ){
  rf <- randomForest(train, labels, xtest=test, ntree=100,mtry=n)
  predictions <- levels(labels)[rf$test$predicted]
  print(table(predictions,outcome)) # print out confusion matrix
  error[n]=1-(sum(diag(table(predictions,outcome)))/600)
  
}
totalerrortwo=c(1:14)
j=1
for(n in seq(50, 700, 50) ){
  totalerrortwo[j]= error[n]
  j=j+1
}
plot(totalerrortwo,xlab = "mtry (*50)",ylab="Error rate",pch=19,col=c("red"),main="comparison on error rate among models with different number of variables")
lines(totalerrortwo) #plot th comparison plot of error rate about different number of variables 


#SVM part

trains <- read.csv("~/Desktop/qbus3820/train.csv")
library(kernlab)
train <- trains[1:1000,-1]
label<- as.factor(trains[1:1000,1])
test=trains[1001:1600,-1]
outcome = trains[1001:1600,1]
train=as.matrix(train)
test=as.matrix(test)
error= c(0,0,0,0,0)

#run the comparison among models with different kernels 
for(i in 1:5){
  if(i==1){   #Gaussian kernel
    svm=ksvm(train,label,type="C-svc",kernel="rbfdot",C=1,scale=FALSE)
    predictions=predict(svm,test)
    error[i]=1-(sum(diag(table(predictions,outcome)))/600)
    print(error[i])
  }
  if(i==2){  #polynomial kernel
    svm=ksvm(train,label,type="C-svc",kernel="polydot",C=1,scale=FALSE)
    predictions=predict(svm,test)
    error[i]=1-(sum(diag(table(predictions,outcome)))/600)
    print(error[i])
  }
  if(i==3){  #linear kernel 
    svm=ksvm(train,label,type="C-svc",kernel="vanilladot",C=1,scale=FALSE)
    predictions=predict(svm,test)
    error[i]=1-(sum(diag(table(predictions,outcome)))/600)
    print(error[i])
  }
  if(i==4){ #laplace kernel
    svm=ksvm(train,label,type="C-svc",kernel="laplacedot",C=1,scale=FALSE)
    predictions=predict(svm,test)
    error[i]=1-(sum(diag(table(predictions,outcome)))/600)
    print(error[i])
  }
  if(i==5){  #Bessel kernel
    svm=ksvm(train,label,type="C-svc",kernel="besseldot",C=1,scale=FALSE)
    predictions=predict(svm,test)
    error[i]=1-(sum(diag(table(predictions,outcome)))/600)
    print(error[i])
  }
  
}


plot(error,pch=19,col="red", main="comparisons on error rate between kernels",xaxt="n",ylab="error rate",xlab="Kernels") 
lines(error)
axis(1,at=1:5,labels=c("Gaussian","Polynomial","Linear","laplace","Bessel"),cex=0.8)
}

#KAGGLE PART
#This piece of code is run for the whole kaggle testing set
#It may run over 3 hours please denote that
#this part is only to use optimal parameters gained from previous part and run model on a larger data set
library(randomForest)

#kaggle total traing set and testing sert
train <- read.csv("../data/train.csv", header=TRUE) 
test <- read.csv("../data/test.csv", header=TRUE)

labels <- as.factor(train[,1])
train <- train[,-1]

rf <- randomForest(train, labels, xtest=test, ntree=950,mtry=50) #optimal perameters
predictions <- levels(labels)[rf$test$predicted]

write(predictions, file="rf_benchmark.csv", ncolumns=1) 

#SVM part
library(kernlab)
label=train[,1]
label.f <- as.factor(label)
train=as.matrix(train[,-1])
test=as.matrix(test)
set.seed(42)
idx.1=sample(c(1:42000),size=42000,replace=F)
train.1=train[idx.1,]
label.1=label.f[idx.1]
svp.1=ksvm(train.1,label.1,type="C-svc",kernel="rbfdot",C=1,scale=FALSE)#optimal kernel
pred.svm.1=predict(svp.1,test)
write.csv(pred.svm.1,file="pred_svm_6.csv")
rm(pred.svm.1)



