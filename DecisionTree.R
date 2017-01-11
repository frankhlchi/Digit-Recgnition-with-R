library(rpart);
cdata = read.csv("C:\\Users\\Administrator\\Desktop\\train.csv",    header = T);
attach(cdata);
y = label;
cdata = data.frame(y, cdata[,2:785]);
Ntot = nrow(cdata);
set.seed(22);
N = 33600; 
s = sample(1:Ntot, N);
train = cdata[s,]
test = cdata[-s,]
rpart.out = rpart( factor(y)~.,data=train,method ='class',control=rpart.control(cp=0.00005))
# missclassification error in training sample
pred.rpart = predict( rpart.out, train[,-1], type="class" )
merror.rpart = mean( pred.rpart!= train$y )
merror.rpart
# missclassification error in validation sample
pred.rpart = predict( rpart.out, test[,-1], type="class" )
merror.rpart = mean( pred.rpart!= test$y ) 
merror.rpart
#################################################
printcp(rpart.out)
plotcp(rpart.out);
ptree = prune(rpart.out, cp=rpart.out$cptable[which.min(rpart.out$cptable[,"xerror"]),"CP"])
# missclassification error in training sample
pred.ptree = predict( ptree, train[,-1], type="class" )
merror.ptree = mean( pred.ptree!= train$y )
merror.ptree
# missclassification error in validation sample
pred.ptree = predict( ptree, test[,-1], type="class" )
merror.ptree = mean( pred.ptree!= test$y ) 
merror.ptree
table(pred.ptree, test$y)