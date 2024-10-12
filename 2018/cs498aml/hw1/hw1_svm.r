wdat<-read.csv('pima-indians-diabetes.csv', header=FALSE)
library(caret)

test_acc = function(predict, label) {
  val<-predict==label
  acc<-sum(val)/(sum(val) + sum(!val))
  return(acc)
}

bigx<-wdat[,-c(9)]
bigx2<-apply(bigx, c(1, 2), function(x)x^2)
bigx<-cbind(bigx, bigx2)
acc<-array(dim=10)
for (wi in 1 : 10)
{
  bigy<-as.factor(wdat[,9])
  #split data
  wtd<-createDataPartition(y=bigy, p=.8, list=FALSE)
  #training data
  svm<-svmlight(bigx[wtd,], bigy[wtd])#, pathsvm='R/win-library/3.5/svmLight')
  #using model to do prediction
  labels<-predict(svm, bigx[-wtd,])
  acc[wi]<-test_acc(labels$class, bigy[-wtd])
}
print(mean(acc))