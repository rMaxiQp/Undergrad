wdat <- read.csv("pima-indians-diabetes.csv", FALSE)
library(klaR)
library(caret)

test_acc = function(predict, label) {
  val<-predict==label
  acc<-sum(val)/(sum(val) + sum(!val))
  return(acc)
}

feature<-wdat[,-c(9)] 
label<-wdat[,9]

trscore<-array(dim=10)
tescore<-array(dim=10)

for (wi in 1:10)
{
  # data process
  train_set<-createDataPartition(y=label, p=.8, list=FALSE)
  train_feature<-feature[train_set, ] # validation dataset
  train_label<-label[train_set]
  
  pos_flag<-train_label>0
  train_pos<-train_feature[pos_flag, ]
  train_neg<-train_feature[!pos_flag,]
  test_feature<-feature[-train_set, ] # test dataset
  test_label<-label[-train_set]
  
  # train process
  mean_pos<-sapply(train_pos, mean, na.rm=TRUE)
  mean_neg<-sapply(train_neg, mean, na.rm=TRUE)
  sd_pos<-sapply(train_pos, sd, na.rm=TRUE)
  sd_neg<-sapply(train_neg, sd, na.rm=TRUE)
  
  # validation process
  ptroffsets<-t(t(train_feature)-mean_pos)
  ptrscales<-t(t(ptroffsets)/sd_pos) # normalize
  ptrlogs<--(1/2)*rowSums(apply(ptrscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(sd_pos))#pdf
  ntroffsets<-t(t(train_feature)-mean_neg)
  ntrscales<-t(t(ntroffsets)/sd_neg)
  ntrlogs<--(1/2)*rowSums(apply(ntrscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(sd_neg))#pdf
  
  # test accuracy
  trscore[wi]<-test_acc(ptrlogs>ntrlogs, train_label)
  
  # test process
  pteoffsets<-t(t(test_feature)-mean_pos)
  ptescales<-t(t(pteoffsets)/sd_pos)
  
  ptelogs<--(1/2)*rowSums(apply(ptescales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(sd_pos))
  nteoffsets<-t(t(test_feature)-mean_neg)
  ntescales<-t(t(nteoffsets)/sd_neg)
  ntelogs<--(1/2)*rowSums(apply(ntescales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(sd_neg))
  
  # test accuracy
  tescore[wi]<-test_acc(ptelogs>ntelogs, test_label)
}

print(mean(tescore))

