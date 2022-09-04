library(randomForest)
library(tidyverse)
library(dplyr)
#train
df=read.csv("features_training.csv")
df=df[sample(c(1:dim(df)[1]), dim(df)[1]*0.1),]
df1=df%>%
  group_by(PERMNO)%>%
  arrange(yyyy,mm,.by_group = TRUE)
a=lead(df1$RET)
df1$RET=a
df1=df1[,18:56]
df1[is.na(df1)]=0
probs = c(0,0.3,0.7,1)
for (i in 1:38){
  bins=quantile(unlist(df1[,i]) ,probs)
  df1[,i]=.bincode(unlist(df1[,i]),bins,include.lowest = TRUE)
  df1[,i]=as.character(unlist(df1[,i]))
  df1[which(df1[,i]==1),i]="small"
  df1[which(df1[,i]==2),i]="medium"
  df1[which(df1[,i]==3),i]="large"
  df1[,i]=as.factor(unlist(df1[,i]))
}
set.seed(1)
rf <- randomForest(RET ~ ., data = df1, mtry = 4, nodesize = 5)  
varImpPlot(rf)
importance(rf)
rf.pred <- predict(rf, df1)
df$RET[is.na(df$RET)]=0
1-sum((df2$RET-rf.pred)^2)/sum((df2$RET-mean(df2$RET))^2)
#function model
RF=function(df){
  df1=df%>%
    group_by(PERMNO)%>%
    arrange(yyyy,mm,.by_group = TRUE)
  df1=df1[,17:54]
  df1[is.na(df1)]=0
  probs = c(0,0.3,0.7,1)
  for (i in 1:38){
    bins=quantile(unlist(df1[,i]) ,probs)
    df1[,i]=.bincode(unlist(df1[,i]),bins,include.lowest = TRUE)
    df1[,i]=as.character(unlist(df1[,i]))
    df1[which(df1[,i]==1),i]="small"
    df1[which(df1[,i]==2),i]="medium"
    df1[which(df1[,i]==3),i]="large"
    df1[,i]=as.factor(unlist(df1[,i]))
  }
  rf.pred <- predict(rf, df2)
  return(rf.pred)
}


