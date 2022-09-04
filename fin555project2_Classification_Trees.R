library(rpart)
library(rpart.plot)
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
default.ct <- rpart(RET ~ ., data = df1, method = "anova", minbucket = 1, maxdepth = 30, cp = 0.001)
prp(default.ct, type = 1, extra = 1)
pred <- predict(default.ct, df1,type="vector")
#function model
tree=function(df){
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
  pred <- predict(default.ct, df1,type="vector")
  return(pred)
}
df$RET[is.na(df$RET)]=0
pred1=tree(df[2:55])
summary(pred)
1-sum((df1$RET-pred1)^2)/sum((df1$RET-mean(df1$RET))^2)
