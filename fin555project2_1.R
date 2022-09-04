library(tidyverse)
library(dplyr)
#1
df=read.csv("features_training.csv")
df1=df%>%
  group_by(PERMNO)%>%
  arrange(yyyy,mm,.by_group = TRUE)
a=lead(df1$RET)
df1$RET=a
df1=df1[,18:56]
df1[is.na(df1)]=0
#2
train.index <- sample(c(1:dim(df1)[1]), dim(df1)[1]*0.6)  
train.df=df1[train.index,]
test.df=df1[-train.index,]
lm1=lm(RET~FFbm+FF_Momentum,data =train.df )
summary(lm1)$r.squared
pred1=predict(lm1,test.df)        
1-var(pred1)/var()
#3
probs = c(0,0.3,0.7,1)
for (i in 18:55){
  bins=quantile(unlist(df1[,i]) ,probs)
  df1[,i]=.bincode(unlist(df1[,i]),bins,include.lowest = TRUE)
  df1[,i]=as.factor(unlist(df1[,i]))
}


