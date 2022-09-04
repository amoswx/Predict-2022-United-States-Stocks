

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
x <- model.matrix(RET~.,df1)[,-1]
y<-df1$RET
set.seed(5)
cv.fit <- cv.glmnet(x,y,alpha=1, type.measure="mse",nfold=5)
plot(cv.fit)
best_model <- glmnet(x, y, alpha = 1)
plot(best_model,xvar="lambda")
lambda.best <- cv.fit$lambda.min
coef.lambda <- predict(cv.fit,s=lambda.best,type="coefficients")[1:20,]
coef.lambda[coef.lambda!=0]
#function model
lasso_model=function(df){
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
  x1= model.matrix(~.,df1)[,-1]
  y_predicted <- predict(best_model, s = lambda.best, newx = x1)
  return(y_predicted)
}

pred=lasso_model(df[,2:56])
1-sum((df1$RET-pred)^2)/sum((df1$RET-mean(df1$RET))^2)
