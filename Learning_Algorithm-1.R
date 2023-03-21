# using the spam data set from kernlab package.
# developing a learning algorithm that identifies spam emails. 

library(kernlab)
library(tibble)
library(ggplot2)
library(caret)
data("spam")
str(spam)

df<-spam
any(is.na(df))

nonspam<-colMeans(df[df[,58]=="nonspam", -58])
spam<-colMeans(df[df[,58]=='spam', -58])
ns1<-cbind(nonspam, spam)

#difference of spam and nonspam, ordered in decreasing order. 
ns1<-as.data.frame(ns1)
ns1<-ns1[-58,]
ns1$dif<-abs(ns1[,1]-ns1[,2])
ns1<-round(ns1[order(ns1[,3], decreasing = TRUE), ], 4)
ns1

#for better visualization, barplot excludes first 3 rows. 
barplot(ns1[10:4, 3], col = "green", horiz = TRUE, 
        names.arg = rownames(ns1)[10:4], cex.names = 0.5)

# LPM- benchmark model 

## make spam and nonspam in "type" column, numerical; spam = 0, nonspam=1
df$type<-as.numeric(df$type)
df$type[df$type==2]<-0
df$type[df$type==1]<-1

## data split into training and testing sets
set.seed(2)
ind<-sample(nrow(df), nrow(df)*0.2, replace=FALSE)
train<-df[-ind,]
test<-df[ind,]

## training and predicting
lpmmodel<-lm(type~., data = train)
phat<-predict(lpmmodel, test)
summary(phat)

## grid search 
grid <-seq(0.01, 0.85, 0.01)
ACC<-c()

#ACC metric threshold and gridsearch 

for (i in 1:length(grid)) {
  yhat<-phat > grid[i]
  cf_tbl<-table(test$type, yhat)
  ACC[i]<-(cf_tbl[1,1] + cf_tbl[2,2])/sum(cf_tbl)
}

max(ACC)
grid[ACC==max(ACC)]

#100 loop
grid <- seq(0.2, 0.85, 0.01)
ACC_max<-c()
TH_opt<-c()

for (j in 1:100) {
  ind <- sample(nrow(df), nrow(df)*0.2, replace=FALSE)
  train <- df[-ind,]
  test <- df[ind,]
  
  model<-lm(type~., data = train)
  phat<-predict(model, newdata=test)
  ACC<-c()
  
  for (i in 1:length(grid)) {
    yhat<-phat > grid[i]
    cf_tbl <- table(test$type, yhat)
    ACC[i]<-(cf_tbl[1,1] + cf_tbl[2,2])/sum(cf_tbl)
  }
  ACC_max[j]<-max(ACC)
  TH_opt[j]<-grid[ACC==max(ACC)][1]
}

mean(ACC_max)

sd(ACC_max)

plot(ACC_max, col="green")
abline(a = mean(ACC_max), b=0, col="red", lwd=2)

# KNN

rm(list = ls())
data("spam")
df <- spam
dfs <- scale(df[, -58])
dfs <- data.frame(dfs, type = df$type)

gth<-seq(0.2, 0.85, 0.01)
gk<-3:30

ACC_2<-c()
y<-c()

for (i in 1:50) {
  ind <- sample(nrow(dfs), nrow(dfs) * 0.2, replace = FALSE)
  train <- dfs[-ind,]
  test <- dfs[ind,]
  
  ACC_1<-c()
  
  for (j in 1:length(gk)) {
    modelknn<-knn3(type~., k=gk[j], data=train)
    phat<-predict(modelknn, newdata=test, "prob")
    
    ACC<-c()
    
    for (z in 1:length(gth)) {
      yhat<-phat[,2] >gth[z]
      cf_tbl<-table(test$type, yhat)
      ACC[z]<-sum(diag(cf_tbl))/sum(cf_tbl)
    }
    
    ACC_1[j]<-max(ACC)
  }
ACC_2[i]<-max(ACC_1)
y[i]<-gk[which.max(ACC_1)]
  }

  mean(ACC_2)

  sd(ACC_2)
  
# Plot the ACCs. 

plot(ACC_2, col = "blue")
abline(a = mean(ACC_2), b=0 ,col = "red", lwd=2)
