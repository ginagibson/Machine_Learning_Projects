# Classification Threshold. 

# 1. Data prep
###  using boston data from MASS package.
library(MASS)
df<-Boston
anyNA(df)

# indicator "high"; 1 if medv>25 or 0. medv: median value of owner occupied homes in $1000s 
df$high<-ifelse(Boston$medv>25, 1, 0)

# 2. Splitting Data.
str(df)
ind<-sample(nrow(df), nrow(df)*0.7)
train<-df[ind, -14]
test<-df[-ind, -14]

# 3. Predicting phat. 
model <- lm (high~., data=train)
phat <- predict(model, newdata=test)   
summary(phat)

###  predicting possibilities using 0.5 threshold.            
yhat <- ifelse(phat > 0.5, 1, 0)

# 4. Confusion table 
cft<-table(test$high, yhat)

cft_a<-function(x){
  cf_table<-matrix(0,2,2)
  cf_table[1,1] <- ifelse(sum(dim(x))>3, x[2,2],0)
  cf_table[2,2] <- ifelse(sum(dim(x))>3, x[1,1], 0)
  cf_table[1,2] <- ifelse(sum(dim(x))>3, x[2,1], 0)
  cf_table[2,1] <- ifelse(sum(dim(x))>3, x[1,2], 0)
  colnames(cf_table) <- c("Y=1", "Y=0")
  rownames(cf_table) <- c("Yhat=1", "Yhat=0")
  cf_table
}

ct<-cft_a(cft)
ct

# 5. TPR and ACC

TPR<-ct[1,1]/(ct[1,1]+ct[2,1])
TPR
ACC<- sum(diag(ct))/sum(ct)
ACC


# 6. ACC test - by changing threshold. 

yhat<-ifelse(phat>0.45, 1, 0)
cft<- table(test$high, yhat)
ct <-cft_a(cft)

ACC<-sum(diag(ct))/sum(ct)
ACC

# 7. Find best th - grid search 
th <- seq(0,0.64,0.01)
ACC <-c()

for(i in 1:length(th)){
  yhat <- ifelse(phat > th[i], 1, 0)
  tb <- table(yhat, test$high)
  
  ct <- cft_a(tb) 
  
  ACC[i] <- sum(diag(ct)) / sum(ct)
}

lst<-data.frame(th=th, ACC=ACC)
imax<-which.max(lst$ACC)

th[imax]
ACC[imax]

which(ACC == max(ACC))

# 100 loop to find th, distribution of MACC and max-th

MACC <-c()
max_th <-c()

th<-seq(0,0.64,0.01)

for (j in 1:500) {
  ind <- sample(nrow(df), nrow(df)*0.7)
  train<-df[ind, -14]
  test <- df[-ind, -14]
  
  model<- lm(high~., data=train)
  phat<-predict(model, newdata = test)
  
  ACC<-c()
  
    for (i in 1:length(th)) {
      yhat <- ifelse(phat> th[i], 1,0)
      tb<-table(yhat, test$high)
      
      cf<-cft_a(tb)
      ACC[i] <-sum(diag(cf))/sum(cf)
      
    }
  MACC[j]<-max(ACC)
  max_th[j]<-th[which.max(ACC)]
}


# using LOGISTIC REGRESSION - to do previous steps. 

MACC <- c()
max_th <- c()

th <- seq(0, 0.64, 0.01)

for(i in 1:length(th)){
  for(j in 1:200){
    
    ind <- sample(nrow(df), nrow(df)*0.70)
    train <- df[ind, -14]
    test <- df[-ind, -14]
    
    model <- lm(high~., data = train) # Model
    phat <- predict(model, newdata = test) # Prediction
    
    yhat <- ifelse(phat > th[i], 1, 0)
    tb <- table(yhat, test$high)
    
    cf <- cft_a(tb) 
    ACC[j] <- sum(diag(cf)) / sum(cf)
  }
  
  MACC[i] <- mean(ACC)   
}

which.max(MACC)

tb[which.max(MACC)]