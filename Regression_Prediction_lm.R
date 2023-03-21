
# 1. Data prep
library(fueleconomy)
df<-vehicles

anyNA(df)
dfna<-which(is.na(df), arr.ind = TRUE)
table(dfna[,2])

### removing the NAs. 
df<-df[complete.cases(df), ]

#converting to character variables to factos for lm. 
for (i in 1:ncol(df)) {
  if(is.character(df[,i])) df[,i]<-as.factor(df[,i])
}


#2. Prediction -- Regression
## use a regression to predict "hwy" using variables "year", "drive", "cyl", "displ"

###random split data - 70% training and 30% test set # trianing the model 
ind<-sample(nrow(df), nrow(df)*0.7)
train<-df[ind,]
test<-df[-ind,]

model <- lm(hwy ~ year + drive + cyl + displ, data = train)

yhat<-predict(model, test)

rmspe<-sqrt(mean((test$hwy-yhat)^2))
rmspe

### repeating this 1000 times 
rmspe <- c()

for (i in 1:1000) {
  ind <- sample(nrow(df), nrow(df)*0.7)
  train <- df[ind, ]
  test <- df[-ind, ]
model <- lm(hwy ~ year + drive + cyl + displ, data = train)
yhat <- predict(model, test)
rmspe[i] <- sqrt(mean((test$hwy - yhat)^2))
}

### plot 
plot(rmspe, col="green")
abline(a = mean(rmspe), b = 0, col="red", lwd = 3)


