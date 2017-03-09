## Lungs 
##Author : Akkash K N R
##Problem Description
# Survival in patients with advanced lung cancer from the North Central Cancer Treatment Group. 
# Performance scores rate how well the patient can perform usual daily activities.

# Problem Statement: Is the patient Dead or Not?
library(mice)
library(VIM)
library(e1071)
library(Metrics)
library(caret)

mydata <- read.csv('http://vincentarelbundock.github.io/Rdatasets/csv/survival/lung.csv', header = T )

summary(mydata)
mydata2 <- mydata[,-c(1,2)]
str(mydata2)
summary(mydata2)

mydata2$status <- as.factor(ifelse(mydata2$status==2,0,1))

mice2 <- aggr(mydata2, col=c('blue', 'yellow'), labels=names(mydata2), x.cex = .5)


# Feature Engineering - Mode and mean imputation
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mydata2$pat.karno[is.na(mydata2$pat.karno)] <- getmode(mydata2$pat.karno)
mydata2$ph.karno[is.na(mydata2$ph.karno)] <- getmode(mydata2$ph.karno)
mydata2$meal.cal[is.na(mydata2$meal.cal)] <- mean(mydata2$meal.cal, na.rm = T)
mydata2$wt.loss[is.na(mydata2$wt.loss)] <- getmode(mydata2$wt.loss)
mydata2$ph.ecog[is.na(mydata2$ph.ecog)] <- getmode(mydata2$ph.ecog)

summary(mydata2)

table(is.na(mydata2))

id <- sample(228, 160)
dtrain <- mydata2[id,]
dtest <- mydata2[-id,]

#EDA
str(dtrain)

#Feature Engineering
col <- c('sex','ph.ecog')
dtrain[col] <- lapply(dtrain[col], factor)
dtest[col] <- lapply(dtest[col], factor)

str(dtrain)
dtrain$wt.loss <- scale(dtrain$wt.loss, scale=T)
dtest$wt.loss <- scale(dtest$wt.loss, scale=T)


#Naive Bayes
naive_fit <- naiveBayes(dtrain[,-2], dtrain[,2])
naive_fit

#Naive Metric
naive_prediction <- predict(naive_fit, newdata = dtrain[,-2])
confusionMatrix(table(naive_prediction, dtrain$status)) # 0.743


naive_test_pred <- predict(naive_fit, newdata = dtest[,-2])
x <- table(naive_test_pred, dtest$status)
confusionMatrix(x) #0.75


#Logisitc regression
log_fit <- glm(status~., data = dtrain, family = binomial)
summary(log_fit)

#Metrics
log_prediction <- predict(log_fit, newdata = dtrain, type='response')
log_test_pred <- predict(log_fit, newdata = dtest, type='response')

#confusion matrix
x <- table(predicted= ifelse(log_prediction>0.5,1,0), actual=dtrain$status)
#Train
confusionMatrix(x) #0.8

xt<- table(predicted= ifelse(log_test_pred>0.5,1,0), actual=dtest$status)
confusionMatrix(xt) #0.6912

#Decision Tree
fitControl <- trainControl(method='cv', number=5)

rpart_fit <- train(status~.,dtrain,'rpart',trControl=fitControl)


lf_pred <- predict(rpart_fit, newdata = dtrain)
x<- table(lf_pred, dtrain$status)
confusionMatrix(x) #Accuacy = 0.7562

lf_pred <- predict(rpart_fit, newdata = dtest)
x<- table(lf_pred, dtest$status)
confusionMatrix(x) #Accuracy = 0.6471

#Random Forest
rf_fit <- train(status~.,dtrain,'rf',tuneLength=4, nTree=300)
rf_fit

lf_pred <- predict(rf_fit, newdata = dtrain)
x<- table(lf_pred, dtrain$status)
confusionMatrix(x) #Accuacy = 1

lf_pred <- predict(rf_fit, newdata = dtest)
x<- table(lf_pred, dtest$status)
confusionMatrix(x) #Accuracy = 0.6618

#Of all the models naive Bayes seems to give the best prediction.



