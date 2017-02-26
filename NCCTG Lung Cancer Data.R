## Lungs 
##Author : Akkash K N R
##Problem Description
# Survival in patients with advanced lung cancer from the North Central Cancer Treatment Group. 
# Performance scores rate how well the patient can perform usual daily activities.

# Problem Statement: Is the patient Dead or Not?


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

library(e1071)
#Naive Bayes
naive_fit <- naiveBayes(dtrain[,-2], dtrain[,2])
naive_fit

#Naive Metric
naive_prediction <- predict(naive_fit, newdata = dtrain[,-2])
table(naive_prediction, dtrain$status)

naive_test_pred <- predict(naive_fit, newdata = dtest[,-2])
table(naive_test_pred, dtest$status)


#Logisitc regression
log_fit <- glm(status~., data = dtrain, family = binomial)
summary(log_fit)

#Metrics
log_prediction <- predict(log_fit, newdata = dtrain, type='response')
log_test_pred <- predict(log_fit, newdata = dtest, type='response')

#confusion matrix
table(predicted= ifelse(log_prediction>0.5,1,0), actual=dtrain$status)

table(predicted= ifelse(log_test_pred>0.5,1,0), actual=dtest$status)
