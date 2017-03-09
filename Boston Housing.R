## Boston Housing 
##Author : Akkash K N R
## Housing Values in Suburbs of Boston
## Model used :- Linear, Log Linear, Decision Tree, Regularization(lasso), RF, xGBoost
## Modev Evaluation Metric :- RMSE

library(MASS)
library(caret)
library(corrplot)
library(glmnet)

attach(Boston)
head(Boston)
str(Boston)

summary(Boston)

plot(Boston)
Boston <- Boston[,-4]

c <- cor(Boston)

library(corrplot)
corrplot(c,method = 'number')

#Plotting
plot(density(crim))
plot(crim,medv)
abline(lm(medv~crim),col='red')

plot(density(rm))
plot(rm,medv)
abline(lm(medv~rm),col='red')

IQR(rm)

plot(density(lstat))
plot(lstat, medv)
abline(lm(medv~lstat),col='red')

plot(density(dis))
plot(dis, medv)
abline(lm(medv~dis),col='red')

plot(density(age))
plot(age,medv)
abline(lm(medv~age),col='red')



plot(density(indus))
plot(indus,medv)

plot(density(zn))
plot(zn,medv)

plot(density(nox))
plot(nox, medv)

plot(density(rad))
plot(rad, medv)

plot(density(tax))
plot(tax, medv)

plot(density(ptratio))
plot(ptratio, medv)

plot(density(black))
plot(black, medv)

#Data preparation
id <- sample(506,400)
train_bost <- Boston[id,]
test_bost <- Boston[-id,]

summary(train_bost)



#Simple Linear Regression

linear_fit <- lm(medv~.,data=train_bost)
summary(linear_fit)
lf_pred <- predict(linear_fit, newdata = train_bost)
sqrt(mean((train_bost$medv - lf_pred)^2)) #4.763859

lf_pred <- predict(linear_fit, newdata = test_bost)
sqrt(mean((test_bost$medv - lf_pred)^2)) #4.699226

#Lets remove insignificance data
train_bost <- train_bost[,-c(3,6)]
test_bost <- test_bost[,-c(3,6)]


#Simple Linear Regression

linear_fit2 <- lm(medv~.,data=train_bost)
summary(linear_fit2)
lf_pred <- predict(linear_fit2, newdata = train_bost)
sqrt(mean((train_bost$medv - lf_pred)^2)) #4.773684

lf_pred <- predict(linear_fit2, newdata = test_bost)
sqrt(mean((test_bost$medv - lf_pred)^2)) #4.641147


#Implement Log Linear

log_fit <- lm(log(medv)~., data=train_bost)
summary(log_fit)

lf_pred <- predict(log_fit, newdata = train_bost)
sqrt(mean((train_bost$medv - lf_pred)^2))#21.244

lf_pred <- predict(log_fit, newdata = test_bost)
sqrt(mean((test_bost$medv - lf_pred)^2))#22.136

#Implementing LM with k-fold validation
fitControl <- trainControl(method = 'repeatedcv',number = 10, repeats = 10)

lm_fit <- train(medv~.,method="lm", data=train_bost, trControl = fitControl)
summary(lm_fit)

lf_pred <- predict(lm_fit, newdata = train_bost)
sqrt(mean((train_bost$medv - lf_pred)^2))#4.773684

lf_pred <- predict(lm_fit, newdata = test_bost)
sqrt(mean((test_bost$medv - lf_pred)^2))#4.4641147

#Implement Dtree
rpartFit <- train(medv~.,data=train_bost, method='rpart')
rpartFit

lf_pred <- predict(rpartFit, newdata = train_bost)
sqrt(mean((train_bost$medv - lf_pred)^2))#5.580122

lf_pred <- predict(rpartFit, newdata = test_bost)
sqrt(mean((test_bost$medv - lf_pred)^2))#5.620694

#Implementing Penalization Technique
#Lasso
lasso_fit <- train(medv~.,train_bost,'lasso',trControl=fitControl)
lasso_fit

lf_pred <- predict(lasso_fit, newdata = train_bost)
sqrt(mean((train_bost$medv - lf_pred)^2))#4.995494

lf_pred <- predict(lasso_fit, newdata = test_bost)
sqrt(mean((test_bost$medv - lf_pred)^2))#4.857717


#Vanilla Lasso

x.tr <- model.matrix(medv~.,data=train_bost)[,-9]; y.tr <- train_bost$medv
x.te <- model.matrix(medv~.,data=test_bost)[,-9]; y.te <- test_bost$medv

vlasso_fit <- glmnet(x.tr,y.tr,alpha=1)
bestlam <- vlasso_fit$lambda.min
goodlam <- vlasso_fit$lambda.1se
plot(vlasso_fit, xvar='lambda', label=TRUE)

lf_pred <- predict(vlasso_fit,s=bestlam, newx = x.tr)
sqrt(mean((y.tr - lf_pred)^2))#cv.glmnet - 5.733292, glmnet - 6.247605

lf_pred <- predict(vlasso_fit,s=bestlam, newx = x.te)
sqrt(mean((y.te - lf_pred)^2))#cv.glmnet - 4.973047, glmnet - 5.616783

#Implementing Ensemble
fitControl <- trainControl(method = 'cv',number = 4)

#Random Forest
#Ommited bcoz its time consuming and costly
rf_fit <- train(medv~.,train_bost,'rf',trControl=fitControl, tuneLength=4)
rf_fit

lf_pred <- predict(rf_fit, newdata = train_bost)
sqrt(mean((train_bost$medv - lf_pred)^2))#1.377526

lf_pred <- predict(rf_fit, newdata = test_bost)
sqrt(mean((test_bost$medv - lf_pred)^2))#3.777269


#xGBoost
set.seed(849)
gbm_fit <- train(medv~.,train_bost, 'xgbLinear', trControl=fitControl, tuneLenght=4)
gbm_fit

varImp(gbm_fit)

lf_pred <- predict(gbm_fit, newdata = train_bost)
sqrt(mean((train_bost$medv - lf_pred)^2))#0.1385374

lf_pred <- predict(gbm_fit, newdata = test_bost)
sqrt(mean((test_bost$medv - lf_pred)^2))#4.351353

#From the given set of Alogrithms. XGBoost performed the best in train where as not so well in test

#Random Forest has given the best results so far.



