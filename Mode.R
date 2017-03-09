## Mode
##Author : Akkash K N R
## a cross-section
## Model used :- NB, Multinom, Decision Tree, RF, GBM
## Modev Evaluation Metric :- Accuracy

library(Ecdat)
library(nnet)
library(e1071)
library(caret)
library(candisc)

attach(Mode)
head(Mode)
summary(Mode)

plot(Mode)
table(is.na(Mode))

boxplot(Mode)

plot(cost.bus)
plot(cost.car)
plot(cost.carpool)
plot(cost.rail)
plot(time.bus)
plot(time.car)
plot(time.carpool)
plot(time.rail)


id <- sample(453,315)
mode_train <- Mode[id,]
mode_test <- Mode[-id,]

col <- names(mode_train[,-1])
mode_train[col] <- lapply(mode_train[col], scale)
mode_test[col]<- lapply(mode_test[col],scale)

boxplot(mode_train)


#Simple Naive bayes
naive_fit <- naiveBayes(mode_train[,-1],mode_train[,1])
naive_fit

lf_pred <- predict(naive_fit, newdata = mode_train[,-1])
x<- table(lf_pred, mode_train$choice)
confusionMatrix(x)

lf_pred <- predict(naive_fit, newdata = mode_test[,-1])
x<- table(lf_pred, mode_test$choice)
confusionMatrix(x) #Accuracy= 0.6449   After Standardizing 0.6522


#Multinomial
multi_fit <- multinom(choice~.,data=mode_train)
summary(multi_fit)

lf_pred <- predict(multi_fit, newdata = mode_train, type="class")
x<- table(lf_pred, mode_train$choice)
confusionMatrix(x) #Accuacy = 0.7016

lf_pred <- predict(multi_fit, newdata = mode_test, type="class")
x<- table(lf_pred, mode_test$choice)
confusionMatrix(x) #Accuracy = 0.6304  After Standardising 0.6377

#Decison Tree
fitControl <- trainControl(method = 'cv', number = 5)

rpart_fit <- train(choice~.,mode_train,'rpart',trControl=fitControl)
rpart_fit

lf_pred <- predict(rpart_fit, newdata = mode_train)
x<- table(lf_pred, mode_train$choice)
confusionMatrix(x) #Accuacy = 0.7016

lf_pred <- predict(rpart_fit, newdata = mode_test)
x<- table(lf_pred, mode_test$choice)
confusionMatrix(x) #Accuracy = 0.6522  After standardizing 0.6377

#Random Forest
rf_fit <- train(choice~.,mode_train,'rf',trControl=fitControl, tuneLength=4, nTrees= 400)
rf_fit

varImp(rf_fit)

lf_pred <- predict(rf_fit, newdata = mode_train)
x<- table(lf_pred, mode_train$choice)
confusionMatrix(x) #Accuacy = 1

lf_pred <- predict(rf_fit, newdata = mode_test)
x<- table(lf_pred, mode_test$choice)
confusionMatrix(x) #Accuracy = 0.6594


#Boosting
gbm_fit <- train(choice~., mode_train, 'gbm', trControl= fitControl, tuneLength = 4)

lf_pred <- predict(gbm_fit, newdata = mode_train)
x<- table(lf_pred, mode_train$choice)
confusionMatrix(x) #Accuacy = 0.73  After standardizing 0.7492

lf_pred <- predict(gbm_fit, newdata = mode_test)
x<- table(lf_pred, mode_test$choice)
confusionMatrix(x) #Accuracy = 0.61 After standardizing 0.6087

# We ran all the model twice. w/wo standardizing the independent varaibles
#wth standardizing both Rpart and Multinom performed the same
#without standardizing only rpart worked the best





