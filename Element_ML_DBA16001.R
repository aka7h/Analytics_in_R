##############################################################################
########## Linear Regression #################################################
##############################################################################

library(ISLR)


idx=sample(1:392,250)
train_auto<-Auto[idx,]
test_auto<-Auto[-idx,]
full_auto <- rbind(train_auto, test_auto)

head(full_auto)
str(full_auto)



full_auto$cylinders <- as.factor(full_auto$cylinders)
full_auto$origin <- as.factor(full_auto$origin)
full_auto$year <- as.factor(full_auto$year)

summary(full_auto)
str(full_auto)


#Contineous Variable are MPG,DISPLACEMNT,HORSEPOWER,WEIGHT,ACCELERATION
full_auto_cont <- subset(full_auto, select = c(mpg,displacement,horsepower,weight,acceleration))

#checking the Missing value
sum(is.na(full_auto_cont))

cor(full_auto_cont)

#Pair Plots
pairs(~mpg+displacement+weight+horsepower+acceleration, data=full_auto_cont)
#How the data is dirstibuted
#hist(full_auto_cont$mpg)
#hist(full_auto_cont$displacement)
#hist(full_auto_cont$horsepower)
#hist(full_auto_cont$weight)
#hist(full_auto_cont$acceleration)

#Here the predicted variable(Y) is MPG
plot(full_auto_cont$mpg, full_auto_cont$displacement) 
#The above plot shows Negative 
plot(full_auto_cont$mpg, full_auto_cont$horsepower)
#The above plot shows Negative
plot(full_auto_cont$mpg, full_auto_cont$acceleration)
#The above plot shows is Neutral
plot(full_auto_cont$mpg, full_auto_cont$weight)
#The above plot shows Negative

#Creating train and Test data
train_auto <- full_auto_cont[1:250,]
test_auto <- full_auto_cont[251:392,]

model_auto1 <- lm(mpg~.,data = train_auto)
summary(model_auto1)

#used Leap to determine the variable strength towards dependent
library(leaps)
leaps <- regsubsets(mpg~., data=train_auto)
plot(leaps, scale = 'adjr2')

#We can understand that Weight and horsepower are significant
model_auto2 <-  lm(mpg~weight+horsepower, data = train_auto)
summary(model_auto2)

test_auto$pred <- predict(model_auto2, newdata = test_auto)
summary(test_auto$pred)

#Called metrices to run the RMSE
library("Metrics", lib.loc="D:/R/R-3.3.2/library")
rmse(test_auto$mpg, test_auto$pred)

#Calculation of RMSE without Library

MeanDifference <- (test_auto$mpg - test_auto$pred)^2
MeanSquare <- mean(MeanDifference)
RMSE <- sqrt(MeanSquare)
#the Root RMSEis 4.202

#############################################################################
#### Logistic Regression ####################################################
#############################################################################

#Here using Logisitc Regression we are determining the High End Cars and Low end Cars
full_auto$Class <- ifelse(full_auto$cylinders == 8,1,0)
full_auto$Class <- as.factor(full_auto$Class)

head(full_auto)
str(full_auto)

train_auto_lo <- full_auto[idx,]
test_auto_lo <- full_auto[-idx,]

#Understanding the Class
table(train_auto_lo$Class)
#boxplot(train_auto_lo$Class, train_auto_lo$mpg)
#boxplot(train_auto_lo$Class, train_auto_lo$displacement)
#boxplot(train_auto_lo$Class, train_auto_lo$horsepower)
#boxplot(train_auto_lo$Class, train_auto_lo$weight)
#boxplot(train_auto_lo$Class, train_auto_lo$acceleration)

#Model 1 By calling all the features
model_auto_lo1 <- glm(Class~mpg+displacement+weight+acceleration+horsepower, 
                      data = train_auto_lo, family = binomial(link='logit'))
summary(model_auto_lo1)

#Model 2
model_auto_lo2 <- glm(Class~1, data=train_auto_lo, 
                      family = binomial(link = 'logit'))

#Using StepWise regression
step(model_auto_lo2, scope = list(lower=model_auto_lo2, upper=model_auto_lo1),
     direction = 'forward')

#from the Above Stepwise decided to take displacement and mpg
model_auto_lo3 <- glm(Class~displacement+mpg, data=train_auto_lo, 
          family=binomial(link='logit'), maxit = 200)
summary(model_auto_lo3)

#Leap plot
leaps_lo <- regsubsets(Class~.,data=train_auto_lo)
plot(leaps_lo, scale = 'Cp')

#Confusion Matrix for Training dataset
table(predict(model_auto_lo3, newdata = train_auto_lo, type = 'response')>0.5, train_auto_lo$Class)

#Confusion matrix for Testing Dataset
confusionMatrix <- table(predict(model_auto_lo3, newdata = test_auto_lo, type = 'response')>0.5, test_auto_lo$Class)
#Precision
precision <- confusionMatrix[2,2]/sum(confusionMatrix[2,])
#Recall
recall <- confusionMatrix[2,2]/sum(confusionMatrix[,2])
