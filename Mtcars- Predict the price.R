## Car Test Frame 
##Author : Akkash K N R
## Predict the Price of the car
## Model used :- Linear, Decision Tree, RF
## Modev Evaluation Metric :- RMSE


full_data <- read.csv('http://vincentarelbundock.github.io/Rdatasets/csv/rpart/car.test.frame.csv', header = T)

head(train)
str(full_data)
summary(full_data)
dim(full_data)

summary(full_data)

table(is.na(full_data))


library(VIM)
library(mice)


md.pattern(train)
mice <- aggr(train, col=c('blue','black'),labels = names(full_data), x.cex=.5)


#Feature Engineering

full_data$Reliability[is.na(full_data$Reliability)] <- 0
full_data$Reliability <- as.factor(full_data$Reliability)

#combning france and Sweeden since there are not datas available 
full_data$Country <- as.character(full_data$Country)

full_data$Country[full_data$Country == 'France'] <- 'Others'
full_data$Country[full_data$Country == 'Sweden'] <- 'Others'
full_data$Country[full_data$Country == 'Korea'] <- 'Others'
full_data$Country[full_data$Country == 'Mexico'] <- 'Others'

#Getting Cylinder details from variable X
cyl <- sapply(strsplit(as.character(full_data$X),""),tail, 1)
full_data$Cylinder <- if(as.numeric(cyl)){as.numeric(cyl)}

full_data$Cylinder[is.na(full_data$Cylinder)] <- 4
full_data$Cylinder <- as.factor(full_data$Cylinder)

id <- sample(60,50)
train <- full_data[id,]
test <- full_data[-id,]


#EDA

plot(train[,c(2,4:10)])
plot(train$Mileage,train$Price) #As the Milage increases the proce decreases
plot(train$Weight,train$Price)#As the Weight increases the price increases
plot(train$Disp.,train$Price)
plot(train$HP,train$Price)

plot(density(train$Price))
plot(density(log(train$Mileage)))
plot(density(train$Weight))
plot(density(log(train$Disp.)))
plot(density(log(train$HP)))

cor(train[,c(2,5,7:9)])

library(ggplot2)

ggplot(train, aes(Type, fill = Reliability))+ geom_bar()

ggplot(train, aes(Type, Price))+geom_boxplot()

ggplot(train, aes(Reliability, Price))+geom_boxplot()

plot(full_data$Cylinder, full_data$Price)

#Creating Model Data
model_data_train <- train[,c(2:10)]
model_data_test <- test[,c(2:10)]


#Simple Linear regression
linear_fit <- lm(Price~., data=model_data_train)
summary(linear_fit)

#Prediction
prediction <- predict(linear_fit, newdata = model_data_train)
test_prediction <- predict(linear_fit, newdata = model_data_test)

#Metircs
sqrt(mean((prediction - model_data_train$Price)^2)) #1372.451
sqrt(mean((test_prediction - model_data_train$Price)^2)) #5319.606

#Lets jsut try and see with the Significance values from the linear regression
sig_fit <- lm(Price~Country+Weight, data= model_data_train)
summary(sig_fit)

sig_pred <- predict(sig_fit, newdata = model_data_train)
sqrt(mean((model_data_train$Price - rf_pred)^2)) #4252.094

sig_pred <- predict(sig_fit, newdata = model_data_test)
sqrt(mean((model_data_test$Price - rf_pred)^2)) #2743.765




#Decision tree
library(rpart)
library(rpart.plot)

rpart_fit <- rpart(Price~., data= model_data_train)
rpart_fit

rpart.plot(rpart_fit)

rpart_prediction <- predict(rpart_fit, newdata = model_data_train)
rpart_test_prediction <- predict(rpart_fit, newdata = model_data_test)

sqrt(mean((rpart_prediction - model_data_train$Price)^2)) #2352.707
#with formula(Price~Country+Weight) 2351.358
sqrt(mean((rpart_prediction - model_data_test$Price)^2)) #5177.894
#with formula(Price~Country+Weight) 5054.783

library(caret)
fitControl <- trainControl(method = 'repeatedcv', number = 10)

#Random Forest
rf_fit <- train(Price~.,model_data_train, 'rf', trControl= fitControl, tuneLength=4,nTrees=100)
rf_fit

rf_pred <- predict(rf_fit, newdata = model_data_train)
sqrt(mean((model_data_train$Price - rf_pred)^2)) #1199.016
#with formula(Price~Country+Weight) 1707.063

rf_pred <- predict(rf_fit, newdata = model_data_test)
sqrt(mean((model_data_test$Price - rf_pred)^2)) #2743.765
#with formula(Price~Country+Weight) 3107.571

#SO far the Random Forest works the best with all the values
#where as the Sig_fit shows to be best than the random forest. 

