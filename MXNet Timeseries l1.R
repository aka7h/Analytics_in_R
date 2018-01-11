#forecasting the number of new cases in EColi

data('ecoli', package = "tscount")
head(ecoli)

class(ecoli)

data <- as.numeric(unlist(ecoli[3]))
data <- ts(matrix(data),start = c(2001,1),end = c(2013,20), frequency = 52)
data

#visualizing
plot(data,xlab="Date",ylab="No. of cases", col="darkblue")

#partial autocorrelation
pacf(data)

require(zoo)
require(quantmod)

data<- zoo(data)

#lag
x1 <- Lag(data,k=1)
x2 <- Lag(data,k=2)
x3 <- Lag(data,k=3)
x4 <- Lag(data,k=4)

x <- cbind(x1,x2,x3,x4,data)
tail(x)

head(x)
x <- x[-(1:4),]


range_data <- function(x){(x-min(x))/(max(x)-min(x))}
x <- data.matrix(x)
min_data <- min(x)
max_data <- max(x)
x <- range_data(x)

summary(x[,1:2])
summary(x[,3:4])

nrow(x)

lag

y <- as.numeric(x[,5])
x <- x[,-5]
ntrain <- 600
xtrain <- x[1:ntrain,]
ytrain <- y[1:ntrain]
xtest <- x[(ntrain+1):nrow(x),]
ytest <- y[(ntrain+1):nrow(x)]


library(mxnet)
mx.set.seed(2018)
model1 <- mx.mlp(xtrain,ytrain,hidden_node=c(10,2), out_node=1,activation = 'sigmoid', out_activation="rmse",
                 num.round=100, array.batch.size=20, learning.rate=0.07, momentum=0.9)
pred_train <- predict(model1,xtrain,ctx = mx.cpu())

library(Metrics)
round(rmse(ytrain,pred_train[,1:600]),5)


#improving model performance
mx.set.seed(2018)
model2 <- mx.mlp(xtrain,ytrain,hidden_node=c(10), out_node=1,activation = 'sigmoid', out_activation="rmse",
                 num.round=100, array.batch.size=20, learning.rate=0.07, momentum=0.9)
pred_train2 <- predict(model2,xtrain,ctx = mx.cpu())

round(rmse(ytrain,pred_train2[,1:600]),5)



mx.set.seed(2018)
model3 <- mx.mlp(xtrain,ytrain,hidden_node=c(10), out_node=1,activation = 'tanh', out_activation="rmse",
                 num.round=100, array.batch.size=20, learning.rate=0.07, momentum=0.9)
pred_train3 <- predict(model3,xtrain,ctx = mx.cpu())

round(rmse(ytrain,pred_train3[,1:600]),5)

plot(model3)


pred_test <- predict(model3,xtest)
round(rmse(ytest,pred_test[,1:40]),5)

unscale_data <- function(x,max_x,min_x){x*(max_x-min_x)+min_x}
unscale_data<???function(x ,max_x,min_x){x *(max_x???min_x)+min_x}

y_actual <- unscale_data(pred_test,max_data,min_data)

y_actual <- ts(matrix(y_actual),end=c(2013,20),frequency = 52)
original_data <- ts(matrix(data[605:644]),end=c(2013,20),frequency=52)
checkit <- cbind(y_actual,original_data)
head(checkit)
