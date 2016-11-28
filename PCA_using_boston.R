#PCA on Boston Data set
boston <- Boston
#Now we are removing the boston$chas since its a factor
boston <- Boston[,c(1:3,5:14)]
head(boston)
str(boston)
#linear model before PCA
model_b_pca <- lm(medv~.,data=boston)
summary(model_b_pca)

#PCA for boston
new.boston <- boston[,-13]
#we are removing the medv since its the dependent variable
head(new.boston)
new.boston.norm <- scale(new.boston, scale = TRUE)
new.boston.prcomp <- prcomp(new.boston.norm, center = TRUE, scale. = TRUE)
summary(new.boston.prcomp)
biplot(new.boston.prcomp, scale=0)
plot(new.boston.prcomp, type='l')
new.boston.prcomp$

stdev.new.boston <- new.boston.prcomp$sdev
variance.new.boston <- stdev.new.boston^2
propotion.variance.new.boston <- variance.new.boston/sum(variance.new.boston)
plot(propotion.variance.new.boston, type='b')
cumulative.new.boston <- cumsum(propotion.variance.new.boston)
plot(cumulative.new.boston, type='b')

#i have plotted the Propotional of Variance and Cumulative Propotion from summary(new.boston.prcomp)
#looking at the plot its okay for me to take till PC8. because am fine with 92%
#this way you can understnad how to do dimension reduction

test.data <- data.frame(medv = boston$medv, new.boston.prcomp$x)
head(test.data)
#here PC1 to PC12 are the new independent data that we will be using 
#instrd of crim, rm, black, ptratio etc.

#taking only the first 8 variables
test.data.8.variables <- test.data
head(test.data.8.variables)

mod<-lm(log(medv)~.,data=test.data.8.variables)
summary(mod)
#run decision tree
library("rpart", lib.loc="D:/R/R-3.3.1/library")
rpart.model <- rpart(medv~.,data=test.data,method="anova")
summary(rpart.model)

testing.data <- predict(new.boston.prcomp, newdata = tail(boston,100))
testing.data <- as.data.frame(testing.data)


#predict
rpart.prediction <- predict(rpart.model, testing.data)
summary(rpart.prediction)
