library(xts)
library(forecast)


train <- read.csv('train.csv')
test <- read.csv('test.csv')

summary(train)
str(train)

summary(ntest)

ntrain <- train
ntest <- test

ntrain$Datetime <- as.POSIXlt(ntrain$Datetime ,format="%d-%m-%Y %H:%M")
ntest$Datetime <- as.POSIXlt(ntest$Datetime ,format="%d-%m-%Y %H:%M")


ndata <- ts(ntrain[,3],frequency = 24)
decomp <- decompose(ndata,type="additive")

plot(decomp)

pred_Holt <- HoltWinters(ndata)
pred_Holt
print(pred_Holt)

plot(pred_Holt)


#ets
pred_ets <- ets(ndata)
pred_ets
plot(forecast(pred_ets))

#ARIMA
pred_ar <- auto.arima(ndata)
pred_ar

#STL
pred_STL <- stlm(ndata,modelfunction = ar)
pred_STL


fcst_Holt <- forecast(pred_Holt,h=5112)
fcst_ets <- forecast(pred_ets,h=5112)
fcst_ar <- forecast(pred_ar,h=5112)
fcst_stl <- forecast(pred_STL,h=5112)
plot(fcst_Holt)
plot(fcst_ets)
plot(fcst_ar)
plot(fcst_stl)

submission <- data.frame('ID'=test$ID,'Count'=fcst_stl)
colnames(submission) <- c('ID','Count')
filename <- paste('ak_stl_',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(submission,paste0(filename,'.csv',collapse = ''),row.names = FALSE)
