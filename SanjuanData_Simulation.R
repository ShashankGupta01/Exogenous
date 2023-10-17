library(tseries)
library(forecast)
library(Metrics)
library(ggplot2)
library(WaveletArima)
library(e1071)
library(fracdiff)
library(tseriesChaos)
library(pracma)
library(Kendall)
library(nonlinearTseries)
library(GeneCycle)
library(fpp2)
library(smooth)

setwd("C:/Users/shash/OneDrive/Desktop/Simulation/Work")
data=read.csv("Sanjuan_data_weekly.csv")

###########################################################
cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_1= 52
train_1 = window(cases_ts, start = 1, end=l1-l_1)
test_1 = window(cases_ts, start= l1-l_1+1,end=l1)

## ARIMAX
Case_ts_1=ts(train_1)

xreg_1=data$Rain
xreg_Train_1=window(xreg_1, start = 1, end=l1-l_1)
xreg_Test_1=window(xreg_1, start= l1-l_1+1, end=l1)

auto.arima(train_1,xreg=xreg_Train_1)
fitARIMAX_1 = auto.arima(train_1,xreg=xreg_Train_1)
summary(fitARIMAX_1)


ARIMA_Rain_1 = auto.arima(xreg_Train_1) 
summary(ARIMA_Rain_1)

predARIMA_Rain_1= forecast::forecast(ARIMA_Rain_1,h=l_1) 
plot(predARIMA_Rain_1)
xreg_pred_1= predARIMA_Rain_1$mean
predARIMAX_Fit_1 = forecast::forecast(fitARIMAX_1,xreg=xreg_pred_1,h=l_1) #test days prediction
summary(predARIMAX_Fit_1)
#accuracy metric
a1=c(mae(test_1,predARIMAX_Fit_1$mean), rmse(test_1,predARIMAX_Fit_1$mean), smape(test_1,predARIMAX_Fit_1$mean))
a1

## ARNNX
fit_nnarx_1 = nnetar(train_1, xreg = xreg_Train_1, repeats = 100)
fit_nnarx_1
summary(fit_nnarx_1)

forecast_nnarx_1 = forecast::forecast(fit_nnarx_1, h = l_1, xreg=xreg_pred_1) # Test days prediction
plot(forecast_nnarx_1)

b1=c(mae(test_1,forecast_nnarx_1$mean), rmse(test_1,forecast_nnarx_1$mean), smape(test_1,forecast_nnarx_1$mean))
b1

## ARIMAX + ARNNX
fit_res_ARNNX_1=nnetar(fitARIMAX_1$residuals,xreg=xreg_Train_1, repeats = 100)
fit_res_ARNNX_1

pred_res_ARNNX_1 = forecast::forecast(fit_res_ARNNX_1, h=l_1, xreg=xreg_pred_1) # Test days prediction
pred_arimax_arnnx_1=predARIMAX_Fit_1$mean+pred_res_ARNNX_1$mean
c1=c(mae(test_1,pred_arimax_arnnx_1), rmse(test_1,pred_arimax_arnnx_1), smape(test_1,pred_arimax_arnnx_1))
c1

## ARIMAX + ARNN 
fit_res_ARNN_1=nnetar(fitARIMAX_1$residuals, repeats = 100)
fit_res_ARNN_1
pred_res_ARNN_1 = forecast::forecast(fit_res_ARNN_1, h=l_1) # Test days prediction
pred_arimax_arnn_1=predARIMAX_Fit_1$mean+pred_res_ARNN_1$mean
d1=c(mae(test_1,pred_arimax_arnn_1), rmse(test_1,pred_arimax_arnn_1), smape(test_1,pred_arimax_arnn_1))
d1

##ETSX
fitETSX_1 = es(train_1, model = "ZZZ", xreg = xreg_Train_1)
modelType(fitETSX_1)
predETSX_1 = forecast::forecast(fitETSX_1, h=l_1, xreg = xreg_Test_1)
summary(predETSX_1)
plot(predETSX_1)

e1=c(mae(test_1,predETSX_1$mean), rmse(test_1,predETSX_1$mean), smape(test_1,predETSX_1$mean))
e1

#################################################################
cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_2= 48
train_2 = window(cases_ts, start = 1, end=l1-l_2)
test_2 = window(cases_ts, start= l1-l_2+1,end=l1)

##ARIMAX
Case_ts_2=ts(train_2)

xreg_2=data$Rain
xreg_Train_2=window(xreg_2, start = 1, end=l1-l_2)
xreg_Test_2=window(xreg_2, start= l1-l_2+1, end=l1)

auto.arima(train_2,xreg=xreg_Train_2)
fitARIMAX_2 = auto.arima(train_2,xreg=xreg_Train_2)
summary(fitARIMAX_2)

ARIMA_Rain_2 = auto.arima(xreg_Train_2)
summary(ARIMA_Rain_2)

predARIMA_Rain_2= forecast::forecast(ARIMA_Rain_2,h=l_2)
plot(predARIMA_Rain_2)
xreg_pred_2= predARIMA_Rain_2$mean
predARIMAX_Fit_2 = forecast::forecast(fitARIMAX_2,xreg=xreg_pred_2,h=l_2) #test days prediction
summary(predARIMAX_Fit_2)
#accuracy metric
a2=c(mae(test_2,predARIMAX_Fit_2$mean), rmse(test_2,predARIMAX_Fit_2$mean), smape(test_2,predARIMAX_Fit_2$mean))
a2

##ARNNX

fit_nnarx_2 = nnetar(train_2, xreg = xreg_Train_2, repeats = 100)
fit_nnarx_2
summary(fit_nnarx_2)

forecast_nnarx_2 = forecast::forecast(fit_nnarx_2, h = l_2, xreg=xreg_pred_2) # Test days prediction
plot(forecast_nnarx_2)

b2=c(mae(test_2,forecast_nnarx_2$mean), rmse(test_2,forecast_nnarx_2$mean), smape(test_2,forecast_nnarx_2$mean))
b2

##ARIMAX + ARNNX

fit_res_ARNNX_2=nnetar(fitARIMAX_2$residuals,xreg=xreg_Train_2, repeats = 100)
fit_res_ARNNX_2

pred_res_ARNNX_2 = forecast::forecast(fit_res_ARNNX_2, h=l_2, xreg=xreg_pred_2) # Test days prediction
pred_arimax_arnnx_2=predARIMAX_Fit_2$mean+pred_res_ARNNX_2$mean
c2=c(mae(test_2,pred_arimax_arnnx_2), rmse(test_2,pred_arimax_arnnx_2), smape(test_2,pred_arimax_arnnx_2))
c2

## ARIMAX + ARNN
fit_res_ARNN_2 = nnetar(fitARIMAX_2$residuals, repeats = 100)
fit_res_ARNN_2
pred_res_ARNN_2 = forecast::forecast(fit_res_ARNN_2, h = l_2) # Test days prediction
pred_arimax_arnn_2 = predARIMAX_Fit_2$mean + pred_res_ARNN_2$mean
d2 = c(mae(test_2, pred_arimax_arnn_2), rmse(test_2, pred_arimax_arnn_2), smape(test_2, pred_arimax_arnn_2))
d2

##ETSX
fitETSX_2 = es(train_2, model = "ZZZ", xreg = xreg_Train_2)
modelType(fitETSX_2)
predETSX_2 = forecast::forecast(fitETSX_2, h = l_2, xreg = xreg_Test_2)
summary(predETSX_2)
plot(predETSX_2)

e2 = c(mae(test_2, predETSX_2$mean), rmse(test_2, predETSX_2$mean), smape(test_2, predETSX_2$mean))
e2


########################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_3= 44
train_3 = window(cases_ts, start = 1, end=l1-l_3)
test_3 = window(cases_ts, start= l1-l_3+1,end=l1)

##ARIMAX
Case_ts_3=ts(train_3)

xreg_3=data$Rain
xreg_Train_3=window(xreg_3, start = 1, end=l1-l_3)
xreg_Test_3=window(xreg_3, start= l1-l_3+1, end=l1)

auto.arima(train_3,xreg=xreg_Train_3)
fitARIMAX_3 = auto.arima(train_3,xreg=xreg_Train_3)
summary(fitARIMAX_3)

ARIMA_Rain_3 = auto.arima(xreg_Train_3)
summary(ARIMA_Rain_3)

predARIMA_Rain_3= forecast::forecast(ARIMA_Rain_3,h=l_3)
plot(predARIMA_Rain_3)
xreg_pred_3= predARIMA_Rain_3$mean
predARIMAX_Fit_3 = forecast::forecast(fitARIMAX_3,xreg=xreg_pred_3,h=l_3) #test days prediction
summary(predARIMAX_Fit_3)
#accuracy metric
a3=c(mae(test_3,predARIMAX_Fit_3$mean), rmse(test_3,predARIMAX_Fit_3$mean), smape(test_3,predARIMAX_Fit_3$mean))
a3

##ARNNX
fit_nnarx_3 = nnetar(train_3, xreg = xreg_Train_3, repeats = 100)
fit_nnarx_3
summary(fit_nnarx_3)

forecast_nnarx_3 = forecast::forecast(fit_nnarx_3, h = l_3, xreg=xreg_pred_3) # Test days prediction
plot(forecast_nnarx_3)

b3=c(mae(test_3,forecast_nnarx_3$mean), rmse(test_3,forecast_nnarx_3$mean), smape(test_3,forecast_nnarx_3$mean))
b3

##ARIMAX + ARNNX
fit_res_ARNNX_3=nnetar(fitARIMAX_3$residuals,xreg=xreg_Train_3, repeats = 100)
fit_res_ARNNX_3

pred_res_ARNNX_3 = forecast::forecast(fit_res_ARNNX_3, h=l_3, xreg=xreg_pred_3) # Test days prediction
pred_arimax_arnnx_3=predARIMAX_Fit_3$mean+pred_res_ARNNX_3$mean
c3=c(mae(test_3,pred_arimax_arnnx_3), rmse(test_3,pred_arimax_arnnx_3), smape(test_3,pred_arimax_arnnx_3))
c3

## ARIMAX + ARNN
fit_res_ARNN_3 = nnetar(fitARIMAX_3$residuals, repeats = 100)
fit_res_ARNN_3
pred_res_ARNN_3 = forecast::forecast(fit_res_ARNN_3, h = l_3) # Test days prediction
pred_arimax_arnn_3 = predARIMAX_Fit_3$mean + pred_res_ARNN_3$mean
d3 = c(mae(test_3, pred_arimax_arnn_3), rmse(test_3, pred_arimax_arnn_3), smape(test_3, pred_arimax_arnn_3))
d3

##ETSX
fitETSX_3 = es(train_3, model = "ZZZ", xreg = xreg_Train_3)
modelType(fitETSX_3)
predETSX_3 = forecast::forecast(fitETSX_3, h = l_3, xreg = xreg_Test_3)
summary(predETSX_3)
plot(predETSX_3)

e3 = c(mae(test_3, predETSX_3$mean), rmse(test_3, predETSX_3$mean), smape(test_3, predETSX_3$mean))
e3

########################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_4= 40
train_4 = window(cases_ts, start = 1, end=l1-l_4)
test_4 = window(cases_ts, start= l1-l_4+1,end=l1)

##ARIMAX
Case_ts_4=ts(train_4)

xreg_4=data$Rain
xreg_Train_4=window(xreg_4, start = 1, end=l1-l_4)
xreg_Test_4=window(xreg_4, start= l1-l_4+1, end=l1)

auto.arima(train_4,xreg=xreg_Train_4)
fitARIMAX_4 = auto.arima(train_4,xreg=xreg_Train_4)
summary(fitARIMAX_4)

ARIMA_Rain_4 = auto.arima(xreg_Train_4)
summary(ARIMA_Rain_4)

predARIMA_Rain_4= forecast::forecast(ARIMA_Rain_4,h=l_4)
plot(predARIMA_Rain_4)
xreg_pred_4= predARIMA_Rain_4$mean
predARIMAX_Fit_4 = forecast::forecast(fitARIMAX_4,xreg=xreg_pred_4,h=l_4) #test days prediction
summary(predARIMAX_Fit_4)
#accuracy metric
a4=c(mae(test_4,predARIMAX_Fit_4$mean), rmse(test_4,predARIMAX_Fit_4$mean), smape(test_4,predARIMAX_Fit_4$mean))
a4

##ARNNX
fit_nnarx_4 = nnetar(train_4, xreg = xreg_Train_4, repeats = 100)
fit_nnarx_4
summary(fit_nnarx_4)

forecast_nnarx_4 = forecast::forecast(fit_nnarx_4, h = l_4, xreg=xreg_pred_4) # Test days prediction
plot(forecast_nnarx_4)

b4=c(mae(test_4,forecast_nnarx_4$mean), rmse(test_4,forecast_nnarx_4$mean), smape(test_4,forecast_nnarx_4$mean))
b4

##ARIMAX + ARNNX
fit_res_ARNNX_4=nnetar(fitARIMAX_4$residuals,xreg=xreg_Train_4, repeats = 100)
fit_res_ARNNX_4

pred_res_ARNNX_4 = forecast::forecast(fit_res_ARNNX_4, h=l_4, xreg=xreg_pred_4) # Test days prediction
pred_arimax_arnnx_4=predARIMAX_Fit_4$mean+pred_res_ARNNX_4$mean
c4=c(mae(test_4,pred_arimax_arnnx_4), rmse(test_4,pred_arimax_arnnx_4), smape(test_4,pred_arimax_arnnx_4))
c4

## ARIMAX + ARNN
fit_res_ARNN_4 = nnetar(fitARIMAX_4$residuals, repeats = 100)
fit_res_ARNN_4
pred_res_ARNN_4 = forecast::forecast(fit_res_ARNN_4, h = l_4) # Test days prediction
pred_arimax_arnn_4 = predARIMAX_Fit_4$mean + pred_res_ARNN_4$mean
d4 = c(mae(test_4, pred_arimax_arnn_4), rmse(test_4, pred_arimax_arnn_4), smape(test_4, pred_arimax_arnn_4))
d4

##ETSX
fitETSX_4 = es(train_4, model = "ZZZ", xreg = xreg_Train_4)
modelType(fitETSX_4)
predETSX_4 = forecast::forecast(fitETSX_4, h = l_4, xreg = xreg_Test_4)
summary(predETSX_4)
plot(predETSX_4)

e4 = c(mae(test_4, predETSX_4$mean), rmse(test_4, predETSX_4$mean), smape(test_4, predETSX_4$mean))
e4

############################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_5= 36
train_5 = window(cases_ts, start = 1, end=l1-l_5)
test_5 = window(cases_ts, start= l1-l_5+1,end=l1)

##ARIMAX
Case_ts_5=ts(train_5)

xreg_5=data$Rain
xreg_Train_5=window(xreg_5, start = 1, end=l1-l_5)
xreg_Test_5=window(xreg_5, start= l1-l_5+1, end=l1)

auto.arima(train_5,xreg=xreg_Train_5)
fitARIMAX_5 = auto.arima(train_5,xreg=xreg_Train_5)
summary(fitARIMAX_5)

ARIMA_Rain_5 = auto.arima(xreg_Train_5)
summary(ARIMA_Rain_5)

predARIMA_Rain_5= forecast::forecast(ARIMA_Rain_5,h=l_5)
plot(predARIMA_Rain_5)
xreg_pred_5= predARIMA_Rain_5$mean
predARIMAX_Fit_5 = forecast::forecast(fitARIMAX_5,xreg=xreg_pred_5,h=l_5) #test days prediction
summary(predARIMAX_Fit_5)
#accuracy metric
a5=c(mae(test_5,predARIMAX_Fit_5$mean), rmse(test_5,predARIMAX_Fit_5$mean), smape(test_5,predARIMAX_Fit_5$mean))
a5

##ARNNX
fit_nnarx_5 = nnetar(train_5, xreg = xreg_Train_5, repeats = 100)
fit_nnarx_5
summary(fit_nnarx_5)

forecast_nnarx_5 = forecast::forecast(fit_nnarx_5, h = l_5, xreg=xreg_pred_5) # Test days prediction
plot(forecast_nnarx_5)

b5=c(mae(test_5,forecast_nnarx_5$mean), rmse(test_5,forecast_nnarx_5$mean), smape(test_5,forecast_nnarx_5$mean))
b5

##ARIMAX + ARNNX
fit_res_ARNNX_5=nnetar(fitARIMAX_5$residuals,xreg=xreg_Train_5, repeats = 100)
fit_res_ARNNX_5

pred_res_ARNNX_5 = forecast::forecast(fit_res_ARNNX_5, h=l_5, xreg=xreg_pred_5) # Test days prediction
pred_arimax_arnnx_5=predARIMAX_Fit_5$mean+pred_res_ARNNX_5$mean
c5=c(mae(test_5,pred_arimax_arnnx_5), rmse(test_5,pred_arimax_arnnx_5), smape(test_5,pred_arimax_arnnx_5))
c5

## ARIMAX + ARNN
fit_res_ARNN_5 = nnetar(fitARIMAX_5$residuals, repeats = 100)
fit_res_ARNN_5
pred_res_ARNN_5 = forecast::forecast(fit_res_ARNN_5, h = l_5) # Test days prediction
pred_arimax_arnn_5 = predARIMAX_Fit_5$mean + pred_res_ARNN_5$mean
d5 = c(mae(test_5, pred_arimax_arnn_5), rmse(test_5, pred_arimax_arnn_5), smape(test_5, pred_arimax_arnn_5))
d5

##ETSX
fitETSX_5 = es(train_5, model = "ZZZ", xreg = xreg_Train_5)
modelType(fitETSX_5)
predETSX_5 = forecast::forecast(fitETSX_5, h = l_5, xreg = xreg_Test_5)
summary(predETSX_5)
plot(predETSX_5)

e5 = c(mae(test_5, predETSX_5$mean), rmse(test_5, predETSX_5$mean), smape(test_5, predETSX_5$mean))
e5

#################################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_6= 32
train_6 = window(cases_ts, start = 1, end=l1-l_6)
test_6 = window(cases_ts, start= l1-l_6+1,end=l1)

##ARIMAX
Case_ts_6=ts(train_6)

xreg_6=data$Rain
xreg_Train_6=window(xreg_6, start = 1, end=l1-l_6)
xreg_Test_6=window(xreg_6, start= l1-l_6+1, end=l1)

auto.arima(train_6,xreg=xreg_Train_6)
fitARIMAX_6 = auto.arima(train_6,xreg=xreg_Train_6)
summary(fitARIMAX_6)

ARIMA_Rain_6 = auto.arima(xreg_Train_6)
summary(ARIMA_Rain_6)

predARIMA_Rain_6= forecast::forecast(ARIMA_Rain_6,h=l_6)
plot(predARIMA_Rain_6)
xreg_pred_6= predARIMA_Rain_6$mean
predARIMAX_Fit_6 = forecast::forecast(fitARIMAX_6,xreg=xreg_pred_6,h=l_6) #test days prediction
summary(predARIMAX_Fit_6)
#accuracy metric
a6=c(mae(test_6,predARIMAX_Fit_6$mean), rmse(test_6,predARIMAX_Fit_6$mean), smape(test_6,predARIMAX_Fit_6$mean))
a6

##ARNNX
fit_nnarx_6 = nnetar(train_6, xreg = xreg_Train_6, repeats = 100)
fit_nnarx_6
summary(fit_nnarx_6)

forecast_nnarx_6 = forecast::forecast(fit_nnarx_6, h = l_6, xreg=xreg_pred_6) # Test days prediction
plot(forecast_nnarx_6)

b6=c(mae(test_6,forecast_nnarx_6$mean), rmse(test_6,forecast_nnarx_6$mean), smape(test_6,forecast_nnarx_6$mean))
b6

##ARIMAX + ARNNX
fit_res_ARNNX_6=nnetar(fitARIMAX_6$residuals,xreg=xreg_Train_6, repeats = 100)
fit_res_ARNNX_6

pred_res_ARNNX_6 = forecast::forecast(fit_res_ARNNX_6, h=l_6, xreg=xreg_pred_6) # Test days prediction
pred_arimax_arnnx_6=predARIMAX_Fit_6$mean+pred_res_ARNNX_6$mean
c6=c(mae(test_6,pred_arimax_arnnx_6), rmse(test_6,pred_arimax_arnnx_6), smape(test_6,pred_arimax_arnnx_6))
c6

## ARIMAX + ARNN
fit_res_ARNN_6 = nnetar(fitARIMAX_6$residuals, repeats = 100)
fit_res_ARNN_6
pred_res_ARNN_6 = forecast::forecast(fit_res_ARNN_6, h = l_6) # Test days prediction
pred_arimax_arnn_6 = predARIMAX_Fit_6$mean + pred_res_ARNN_6$mean
d6 = c(mae(test_6, pred_arimax_arnn_6), rmse(test_6, pred_arimax_arnn_6), smape(test_6, pred_arimax_arnn_6))
d6

##ETSX
fitETSX_6 = es(train_6, model = "ZZZ", xreg = xreg_Train_6)
modelType(fitETSX_6)
predETSX_6 = forecast::forecast(fitETSX_6, h = l_6, xreg = xreg_Test_6)
summary(predETSX_6)
plot(predETSX_6)

e6 = c(mae(test_6, predETSX_6$mean), rmse(test_6, predETSX_6$mean), smape(test_6, predETSX_6$mean))
e6

##################################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_7= 28
train_7 = window(cases_ts, start = 1, end=l1-l_7)
test_7 = window(cases_ts, start= l1-l_7+1,end=l1)

##ARIMAX
Case_ts_7=ts(train_7)

xreg_7=data$Rain
xreg_Train_7=window(xreg_7, start = 1, end=l1-l_7)
xreg_Test_7=window(xreg_7, start= l1-l_7+1, end=l1)

auto.arima(train_7,xreg=xreg_Train_7)
fitARIMAX_7 = auto.arima(train_7,xreg=xreg_Train_7)
summary(fitARIMAX_7)

ARIMA_Rain_7 = auto.arima(xreg_Train_7)
summary(ARIMA_Rain_7)

predARIMA_Rain_7= forecast::forecast(ARIMA_Rain_7,h=l_7)
plot(predARIMA_Rain_7)
xreg_pred_7= predARIMA_Rain_7$mean
predARIMAX_Fit_7 = forecast::forecast(fitARIMAX_7,xreg=xreg_pred_7,h=l_7) #test days prediction
summary(predARIMAX_Fit_7)
#accuracy metric
a7=c(mae(test_7,predARIMAX_Fit_7$mean), rmse(test_7,predARIMAX_Fit_7$mean), smape(test_7,predARIMAX_Fit_7$mean))
a7

##ARNNX
fit_nnarx_7 = nnetar(train_7, xreg = xreg_Train_7, repeats = 100)
fit_nnarx_7
summary(fit_nnarx_7)

forecast_nnarx_7 = forecast::forecast(fit_nnarx_7, h = l_7, xreg=xreg_pred_7) # Test days prediction
plot(forecast_nnarx_7)

b7=c(mae(test_7,forecast_nnarx_7$mean), rmse(test_7,forecast_nnarx_7$mean), smape(test_7,forecast_nnarx_7$mean))
b7

##ARIMAX + ARNNX
fit_res_ARNNX_7=nnetar(fitARIMAX_7$residuals,xreg=xreg_Train_7, repeats = 100)
fit_res_ARNNX_7

pred_res_ARNNX_7 = forecast::forecast(fit_res_ARNNX_7, h=l_7, xreg=xreg_pred_7) # Test days prediction
pred_arimax_arnnx_7=predARIMAX_Fit_7$mean+pred_res_ARNNX_7$mean
c7=c(mae(test_7,pred_arimax_arnnx_7), rmse(test_7,pred_arimax_arnnx_7), smape(test_7,pred_arimax_arnnx_7))
c7

## ARIMAX + ARNN
fit_res_ARNN_7 = nnetar(fitARIMAX_7$residuals, repeats = 100)
fit_res_ARNN_7
pred_res_ARNN_7 = forecast::forecast(fit_res_ARNN_7, h = l_7) # Test days prediction
pred_arimax_arnn_7 = predARIMAX_Fit_7$mean + pred_res_ARNN_7$mean
d7 = c(mae(test_7, pred_arimax_arnn_7), rmse(test_7, pred_arimax_arnn_7), smape(test_7, pred_arimax_arnn_7))
d7

##ETSX
fitETSX_7 = es(train_7, model = "ZZZ", xreg = xreg_Train_7)
modelType(fitETSX_7)
predETSX_7 = forecast::forecast(fitETSX_7, h = l_7, xreg = xreg_Test_7)
summary(predETSX_7)
plot(predETSX_7)

e7 = c(mae(test_7, predETSX_7$mean), rmse(test_7, predETSX_7$mean), smape(test_7, predETSX_7$mean))
e7


##################################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_8= 24
train_8 = window(cases_ts, start = 1, end=l1-l_8)
test_8 = window(cases_ts, start= l1-l_8+1,end=l1)

##ARIMAX
Case_ts_8=ts(train_8)

xreg_8=data$Rain
xreg_Train_8=window(xreg_8, start = 1, end=l1-l_8)
xreg_Test_8=window(xreg_8, start= l1-l_8+1, end=l1)

auto.arima(train_8,xreg=xreg_Train_8)
fitARIMAX_8 = auto.arima(train_8,xreg=xreg_Train_8)
summary(fitARIMAX_8)

ARIMA_Rain_8 = auto.arima(xreg_Train_8)
summary(ARIMA_Rain_8)

predARIMA_Rain_8= forecast::forecast(ARIMA_Rain_8,h=l_8)
plot(predARIMA_Rain_8)
xreg_pred_8= predARIMA_Rain_8$mean
predARIMAX_Fit_8 = forecast::forecast(fitARIMAX_8,xreg=xreg_pred_8,h=l_8) #test days prediction
summary(predARIMAX_Fit_8)
#accuracy metric
a8=c(mae(test_8,predARIMAX_Fit_8$mean), rmse(test_8,predARIMAX_Fit_8$mean), smape(test_8,predARIMAX_Fit_8$mean))
a8

##ARNNX
fit_nnarx_8 = nnetar(train_8, xreg = xreg_Train_8, repeats = 100)
fit_nnarx_8
summary(fit_nnarx_8)

forecast_nnarx_8 = forecast::forecast(fit_nnarx_8, h = l_8, xreg=xreg_pred_8) # Test days prediction
plot(forecast_nnarx_8)

b8=c(mae(test_8,forecast_nnarx_8$mean), rmse(test_8,forecast_nnarx_8$mean), smape(test_8,forecast_nnarx_8$mean))
b8

##ARIMAX + ARNNX
fit_res_ARNNX_8=nnetar(fitARIMAX_8$residuals,xreg=xreg_Train_8, repeats = 100)
fit_res_ARNNX_8

pred_res_ARNNX_8 = forecast::forecast(fit_res_ARNNX_8, h=l_8, xreg=xreg_pred_8) # Test days prediction
pred_arimax_arnnx_8=predARIMAX_Fit_8$mean+pred_res_ARNNX_8$mean
c8=c(mae(test_8,pred_arimax_arnnx_8), rmse(test_8,pred_arimax_arnnx_8), smape(test_8,pred_arimax_arnnx_8))
c8

## ARIMAX + ARNN
fit_res_ARNN_8 = nnetar(fitARIMAX_8$residuals, repeats = 100)
fit_res_ARNN_8
pred_res_ARNN_8 = forecast::forecast(fit_res_ARNN_8, h = l_8) # Test days prediction
pred_arimax_arnn_8 = predARIMAX_Fit_8$mean + pred_res_ARNN_8$mean
d8 = c(mae(test_8, pred_arimax_arnn_8), rmse(test_8, pred_arimax_arnn_8), smape(test_8, pred_arimax_arnn_8))
d8

##ETSX
fitETSX_8 = es(train_8, model = "ZZZ", xreg = xreg_Train_8)
modelType(fitETSX_8)
predETSX_8 = forecast::forecast(fitETSX_8, h = l_8, xreg = xreg_Test_8)
summary(predETSX_8)
plot(predETSX_8)

e8 = c(mae(test_8, predETSX_8$mean), rmse(test_8, predETSX_8$mean), smape(test_8, predETSX_8$mean))
e8

#####################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_9= 20
train_9 = window(cases_ts, start = 1, end=l1-l_9)
test_9 = window(cases_ts, start= l1-l_9+1,end=l1)

##ARIMAX
Case_ts_9=ts(train_9)

xreg_9=data$Rain
xreg_Train_9=window(xreg_9, start = 1, end=l1-l_9)
xreg_Test_9=window(xreg_9, start= l1-l_9+1, end=l1)

auto.arima(train_9,xreg=xreg_Train_9)
fitARIMAX_9 = auto.arima(train_9,xreg=xreg_Train_9)
summary(fitARIMAX_9)

ARIMA_Rain_9 = auto.arima(xreg_Train_9)
summary(ARIMA_Rain_9)

predARIMA_Rain_9= forecast::forecast(ARIMA_Rain_9,h=l_9)
plot(predARIMA_Rain_9)
xreg_pred_9= predARIMA_Rain_9$mean
predARIMAX_Fit_9 = forecast::forecast(fitARIMAX_9,xreg=xreg_pred_9,h=l_9) #test days prediction
summary(predARIMAX_Fit_9)
#accuracy metric
a9=c(mae(test_9,predARIMAX_Fit_9$mean), rmse(test_9,predARIMAX_Fit_9$mean), smape(test_9,predARIMAX_Fit_9$mean))
a9

##ARNNX
fit_nnarx_9 = nnetar(train_9, xreg = xreg_Train_9, repeats = 100)
fit_nnarx_9
summary(fit_nnarx_9)

forecast_nnarx_9 = forecast::forecast(fit_nnarx_9, h = l_9, xreg=xreg_pred_9) # Test days prediction
plot(forecast_nnarx_9)

b9=c(mae(test_9,forecast_nnarx_9$mean), rmse(test_9,forecast_nnarx_9$mean), smape(test_9,forecast_nnarx_9$mean))
b9

##ARIMAX + ARNNX
fit_res_ARNNX_9=nnetar(fitARIMAX_9$residuals,xreg=xreg_Train_9, repeats = 100)
fit_res_ARNNX_9

pred_res_ARNNX_9 = forecast::forecast(fit_res_ARNNX_9, h=l_9, xreg=xreg_pred_9) # Test days prediction
pred_arimax_arnnx_9=predARIMAX_Fit_9$mean+pred_res_ARNNX_9$mean
c9=c(mae(test_9,pred_arimax_arnnx_9), rmse(test_9,pred_arimax_arnnx_9), smape(test_9,pred_arimax_arnnx_9))
c9

## ARIMAX + ARNN
fit_res_ARNN_9 = nnetar(fitARIMAX_9$residuals, repeats = 100)
fit_res_ARNN_9
pred_res_ARNN_9 = forecast::forecast(fit_res_ARNN_9, h = l_9) # Test days prediction
pred_arimax_arnn_9 = predARIMAX_Fit_9$mean + pred_res_ARNN_9$mean
d9 = c(mae(test_9, pred_arimax_arnn_9), rmse(test_9, pred_arimax_arnn_9), smape(test_9, pred_arimax_arnn_9))
d9

##ETSX
fitETSX_9 = es(train_9, model = "ZZZ", xreg = xreg_Train_9)
modelType(fitETSX_9)
predETSX_9 = forecast::forecast(fitETSX_9, h = l_9, xreg = xreg_Test_9)
summary(predETSX_9)
plot(predETSX_9)

e9 = c(mae(test_9, predETSX_9$mean), rmse(test_9, predETSX_9$mean), smape(test_9, predETSX_9$mean))
e9

########################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_10= 16
train_10 = window(cases_ts, start = 1, end=l1-l_10)
test_10 = window(cases_ts, start= l1-l_10+1,end=l1)

##ARIMAX
Case_ts_10=ts(train_10)

xreg_10=data$Rain
xreg_Train_10=window(xreg_10, start = 1, end=l1-l_10)
xreg_Test_10=window(xreg_10, start= l1-l_10+1, end=l1)

auto.arima(train_10,xreg=xreg_Train_10)
fitARIMAX_10 = auto.arima(train_10,xreg=xreg_Train_10)
summary(fitARIMAX_10)

ARIMA_Rain_10 = auto.arima(xreg_Train_10)
summary(ARIMA_Rain_10)

predARIMA_Rain_10= forecast::forecast(ARIMA_Rain_10,h=l_10)
plot(predARIMA_Rain_10)
xreg_pred_10= predARIMA_Rain_10$mean
predARIMAX_Fit_10 = forecast::forecast(fitARIMAX_10,xreg=xreg_pred_10,h=l_10) #test days prediction
summary(predARIMAX_Fit_10)
#accuracy metric
a10=c(mae(test_10,predARIMAX_Fit_10$mean), rmse(test_10,predARIMAX_Fit_10$mean), smape(test_10,predARIMAX_Fit_10$mean))
a10

##ARNNX
fit_nnarx_10 = nnetar(train_10, xreg = xreg_Train_10, repeats = 100)
fit_nnarx_10
summary(fit_nnarx_10)

forecast_nnarx_10 = forecast::forecast(fit_nnarx_10, h = l_10, xreg=xreg_pred_10) # Test days prediction
plot(forecast_nnarx_10)

b10=c(mae(test_10,forecast_nnarx_10$mean), rmse(test_10,forecast_nnarx_10$mean), smape(test_10,forecast_nnarx_10$mean))
b10

##ARIMAX + ARNNX
fit_res_ARNNX_10=nnetar(fitARIMAX_10$residuals,xreg=xreg_Train_10, repeats = 100)
fit_res_ARNNX_10

pred_res_ARNNX_10 = forecast::forecast(fit_res_ARNNX_10, h=l_10, xreg=xreg_pred_10) # Test days prediction
pred_arimax_arnnx_10=predARIMAX_Fit_10$mean+pred_res_ARNNX_10$mean
c10=c(mae(test_10,pred_arimax_arnnx_10), rmse(test_10,pred_arimax_arnnx_10), smape(test_10,pred_arimax_arnnx_10))
c10

## ARIMAX + ARNN
fit_res_ARNN_10 = nnetar(fitARIMAX_10$residuals, repeats = 100)
fit_res_ARNN_10
pred_res_ARNN_10 = forecast::forecast(fit_res_ARNN_10, h = l_10) # Test days prediction
pred_arimax_arnn_10 = predARIMAX_Fit_10$mean + pred_res_ARNN_10$mean
d10 = c(mae(test_10, pred_arimax_arnn_10), rmse(test_10, pred_arimax_arnn_10), smape(test_10, pred_arimax_arnn_10))
d10

##ETSX
fitETSX_10 = es(train_10, model = "ZZZ", xreg = xreg_Train_10)
modelType(fitETSX_10)
predETSX_10 = forecast::forecast(fitETSX_10, h = l_10, xreg = xreg_Test_10)
summary(predETSX_10)
plot(predETSX_10)

e10 = c(mae(test_10, predETSX_10$mean), rmse(test_10, predETSX_10$mean), smape(test_10, predETSX_10$mean))
e10

########################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_11= 12
train_11 = window(cases_ts, start = 1, end=l1-l_11)
test_11 = window(cases_ts, start= l1-l_11+1,end=l1)

##ARIMAX
Case_ts_11=ts(train_11)

xreg_11=data$Rain
xreg_Train_11=window(xreg_11, start = 1, end=l1-l_11)
xreg_Test_11=window(xreg_11, start= l1-l_11+1, end=l1)

auto.arima(train_11,xreg=xreg_Train_11)
fitARIMAX_11 = auto.arima(train_11,xreg=xreg_Train_11)
summary(fitARIMAX_11)

ARIMA_Rain_11 = auto.arima(xreg_Train_11)
summary(ARIMA_Rain_11)

predARIMA_Rain_11= forecast::forecast(ARIMA_Rain_11,h=l_11)
plot(predARIMA_Rain_11)
xreg_pred_11= predARIMA_Rain_11$mean
predARIMAX_Fit_11 = forecast::forecast(fitARIMAX_11,xreg=xreg_pred_11,h=l_11) #test days prediction
summary(predARIMAX_Fit_11)
#accuracy metric
a11=c(mae(test_11,predARIMAX_Fit_11$mean), rmse(test_11,predARIMAX_Fit_11$mean), smape(test_11,predARIMAX_Fit_11$mean))
a11

##ARNNX
fit_nnarx_11 = nnetar(train_11, xreg = xreg_Train_11, repeats = 100)
fit_nnarx_11
summary(fit_nnarx_11)

forecast_nnarx_11 = forecast::forecast(fit_nnarx_11, h = l_11, xreg=xreg_pred_11) # Test days prediction
plot(forecast_nnarx_11)

b11=c(mae(test_11,forecast_nnarx_11$mean), rmse(test_11,forecast_nnarx_11$mean), smape(test_11,forecast_nnarx_11$mean))
b11

##ARIMAX + ARNNX
fit_res_ARNNX_11=nnetar(fitARIMAX_11$residuals,xreg=xreg_Train_11, repeats = 100)
fit_res_ARNNX_11

pred_res_ARNNX_11 = forecast::forecast(fit_res_ARNNX_11, h=l_11, xreg=xreg_pred_11) # Test days prediction
pred_arimax_arnnx_11=predARIMAX_Fit_11$mean+pred_res_ARNNX_11$mean
c11=c(mae(test_11,pred_arimax_arnnx_11), rmse(test_11,pred_arimax_arnnx_11), smape(test_11,pred_arimax_arnnx_11))
c11

## ARIMAX + ARNN
fit_res_ARNN_11 = nnetar(fitARIMAX_11$residuals, repeats = 100)
fit_res_ARNN_11
pred_res_ARNN_11 = forecast::forecast(fit_res_ARNN_11, h = l_11) # Test days prediction
pred_arimax_arnn_11 = predARIMAX_Fit_11$mean + pred_res_ARNN_11$mean
d11 = c(mae(test_11, pred_arimax_arnn_11), rmse(test_11, pred_arimax_arnn_11), smape(test_11, pred_arimax_arnn_11))
d11

##ETSX
fitETSX_11 = es(train_11, model = "ZZZ", xreg = xreg_Train_11)
modelType(fitETSX_11)
predETSX_11 = forecast::forecast(fitETSX_11, h = l_11, xreg = xreg_Test_11)
summary(predETSX_11)
plot(predETSX_11)

e11 = c(mae(test_11, predETSX_11$mean), rmse(test_11, predETSX_11$mean), smape(test_11, predETSX_11$mean))
e11

########################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_12= 8
train_12 = window(cases_ts, start = 1, end=l1-l_12)
test_12 = window(cases_ts, start= l1-l_12+1,end=l1)

##ARIMAX
Case_ts_12=ts(train_12)

xreg_12=data$Rain
xreg_Train_12=window(xreg_12, start = 1, end=l1-l_12)
xreg_Test_12=window(xreg_12, start= l1-l_12+1, end=l1)

auto.arima(train_12,xreg=xreg_Train_12)
fitARIMAX_12 = auto.arima(train_12,xreg=xreg_Train_12)
summary(fitARIMAX_12)

ARIMA_Rain_12 = auto.arima(xreg_Train_12)
summary(ARIMA_Rain_12)

predARIMA_Rain_12= forecast::forecast(ARIMA_Rain_12,h=l_12)
plot(predARIMA_Rain_12)
xreg_pred_12= predARIMA_Rain_12$mean
predARIMAX_Fit_12 = forecast::forecast(fitARIMAX_12,xreg=xreg_pred_12,h=l_12) #test days prediction
summary(predARIMAX_Fit_12)
#accuracy metric
a12=c(mae(test_12,predARIMAX_Fit_12$mean), rmse(test_12,predARIMAX_Fit_12$mean), smape(test_12,predARIMAX_Fit_12$mean))
a12

##ARNNX
fit_nnarx_12 = nnetar(train_12, xreg = xreg_Train_12, repeats = 100)
fit_nnarx_12
summary(fit_nnarx_12)

forecast_nnarx_12 = forecast::forecast(fit_nnarx_12, h = l_12, xreg=xreg_pred_12) # Test days prediction
plot(forecast_nnarx_12)

b12=c(mae(test_12,forecast_nnarx_12$mean), rmse(test_12,forecast_nnarx_12$mean), smape(test_12,forecast_nnarx_12$mean))
b12

##ARIMAX + ARNNX
fit_res_ARNNX_12=nnetar(fitARIMAX_12$residuals,xreg=xreg_Train_12, repeats = 100)
fit_res_ARNNX_12

pred_res_ARNNX_12 = forecast::forecast(fit_res_ARNNX_12, h=l_12, xreg=xreg_pred_12) # Test days prediction
pred_arimax_arnnx_12=predARIMAX_Fit_12$mean+pred_res_ARNNX_12$mean
c12=c(mae(test_12,pred_arimax_arnnx_12), rmse(test_12,pred_arimax_arnnx_12), smape(test_12,pred_arimax_arnnx_12))
c12

## ARIMAX + ARNN
fit_res_ARNN_12 = nnetar(fitARIMAX_12$residuals, repeats = 100)
fit_res_ARNN_12
pred_res_ARNN_12 = forecast::forecast(fit_res_ARNN_12, h = l_12) # Test days prediction
pred_arimax_arnn_12 = predARIMAX_Fit_12$mean + pred_res_ARNN_12$mean
d12 = c(mae(test_12, pred_arimax_arnn_12), rmse(test_12, pred_arimax_arnn_12), smape(test_12, pred_arimax_arnn_12))
d12

##ETSX
fitETSX_12 = es(train_12, model = "ZZZ", xreg = xreg_Train_12)
modelType(fitETSX_12)
predETSX_12 = forecast::forecast(fitETSX_12, h = l_12, xreg = xreg_Test_12)
summary(predETSX_12)
plot(predETSX_12)

e12 = c(mae(test_12, predETSX_12$mean), rmse(test_12, predETSX_12$mean), smape(test_12, predETSX_12$mean))
e12

########################################################

cases_ts=ts(data$Cases)
l1=length(data$Cases)
l_13= 4
train_13 = window(cases_ts, start = 1, end=l1-l_13)
test_13 = window(cases_ts, start= l1-l_13+1,end=l1)

##ARIMAX
Case_ts_13=ts(train_13)

xreg_13=data$Rain
xreg_Train_13=window(xreg_13, start = 1, end=l1-l_13)
xreg_Test_13=window(xreg_13, start= l1-l_13+1, end=l1)

auto.arima(train_13,xreg=xreg_Train_13)
fitARIMAX_13 = auto.arima(train_13,xreg=xreg_Train_13)
summary(fitARIMAX_13)

ARIMA_Rain_13 = auto.arima(xreg_Train_13)
summary(ARIMA_Rain_13)

predARIMA_Rain_13= forecast::forecast(ARIMA_Rain_13,h=l_13)
plot(predARIMA_Rain_13)
xreg_pred_13= predARIMA_Rain_13$mean
predARIMAX_Fit_13 = forecast::forecast(fitARIMAX_13,xreg=xreg_pred_13,h=l_13) #test days prediction
summary(predARIMAX_Fit_13)
#accuracy metric
a13=c(mae(test_13,predARIMAX_Fit_13$mean), rmse(test_13,predARIMAX_Fit_13$mean), smape(test_13,predARIMAX_Fit_13$mean))
a13

##ARNNX
fit_nnarx_13 = nnetar(train_13, xreg = xreg_Train_13, repeats = 100)
fit_nnarx_13
summary(fit_nnarx_13)

forecast_nnarx_13 = forecast::forecast(fit_nnarx_13, h = l_13, xreg=xreg_pred_13) # Test days prediction
plot(forecast_nnarx_13)

b13=c(mae(test_13,forecast_nnarx_13$mean), rmse(test_13,forecast_nnarx_13$mean), smape(test_13,forecast_nnarx_13$mean))
b13

##ARIMAX + ARNNX
fit_res_ARNNX_13=nnetar(fitARIMAX_13$residuals,xreg=xreg_Train_13, repeats = 100)
fit_res_ARNNX_13

pred_res_ARNNX_13 = forecast::forecast(fit_res_ARNNX_13, h=l_13, xreg=xreg_pred_13) # Test days prediction
pred_arimax_arnnx_13=predARIMAX_Fit_13$mean+pred_res_ARNNX_13$mean
c13=c(mae(test_13,pred_arimax_arnnx_13), rmse(test_13,pred_arimax_arnnx_13), smape(test_13,pred_arimax_arnnx_13))
c13

## ARIMAX + ARNN
fit_res_ARNN_13 = nnetar(fitARIMAX_13$residuals, repeats = 100)
fit_res_ARNN_13
pred_res_ARNN_13 = forecast::forecast(fit_res_ARNN_13, h = l_13) # Test days prediction
pred_arimax_arnn_13 = predARIMAX_Fit_13$mean + pred_res_ARNN_13$mean
d13 = c(mae(test_13, pred_arimax_arnn_13), rmse(test_13, pred_arimax_arnn_13), smape(test_13, pred_arimax_arnn_13))
d13

##ETSX
fitETSX_13 = es(train_13, model = "ZZZ", xreg = xreg_Train_13)
modelType(fitETSX_13)
predETSX_13 = forecast::forecast(fitETSX_13, h = l_13, xreg = xreg_Test_13)
summary(predETSX_13)
plot(predETSX_13)

e13 = c(mae(test_13, predETSX_13$mean), rmse(test_13, predETSX_13$mean), smape(test_13, predETSX_13$mean))
e13



