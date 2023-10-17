

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


###########################################################
var_ts=ts(data$var)
l1=length(data$var)
l= n
train = window(var_ts, start = 1, end=l1-l)
test = window(var_ts, start= l1-l+1,end=l1)

## ARIMAX
Case_ts=ts(train)

xreg=data$Exo_var
xreg_Train=window(xreg, start = 1, end=l1-l)
xreg_Test=window(xreg, start= l1-l+1, end=l1)

auto.arima(train,xreg=xreg_Train)
fitARIMAX = auto.arima(train,xreg=xreg_Train)
summary(fitARIMAX)

ARIMA_Exo_var = auto.arima(xreg_Train) 
summary(ARIMA_Exo_var)

predARIMA_Exo_var= forecast::forecast(ARIMA_Exo_var,h=l) 
plot(predARIMA_Exo_var)
xreg_pred= predARIMA_Exo_var$mean
predARIMAX_Fit = forecast::forecast(fitARIMAX,xreg=xreg_pred,h=l) #test days prediction
summary(predARIMAX_Fit)


## ARNNX
fit_nnarx = nnetar(train, xreg = xreg_Train, repeats = 100)
fit_nnarx
summary(fit_nnarx)

forecast_nnarx = forecast::forecast(fit_nnarx, h = l, xreg=xreg_pred) # Test days prediction
plot(forecast_nnarx)


## ARIMAX + ARNNX
fit_res_ARNNX=nnetar(fitARIMAX$residuals,xreg=xreg_Train, repeats = 100)
fit_res_ARNNX

pred_res_ARNNX = forecast::forecast(fit_res_ARNNX, h=l, xreg=xreg_pred) # Test days prediction
pred_arimax_arnnx=predARIMAX_Fit$mean+pred_res_ARNNX$mean


## ARIMAX + ARNN 
fit_res_ARNN=nnetar(fitARIMAX$residuals, repeats = 100)
fit_res_ARNN
pred_res_ARNN = forecast::forecast(fit_res_ARNN, h=l) # Test days prediction
pred_arimax_arnn=predARIMAX_Fit$mean+pred_res_ARNN$mean


##ETSX
fitETSX = es(train, model = "ZZZ", xreg = xreg_Train)
modelType(fitETSX)
predETSX = forecast::forecast(fitETSX, h=l, xreg = xreg_Test)
summary(predETSX)
plot(predETSX)


