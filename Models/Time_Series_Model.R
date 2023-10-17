#' @title Time Series Modeling and Forecasting with ARIMAX, NNETAR, and ETSX
#' @description
#' This code performs time series modeling and forecasting using various methods, including ARIMAX, NNETAR, and ETSX models.
#' @param n - An integer value indicating the desired forecast horizons (positive integer).
#' @param train - A time series object for training the model.
#' @param test - A time series object for testing the model.
#' @param x_reg - A covariate time series object used for training.
#' @section ARIMAX: ARIMA with Exogenous Variables
#' The ARIMAX section of the code fits an ARIMA model with exogenous variables.
#' It uses the auto.arima function to automatically select the ARIMA model order.
#' @section NNETAR: Neural Network Time Series Forecasting
#' The NNETAR section utilizes the nnetar function to perform time series forecasting using neural networks.
#' @section ARIMAX + NNETAR
#' This section combines ARIMAX and NNETAR models for forecasting.
#' @section ARIMAX + ARNN
#' Here, ARIMAX and ARNN models are combined for forecasting.
#' @section ETSX: Exponential Smoothing with Exogenous Variables
#' The ETSX section applies Exponential Smoothing with exogenous variables using the es function.
#' @packages required - tseries, forecast


var_ts=ts(data$var)
L=length(data$var)
l= n
train = window(var_ts, start = 1, end=L-l)
test = window(var_ts, start= L-l+1,end=L)

xreg=data$Exo_var
xreg_Train=window(xreg, start = 1, end=L-l)
xreg_Test=window(xreg, start= L-l+1, end=L)


## ARIMAX
fitARIMAX = auto.arima(train,xreg=xreg_Train)
ARIMA_Exo_var = auto.arima(xreg_Train) 

predARIMA_Exo_var= forecast::forecast(ARIMA_Exo_var,h=l) 
xreg_pred= predARIMA_Exo_var$mean
pred_ARIMAX = forecast::forecast(fitARIMAX,xreg=xreg_pred,h=l) 


## ARNNX
fit_nnarx = nnetar(train, xreg = xreg_Train, repeats = 100)
fit_nnarx

pred_nnarx = forecast::forecast(fit_nnarx, h = l, xreg=xreg_pred) 


## ARIMAX + ARNNX
fit_res_ARNNX=nnetar(fitARIMAX$residuals,xreg=xreg_Train, repeats = 100)
fit_res_ARNNX

pred_res_ARNNX = forecast::forecast(fit_res_ARNNX, h=l, xreg=xreg_pred) 
pred_arimax_arnnx=pred_ARIMAX$mean+pred_res_ARNNX$mean


## ARIMAX + ARNN 
fit_res_ARNN=nnetar(fitARIMAX$residuals, repeats = 100)
fit_res_ARNN

pred_res_ARNN = forecast::forecast(fit_res_ARNN, h=l) 
pred_arimax_arnn=pred_ARIMAX$mean+pred_res_ARNN$mean


##ETSX
fitETSX = es(train, model = "ZZZ", xreg = xreg_Train)
modelType(fitETSX)

pred_ETSX = forecast::forecast(fitETSX, h=l, xreg = xreg_Test)
