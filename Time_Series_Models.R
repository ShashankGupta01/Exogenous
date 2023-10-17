#' @title Implementation of deep learning forecasting models for time series modelling
#' @param \code{train} is a time series object for training the model
#' @param \code{test} is a time series object for testing the model
#' @param \code{x_reg} is a covariate time series object used for training
#' @param n an \code{integer} value indicating the desired forecast horizons
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
predARIMAX_Fit = forecast::forecast(fitARIMAX,xreg=xreg_pred,h=l) 


## ARNNX
fit_nnarx = nnetar(train, xreg = xreg_Train, repeats = 100)
fit_nnarx

forecast_nnarx = forecast::forecast(fit_nnarx, h = l, xreg=xreg_pred) 


## ARIMAX + ARNNX
fit_res_ARNNX=nnetar(fitARIMAX$residuals,xreg=xreg_Train, repeats = 100)
fit_res_ARNNX

pred_res_ARNNX = forecast::forecast(fit_res_ARNNX, h=l, xreg=xreg_pred) 
pred_arimax_arnnx=predARIMAX_Fit$mean+pred_res_ARNNX$mean


## ARIMAX + ARNN 
fit_res_ARNN=nnetar(fitARIMAX$residuals, repeats = 100)
fit_res_ARNN
pred_res_ARNN = forecast::forecast(fit_res_ARNN, h=l) 
pred_arimax_arnn=predARIMAX_Fit$mean+pred_res_ARNN$mean


##ETSX
fitETSX = es(train, model = "ZZZ", xreg = xreg_Train)
modelType(fitETSX)
predETSX = forecast::forecast(fitETSX, h=l, xreg = xreg_Test)
