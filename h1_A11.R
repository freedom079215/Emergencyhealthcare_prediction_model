H1data_A11 <- msts(H1[1:1500,'A11'],start=1 ,seasonal.periods=c(6,42))
H1data
plot(H1data_A11)
acf(H1data_A11)
pacf(H1data_A11)

H1data_A11arma <- arima(H1data_A11, order = c(6,0,0))
H1data_A11armaforecast<-forecast.Arima(H1data_A11arma,h=690,level=c(99.5))
H1data_A11armaforecast
plot(forecast(H1data_A11armaforecast))
plot(forecast(H1data_A11arma))

acf(H1data_A11armaforecast$residuals, lag.max = 20)
Box.test(H1data_A11armaforecast$residuals, lag=20, type="Ljung-Box")
plot.ts(H1data_A11arma$residuals)
plotForecastErrors(H1data_A11arma$residuals)

accuracy(H1data_A11armaforecast$mean,H1[1501:2190,'A11'] )
