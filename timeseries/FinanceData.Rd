library("forecast")
library("TTR")

print("Reading data into dataframe")
gsData <- read.csv("GS_15 years.csv")

print("Structure of gsData")
str(gsData)

print("Summary of gsData")
print(summary(gsData))

print("Taking a look at some rows of data")
print(head(gsData))

print("Plotting data for visualization")
plot(gsData$Close, type="l", ylab="Close")

print("Checking PACF of data")
pacf(gsData$Close)

print("Checking ACF of data")
acf(gsData$Close)

print("Checking PACF and ACF after doing first degree differencing")
pacf(diff(gsData$Close))
print("After the first lag, the correlogram trails off. Though there are a few lags that exceed the significance value, this could be due to chance or seasonality. p in ARIMA(p, d, q) is 1.")
acf(diff(gsData$Close))
print("After the first lag, the autocorrelogram trails off. q in ARIMA(p, d, q) is 1.")

print("Doing the Ljung-Box test after differencing data")
print(Box.test(diff(gsData$Close), type="Ljung-Box"))

print("Plot differenced data to check if mean and variance look constant")
plot(diff(gsData$Close), type="l", ylab="Close", main="Differenced data")

print("Creating time series of the data")
gsTs <- ts(gsData$Close, frequency=365, start=c(1999, 5, 4), end=c(2016, 9, 9))

print("Decomposing data into trend, seasonal and irregular components")
gsDecompose <- decompose(gsTs)

print("Plotting components of the data")
plot(gsDecompose)

print("Creating ARIMA model with order (1, 1, 0)")
gsTsArima <- arima(gsTs, order=c(1,1,0))
print(gsTsArima)

print("Forecasting for the next 400 days")
gsTsForecasts <- forecast.Arima(gsTsArima, h=400, level=c(99.5))
print(head(gsTsForecasts))

print("Plotting forecast numbers")
plot.forecast(gsTsForecasts)

print("Checking ACF and PACF for the forecast errors")
pacf(gsTsForecasts$residuals, main="PACF")
acf(gsTsForecasts$residuals, main="ACF")

print("Doing the Ljung-Box test")
print(Box.test(gsTsForecasts$residuals, type="Ljung-Box"))

print("Trying something else...")
gsTsPred <- predict(gsTsArima, n.ahead=400)
plot(gsTsPred$pred, col="blue", ylim=c(20,270))
lines(gsTsPred$pred+2*gsTsPred$se, col="red")
lines(gsTsPred$pred-2*gsTsPred$se, col="red")
