library("forecast")
library("TTR")

print("Reading data into dataframe")
data <- read.csv("GS_1year_StockData.csv")
summary(data)

plot(data$Date, data$Close, xlab="Dates", ylab="Close price", type="l", col="red", main="Closing prices of Goldman Sachs for past 2 years")

print("PACF of original data")
pacf(gsData$Close)
print("ACF of original data")
acf(gsData$Close)

pacf(diff(gsData$Close))
acf(diff(gsData$Close))

print("Creating training and testing sets")
gs_time <- ts(data$Close, frequency=52)
gs_train <- gs_time[1:(0.9 * length(gs_time))]
gs_test <- gs_time[(0.9 * length(gs_time + 1)):length(gs_time)]

print("Building ARIMA model")
fit <- arima(gs_train, order=c(1,2,1))

print("Forecasting close prices")
arima_forecasts <-forecast.Arima(fit, h=length(gs_test))
print(arima_forecasts)
print(accuracy(arima_forecasts, gs_test))