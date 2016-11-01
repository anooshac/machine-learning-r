library("forecast")
library("TTR")

print("Reading data into dataframe")
gsData <- read.csv("GS_15 years.csv")

print("Plotting data for visualization")
plot(gsData$Close, type="l", ylab="Close")

print("Checking PACF of data")
pacf(data$Close)

print("Checking ACF of data")
acf(data$Close)

print("Checking PACF and ACF after doing differencing")
pacf(diff(log(data$Close)))
print("After the first lag, the correlogram trails off. Though there are a few lags that exceed the significance value, this could be due to chance or seasonality.")
acf(diff(log(data$Close)))
print("After the first lag, the autocorrelogram trails off.")

print("Doing the Ljung-Box test after differencing data")
print(Box.test(diff(log(data$Close)), type="Ljung-Box"))

print("Plot differenced data to check if mean and variance look constant")
plot((diff(log(data$Close))), type="l", ylab="Close", main="Differenced data")

print("Creating time series of the data")
timeseries <- ts(data$Close, frequency=365, start=c(1999, 5, 4), end=c(2016, 9, 9))

print("Decomposing data into trend, seasonal and irregular components")
decomposed <- decompose(timeseries)
print("Plotting components of the data")
plot(decomposed)
