library("caTools")

climate <- read.csv("climate_change.csv")

print("Structure of climate dataset")
str(climate)

print("Summary of climate dataset")
print(summary(climate))

print("Splitting dataset into train and test sets")
set.seed(28)
sample.split(climate$Temp, SplitRatio=0.75)
train <- subset(climate, split==TRUE)
test <- subset(climate, split==FALSE)

print("Removing any NA values in the train and test sets")
train <- subset(train, !(is.na(train$Temp)))
test <- subset(test, !(is.na(test$Temp)))

print("Summary of train set")
print(summary(train))
print("Summary of test set")
print(summary(test))

print("Creating initial linear reg model")
model=lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
print(summary(model))

print("Checking	 correlation between variables")
print(cor(climate))

print("Creating improved linear reg model 1")
model1=lm(Temp ~ MEI + N2O + CFC.11 + TSI + Aerosols, data = train)
print(summary(model1))

print("Creating improved linear reg model 2")
model2=lm(Temp ~ MEI + N2O + TSI + Aerosols, data = train)
print(summary(model2))

print("Using the step function to obtain a simplified model automatically")
model3=step(model)
print(summary(model3))

print("Using test data to predict Temp, multiple R-squared value is: ")
tempPredict <- predict(model3, newdata=test)
SSE <- sum((tempPredict - test$Temp)^2)
SST <- sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
print(R2)