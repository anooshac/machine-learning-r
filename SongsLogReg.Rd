library("caTools")

songs <- read.csv("songs.csv")

print("Structure of songs dataset")
str(songs)

print("Summary of songs dataset")
print(summary(songs))

print("Splitting dataset into train and test sets")
set.seed(28)
songsTrain <- subset(songs, year<=2009)
songsTest <- subset(songs, year>2009)

nonNumericVars <- c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain <- songsTrain[, !(names(songsTrain) %in% nonNumericVars)]
songsTest <- songsTest[, !(names(songsTest) %in% nonNumericVars)]

print("Creating initial logistic reg model")
model=glm(Top10 ~ ., data=songsTrain, family=binomial)
print(summary(model))

print("Checking correlation between variables")
print(cor(songsTrain))

print("Creating improved logistic reg model 2")
model2=glm(Top10 ~ .-energy, data=songsTrain, family=binomial)
print(summary(model2))

print("Using test data to predict Top10")
top10Predict <- predict(model2, newdata=songsTest, type="response")
print("Creating a confusion matrix, with threshold of 0.45")
print(table(songsTest$Top10, top10Predict>=0.45))
print("Computing accuracy, accuracy = ")
accuracy=(309+19)/(309+5+40+19)
print(accuracy)

print("Comparing against a baseline model, where no songs make it to top 10")
print("Computing accuracy of baseline model, accuracy = ")
baselineAccuracy=(309+5)/(309+5+40+19)
print(baselineAccuracy)