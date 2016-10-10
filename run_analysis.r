#Library Import
library(dplyr)
library(data.table)

#Get Current Working Directory
path <- getwd()
setwd(path)
path

# Bullet 1 - Merges the training and the test sets to create one data set.
#Download the data for the excersice
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {
  dir.create(path)
}
download.file(url, file.path(path, f))  

#Unzip the file to access all the inner files (Running this from Macos X)
unzip(f, files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = ".", unzip = "internal", setTimes = FALSE)
  
#Target the Path of the unziped files
pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

#Read the subject train data
dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
#Read the subject test data
dtSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))

#Read the train data
dtActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
#Read the test data
dtActivityTest <- fread(file.path(pathIn, "test", "Y_test.txt"))

#This function creates a data Frame out of data files (previously loaded)
fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}

#Create the Data Table for both Test and Train Data
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(pathIn, "test", "X_test.txt"))

#Data table concatenation
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)
#Merge the columns
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)
#Set key
setkey(dt, subject, activityNum)

# Bullet 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
#Read the mesuarement variables based on the "features.txt" file
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

#Subset the measurements the mean and standard deviation.
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
#print the result
head(dtFeatures)

dtFeatures$featureCode

#Subset the variables using variable names
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]

#Bullet 3 - Uses descriptive activity names to name the activities in the data set
#Read the "activity_labels.txt" file
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

#Merge activity labels
dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)

#Add Key
setkey(dt, subject, activityNum, activityName)

#Merge the data in order to change the formar
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))

#Merge Activity name
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", all.x = TRUE)

#Bullet 4 - Appropriately labels the data set with descriptive variable names
#Create a new variable "activity" equivalent to activityName and a new variable "feature" equivalent to featureName
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

#Separate features from deatureName
grepthis <- function(regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

r1 <- nrow(dt[, .N, by = c("feature")])
r2 <- nrow(dt[, .N, by = c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

#Bullet 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]

write.table(dtTidy, file = "DatasetHumanActivityRecognitionUsingSmartphones.csv", sep = "\t", row.name=FALSE)
