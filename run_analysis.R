## Set working directory
setwd("E:/Users/Hal/Documents/R/Getting and Cleaning Data/Course Project")

## Load test data
testFileName <- "E://Users//Hal//Documents//R//Getting and Cleaning Data//Course Project//UCI HAR Dataset//test//X_test.txt"
testActivityIdFileName <- "E://Users//Hal//Documents//R//Getting and Cleaning Data//Course Project//UCI HAR Dataset//test//y_test.txt"
testSubjectIdFileName <- "E://Users//Hal//Documents//R//Getting and Cleaning Data//Course Project//UCI HAR Dataset//test//subject_test.txt"

testData <- read.csv(file=testFileName, header=TRUE, sep="\t")
testColCt <- ncol(testData)
testActivityIdData <- read.csv(testActivityIdFileName, header=FALSE, sep="\t")
testSubjectIdData <- read.csv(testSubjectIdFileName, header=FALSE, sep="\t")

## Load train data
trainFileName <- "E://Users//Hal//Documents//R//Getting and Cleaning Data//Course Project//UCI HAR Dataset//train//X_train.txt"
trainActivityIdFileName <- "E://Users//Hal//Documents//R//Getting and Cleaning Data//Course Project//UCI HAR Dataset//train//y_train.txt"
trainSubjectIdFileName <- "E://Users//Hal//Documents//R//Getting and Cleaning Data//Course Project//UCI HAR Dataset//train//subject_train.txt"

trainVector <- scan(trainFileName)
trainMatrix <- matrix(trainVector, nrow = length(trainVector) / testColCt, ncol = testColCt)
trainData <- data.frame(trainMatrix)
trainActivityIdData <- read.csv(trainActivityIdFileName, header=FALSE, sep="\t")
trainSubjectIdData <- read.csv(trainSubjectIdFileName, header=FALSE, sep="\t")

## Combine test and train data
colnames(trainData) <- colnames(testData)
combinedData <- rbind(testData, trainData)
combinedData$activityId <- rbind(testActivityIdData, trainActivityIdData)
combinedData$subjectId <- rbind(testSubjectIdData, trainSubjectIdData)

## Extract mean and std dev
featuresFileName <- "E://Users//Hal//Documents//R//Getting and Cleaning Data//Course Project//UCI HAR Dataset//features.txt"
featureColName <- read.csv(file=featuresFileName, header=FALSE, sep=" ")
colnames(featureColName) <- c("featureId", "featureDescrip")
extractedData <- combinedData[grepl("mean()", featureColName$featureDescrip, fixed = TRUE) | grepl("std()", featureColName$featureDescrip, fixed = TRUE)]

## Add descriptive activity name
activityLabelFileName <- "E://Users//Hal//Documents//R//Getting and Cleaning Data//Course Project//UCI HAR Dataset//activity_labels.txt"
activityLabelData <- read.csv(file=activityLabelFileName, header=FALSE, sep=" ")
colnames(activityLabelData) <- c("activityId", "activityDescrip")
extractedData$activityDescrip <- activityLabelData$activityDescrip[sapply(extractedData$activityId, as.integer)]
extractedData$activityId <- NULL

## Descriptive column names
extractedColNames <- colnames(extractedData)
extractedColNamesVector <- strsplit(extractedColNames, split = ".", fixed = TRUE)

descriptiveName <- function(charVec) {
  if (substr(charVec[1], 1, 1) == "X")
    charVec <- charVec[-1]
  for (i in 1:length(charVec) ) {
    if (nchar(charVec[i]) == 0)
      charVec <- charVec[-i]
  }
  for (i in 1:length(charVec) ) {
    if (charVec[i] == "mean")
      charVec[i] <- "_Mean"
  }
  for (i in 1:length(charVec) ) {
    if (charVec[i] == "std")
      charVec[i] <- "_Std"
  }
  return(paste(charVec, collapse = ""))
}
colnames(extractedData) <- sapply(extractedColNamesVector, descriptiveName)

## Extract average by activity and subject
extractedData$subjectId <- sapply(extractedData$subjectId, as.numeric)
extractedData$subjectId <- factor(extractedData$subjectId)
summarizedExtractedData <- suppressWarnings(aggregate(extractedData, list(Activity = extractedData$activityDescrip, Subject = extractedData$subjectId), mean))
summarizedExtractedData$activityDescrip <- NULL
summarizedExtractedData$subjectId <- NULL

## Write out table 
outputFileName <- "E://Users//Hal//Documents//R//Getting and Cleaning Data//Course Project//step5.txt"
write.table(summarizedExtractedData, file = outputFileName, row.name = FALSE)

