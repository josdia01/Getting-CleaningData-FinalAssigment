
######################################
#Johns Hopkins University
## Course: Getting and Cleaning Data 
### FINAL ASSIGMENT

### JOSE JAIME DIAZ GARCIA
### https://github.com/josdia01
######################################

# The purpose of this project is to demonstrate your ability to collect, work with, 
# and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 
# You will be graded by your mentors on a series of yes/no questions related to the project. 
# You will be required to submit: 
    # 1) a tidy data set as described below, 
    # 2) a link to a Github repository with your script for performing the analysis, 
    # 3) a code book that describes the variables, the data, and any transformations or 
    #    work that you performed to clean up the data called CodeBook.md. 
    # You should also include a README.md in the repo with your scripts. 
    # This repo explains how all of the scripts work and how they are connected.

# A full description is available at the site where the data was obtained:
  # http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:
  # https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# You should create one R script called run_analysis.R that does the following.
    # Merges the training and the test sets to create one data set.
    # Extracts only the measurements on the mean and standard deviation for each measurement.
    # Uses descriptive activity names to name the activities in the data set
    # Appropriately labels the data set with descriptive variable names.
    # From the data set in step 4, creates a second, independent tidy data set with the average 
    # of each variable for each activity and each subject.

#Step 0: Libraries needed
library(reshape2)



# Step 1: Download the file, unzip it, and charge it in r.
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- file.path(getwd(), "Dataset.zip")
filename <- "Dataset.zip"


download.file(URL , destfile = destfile)

if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}


#Step 2: Load activity labels + extra features and info

activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

activityLabels[,2] <- as.character(activityLabels[,2])
features[,2] <- as.character(features[,2])

# Step 3 (question 1): Load de Train and Test dataset
trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")

testData <- read.table("UCI HAR Dataset/test/X_test.txt")
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")

joinData <- rbind(trainData, testData)
joinActivity <- rbind(trainActivities, testActivities)

Complete <- cbind(joinData , joinActivity)



#Step 4 (question 2): Extracts only the measurements on the mean and standard deviation. 

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
joinData <- joinData[, meanStdIndices]
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

joinData

# Step5 (question 3): Uses descriptive activity names to name the activities in the data set

activityLabels[, 2] <- tolower(gsub("_", "", activityLabels[, 2]))
substr(activityLabels[2, 2], 8, 8) <- toupper(substr(activityLabels[2, 2], 8, 8))
substr(activityLabels[3, 2], 8, 8) <- toupper(substr(activityLabels[3, 2], 8, 8))
activityLabel <- activityLabels[joinActivity[, 1], 2]

joinActivity[, 1] <- activityLabel
names(joinActivity) <- "activity"

joinActivity

# Step6 (question 4):  Appropriately labels the data set with descriptive activity names. 

joinSubject <- rbind(trainSubjects, testSubjects)


cleanedData <- cbind(joinSubject, joinActivity, joinData)
write.table(cleanedData, "merged_data.txt") # get the clean dataset in txt to future use.


# Step6 (question 5):   Creates a second, independent tidy data set with the average of each variable for 
# each activity and each subject. 

subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(activityLabels)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activityLabels[j, 2]
    bool1 <- i == cleanedData$v1
    bool2 <- activityLabels[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result, "data_with_means.txt") # write out the 2nd dataset


