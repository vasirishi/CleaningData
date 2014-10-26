#######################################################
## Program Name:    run_analysis.R
## Author:          Kannan Subbiah
## Date:            10/25/2014
## Version:         1.0.0
#######################################################

#######################################################
## 1.   Merges the training and the test sets to create 
##      one data set.
#######################################################

# Set the working directory to where the data set is stored.
setwd("./UCI HAR Dataset")

# Read Activity Labels text and store in dataframe ActivityLabels
ActivityLabels <- read.table("activity_labels.txt")

# Assign column names for Activity Labels.
names(ActivityLabels) <- c("ActivityCode","Activity")

# Read the Feature labels text and store in dataframe FeatureLabels
FeatureLabels <- read.table("features.txt")

# Assign column names for Feature Labels
names(FeatureLabels) <- c("FeatureId","FeatureDescription")

# Read the "Train Subject" mapping data on to dataframe TrainSubject
TrainSubject <- read.table("train//subject_train.txt")

# Label the TrainSubject data frame columns.
names(TrainSubject) <- c("SubjectId")

TestSubject <- read.table("test//subject_test.txt")

# Label the TestSubject data frame columns.
names(TestSubject) <- c("SubjectId")

# Read the Test Y ActivityCode data
TestY <- read.table("test//y_test.txt")

# Add a Column "Type" with value "Test"  for refer this data as Test Data.
TestY <- cbind(Type = c("Test"), TestY)
names(TestY) <- c("Type","ActivityCode")

# Merge TestY's data with ActivityLabels to combine
# Activity Code with Activity Description
TestY <- merge(TestY, ActivityLabels, by="ActivityCode")

# Read the Train Y ActivityCode data
TrainY <- read.table("train//y_train.txt")

# Add a Column "Type" with value "Train"  for refer this data as Train Data.
TrainY <- cbind(Type = c("Train"), TrainY)

# Add Column names to TrainY
names(TrainY) <- c("Type","ActivityCode")

# Merge TrainY's data with ActivityLabels to combine
# Activity Code with Activity Description
TrainY <- merge(TrainY, ActivityLabels, by="ActivityCode")

# Feature names have characters like -, () etc.
# We need to clean it up 
# make.names cleans up the names and converts those characters in to 
# R accepted column variable names. all the -mean() will be converted to
# .mean..

FeatureLabels$FeatureDescription <- make.names(FeatureLabels$FeatureDescription)

# Select the "mean" column names that end with "mean.."
MeanOnly <- FeatureLabels$FeatureDescription[grep("mean\\.\\.$",FeatureLabels$FeatureDescription)]

# Select the "std" column names that end with "std.."
StdOnly  <- FeatureLabels$FeatureDescription[grep("std\\.\\.$",FeatureLabels$FeatureDescription)]

# Read the Train data from X_train.txt and store in to data frame TrainX
TrainX <- read.table("train//X_train.txt")

# Bind SubjectId, Activity and Type as "Train" with TrainX data.
TrainX <- cbind(TrainSubject, TrainY$Activity, Type = c("Train"), TrainX)

# Name the columns for TrainX
names(TrainX) <- c("Subject", "Activity", "Type",FeatureLabels$FeatureDescription)


# Read the Test data from X_test.txt and store in to data frame TestX

TestX <- read.table("test//X_test.txt")

# Bind SubjectId, Activity and Type as "Test" with TestX data.
TestX <- cbind(TestSubject, TestY$Activity, Type = c("Test"), TestX)

# Name the columns for TestX
names(TestX) <- c("Subject", "Activity", "Type",FeatureLabels$FeatureDescription)

# Merge the TestX and TrainX dataframe together using rbind and store 
# it in to CombinedData

CombinedData <- rbind(TestX, TrainX)


#######################################################
## 2.   Extracts only the measurements on the mean and 
##      standard deviation for each measurement.  
#######################################################

# Create a character list RowIndex to store the 
# Mean and Standard deviation column names for tidying the data.
RowIndex <- c(MeanOnly,StdOnly)

# Extract the SubjectId, Activity and Type and store it in to 
# MyData.  This would be the initial data that we have.
# Now we will extract the mean and standard deviation columns and
# bind it to MyData dataframe.

MyData <- data.frame(CombinedData$Subject, CombinedData$Activity, CombinedData$Type)

# Simple for loop to loop through all the Mean and Standard deviation
# column names stored in RowIndex
for (i in RowIndex) {
 
        # Bind the column names with end with mean and std on to MyData.
        MyData <- cbind(MyData, data.frame(CombinedData[[i]]))

}


#######################################################
# 3.   Uses descriptive activity names to name the 
#       activities in the data set
#######################################################
# Column names for activities are given as part of the 
# following code


# Replace the mean.. with mean and std.. with std in RowIndex

RowIndex <- c(gsub("\\.\\." ,"", RowIndex))

# Replace the mean.. with mean in MeanOnly
MeanOnly <- c(gsub("\\.\\." ,"", MeanOnly))

# Replace the std.. with std in StdOnly
StdOnly <- c(gsub("\\.\\." ,"", StdOnly))

# Add Column names to MyData with all the mean and std column names.
names(MyData) <- c("Subject", "Activity", "Type",RowIndex)


#######################################################
# 3.    Uses descriptive activity names to name the 
#       activities in the data set
# 4.    Appropriately labels the data set with descriptive 
#       variable names. 
#######################################################
# Column names for activities are given as part of the 
# following code

# Data is stored in columns instead of rows in the data set.
# All the means are to be summarised in to a column with individual 
# mean values as part of column data instead of name itself.
# To do with we use the gather function to split the columns having mean in to Name and Value.
# In this case for eg: tBodyAccMag.mean will be gathered in to 
# Mean as tBodyAccMag.mean and MeanValue as its value

GatherData <- MyData %>%
     gather(Mean, MeanValue,tBodyAccMag.mean:fBodyBodyGyroJerkMag.mean) %>%
     gather(Std, StdValue, tBodyAccMag.std:fBodyBodyGyroJerkMag.std)
names(GatherData)  <- c("Subject", "Activity", "Type", "MeanType", "MeanValue", "StdType", "StdValue")

#   MeanType has values ending with .mean.  Eg: tBodyAccMag.mean.  
#   We need to separate the .mean from the value.
#   The following command seprates the value in to tBodyAccMag 
#   under Feature name and StdName column and “mean” 
#   under “Mean” , std under Std column.  
#   Now that Mean column and Std column are redundant we can drop them.

SeparateData <- GatherData %>%
    separate(MeanType, into = c("Feature","Mean"), sep = "\\.") %>%
    separate(StdType, into = c("StdName", "Std"), sep = "\\.")

# Following code removes those redundant columns Mean and Std 
# which has values “mean” and “std”.   StdName and Feature are 
# redundant. So we can drop StdName as well.
SeparateData <- subset(SeparateData, select = -c(Mean,StdName,Std))

#######################################################
# 5.    From the data set in step 4, creates a second, 
#       independent tidy data set with the average of 
#       each variable for each activity and each subject.
#######################################################

# 
TidyData <- SeparateData %>%
     group_by(Subject, Activity, Feature) %>%
     select(MeanValue, StdValue) %>%
     summarise(Mean = mean(MeanValue), StandardDeviation = mean(StdValue))

write.csv(TidyData, file="TidyData.txt", row.names=FALSE, quote = FALSE)

