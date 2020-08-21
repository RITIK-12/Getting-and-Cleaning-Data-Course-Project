# run_analysis.R should do the following:
#   
#   1)  Merges the training and the test sets to create one data set.
# 
#   2)  Extracts only the measurements on the mean and standard deviation for each measurement. 
# 
#   3)  Uses descriptive activity names to name the activities in the data set
# 
#   4)  Appropriately labels the data set with descriptive variable names. 
#   
#   5)  From the data set in step 4, creates a second, independent tidy data set with the average
#       of each variable for each activity and each subject.

#     Please upload the tidy data set created in step 5 of the instructions. 

#     Please upload your data set as a txt file created with write.table() using row.name=FALSE 
#     (do not cut and paste a dataset directly into the text box, as this may cause errors 
#     saving your submission).

# Load the needed packages
packages <- c("data.table", "reshape2", "dplyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# Assumes the Git repository : https://github.com/dholtz/GettingAndCleaningData
# has been cloned to a users local machine, and the R, setwd(), has been used 
# to set the working directory to the root of this cloned repository.
path <- getwd()

# Give warning to set the working directory if not able to find data files.
projectDataPath <- file.path(path, "project_data")
fileCount <- length(list.files(projectDataPath, recursive=TRUE))
if (fileCount != 28) {
  stop("Please use setwd() to the root of the cloned repository.")
}

# Read in the 'Subject' data
dtTrainingSubjects <- fread(file.path(projectDataPath, "train", "subject_train.txt"))
dtTestSubjects  <- fread(file.path(projectDataPath, "test" , "subject_test.txt" ))

# Read in the 'Activity' data
dtTrainingActivity <- fread(file.path(projectDataPath, "train", "Y_train.txt"))
dtTestActivity  <- fread(file.path(projectDataPath, "test" , "Y_test.txt" ))

# Read in the 'Measurements' data
# Switching to standard, read.table to avoid the following possible error:
# https://github.com/Rdatatable/data.table/issues/487
# No time to figure out where this, 'works again now' version is
dtTrainingMeasures <- data.table(read.table(file.path(projectDataPath, "train", "X_train.txt")))
dtTestMeasures  <- data.table(read.table(file.path(projectDataPath, "test" , "X_test.txt")))

# Row merge the Training and Test Subjects
# http://www.statmethods.net/management/merging.html
dtSubjects <- rbind(dtTrainingSubjects, dtTestSubjects)
# Why setnames() ?? http://stackoverflow.com/questions/10655438/rename-one-named-column-in-r
setnames(dtSubjects, "V1", "subject")

# Row merge the Training and Test Activities
dtActivities <- rbind(dtTrainingActivity, dtTestActivity)
setnames(dtActivities, "V1", "activityNumber")

# Merge the Training and Test 'Measurements' data
dtMeasures <- rbind(dtTrainingMeasures, dtTestMeasures)

# Column merge the subjects to activities
dtSubjectActivities <- cbind(dtSubjects, dtActivities)
dtSubjectAtvitiesWithMeasures <- cbind(dtSubjectActivities, dtMeasures)

# Order all of the combined data by, subject and activity
setkey(dtSubjectAtvitiesWithMeasures, subject, activityNumber)

## Read in the 'features.txt' 
## This file matches up to the columns in the data.table, dtSubjectActivitiesWithMeasures
## with the features/measures.
dtAllFeatures <- fread(file.path(projectDataPath, "features.txt"))
setnames(dtAllFeatures, c("V1", "V2"), c("measureNumber", "measureName"))

# Use grepl to just get features/measures related to mean and std
dtMeanStdMeasures <- dtAllFeatures[grepl("(mean|std)\\(\\)", measureName)]
# Create a column to 'index/cross reference' into the 'measure' headers
# in dtSubjectActivitiesWithMeasures
dtMeanStdMeasures$measureCode <- dtMeanStdMeasures[, paste0("V", measureNumber)]

# Build up the columns to select from the data.table,
# dtSubjectActivitiesWithMeasures
columnsToSelect <- c(key(dtSubjectAtvitiesWithMeasures), dtMeanStdMeasures$measureCode)
# Just take the rows with the columns of interest ( std() and mean() )
dtSubjectActivitesWithMeasuresMeanStd <- subset(dtSubjectAtvitiesWithMeasures, 
                                                select = columnsToSelect)

# Read in the activity names and give them more meaningful names
dtActivityNames <- fread(file.path(projectDataPath, "activity_labels.txt"))
setnames(dtActivityNames, c("V1", "V2"), c("activityNumber", "activityName"))

# Merge the 'meaningful activity names' with the 
# dtSubjectActiitiesWithMeasuresMeanStd
dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd, 
                                               dtActivityNames, by = "activityNumber", 
                                               all.x = TRUE)

# Sort the data.table, dtSubjectActivitesWithMeasuresMeanStd
setkey(dtSubjectActivitesWithMeasuresMeanStd, subject, activityNumber, activityName)

# Convert from a wide to narrow data.table using the keys created earlier
dtSubjectActivitesWithMeasuresMeanStd <- data.table(melt(dtSubjectActivitesWithMeasuresMeanStd, 
                                                         id=c("subject", "activityName"), 
                                                         measure.vars = c(3:68), 
                                                         variable.name = "measureCode", 
                                                         value.name="measureValue"))

# Merge measure codes
dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd, 
                                               dtMeanStdMeasures[, list(measureNumber, measureCode, measureName)], 
                                               by="measureCode", all.x=TRUE)

# Convert activityName and measureName to factors
dtSubjectActivitesWithMeasuresMeanStd$activityName <- 
  factor(dtSubjectActivitesWithMeasuresMeanStd$activityName)
dtSubjectActivitesWithMeasuresMeanStd$measureName <- 
  factor(dtSubjectActivitesWithMeasuresMeanStd$measureName)

# Reshape the data to get the averages 
measureAvgerages <- dcast(dtSubjectActivitesWithMeasuresMeanStd, 
                          subject + activityName ~ measureName, 
                          mean, 
                          value.var="measureValue")

# Write the tab delimited file
write.table(measureAvgerages, file="tidyData.txt", row.name=FALSE, sep = "\t")