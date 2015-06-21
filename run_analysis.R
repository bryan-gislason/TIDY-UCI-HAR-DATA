### get dependencies
library(dplyr)

### Read files

# Test files
subjectTest.file <- "subject_test.txt"
XTest.file <- "X_test.txt"
YTest.file <- "Y_test.txt"
test.dir <- "test"
XTest.path <- paste(test.dir, "/", XTest.file, sep="")
YTest.path <- paste(test.dir, "/", YTest.file, sep="")
subjectTest.path <- paste(test.dir, "/", subjectTest.file, sep="")
YTest.data <- read.table(YTest.path)
XTest.data <- read.table(XTest.path)
subjectTest.data <- read.table(subjectTest.path)

# Train files
subjectTrain.file <- "subject_train.txt"
XTrain.file <- "X_train.txt"
YTrain.file <- "Y_train.txt"
train.dir <- "train"
XTrain.path <- paste(train.dir, "/", XTrain.file, sep="")
XTrain.path <- paste(train.dir, "/", XTrain.file, sep="")
subjectTrain.path <- paste(train.dir, "/", subjectTrain.file, sep="")
YTrain.data <- read.table(YTrain.path)
XTrain.data <- read.table(XTrain.path)
subjectTrain.data <- read.table(subjectTrain.path)

# read in features variable names
varNames <- read.table("features.txt")

# Bind the subject IDs, label IDs, with feature data
train.data <- cbind(subjectTrain.data, YTrain.data, XTrain.data)
test.data <- cbind(subjectTest.data, YTest.data, XTest.data)

# Merge by row the train and test data
trainTest.data <- rbind(train.data, test.data)


### Rename trainTest column names

### Appropriately labels the data set with descriptive variable names.
trainTest.colnames <- c("subject", "activity", as.character(varNames$V2))
valid_column_names <- make.names(names=trainTest.colnames, unique=TRUE, allow_ = TRUE)
# set colnames
colnames(trainTest.data) <- valid_column_names


### Extracts only the measurements on the mean and standard deviation for each measurement.
# X data was moved over by 2 columns, so add 2 to the indices from the features indices
stdmeanColIndices <- grep("std()|mean()", varNames$V2)
trainTest.mean_std <- select(trainTest.data, subject:activity, stdmeanColIndices + 2)

### Uses descriptive activity names to name the activities in the data set
activityLabels <- read.table("activity_labels.txt")

for (i in 1:nrow(activityLabels)) {
    trainTest.mean_std[trainTest.mean_std$activity == i, 2] <- as.character(activityLabels[i,]$V2)
}


### Tidy data set
trainTest.mean_std.mean_by_subject_activity <- aggregate(trainTest.mean_std, by=list(subjectId = trainTest.mean_std$subject, activityLabel = trainTest.mean_std$activity), mean) %>% select(-(3:4))
colnames(trainTest.mean_std.mean_by_subject_activity)[1] <- "subject"
colnames(trainTest.mean_std.mean_by_subject_activity)[2] <- "activity"

write.table(trainTest.mean_std.mean_by_subject_activity, file="tidyData.txt", row.name=FALSE)
