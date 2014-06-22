## Download and unzip file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

dataExist <- file.exists("UCI HAR Dataset")
if (!dataExist) {
    download.file(url, destfile = "getdata-projectfiles-UCI HAR Dataset.zip", method = "curl")
    unzip("getdata-projectfiles-UCI HAR Dataset.zip")
    file.remove("getdata-projectfiles-UCI HAR Dataset.zip")
}

## Merges the training and the test sets to create one data set.
## Import Labels
label <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE,
                    col.names = c("y", "label"))

features <- read.table("UCI HAR Dataset/features.txt", row.names = 1, stringsAsFactors = FALSE,
                       col.names = c("ID", "fnames"))

features <- features$fnames

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "Subject Train")

xTrain <- read.table("UCI HAR Dataset/train/X_train.txt")

names(xTrain) <- features

yTrain <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "y")

train <- cbind(subjectTrain, xTrain, yTrain)

## Test data to create one dataset
test <- read.Table("UCI HAR Dataset/test/subject_test.txt", col.names = "Subject Train")

xTest <- read.table("UCI HAR Dataset/test/X_test.txt") names(xTest) <- colnames

yTest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "y")

masterTest <- cbind(test, xTest, yTest)

data <-rbind(masterTrain, test)

## Extracts only the measurements on the mean and standard deviation for each measurement.
meanSTD <- grep("mean\\(|std\\(", names(data), value = TRUE)
data <- data[, c("Subject Train", "y", meanSTD)]

data <- inner_join(data, label)

## Relabel & Reorder columns with descriptive activity names
data <- data[, c("Subject Train", "label", meanSTD)]

colname <- names(data)[-(1:2)]
colname <- gsub("\\-|\\(|\\)", "", colname)
colname <- gsub("-std$","StdDev", colname)
colname <- gsub("-mean","Mean", colname)
colname <- gsub("^(t)", "Time", colname) 
colname <- gsub("^(f)", "Frequency", colname)
colname <- gsub("([Gg]ravity)","Gravity", colname)
colname <- gsub("Gyro", "Gyroscope", colname) 
colname <- gsub("Acc", "Accelerometer", colname)
colname <- gsub("Mag", "Magnitude", colname)
colname <- gsub("BodyBody", "Body", colname)
colname <- gsub("JerkMag","JerkMagnitude", colname)
colname <- gsub("GyroMag","GyroMagnitude", colname)
colname <- tolower(colname)
names(data)[-(1:2)] <- colname

## Create a second, independent tidy data set with the average of each 
## variable for each activity and each subject. 
data_aggregate <- data %>% group_by(Subject Train, label) %>% summarise_each(funs(mean))

write.csv(data_aggregate, "tidydata.csv", row.names = FALSE, sep = "\t")






