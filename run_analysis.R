library(dplyr)

# Download files from web
zip_file <- "Coursera_GCD_PGQ.zip"
# Checking if archieve already exists.
if (!file.exists(zip_file)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, zip_file, method="curl")
} 

# Checking if the file is already uncompressed
if (!file.exists("UCI HAR Dataset")) { 
  unzip(zip_file) 
} 

# Load text files to data tables
subject_train <- read.table("peer-graded//UCI HAR Dataset//train/subject_train.txt", col.names = "Subject")
subject_test <- read.table("peer-graded//UCI HAR Dataset/test//subject_test.txt", col.names = "Subject")
y_train <- read.table("peer-graded//UCI HAR Dataset/train/y_train.txt", col.names = "Activity")
y_test <- read.table("peer-graded//UCI HAR Dataset/test/y_test.txt", col.names = "Activity")
feature_names <- read.table("peer-graded//UCI HAR Dataset/features.txt", col.names = c("Index", "Function"))
X_train <- read.table("peer-graded/UCI HAR Dataset/train/X_train.txt", col.names = feature_names$Function)
X_test <- read.table("peer-graded/UCI HAR Dataset/test//X_test.txt", col.names = feature_names$Function)
activities <- read.table("peer-graded/UCI HAR Dataset/activity_labels.txt", col.names = c("Activity", "ActivityLongName"), header = FALSE)


# Verify if everything looks good
#head(subject_train)
#head(X_train)
#head(y_train)
#head(subject_test)
#head(X_test)
#head(y_test)
#head(activities)

# TASK 1: Combine all training and testing data
training_data <- cbind(subject_train, X_train, y_train)
head(training_data)
testing_data <- cbind(subject_test, X_test, y_test)
head(testing_data)
merged_data <- rbind(training_data, testing_data)
head(merged_data)

# TASK 2: Extract only mean and std measurements
merged_data <- select(merged_data, Subject, Activity, contains("mean"), contains("std"))

# TASK 3: Use descriptive activity names to name the activities in the data set
merged_data <- mutate(merged_data, "Activity" = activities$ActivityLongName[merged_data$Activity])

# TASK 4: Appropriately label the data set with descriptive variable names
names(merged_data) <- gsub("Acc", "Accelerometer", names(merged_data))
names(merged_data) <- gsub("Gyro", "Gyroscope", names(merged_data))
names(merged_data) <- gsub("BodyBody", "Body", names(merged_data))
names(merged_data) <- gsub("Mag", "Magnitude", names(merged_data))
names(merged_data) <- gsub("^t", "Time", names(merged_data))
names(merged_data) <- gsub("^f", "Frequency", names(merged_data))
names(merged_data) <- gsub("tBody", "TimeBody", names(merged_data))
names(merged_data) <- gsub("-mean()-", "Mean", names(merged_data), ignore.case = TRUE)
names(merged_data) <- gsub("-std()-", "STD", names(merged_data), ignore.case = TRUE)
names(merged_data) <- gsub("-freq()-", "Frequency", names(merged_data), ignore.case = TRUE)
names(merged_data) <- gsub("angle", "Angle", names(merged_data))
names(merged_data) <- gsub("gravity", "Gravity", names(merged_data))
#names(merged_data)

# TASK 5: Create new/final data set with average of each variable for each activity and subject
tidy_data <- merged_data %>%
  group_by(Subject, Activity) %>%
  select(Subject, Activity, everything()) %>% 
  summarise_all(funs(mean))
#head(tidy_data)

# Dump the final tidy data table to a text file
write.table(tidy_data, "Final_Tidy_Data.txt", row.name = FALSE)


