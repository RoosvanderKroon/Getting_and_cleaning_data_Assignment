#setwd to folder with the data and start reading it into r
#first features
features<- read.table("features.txt", col.names = c("n", "functions"))

#load test and train data

x_test <- read.table("test/X_test.txt", col.names = features$functions)
x_train <- read.table("train/X_train.txt", col.names = features$functions)

y_test <- read.table("test/y_test.txt", col.names = "code")
y_train <- read.table("train/y_train.txt", col.names = "code")

#load subject data
subject_test <- read.table("test/subject_test.txt", col.names = "subject")
subject_train <- read.table("train/subject_train.txt", col.names = "subject")

#combine it all
combined<-rbind(subject_train, subject_test)
y_data <- rbind(y_train, y_test)
x_data <- rbind(x_train, x_test)

#Then the big merge
merged_data <- cbind(combined, y_data, x_data)


# Step 2
mean_std_cols <- grepl("mean\\(\\)|std\\(\\)", names(merged_data))

##first two columns, is code and subjects, and from them you wany only the col that have mean and std
selected_columns <- c(TRUE, TRUE, mean_std_cols)
filtered_data <- merged_data[, selected_columns]



#step3

activities <- read.table("activity_labels.txt", col.names = c("code", "activity"))

# Replace numeric activity codes with names using merge
descriptive_data <- merge(filtered_data, activities, by = "code", all.x = TRUE)


#Step 4 renaming labels with gsub
names(descriptive_data) <- names(descriptive_data) %>%
  gsub("^t", "Time", .) %>%
  gsub("^f", "Frequency", .) %>%
  gsub("Acc", "Accelerometer", .) %>%
  gsub("Gyro", "Gyroscope", .) %>%
  gsub("Mag", "Magnitude", .) %>%
  gsub("BodyBody", "Body", .) %>%
  gsub("-mean\\(\\)", "Mean", .) %>%
  gsub("-std\\(\\)", "STD", .) %>%
  gsub("-meanFreq\\(\\)", "MeanFrequency", .) %>%
  gsub("-X", "X", .) %>%
  gsub("-Y", "Y", .) %>%
  gsub("-Z", "Z", .) %>%
  gsub("\\()", "", .) %>%
  gsub("-", "", .)

#Step 5 tiy data set
tidy_data <- descriptive_data %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean), .groups = "drop")
tidy_data




