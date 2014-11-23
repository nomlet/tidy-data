library(dplyr)
library(reshape2)

# Datasets downloaded locally to "./UCI HAR Dataset" directory.

# Extract descriptive variable names for each type of recorded
# measurement from "features.txt".

var_names <- read.table("UCI HAR Dataset/features.txt", 
        stringsAsFactors = FALSE, col.names = c("id", "measurement"))

# Extract activity description from "activity_labels.txt".

activity <- read.table("UCI HAR Dataset/activity_labels.txt", 
        stringsAsFactors = FALSE, col.names = c("code", "activity_type"))

# Read in test and train data:  Label the columns with descriptive
# names taken from "features.txt".

test <- read.table("UCI HAR Dataset/test/X_test.txt",
                   stringsAsFactors = FALSE, col.names = var_names$measurement)
train <- read.table("UCI HAR Dataset/train/X_train.txt",
                   stringsAsFactors = FALSE, col.names = var_names$measurement)

# Read in label for the activity type for each record.  Will later merge
# into data.frame as an "activity" column.

test_label <- read.table("UCI HAR Dataset/test/y_test.txt",
                         stringsAsFactors = FALSE)
train_label <- read.table("UCI HAR Dataset/train/y_train.txt",
                         stringsAsFactors = FALSE)

# Read in subject id for each record.  Will later merge into the data.frame
# as "subject" column.

test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt",
                           stringsAsFactors = FALSE)
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt",
                           stringsAsFactors = FALSE)

# Add columns to identify the subject and the activity.
# (Activity is a factor with levels labeled by the activity type.)

test <- mutate(test, 
    activity = factor(test_label$V1, labels = activity$activity_type), 
    subject = test_subject$V1)
train <- mutate(train, 
    activity = factor(train_label$V1, labels = activity$activity_type),
    subject = train_subject$V1)

# Merge the test and train data sets into one.

total <- rbind(test,train)

# Subset the combined set, leaving only data corresponding to 
# calculated mean and std values.  (Also leave the subject id and
# activity type.)

sub <- select(total, matches("(mean|std|activity|subject)"))

# "Melt" data.frame for easy summary of ALL measurements.

sub_melt <- melt(sub, id.vars = c("subject", "activity"))

# Breakdown measurements by subject and activity.

sub_group <- group_by(sub_melt, subject, activity, variable)

# Calculate the average of each measurement for all subject/activity
# combinations.

total_bd <- summarize(sub_group, avg = mean(value))

# Now "recast" as a data.frame with one variable in each column.

tidy <- dcast(total_bd, subject + activity ~ variable)

# Output the new "tidy" dataset.

write.table(tidy, file = "tidy.txt", row.name = FALSE)