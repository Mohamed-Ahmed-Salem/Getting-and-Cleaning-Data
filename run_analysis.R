setwd("C:/Study/Data Science Specialization/Getting and Cleaning Data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train")
train_x <-  read.table("X_train.txt",  sep="\t",fill=FALSE, strip.white=TRUE)
train_y <- read.table("y_train.txt",  sep="\t",fill=FALSE, strip.white=TRUE)
subject_train <- read.table("subject_train.txt",  sep="\t",fill=FALSE, strip.white=TRUE)

setwd("C:/Study/Data Science Specialization/Getting and Cleaning Data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test")
test_x <- read.table("X_test.txt",  sep="\t",fill=FALSE, strip.white=TRUE)
test_y <- read.table("y_test.txt",  sep="\t",fill=FALSE, strip.white=TRUE)
subject_test <- read.table("subject_test.txt",  sep="\t",fill=FALSE, strip.white=TRUE)

X <- rbind(train_x, test_x)
Y <- rbind(train_y, test_y)
S <- rbind(subject_train, subject_test)

setwd("C:/Study/Data Science Specialization/Getting and Cleaning Data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")
features <- read.table("features.txt",  sep="\t",fill=FALSE, strip.white=TRUE)
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 1])
X <- X[indices_of_good_features ,]
names(X) <- features[indices_of_good_features, 1]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))



activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"





names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")



uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]



row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(array(as.numeric(tmp[, 3:numCols]),dim = c(1, 2)))
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt")
