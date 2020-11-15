##1.Merge the training and the test sets to create one data set.
testSet <- read.table("test/X_test.txt", sep = "", header = FALSE)
trainSet <- read.table("train/X_train.txt", sep = "", header = FALSE)
dataSet <- rbind(testSet, trainSet)

##2.Extract only the measurements on the mean and standard deviation for each measurement
featureData <- read.table("features.txt", sep = "", header = FALSE)

mean_set <- which(grepl("mean\\b", featureData$V2))
std_set <- which(grepl("std", featureData$V2))
final_set <- c(mean_set, std_set)
final_set <- sort(final_set)
newDataset <- dataSet[, final_set]

##3.Uses descriptive activity names to name the activities in the data set
set_Test <- read.table("test/subject_test.txt", sep = "", header = FALSE)
y_test <- read.table("table/y_test.txt", sep = "", header = FALSE)
set_Ytraindata <- cbind(set_Test, y_test)
combineSet_Ydata <- rbind(set_Ytestdata, set_Ytraindata)

colnames(combineSet_Ydata)[1] <- "subjectIDs"
colnames(combineSet_Ydata)[2] <- "Activites"

finalDataSet <- merge(subjectIDs, newDataset, by = Activites)

##4.Appropriately labels the data set with descriptive variables names
indices <- sub("^V", "", colnames(completeDataset))
indices <- as.numeric(indices)
indices <- na.omit(indices)
featureDatavector <- featureData$V2
sizeOfindices <- length(indices)

j <- 3
for (i in 1:sizeOfindices) {
 temp <- indice[1]
 colnames(completeDataset)[j] = paste(featureDatavector[temp])
 j <- j + 1
}

j <- 3
for (i in 1:sizeOfindices) {
 colnames(completeDataset)[j] <- sub("^t", "time", colnames(completeDataset)[j])
 colnames(completeDataset)[j] <- sub("^f", "freq", colnames(completeDataset)[j])
 colnames(completeDataset)[j] <- sub("mean", "mean", colnames(completeDataset)[j])
 colnames(completeDataset)[j] <- sub("std", "std", colnames(completeDataset)[j])
 colnames(completeDataset)[j] <- gsub("^[Aa]cc", "accelerometer", colnames(completeDataset)[j])
 colnames(completeDataset)[j] <- sub("^[Gg]yro", "Gyroscope", colnames(completeDataset)[j])
 j <- j + 1
}

colnames(completeDataset)

##5.From the dataset in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject

tidyData <- aggregate(x = completeDataset[, 3:ncol(completeDataset)], by = completeDataset[c("subjectIDs", "Activites")], FUN = mean)

write.table(x = tidyData, file = "tidyDataset.txt", row.names = FALSE)