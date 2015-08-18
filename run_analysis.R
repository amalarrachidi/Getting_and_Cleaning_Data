# Question1: Merges the training and the test sets to create one data set.

trainData <- read.table("Getting-and-Cleaning-Data/train/X_train.txt") 
dim(trainData) # 7352*561
head(trainData)

trainLabel <- read.table("Getting-and-Cleaning-Data/train/y_train.txt")
table(trainLabel)

trainSubject <- read.table("Getting-and-Cleaning-Data/train/subject_train.txt")
testData <- read.table("Getting-and-Cleaning-Data/test/X_test.txt")
dim(testData) # 2947*561

testLabel <- read.table("Getting-and-Cleaning-Data/test/y_test.txt") 
table(testLabel) 

testSubject <- read.table("Getting-and-Cleaning-Data/test/subject_test.txt")

joinData <- rbind(trainData, testData)
dim(joinData) # 10299*561
joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) # 10299*1

joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) # 10299*1


# Question2:Extracts only the measurements on the mean and standard deviation for each measurement. 


ftr <- read.table("Getting-and-Cleaning-Data/features.txt")
dim(ftr)  # 561*2
Mean_Std <- grep("mean\\(\\)|std\\(\\)", ftr[, 2])
length(Mean_Std) # 66
joinData <- joinData[, Mean_Std]
dim(joinData) # 10299*66
names(joinData) <- gsub("\\(\\)", "", ftr[Mean_Std, 2])


# Question3: Uses descriptive activity names to name the activities in the data set

ACT <- read.table("Getting-and-Cleaning-Data/activity_labels.txt")
ACT[, 2] <- tolower(gsub("_", "", ACT[, 2]))
substr(ACT[2, 2], 8, 8) <- toupper(substr(ACT[2, 2], 8, 8))
substr(ACT[3, 2], 8, 8) <- toupper(substr(ACT[3, 2], 8, 8))
ACT_LBL <- ACT[joinLabel[, 1], 2]
joinLabel[, 1] <- ACT_LBL
names(joinLabel) <- "activity"


# Question4: Appropriately labels the data set with descriptive variable names.

names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) # 10299*68
write.table(cleanedData, "merged_data.txt")


# Question 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(ACT)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- ACT[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- ACT[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result,"data_final.txt",row.names = FALSE)