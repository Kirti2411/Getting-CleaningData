training_data <- read.table("./data/train/X_train.txt")
dim(training_data) 
head(training_data)
training_Label <- read.table("./data/train/y_train.txt")
table(training_Label)
training_Subject <- read.table("./data/train/subject_train.txt")
testData <- read.table("./data/test/X_test.txt")
dim(testData) 
testLabel <- read.table("./data/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./data/test/subject_test.txt")
joinData <- rbind(training_data, testData)
dim(joinData) 
joinLabel <- rbind(training_Label, testLabel)
dim(joinLabel) 
joinSubject <- rbind(training_Subject, testSubject)
dim(joinSubject) 

# Step2. This step will extract the mean and standard deviations. 
features <- read.table("./data/features.txt")
dim(features) 
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
joinData <- joinData[, meanStdIndices]
dim(joinData)
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2])

names(joinData) <- gsub("mean", "Mean", names(joinData))

names(joinData) <- gsub("std", "Std", names(joinData)) names(joinData) <- gsub("-", "", names(joinData)) 


# Step3. For Description of the activities in the data set
act <- read.table("./data/act_labels.txt")
act[, 2] <- tolower(gsub("_", "", act[, 2]))
substr(act[2, 2], 8, 8) <- toupper(substr(act[2, 2], 8, 8))
substr(act[3, 2], 8, 8) <- toupper(substr(act[3, 2], 8, 8))
actLabel <- act[joinLabel[, 1], 2]
joinLabel[, 1] <- actLabel
names(joinLabel) <- "act"

# Step4.Changing the Data Labels as mentioned
 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) # 10299*68
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

# Step5. Creation of tidy data

subjectLen <- length(table(joinSubject)) # 30
actLen <- dim(act)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*actLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
    for(j in 1:actLen) {
        result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
        result[row, 2] <- act[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- act[j, 2] == cleanedData$act
        result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}
head(result)
write.table(result, "data_with_means.txt") # write out the 2nd dataset

