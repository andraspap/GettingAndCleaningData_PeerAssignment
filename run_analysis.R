#setwd("F:/Andras_work/Courses/Coursera/2013-14/B_SP_DS_04-07_GettingAndCleaningData/PeerAssignment")
library(data.table) # Fast C-based library

#############################################
########## Read data function ###############
#############################################
# Function to read in the raw data tables
raw_data <- function(dataName, setName, bIS = F) {
	# Prepare file name with path
	dirName <- paste("UCI HAR Dataset/", setName, sep="")
	fileName <- paste(dirName, "/", sep="")
		
	if(bIS) {
		fileName <- paste(fileName, "Inertial Signals/", sep="")
	}
	
	fileName <- paste(fileName, dataName, sep="")
	fileName <- paste(fileName, "_", sep="")	
	fileName <- paste(fileName, setName, sep="")	
	fileName <- paste(fileName, ".txt", sep="")
	
	print(fileName) # print it out as a progress indicator
	
	# This a large data set takes long time to read in, specifying colClasses and comment.char speeds things up	
	ret <- data.table(read.table(fileName, colClasses = "numeric", comment.char = ""))
	
	return(ret)
}

#############################################
########## READ DATA ########################
#############################################
features <- read.table("UCI HAR Dataset/Features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

subject_test <- raw_data("subject","test")
X_test <- raw_data("X","test")
y_test <- raw_data("y","test")

subject_train <- raw_data("subject","train")
X_train <- raw_data("X","train")
y_train <- raw_data("y","train")

#############################################
########## Question one #####################
#############################################
# Merge the training and the test sets to create one data set.
A1 <- rbind(X_test, X_train)

#############################################
########## Question two #####################
#############################################
# Extract only the measurements on the mean and standard deviation for each measurement. 
# Add the column names to A1
names(A1) <- as.character(features$V2)
# Get the column names vector
namesA1 <-  as.character(features$V2)

# Get the columns with "mean()" in their name
means <- lapply(namesA1, function(x) length(grep("mean\\(\\)",x)))
# Get the columns with "std()" in their name
stds <- lapply(namesA1, function(x) length(grep("std\\(\\)",x)))
# Joing them to get the names containing "mean()" OR "std()"
namesA1 <- namesA1[as.logical(means) | as.logical(stds)]
# Get those columns of A1 that contain "mean()" OR "std()" in their name
A2 <- A1[,namesA1,with=F]

#############################################
########## Question three ###################
#############################################
# Use descriptive activity names to name the activities in the data set
# Create the activities index vector
y_A1 <- rbind(y_test, y_train)
# Create a data table that has a single column containing the activity names
# in 1 to 1 correspondance to the activity index in A1
A3 <- data.table(unlist(lapply(as.numeric(y_A1$V1),function(x) as.character(activity_labels[x,2]))))
# Create a new data table that contain the activities names
A3 <- cbind(A3,A2)
# Add column name for the activities column
tmp <- names(A3)
tmp[1] <- "Activity"
names(A3) <- tmp

#############################################
########## Question four ####################
#############################################
# Appropriately labels the data set with descriptive activity names.
# Add subject id-s to the data table
# Create the subject id index vector
A4 <- rbind(subject_test,subject_train)
# Create a new data table that contain the subject ids
A4 <- cbind(A4,A3)
# Add column name for the subject id-s column
tmp <- names(A4)
tmp[1] <- "SubjectID"
names(A4) <- tmp

#############################################
########## Question five ####################
#############################################
A5 <- A4[, lapply(.SD, mean), by=c("SubjectID","Activity"), .SDcols=3:68]
# Write it out into a csv file
write.table(A5,file="A5.csv",row.names=F,col.names=T,sep=",",quote=F)


