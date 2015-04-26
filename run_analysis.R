##########################################################################################################
## Coursera Getting and Cleaning Data Course Project
#objective
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names.
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##########################################################################################################
# 1. Merge the training and the test sets to create one data set.
#set working directory to the location
setwd('H:/Documents/UCI HAR Dataset/')
# Read in the data from files
features = read.table('./features.txt',header=FALSE)
activitylabels = read.table('./activity_labels.txt',header=FALSE)
# Read in the train data
subjecttrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain = read.table('./train/X_train.txt',header=FALSE) 
yTrain = read.table('./train/y_train.txt',header=FALSE) 
# Read in the test data
subjecttest = read.table('./test/subject_test.txt',header=FALSE) 
xTest = read.table('./test/x_test.txt',header=FALSE) 
yTest = read.table('./test/y_test.txt',header=FALSE) 

#column names train
colnames(activitylabels) = c('activityId','activityType')
colnames(subjecttrain) = "subjectId"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityId"
Datatraining = cbind(yTrain,subjecttrain,xTrain)

#column names test
colnames(subjecttest) = "subjectId"
colnames(xTest) = features[,2]
colnames(yTest) = "activityId"
Datatest = cbind(yTest,subjecttest,xTest)

#dataset_all
dataset_all = rbind(Datatraining,Datatest)
colNames = colnames(dataset_all)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# LogicalVector contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
dataset_all = dataset_all[logicalVector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set
# Merge the finalData set with the activitylabels table
dataset_all = merge(dataset_all,activitylabels,by='activityId',all.x=TRUE)
colNames = colnames(dataset_all)

# 4. Appropriately label the data set with descriptive activity names.
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(dataset_all) = colNames
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
# Create a new table, finalDataSANSActivityType
finalDataSANSActivityType = dataset_all[,names(dataset_all) != 'activityType']
# Summarizing the finalDataNoActivityType table to include just the mean 
data_out = aggregate(finalDataSANSActivityType[,names(finalDataSANSActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataSANSActivityType$activityId,subjectId = finalDataSANSActivityType$subjectId),mean)
# Merging the tidyData to include descriptive acitvity 
data_out = merge(data_out,activitylabels,by='activityId',all.x=TRUE)
# Export the tidyData set
write.table(data_out, './data_out.txt',row.names=FALSE,sep='\t')
