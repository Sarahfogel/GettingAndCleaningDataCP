#============Getting and Cleaning Data Course Project===========================

#Begin by downloading and unzipping the data file into your working directory
#  It should still be in the folder titled "UCI HAR Dataset"

#==============Step 1: Merging the training and test sets=======================

# First read the two sets in

    train.set<-read.table("UCI HAR Dataset//train//X_train.txt")
    test.set<-read.table("UCI HAR Dataset//test//X_test.txt")


#Rename the data sets with their names so they can be worked with more easily
# NOTE: This is step 4, but I found it easier to work with the data with descriptive
# labels already in place

    labels<-read.table("UCI HAR Dataset//features.txt")

    names(train.set)<-labels$V2
    names(test.set)<-labels$V2

#Combine the two data sets with a row bind

    all.data<-rbind(train.set, test.set)


#=============Step 2: Extract the mean and sd for each measurement============== 

#Extract the mean and sd for each measurement by locating the variable names that
#  have either "-mean()" or "-std()" in the names and subsetting to just include those
#  columns.

    Mean.Sd.Only<-all.data[,c(grep("-mean()", fixed=T, names(all.data)), grep("-std()", fixed=T, names(all.data)))]

#===================Step 3:Descriptive Activity Names===========================

# First read in the activity codes

    train.codes<-read.table("UCI HAR Dataset//train//y_train.txt")
    test.codes<-read.table("UCI HAR Dataset//test//y_test.txt")

# Combine the 2 vectors
    activity.codes<-c(train.codes$V1, test.codes$V1)

# Convert to a factor with descriptive labels
    activity.codes<-as.factor(activity.codes)

    levels(activity.codes)<-c("Walking", "Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing", "Lying")
# Add that column onto the previous data frame

    Mean.Sd.Activity<-cbind(activity.codes=activity.codes, Mean.Sd.Only)


#=================Step 4: Descriptive Variable Names============================

#This has already been done in step 1 above.  I found it easier to extract the 
#   desired columns when they were already descriptively labelled.


#===Step 5: New Data Set - Avg of each var for each activity for each subject====

#Begin by adding the subjects codes to the data
    subject.train<-read.table("UCI HAR Dataset//train//subject_train.txt")
    subject.test<-read.table("UCI HAR Dataset//test//subject_test.txt")
    all.subject<-c(subject.train$V1, subject.test$V1)

    Mean.Sd.Activity.Subj<-cbind(Subject=all.subject, Mean.Sd.Activity)


# Use split-apply-combine method for each variable

    Final.Data<-data.frame(rep(NA, 180))

    for (i in 1:66) {
    
        splitData<-split(Mean.Sd.Activity.Subj[,i+2], 
                         f=list(Mean.Sd.Activity.Subj$activity.codes, Mean.Sd.Activity.Subj$Subject) )
        means<-sapply(splitData, mean)
        Final.Data[,i]<-means
        names(Final.Data)[i]<-names(Mean.Sd.Activity.Subj)[i+2]

    }

    Final.Data<- cbind(Group=names(means), Final.Data)

#=====================Write table for upload====================================

    write.table(Final.Data, file="FinalData.txt", row.names=FALSE)


