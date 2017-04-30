## run_analysis uses a list of files in the folder UCI HAR Dataset  
## in RStudio default workspace.  So please make sure all data files
## for this project are placed there before run this method.
##
## run_analysis performs the following functions: 
## 1. merges two data frames created from text files X_train.txt and X_test.txt into one data frame.
## 2. extract columns that contain -std() or -mean() into a new data frame.
## 3. group the new data frame by subject, activity columns into a new data frame.
## 4. summarise grouped data frame by applying mean function to all columns, grouped by subject, activity columns.
## 5. summarised data set should have 180 rows data for all 30 subjects (volunteers), each has 6 acitivity categories.
## 6. write the final data frame to a text file.
## 7. print out the output file path.

run_analysis <- function (){
  
  ##load dplyr library
  library(dplyr)
  
  fileTrain <- "UCI HAR Dataset/train/X_train.txt"
  fileTrainLable <-"UCI HAR Dataset/train/y_train.txt"
  fileTest  <- "UCI HAR Dataset/test/X_test.txt"
  fileTestLabel <- "UCI HAR Dataset/test/y_test.txt"
  fileTrainSubject <- "UCI HAR Dataset/train/subject_train.txt"
  fileTestSubject <- "UCI HAR Dataset/test/subject_test.txt"
  
  fileoutput <- "accelerometer-gyroscope-mean-std-dataset.txt"
  
  datTrain <- get_activity_record(fileTrain, fileTrainSubject, fileTrainLable)
  datTest  <- get_activity_record(fileTest, fileTestSubject,fileTestLabel)
  
  targetExpression <- "-std()|-mean()"
  
  datAll <- bind_rows(datTrain[,1:dim(datTrain)[2]], datTest[, 1:dim(datTest)[2]])
  
  ## create a data frame with one hundred million rows by repeatedly appending itself. 
  # counter <- 10
  # repeat{
  #       counter <- counter -1
  #       datAll <- bind_rows(datAll[,1:dim(datAll)[2]], datAll[, 1:dim(datAll)[2]])
  #       print (paste("total rows: ", dim(datAll)[1], sep=""))
  #       print (object.size(datAll), units="GB")
  #       if (counter==1) break
  # }
  # print (dim(select(datAll, subject, activity, grep(targetExpression, names(datAll)))))
  datAll %>% select(subject, activity, grep(targetExpression, names(datAll))) %>%
  group_by(subject, activity) %>% 
  summarise_all(mean) %>% 
  as.data.frame() %>% 
  write.table(fileoutput, row.names=FALSE)
  
  print(paste("output file: ", getwd(), "/", fileoutput, sep=""))

}

##  get_activity_subject_records performs the following function:
##  1. take subject record file, either from traing or from testing data set.
##  2. create a subject data frame with its column named as subject.
##
##

get_activity_subject_records <- function(fileActivitySubjectRecord)
{
      if (is.na(fileActivitySubjectRecord) | fileActivitySubjectRecord=="")
        fileActivitySubjectRecord<-"UCI HAR DataSet/test/subject_test.txt"
      
      datActivitySubjectRecord <- read.table(fileActivitySubjectRecord, sep="", header =FALSE, stringsAsFactor =FALSE)
      datActivitySubjectRecord <- rename(datActivitySubjectRecord, subject=V1)
      datActivitySubjectRecord
}

##  get_activity_records_label performs the following functions:
##  1. takes a activity label file from either testing or training data set.
##  2. creates a data frame that has activity label records from either testing or training data set.
##  3. add a new column named as measurementnumber to this data frame using each row index of the data frame.
##  4. merge this data frame with six activity labels data frame by matching their id to create a new data frame
##     that contains both activity label records and their index, which is the measurementnumber.
##  5. sort the final data frame by its measurementnumber from low to high.  This is to insure it to be properly bound
##  to activity records data frame later on.
##

get_activity_records_label <- function(fileActivityRecordsLabel){
    
    fileActivity <- "UCI HAR DataSet/activity_labels.txt"
    datActivity <- read.table(fileActivity, sep="", stringsAsFactors = FALSE, header= FALSE)
   
    if (is.na(fileActivityRecordsLabel) | fileActivityRecordsLabel=="")
        fileActivityRecordsLabel<-"UCI HAR DataSet/test/y_test.txt"
    
    datActivityRecordsLabels <- read.table(fileActivityRecordsLabel, sep="", header =FALSE, stringsAsFactor =FALSE)
    
    ## create an index column to keep original record index order in place.
    datActivityRecordsLabels <- mutate(datActivityRecordsLabels, recordindex = row(datActivityRecordsLabels))
    ## add column headers to both activity data frame and activity label records data frame.
    datActivity <- rename(datActivity, activityid=V1, activity=V2)
    datActivityRecordsLabels <- rename(datActivityRecordsLabels, activityid=V1)
    
    ## merge activity label data frame with activity records to create a labeled activity record data frame.
    ## the resulting data frame will be used to bind with activity records via bind_cols call.
    datActivityRecordsLabels <- merge(datActivityRecordsLabels, datActivity, by.x="activityid", by.y="activityid", all=TRUE)
    
    ## sort the final data frame by measurementnumber so it has original record data order.
    datActivityRecordsLabels <- datActivityRecordsLabels[order(datActivityRecordsLabels[["recordindex"]]),]
    datActivityRecordsLabels 
}

##  get_activity_record performs the following functions:
##  1. create a feature data frame using feature list file, features.txt.
##  2. the feature data frame should contains 561 rows as explained in features.txt.
##  3. create a activity record data frame using specified activity records file, either X_train.txt or X_test.txt.
##  4. create a activity subject data frame using specified subject file, either subject_train.txt or subject_test.txt
##  5. create a activity record label data frame using specified file, either y_train.txt or y_test.txt.
##  6. check the number of rows in each of the data frames and make sure the following assumptions true:
##        a. each activity record should have 561 columns that are equal to the number of rows in feature data frame.
##        b. the total number of activity records should be equal to the total number of activity subject data frame.
##        c. the total number of activity records should be equal to the total number of activity label data frame.
##  7. set column names of activity record data frame using the features from feature data frame.
##  8. bind activity subject data frame, activity label data frame to activity data frame so the output data frame 
##     has two new columns, subject and activity, making it total 563 columns.

get_activity_record <- function (fileactivityrecord, fileactivitysubjectrecord, fileactivitylabelrecord){
      
      ## setup default files
      fileFeatures <- "UCI HAR DataSet/features.txt"
      
      if (is.na(fileactivitysubjectrecord) | fileactivitysubjectrecord=="" )
        fileactivitysubjectrecord <- "UCI HAR Dataset/test/subject_test.txt"
    
      if (!file.exists(fileactivitylabelrecord)) stop (paste("Activity label file", fileactivitylabelrecord, "not found.", sep=" "))
      
      if (is.na(fileactivitylabelrecord) | fileactivitylabelrecord=="" )
        fileactivitylabelrecord <- "UCI HAR Dataset/test/y_test.txt"
      
      if (!file.exists(fileactivitysubjectrecord)) stop(paste("Subject file", fileactivitysubjectrecord, "not found.", sep=" "))
      ## create an activity subject records data frame from suject record file.
      datActivitySubjects<-get_activity_subject_records(fileactivitysubjectrecord)
      ## create an activity label records data frame from the activity lable record file.
      datActivityRecordsLabels <- get_activity_records_label(fileactivitylabelrecord)
    
      ## create a feature data frame from feature list file.
      datFeatures <- read.table(fileFeatures, header = FALSE, sep="", stringsAsFactors=FALSE)
      
      ## create activity measurement record data frame.
      if (is.na(fileactivityrecord) | fileactivityrecord=="")
        fileactivityrecord <- "UCI HAR Dataset/test/X_test.txt"
      
      if (!file.exists(fileactivityrecord)) stop (paste("Activity data record file", fileactivityrecord, "not found", sep=" "))
      
      datRecords <- read.table(fileactivityrecord, header=F, sep="", stringsAsFactors = F)
      
      ## get rows in all data frames that are going to be bound or merged.
      intrecords = dim(datRecords)[1]
      intcols = dim(datRecords)[2]
      intfeatures = dim(datFeatures)[1]
      intsubjects = dim(datActivitySubjects)[1]
      intactivitylabels = dim(datActivityRecordsLabels)[1]
      
      if (intfeatures!=intcols) stop("feature list does match training records.")
      ## set column headers of the activity records using feature list
      datRecords<- setNames(datRecords, datFeatures[,2])
      
      if (intrecords != intactivitylabels) stop("The number of measured records does not match activity label records. ")
      
      if (intrecords != intsubjects) stop("The number of measured records does not match subject records. ")
      
      ## Merge acitivity label records with training activity records via bind_cols call.
      ## this is done after the confirmation that they both have the same numer of rows.
      datRecords <- datActivitySubjects %>% bind_cols(datActivityRecordsLabels) %>% bind_cols(datRecords)
      
      datRecords
}