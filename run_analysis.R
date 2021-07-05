# The function call needs no arguments in this case:
runyourself <- function(){  
        
        # set up the environment with deplyr:        
        install.packages("dplyr")
        library(dplyr)

        # load in the training data:
        # train_subjects - these are the code numbers for the subjects selected for the train data of the study
        # train_activities - these are the code numbers for the six activities investigated
        # train_set - these are the measurements taken by the device
        train_subjects <- read.table("data/fit/UCI HAR Dataset/train/subject_train.txt")
        train_activities <- read.table("data/fit/UCI HAR Dataset/train/y_train.txt")
        train_set <- read.table("data/fit/UCI HAR Dataset/train/x_train.txt")
        
        # build the training dataframe called 'train_data' (note this includes the left 2 columns for subject and activity ID):
        train_data <- cbind(train_subjects, train_activities, train_set)

        # load in the test data:
        # test_subjects - these are the code numbers for the subjects selected for the test data of the study
        # test_activities - these are the code numbers for the six activities investigated
        # test_set - these are the measurements taken by the device        
        test_subjects <- read.table("data/fit/UCI HAR Dataset/test/subject_test.txt")
        test_activities <- read.table("data/fit/UCI HAR Dataset/test/y_test.txt")
        test_set <- read.table("data/fit/UCI HAR Dataset/test/x_test.txt")
        
        # build the test dataframe called 'test_data' (note this includes the left 2 columns for subject and activity ID):
        test_data <- cbind(test_subjects, test_activities, test_set)
        
        # now to load in and modify the features, which will be our column names:
        # 'features' are the 561 descriptive measures taken for each individual and each activity in this study
        # I first read it it, excise the 2nd column, which has the actual descriptions,
        # Last, I concatenate to the top the titles 'subject' and 'activity' for the two columns I add to identify each row
        features <- read.table("data/fit/UCI HAR Dataset/features.txt")
        features <- features$V2
        features <- c("subject", "activity", features) 
        
        # Combine the dataframes:
        data <- rbind(train_data, test_data)
        
        # Give them column names:
        colnames(data) <- features
        
        # Swap out the activity codes with activity names, 1 substitution at a time:
        data[,2] <- gsub("1", "walking", data[,2])
        data[,2] <- gsub("2", "walking_upstairs", data[,2])
        data[,2] <- gsub("3", "walking_downstairs", data[,2])
        data[,2] <- gsub("4", "sitting", data[,2])
        data[,2] <- gsub("5", "standing", data[,2])
        data[,2] <- gsub("6", "laying", data[,2])
        
        # Now to subset those columns with 'std' or 'mean' in the title:
        # df2 - a sub-df made only of those columns with 'std' in the title
        # df3 - a sub-df made only of those columns with 'mean' in the title
        df2 <- select(data, matches("std"))      
        df3 <- select(data, matches("mean"))
        
        # cbind them back together, along with the subject and activity columns from the initial df 
        cleandf <- cbind(data$subject, data$activity, df3, df2)
        
        # rename the subject and activity columns:
        colnames(cleandf)[1] <- "subject"
        colnames(cleandf)[2] <- "activity"
        
        # group the cleandf by subject, then activity, and summarize the means across every column:
        output <- cleandf %>% group_by(subject, activity) %>% summarize(across(everything(), mean))
        
        # last, return the output table:
        return(output)
}

write.table(output, file = "tidy_samsung_data.txt", row.name = FALSE)
        
        
        
        