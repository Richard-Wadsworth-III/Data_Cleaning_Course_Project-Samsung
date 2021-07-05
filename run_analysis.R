runyourself <- function(){  
        install.packages("dplyr")
        library(dplyr)
        
        train_subjects <- read.table("data/fit/UCI HAR Dataset/train/subject_train.txt")
        train_activities <- read.table("data/fit/UCI HAR Dataset/train/y_train.txt")
        train_set <- read.table("data/fit/UCI HAR Dataset/train/x_train.txt")
        
        train_data <- cbind(train_subjects, train_activities, train_set)
        
        test_subjects <- read.table("data/fit/UCI HAR Dataset/test/subject_test.txt")
        test_activities <- read.table("data/fit/UCI HAR Dataset/test/y_test.txt")
        test_set <- read.table("data/fit/UCI HAR Dataset/test/x_test.txt")
        
        test_data <- cbind(test_subjects, test_activities, test_set)
        
        features <- read.table("data/fit/UCI HAR Dataset/features.txt")
        features <- features$V2
        features <- c("subject", "activity", features) 
        
        data <- rbind(train_data, test_data)
        colnames(data) <- features
        data[,2] <- gsub("1", "walking", data[,2])
        data[,2] <- gsub("2", "walking_upstairs", data[,2])
        data[,2] <- gsub("3", "walking_downstairs", data[,2])
        data[,2] <- gsub("4", "sitting", data[,2])
        data[,2] <- gsub("5", "standing", data[,2])
        data[,2] <- gsub("6", "laying", data[,2])
        
        
        df2 <- select(data, matches("std"))      
        df3 <- select(data, matches("mean"))
        cleandf <- cbind(data$subject, data$activity, df3, df2)
        colnames(cleandf)[1] <- "subject"
        colnames(cleandf)[2] <- "activity"
        
        output <- cleandf %>% group_by(subject, activity) %>% summarize(across(everything(), mean))
        
        return(output)
}
        
        
        
        