run_analysis <- function() {
    #read train data
    train <- read.table(".//train//X_train.txt")
    train_lable <- read.table(".//train//y_train.txt")
    subject_train <- read.table(".//train//subject_train.txt")
    names(subject_train) <- c("subject")
    #read test data
    test <- read.table(".//test/X_test.txt")
    test_lable <- read.table(".//test//y_test.txt")
    subject_test <- read.table(".//test//subject_test.txt")
    names(subject_test) <- c("subject")
    #read common parts
    feature <- read.table(".//features.txt")
    lable_name <- read.table(".//activity_labels.txt")
    names(lable_name)[2] <- "activity"
    #train data
    names(train) <- feature$V2
    train <- cbind(subject_train, train_lable, train)
    train1 <- merge(lable_name, train, by.x="V1", by.y="V1")
    train1$V1 <- NULL
    #test data
    names(test) <- feature$V2
    test <- cbind(subject_test, test_lable, test)
    test1 <- merge(lable_name, test, by.x="V1", by.y="V1")
    test1$V1 <- NULL
    
    #merge to one data set
    m <- rbind(train1, test1)
    
    #Only the measurements on the mean and std
    m <- m[,c("activity", "subject", names(m)[which(grepl("-(mean|std)\\(\\)", names(m)))])]
    
    #Decriptive variable name
    names(m) <- gsub("-", "", gsub("std\\(\\)", "Std", gsub("mean\\(\\)", "Mean", names(m))))
    
    #Aggregate
    library(reshape2)
    MeltM <- melt(m, id=c("subject", "activity"), measure.vars=names(m)[which(!(names(m) %in% c("subject", "activity")))])
    agg <- aggregate(value ~ subject + activity + variable, data=MeltM, mean)
    write.csv(agg, file="analysis_results.csv", row.names=FALSE)
}