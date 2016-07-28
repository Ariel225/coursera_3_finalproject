rm(list=ls())
library(dplyr)
library(tidyr)

setwd("J:/personal/r-course/2016")

#download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="q3file.zip")
#unzip("q3file.zip")
setwd("UCI HAR Dataset")

#read the data
activity_labels<-read.table("activity_labels.txt")
features<-read.table("features.txt")

xtest<-read.table("./test/X_test.txt")
ytest<-read.table("./test/y_test.txt")
subject_test<-read.table("./test/subject_test.txt")

xtrain<-read.table("./train/X_train.txt")
ytrain<-read.table("./train/y_train.txt")
subject_train<-read.table("./train/subject_train.txt")
 
#create intermediate working files for debugging
save(list=ls(),file="working.Rdata")
rm(list=ls()); load("working.Rdata")

#################

# set a var to define where the  origianl data came from
xtest$source<-"test"
xtrain$source<-"train"

#combine the test and training data sets together.
xcombined<-bind_rows(xtrain, xtest)
ycombined<-bind_rows(ytrain, ytest)
subjectcombined<-bind_rows(subject_train, subject_test)
rm(xtest,xtrain,ytest,ytrain,subject_test,subject_train)

#assign the subject number to the x data set
xcombined$subject<-unlist(subjectcombined)
rm(subjectcombined)

#assign the labels to the y data set, then map it onto the core x data
ynew<-merge(ycombined,activity_labels)
xcombined$activity<-ynew$V2
rm(ynew, ycombined,activity_labels)

#reshape the data
xcombined<-xcombined %>% gather(a, measure, V1:V561)
xcombined<-xcombined %>% rename(V1 = a)
xcombined$V1<-as.integer(gsub("V","",xcombined$V1))

#add in the features variable, and then get rid of the linking id. There may be faster ways to do this.
finalset<-merge(xcombined,features)
finalset<-rename(finalset,feature=V2)
finalset$V1<-NULL
rm(xcombined,features)

#create more  intermediate working files for debugging
save(list=ls(),file="allclean.Rdata")
rm(list=ls()); load("allclean.Rdata")

#######

# filter out the specific observations needed, as per the assignment.
meansonly<-finalset %>% filter(grepl("mean()",feature))

sdsonly<-finalset %>% filter(grepl("std()",feature))

question5<-finalset %>% group_by(subject,activity, feature) %>% 
           summarise(avg = mean(measure))
write.table(question5, "question5.txt", row.name=FALSE)

table(finalset$subject)
