### run_analysis.R
#
#- unzip uci data as '~/uci'. You would see '~/uci/test' and '~/uci/train' directories
#- source( 'run_analysis.R')
#- createTidy( '~/uci', 'tidy.txt' ); this generates file 'tidy.txt' for submission
#
# location: path to uci data
# tt: 'test' or 'train'
library(reshape2)
readData <- function(tt, location) {
    fpath <- file.path(location, paste(tt,'/', 'subject_', tt, '.txt', sep=''))
    subject_data <- read.table(fpath, header=F, col.names=c('SubjectID'))

    fpath <- file.path(location, paste(tt,'/','y_', tt, '.txt', sep=''))
    y_data <- read.table(fpath, header=F, col.names=c('ActivityID'))
    
    fpath <- file.path(location, 'features.txt')
    data_cols <- read.table(fpath, header=F, as.is=T, col.names=c('MeasureID', 'MeasureName'))
    
    # read the X data file
    fpath <- file.path(location, paste(tt,'/', 'X_', tt, '.txt',sep=''))
    data <- read.table(fpath, header=F, col.names=data_cols$MeasureName)
    
    # subset the data with 'mean' or 'std'                                   
    subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
    data <- data[,subset_data_cols]
    
    # append the activity id and subject id columns
    data$ActivityID <- y_data$ActivityID
    data$SubjectID <- subject_data$SubjectID
    data
}
#
# location is top directory of uci data: '~/uci'
#
combineData <- function(location) {
    data <- rbind(readData('test', location), readData('train', location))
    
    cnames <- colnames(data)
    cnames <- gsub("\\.+mean\\.+", cnames, replacement='mean')
    cnames <- gsub("\\.+std\\.+",  cnames, replacement='std')
    colnames(data) <- cnames
    data

}
#
# location is top directory of uci data: '~/uci'
#
applyLabels <- function( location ) {
    mydata<-combineData(location)

    fpath <- file.path(location, 'activity_labels.txt')
    mylabels <- read.table(fpath, header=F, as.is=T, col.names=c('ActivityID', 'ActivityName'))
    mylabels$ActivityName <- as.factor(mylabels$ActivityName)
    mylabeled <- merge(mydata, mylabels)
    mylabeled
}
createTidy<- function( location, myfilename) {
    # melt the dataset
    mylabeled <- applyLabels( location )
    id_vars <- c('ActivityID', 'ActivityName', 'SubjectID')
    myvars <- setdiff(colnames(mylabeled), id_vars)
    mymelted <- melt(mylabeled, id=id_vars, measure.vars=myvars)
    
    # create file
    mytidy <- dcast(mymelted, ActivityName + SubjectID ~ variable, mean)
    write.table(mytidy, myfilename)
}

