getFile<-function(myurl) {
    myfilename<-tempfile()
    download.file(myurl, myfilename)
    myfiles<-unzip(mytemp)
    myfiles
}

myurl<-'http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
myfiles<-getFile(myurl)
mycode<-readRDS(myfiles[1]) #'./Source_Classification_Code.rds'
mypm25<-readRDS(myfiles[2]) #'./summarySCC_PM25.rds'
dim(mycode) #11717 15
dim(mypm25) #6497651 6

library(ggplot2)
getGraph( mydata, mypng ) {
    myyearly<-aggregate(Emissions ~ year, mydata, sum)
    png(mypng)
    qplot(myyearly$year, myyearly$Emissions)
    dev.off()
}
