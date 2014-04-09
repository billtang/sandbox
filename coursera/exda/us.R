#
# get data from file
#
getFile<-function(myurl) {
    myfilename<-tempfile()
    download.file(myurl, myfilename)
    myfiles<-unzip(mytemp)
    myfiles
}

myurl<-'http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
myfiles<-getFile(myurl)    
mycode<-readRDS(myfiles[1]) #myfiles[1] is './Source_Classification_Code.rds'
mypm25<-readRDS(myfiles[2]) #myfiles[2] is './summarySCC_PM25.rds'

myyearly<-aggregate(Emissions ~ year, mypm25, sum)
png('us.png')
plot(myyearly$year, myyearly$Emissions)
dev.off()

