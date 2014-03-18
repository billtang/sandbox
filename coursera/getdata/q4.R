
mydata<-read.csv('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv')
strsplit(mynames=names(mydata),"wgtp")[[123]]
# "" "15"

# skil 4 lines
mydata<-read.csv(text=readLines('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv')[-(1:4)])
mygdp<- as.numeric(gsub(",","",mydata$X.4[1:215])) 
summary(mygdp)

fu<-grep("^United", foo$X.3)
fu

urledu='http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
myedu=read.csv(urledu)
foo=grep('Fiscal year end: June', myedu$Special.Notes)
foo

library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 
amzn2012<-amzn['2012']
