# q3.1 : get row numbers for data meeting condition
mydata=read.csv('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv')
mydata2=subset(mydata, mydata$ACR==3 & mydata$AGS==6 )
head(mydata2, 3)

# need libjpeg-turbo-devel
install.packages('jpeg')
library(jpeg)
myurl="http://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(myurl, destfile='./myimg.jpg',method='curl')
myimg=readJPEG('./myimg.jpg', native=T)
quantile(myimg, probs=c(0.3,0.8))

#
urlgdp='http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
urledu='http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
mygdp=read.csv(urlgdp, colClasses='character') # need to skip the few header rows
myedu=read.csv(urledu, colClasses='character')

mygdp$Gross.domestic.product.2012=suppressWarnings(as.numeric(mygdp$Gross.domestic.product.2012))
mygdp2=mygdp[mygdp$Gross.domestic.product.2012 > 0 & !is.na(mygdp$Gross.domestic.product.2012), ]
mygdp2[ c(12:14),] # print spain as rank 13th entry

# 13 from the last
mybad=mygdp2[order( mygdp2[,2], decreasing=T),]
head(mybad[, c(1,2)],15)


mygdpcode=unique(mygdp2[,1]) # 190
myeducode=unique(myedu[,1]) # 234
length( intersect( mygdpcode, myeducode)) #

mycode=myedu[,c(1,3)] # code, incomeGroup



myoecd=mycode[ mycode$Income.Group=='High income: OECD',] #30,2
mynonoecd=mycode[ mycode$Income.Group=='High income: nonOECD',] #37,2

#foo=cbind( mygdp2$X, mygdp2$Gross.domestic.product.2012 )
#colnames(foo)<-c('code','rank')
myY=subset(mygdp2, match( mygdp2$X, myoecd$CountryCode ) > 0)
myN=subset(mygdp2, match( mygdp2$X, mynonoecd$CountryCode ) > 0)

mean(myY$Gross.domestic.product.2012)
mean(myN$Gross.domestic.product.2012)


#mylowermiddlecode=mycode[ mycode$Income.Group=='Lower middle income' |mycode$Income.Group=='Low income'  ,]
#mylowermiddle=subset(mygdp2, match( mygdp2$X, mylowermiddlecode$CountryCode ) > 0)

mylowermiddlecode=mycode[ mycode$Income.Group=='Lower middle income',]
mylowermiddle=subset(mygdp2, match( mygdp2$X, mylowermiddlecode$CountryCode ) > 0 )
mybest=subset(mylowermiddle, mylowermiddle$Gross.product.2012 <=38)
mybest


