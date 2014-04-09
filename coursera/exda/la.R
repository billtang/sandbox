#mycode<-readRDS(myfiles[1]) #myfiles[1] is './Source_Classification_Code.rds'
mycode<-readRDS('./Source_Classification_Code.rds')

myv<-subset(mycode, grepl('ehicle', mycode$SCC.Level.Two)) # get vehicles

my24510v<-mypm25[ mypm25$SCC %in% myv$SCC & mypm25$fips=='24510', ]
my06037v<-mypm25[ mypm25$SCC %in% myv$SCC & mypm25$fips=='06037', ]

myyearly24510v<-aggregate(Emissions ~ year, my24510v, sum)
myyearly06037v<-aggregate(Emissions ~ year, my06037v, sum)

library(lattice)
png('la.png')
xyplot(myyearly24510v$Emissions + myyearly06037v$Emissions 
       ~ myyearly24510v$year, type=c('l','l'), auto.key=T)

dev.off()
