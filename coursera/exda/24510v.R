#mycode<-readRDS(myfiles[1]) #myfiles[1] is './Source_Classification_Code.rds'
mycode<-readRDS('./Source_Classification_Code.rds')

myv<-subset(mycode, grepl('ehicle', mycode$SCC.Level.Two)) # get vehicles

my24510v<-mypm25[ mypm25$SCC %in% myv$SCC & mypm25$fips==24510, ]

myyearly24510v<-aggregate(Emissions ~ year, my24510v, sum)
png('24510v.png')
plot(myyearly24510v$year, myyearly24510v$Emissions)
dev.off()
