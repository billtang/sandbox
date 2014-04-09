mylevels<-levels(as.factor(mypm25$type))
#'NON-ROAD' 'NONPOINT' 'ON-ROAD' 'POINT'

my24510p<-mypm25[ mypm25$fips==24510 & mypm25$type == 'POINT', ]
my24510np<-mypm25[ mypm25$fips==24510 & mypm25$type == 'NONPOINT', ]
my24510r<-mypm25[ mypm25$fips==24510 & mypm25$type == 'ON-ROAD', ]
my24510nr<-mypm25[ mypm25$fips==24510 & mypm25$type == 'NON-ROAD', ]


myyearly24510p<-aggregate(Emissions ~ year, my24510p, sum)
myyearly24510np<-aggregate(Emissions ~ year, my24510np, sum)
myyearly24510r<-aggregate(Emissions ~ year, my24510r, sum)
myyearly24510nr<-aggregate(Emissions ~ year, my24510nr, sum)

my24510all<-data.frame(point = myyearly24510p$Emissions, nonpoint = myyearly24510np$Emissions
                       onroad = myyearly24510r$Emissions, nonroad = myyearly24510nr$Emissions,
                       year = myyearly24510p$year)
library(lattice)
png('24510m.png')
xyplot( my24510all$point + my24510all$nonpoint
       + my24510all$onroad + my24510all$nonroad
       ~ my24510all$year, type=c('l','l','l','l'), auto.key=T)
dev.off()

