my24510<-mypm25[ mypm25$fips==24510, ]
myyearly24510<-aggregate(Emissions ~ year, my24510, sum)
png('24510.png')
plot(myyearly24510$year, myyearly24510$Emissions)
dev.off()

