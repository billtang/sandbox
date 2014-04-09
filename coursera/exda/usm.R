mycode<-readRDS(myfiles[1]) #myfiles[1] is './Source_Classification_Code.rds'
mycomb<-subset(mycode, grepl('omb', mycode$Short.Name) # get combustion type
               

mypm25comb<-mypm25[ mypm25$SCC %in% mycomb$SCC, ]

myyearlycomb<-aggregate(Emissions ~ year, mypm25comb, sum)
png('usm.png')
plot(myyearlycomb$year, myyearlycomb$Emissions)
dev.off()
