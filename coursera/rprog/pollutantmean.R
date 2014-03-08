pollutantmean <- function(directory, pollutant, id=1:332) {
    if (pollutant == 'nitrate'||pollutant=='sulfate') {
        myfiles <- sprintf('%s/%03d.csv',directory,id)
        mydata.list <- lapply(myfiles, read.csv)
        mydata.cat <- do.call(rbind, mydata.list)
        mean(mydata.cat[,pollutant],na.rm=T)
    }	      
}
