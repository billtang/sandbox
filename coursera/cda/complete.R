complete<-function(directory='specdata',id=1:332) {
   myresult <- data.frame(id=NA, nobs=NA)
   myindex<-1
   for (myi in id) {
      myfilename <- paste(directory, sprintf('/%03d.csv', myi), sep='')
      mydata <- read.csv( myfilename )
      mycases <- sum(complete.cases(mydata[,2:3]))
      myresult[myindex,] <- c(myi, mycases)
      myindex<-myindex+1
   }
   return(myresult)
}

