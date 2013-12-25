
corr<-function(directory,threshold=0) {
   myresult <- vector()
   foo<-complete(directory)
   id<-1:332
   for (myi in id) {
    if (foo[myi, 2] > threshold) { 
      mycor<-mycorr(directory, myi)
      myresult<-c(myresult, mycor)
      #print(myresult)
     }
   }
   return(myresult)
}

mycorr<-function(directory='specdata',id=1) {
      myfilename <- paste(directory, sprintf('/%03d.csv', id), sep='')
      #print(myfilename)
      mydata <- read.csv( myfilename )
      myx<-subset(mydata$sulfate, !is.na(mydata$sulfate) & !is.na(mydata$nitrate))
      myy<-subset(mydata$nitrate, !is.na(mydata$sulfate) & !is.na(mydata$nitrate))
      mycor<-cor(myx,myy)
      return (mycor)
}