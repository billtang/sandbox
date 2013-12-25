getmonitor <- function( id, directory="specdata", summarize=FALSE ) {
  if (mode(id) == 'numeric') {
    for (myi in id) {
      myfilename <- paste(directory, sprintf('/%03d.csv', myi), sep='')
      mydata <- read.csv( myfilename )
      if (summarize==TRUE) {print(summary(mydata))}
      return(mydata)
    }
  } else {
    myfilename <- paste(directory, '/', id, '.csv', sep='')
    mydata <- read.csv( myfilename )
    if (summarize==TRUE) {print(summary(mydata))}
    return(mydata)
  }
}
