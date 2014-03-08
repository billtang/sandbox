corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    id=1:332
    myfiles <- sprintf('%s/%03d.csv',directory,id)
    mydata <- lapply(myfiles, read.csv)
    mycases <- lapply(mydata, complete.cases)
    nobs <- unlist( lapply(mycases, sum) )
    foo <- data.frame(id, nobs)
    myid <- foo[foo$nobs > threshold, ]$id
    myset <- mydata[unlist(myid)]
    sapply( myset, function(x) cor(x$sulfate, x$nitrate, use='complete.obs'))
}
