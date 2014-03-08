best<-function(state,outcome) {
    mydata<-read.csv('pa3/outcome-of-care-measures.csv',colClasses='character')
    #mystates<-unique(mydata$State)
    mystates<-levels(as.factor(mydata$State))
    if (is.element(state, mystates)) {
        mydataset<-mydata[mydata$State == state, ]
        if (outcome=='heart attack') { i <-11 }
        else if (outcome=='heart failure') { i <-17 }
        else if (outcome=='pneumonia') { i <-23 }
        else { stop('invalid name') }
        mydataset[,i]<- suppressWarnings( as.numeric(mydataset[,i]) )
        mydataset <- mydataset[ complete.cases(mydataset), ]
        mydataset <-  mydataset[ order( mydataset[,i]), ]
        print (head( mydataset$Hospital.Name, n=1))
    } else {
        stop('invalid state')
    }
}
