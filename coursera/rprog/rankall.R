rankall<-function( outcome, num='best') {
    if (outcome=='heart attack') { i <-11 }
    else if (outcome=='heart failure') { i <-17 }
    else if (outcome=='pneumonia') { i <-23 }
    else { stop('invalid outcome') }
    mydata<-read.csv('pa3/outcome-of-care-measures.csv', colClasses='character', na.strings='Not Available')
    mydata2<-mydata[ c(2,7,i)]  #Name, State, itemSelected
    mydata2[,3]<- suppressWarnings( as.numeric(mydata2[,3]) )
    mydata3 <-  mydata2[ complete.cases(mydata2), ]
    #mydata3<-mydata2
    colnames(mydata3)[3]<-'Outcome' #Name, State, Outcome

    myresult<-c()
    for (mystate in unique(mydata3$State)) {
        foo <- mydata3[ mydata3$State == mystate, ]
        if (num=='best') {
            bar<-foo[ order( foo[,3], foo[,1], na.last=F ), ]
            myresult<-rbind(myresult, c(bar[1,1], mystate))
        } else if (num=='worst') {
            bar<-foo[ order( foo[,3], foo[,1], decreasing=T ), ]
            myresult<-rbind(myresult, c(bar[1,1], mystate))            
        } else {
            mynum<-as.numeric(num)
            bar<- foo[ order(foo[,3], foo[,1], na.last=F), ]
            myresult<-rbind(myresult, c(bar[mynum,1], mystate))
        }
    }
    colnames(myresult)<-c('hospital','state')
    return(as.data.frame(myresult))
}
