rankhospital<-function(state, outcome, num='best') {
    if (outcome=='heart attack') { i <-11 }
    else if (outcome=='heart failure') { i <-17 }
    else if (outcome=='pneumonia') { i <-23 }
    else { stop('invalid outcome') }
    mydata<-read.csv('pa3/outcome-of-care-measures.csv', colClasses='character')
    mystates<-unique(mydata$State)
    if (is.element(state, mystates)) {
        mydata2<-mydata[ mydata$State == state , c(2,7,i)]  #Name, State, itemSelected
        mydata2[,3]<- suppressWarnings( as.numeric(mydata2[,3]) )
        mydata3 <-  mydata2[ complete.cases(mydata2), ] 
        mydata4 <-  mydata3[ order( mydata3[,3], mydata3[,1]), ]
        if (num=='best') {
            print (head( mydata4$Hospital.Name, n=1))
        } else if (num=='worst') {
            print (tail( mydata4$Hospital.Name, n=1))
        } else {
            #mynum<-suppressWarnings(as.numeric(num))
            mynum<-as.numeric(num)
            print (mydata4$Hospital.Name[mynum])
        }
        ## mydata2<-mydata[ mydata$State == state , c(2,7,i)]  #Name, State, itemSelected
        ## mydata2[,3]<- suppressWarnings( as.numeric(mydata2[,3]) )
        ## mydata3 <- mydata2[ complete.cases(mydata2), ]
        ## mydata3$Ranking<- ave( mydata3$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, mydata3$State, FUN=rank)
        ## # mydata4<- mydata3[ order(-1* as.numeric(num), 
        ## return( mydata3 )
    } else { stop('invalid state') }  			      
}
