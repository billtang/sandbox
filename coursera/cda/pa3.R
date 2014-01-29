# PA3.1
outcome<-read.csv('outcome-of-care-measures.csv', colClasses='character')
names(outcome)
for (i in c(11, 17, 23)){outcome[,i]<-as.numeric(outcome[,i])}
hist(outcome[,11],xlab='30-day death rate',main='heart attack 30-day death rate')
# PA3.2
#[11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
#[17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
#[23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
#
# PA3.2 draw hist in rows
#
par(mfrow=c(3,1))
hist(outcome[,11],xlab='30-day death rate',main='heart attack 30-day death rate')
hist(outcome[,17],xlab='30-day death rate',main='heart failure 30-day death rate')
hist(outcome[,23],xlab='30-day death rate',main='pneumonia 30-day death rate')
#
# PA3.2.1 draw hist in columns
# PA3.2.2 draw mean
#
par(mfcol=c(1,3))
x11<-mean(as.numeric(outcome[,11]),na.rm=T)
x17<-mean(as.numeric(outcome[,17]),na.rm=T)
x23<-mean(as.numeric(outcome[,23]),na.rm=T)
hist(outcome[,11],xlab='30-day death rate',main='heart attack 30-day death rate')
abline(v=x11,col='blue',lwd=2)
hist(outcome[,17],xlab='30-day death rate',main='heart failure 30-day death rate')
abline(v=x17,col='blue',lwd=2)
hist(outcome[,23],xlab='30-day death rate',main='pneumonia 30-day death rate')
abline(v=x23,col='blue',lwd=2)
#
# PA3.2.3 : density
par(mfrow=c(3,1))
hist(outcome[,11],xlab='30-day death rate',main='heart attack 30-day death rate',prob=T)
lines(density(outcome[,11],na.rm=T))
hist(outcome[,17],xlab='30-day death rate',main='heart failure 30-day death rate',prob=T)
lines(density(outcome[,17],na.rm=T))
hist(outcome[,23],xlab='30-day death rate',main='pneumonia 30-day death rate',prob=T)
lines(density(outcome[,23],na.rm=T))
#
# PA3.3
#
death<-outcome[,11]
state<-outcome$State
par(las=2)  # x axis labels perpendicular to the axis
boxplot(death ~ state, xlab='State', ylab='30-day death rate', main='heart attach 30-day death rate by state')
#
# filter out states with fewer than 20 hospitals (46 out of 54 remains)
#
foo<-table(outcome$State)
foo2<-as.data.frame(foo)
foo3<-subset(foo2, foo2$Freq>20)
outcome2<-subset(outcome,outcome$State %in% foo3[,1])
death<-outcome2[,11]
state<-outcome2$State
par(las=2)  # x axis labels perpendicular to the axis
boxplot(death ~ state, xlab='State', ylab='30-day death rate', main='heart attach 30-day death rate by state')
#
# PA3.4: plot
#
outcome<-read.csv('outcome-of-care-measures.csv', colClasses='character')
hospital<-read.csv('hospital-data.csv', colClasses='character')
outcome.hospital<-merge(outcome,hospital,by='Provider.Number')
death<-as.numeric(outcome.hospital[,11])
npatient<-as.numeric(outcome.hospital[,15])
owner<-factor(outcome.hospital$Hospital.Ownership)
library(lattice)
xyplot(death ~ npatient | owner, group=owner, xlab='number of patients seen', ylab='30-day death rate', main='heart attack 30-day death rate by ownership')
#
# linear regression
#
xyplot(death ~ npatient | owner, group=owner, xlab='number of patients seen', ylab='30-day death rate', main='heart attack 30-day death rate by ownership',panel=function(x,y) {panel.xyplot(x,y);panel.lmline(x,y)})
#
# PA3.5
#
best<-function(state,outcome) {
    if (outcome=='heart attack') { i <-11 }
    else if (outcome=='heart failure') { i <-17 }
    else if (outcome=='pneumonia') { i <-23 }
    else { stop('invalid name') }
    mydata<-read.csv('outcome-of-care-measures.csv', colClasses='character')
    mystates<-unique(mydata$State)
    if (is.element(state, mystates)) {
        mydata2<-mydata[ mydata$State == state , c(2,7,i)]  #Name, State, itemSelected
        mydata2[,3]<- suppressWarnings( as.numeric(mydata2[,3]) )
        mydata3 <- mydata2[ complete.cases(mydata2), ]
        mydata4 <-  mydata3[ order( mydata3[,3]), ]
        print (head( mydata4$Hospital.Name, n=1))
    } else { stop('invalid state') }  			      
}
#
# PA3.6
#
rankhospital<-function(state, outcome, num='best') {
    if (outcome=='heart attack') { i <-11 }
    else if (outcome=='heart failure') { i <-17 }
    else if (outcome=='pneumonia') { i <-23 }
    else { stop('invalid outcome') }
    mydata<-read.csv('outcome-of-care-measures.csv', colClasses='character')
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
#
# PA3.7
#
rankall<-function(outcome, num='best') {
    if (outcome=='heart attack') { i <-11 }
    else if (outcome=='heart failure') { i <-17 }
    else if (outcome=='pneumonia') { i <-23 }
    else { stop('invalid outcome') }
    
    mydata<-read.csv('outcome-of-care-measures.csv', colClasses='character')
    mystates<-unique(mydata$State)
    bar <- data.frame( character(), character() )
    for(state in sort(mystates)) {
        mydata2<-mydata[ mydata$State == state , c(2,7,i)]  #Name, State, itemSelected
        mydata2[,3]<- suppressWarnings( as.numeric(mydata2[,3]) )
        mydata3 <-  mydata2[ complete.cases(mydata2), ] 
        mydata4 <-  mydata3[ order( mydata3[,3], mydata3[,1]), ]
        if (num=='best') {
            foo<-mydata4[ 1, c(2,1) ]
        } else if (num=='worst') {
            foo<-mydata4[ nrow(mydata4), c(2,1) ]
        } else {
            mynum<-as.numeric(num)
            foo<-mydata4[num, c(2,1) ]
        }
        bar<-rbind( bar,foo )
    }
    names(bar)[1]<-'state'
    names(bar)[2]<-'hospital'
    return (bar)
}
