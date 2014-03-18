#
foo=read.csv('myfile.txt', sep='\t', header=F)
summary(foo$V5)

#    I.setosa I.versicolor  I.virginica
#          58           51           41

mydata=as.numeric(foo$V1)
mybreaks=seq(from=min(foo$V1),to=max(foo$V1), length=11)
str(hist(mydata, breaks=mybreaks, plot=F))

# $ breaks  : num [1:11] 4.3 4.64 4.98 5.32 5.66 6 6.34 6.68 7.02 7.36 ...
# $ counts  : int [1:10] 16 17 27 9 23 18 12 20 3 5
