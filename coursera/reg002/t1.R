#t1
x<-c(0.18,-1.54,0.42,0.95)
w<-c(2,1,3,1)

sum(w*(x-0.0025)^2)
sum(w*(x-1.077)^2)
sum(w*(x-0.1471)^2) # smallest
sum(w*(x-0.3)^2)

#t2  # omitting intercept: 0.8263
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(formula = y ~ x - 1)

#t3
data(mtcars)
lm(mtcars$mpg ~ mtcars$wt)

#t4
# cor(x,y)=cov(x,y)/sd(x)/sd(y)

#t5

#t6 normalized item
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x[1] - mean(x))/sd(x)

#t7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x)
# intercept is 1.567

#t8 : mean zero --> zero

#t9  mean(x) will minimize the sum of squared distances between itself and all points
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)  # 0.573

#10: 
# b1 = cor(y,x)sd(y)/sd(x); 
# r1 = cor(x,y)sd(x)/sd(y); 
# b1/r1 = (sd(y)/sd(x))/(sd(x)/sd(y)) = sd(y)^2/sd(x)^2=var(y)/var(x)

