#???????? 5
dataset <- read.csv(file.choose(),header = T )

library(e1071)

x1 <- dataset[,1]
mean_x1 <- mean(x1)
med_x1 <- median(x1)
sd_x1 <- sd(x1)
Q1_x1 <-quantile(x1, 0.25)
Q3_x1 <-quantile(x1, 0.75)
names(Q1_x1a) <- NULL
names(Q3_x1) <- NULL
skewness_x1 <- skewness(x1)


x3 <- dataset[,2]
mean_x2 <- mean(x2)
med_x2 <- median(x2)
sd_x2 <- sd(x2)
Q1_x2 <-quantile(x2, 0.25)
Q3_x2 <-quantile(x2, 0.75)
names(Q1_x2) <- NULL
names(Q3_x2) <- NULL
skewness_x2 <- skewness(x2)

x3 <- dataset[,3]
mean_x3 <- mean(x3)
med_x3 <- median(x3)
sd_x3 <- sd(x3)
Q1_x3 <-quantile(x3, 0.25)
Q3_x3 <-quantile(x3, 0.75)
names(Q1_x3) <- NULL
names(Q3_x3) <- NULL
skewness_x3 <- skewness(x3)


x4 <- dataset[,4]
mean_x4 <- mean(x4)
med_x4 <- median(x4)
sd_x4 <- sd(x4)
Q1_x4 <-quantile(x4, 0.25)
Q3_x4 <-quantile(x4, 0.75)
names(Q1_x4) <- NULL
names(Q3_x4) <- NULL
skewness_x4 <- skewness(x4)



x5 <- dataset[,5]
mean_x5 <- mean(x5)
med_x5 <- median(x5)
sd_x5 <- sd(x5)
Q1_x5 <-quantile(x5, 0.25)
Q3_x5 <-quantile(x5, 0.75)
names(Q1_x5) <- NULL
names(Q3_x5) <- NULL
skewness_x5 <- skewness(x5)

x6 <- dataset[,6]
mean_x6 <- mean(x6)
med_X6 <- median(x6)
sd_x6 <- sd(x6)
Q1_x6 <-quantile(x6, 0.25)
Q3_x6 <-quantile(x6, 0.75)
names(Q1_x6) <- NULL
names(Q3_x6) <- NULL
skewness_x6 <- skewness(x6)

x7 <- dataset[,7]
mean_x7 <- mean(x7)
med_x7 <- median(x7)
sd_x7 <- sd(x7)
Q1_x7 <-quantile(x7, 0.25)
Q3_x7 <-quantile(x7, 0.75)
names(Q1_x7) <- NULL
names(Q3_x7) <- NULL
skewness_x7 <- skewness(x7)

x8 <- dataset[,8]
mean_x8 <- mean(x8)
med_x8 <- median(x8)
sd_x8 <- sd(x8)
Q1_x8 <-quantile(x8, 0.25)
Q3_x8 <-quantile(x8, 0.75)
names(Q1_x8) <- NULL
names(Q3_x8) <- NULL
skewness_x8 <- skewness(x8)

x9 <- dataset[,9]
mean_x9 <- mean(x9)
med_x9 <- median(x9)
sd_x9 <- sd(x9)
Q1_x9 <-quantile(x9, 0.25)
Q3_x9 <-quantile(x9, 0.75)
names(Q1_x9) <- NULL
names(Q3_x9) <- NULL
skewness_x9 <- skewness(x9)

y <- dataset[,10]
mean_y <- mean(y)
med_y <- median(y)
sd_y <- sd(y)
Q1_y <-quantile(y, 0.25)
Q3_y <-quantile(y, 0.75)
names(Q1_y) <- NULL
names(Q3_y) <- NULL
skewness_y <- skewness(y)

#???????? 7
options(scipen =12)
format(1e6,big.mark=",",scientific)
#X3
hist(dataset$sqft_living,prob= TRUE, main='sqft living space',xlab = 'sqft_living',col="grey")
lines(density(dataset$sqft_living),col="blue",lwd=2)
plot(ecdf(dataset$sqft_living),main='sqft living space', xlab='sqft_living',ylab='Fn(x)')
#X4
hist(dataset$sqft_lot,prob= TRUE, main='sqft lot space',xlab = 'sqft_lot',col="grey")
lines(density(dataset$sqft_lot),col="blue",lwd=2)
plot(ecdf(dataset$sqft_lot),main='sqft lot space', xlab='sqft_lot',ylab='Fn(x)')
#X8
hist(dataset$yr_built,prob= TRUE, main='built year',xlab = 'yr_built',col="grey")
lines(density(dataset$yr_built),col="blue",lwd=2)
plot(ecdf(dataset$yr_built),main='built year', xlab='yr_built',ylab='Fn(x)')


#Q9
cbind(Freq=table(cut(dataset$bedrooms,breaks= seq(0,5,1))),
      relative=prop.table(table(cut(dataset$bedrooms,breaks= seq(0,5,1)))))

cbind(Freq=table(cut(dataset$yr_built,breaks= seq(1900,2020,20))),
      relative=prop.table(table(cut(dataset$yr_built,breaks= seq(1900,2020,20)))))

tableconditionProb<-cbind(freq=table(dataset$renovated,
                                   cut(dataset$condition,breaks=seq(0,5,1))))

tablePriceProb<-cbind(freq=table(dataset$floors,
                                        cut(dataset$price,breaks=seq(170000,3300000,500000))))
