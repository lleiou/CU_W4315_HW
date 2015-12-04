data<-read.table("patient_satisfaction.txt", header=T)
y<-data[,1]
x1<-data[,2]
x2<-data[,3]
x3 <-data[,4]


#1a
fit<-lm(y~x1+x2+x3)
library(car)
residualPlots(fit)



#1b

ncvTest(fit,~x1+x2+x3)









#3a
data<-read.table("SENIC.txt", header=T)
y<-data[,2]
x1<-data[,3]
x2<-data[,6]
x3<-data[,10]
fit<-lm(y~x1+x2+x3)
library(car)
residualPlots(fit)

#3b
ncvTest(fit,~x1+x2+x3)





#4a
data<-read.table("SENIC.txt", header=T)
y<-1/data[,2]
x1<-data[,3]
x2<-data[,6]
x3<-log(data[,10])
fit<-lm(y~x1+x2+x3)
library(car)
residualPlots(fit)

#4b
ncvTest(fit,~x1+x2+x3)
