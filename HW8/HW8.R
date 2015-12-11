
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





#2a
rstudent(fit)
library(car) 
outlierTest(fit,cutoff=1)

#2b

influence(fit)$hat


#2c
xnew<-c(30,58,2)
t(xnew)%*%solve((t(X)%*%X))%*%xnew
max(influence(fit)$hat)


#2d


cooks.distance(fit)
cook<-cook.distance(fit)
cook[order(cook, decreasing=T)]
infIndexPlot(fit)



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




#3C
rstudent(fit)
library(car) 
outlierTest(fit,cutoff=1)


#3D
influence(fit)$hat
hat<-influence(fit)$hat
hat[order(hat,decreasing=T)]

#3E

cooks.distance(fit)
cook<-cooks.distance(fit)
cook[order(cook, decreasing=T)]
infIndexPlot(fit)



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


#4C
rstudent(fit)
library(car) 
outlierTest(fit,cutoff=1)


#4D
influence(fit)$hat
hat<-influence(fit)$hat
hat[order(hat,decreasing=T)]

#4E

cooks.distance(fit)
cook<-cooks.distance(fit)
cook[order(cook, decreasing=T)]
infIndexPlot(fit)
