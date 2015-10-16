#SENIC题目: 
#先对其进行回归, 然后绘制QQPLOT以及残差关于估计值的散点图.
#并使用identify函数获取两个坏点的所在行数

data<-read.table("SENIC.txt",header=T)

x1<-data[,4]
x2<-data[,12]
x3<-data[,6]
y<-data[,2]

summary(lm(y~x1))


b0<-coef(lm(y~x1))[1]
b1<-coef(lm(y~x1))[2]

f<-function(x){b0+b1*x1}
e<-y-b0-b1*x1
plot(e~f(x1))
abline(lm(e~f(x1)))


qqnorm(e)
qqline(e)



##########################
#delete row 47、112
#conduct regression for the remaining data

data<-read.table("SENIC.txt",header=T)[c(-47,-112),]


x1<-data[,4]
x2<-data[,12]
x3<-data[,6]
y<-data[,2]


summary(lm(y~x1))


b0<-coef(lm(y~x1))[1]
b1<-coef(lm(y~x1))[2]


e<-y-b0-b1*x1
plot(e~y)
abline(lm(e~y))


qqnorm(e)
qqline(e)


###################
####### Predict the 95% interval for two deleted points

f<-function(x){b0+b1*x1}

xh<-read.table("SENIC.txt",header=T)[47,4]
n<-length(data[,1])
z1<-(xh-mean(x1))^2
z2<-sum((x1-mean(x1))^2)
mse<-sum((y-f(x1))^2)/(n-2)

b0+b1*xh+qt(0.975, n-2)*(mse*(1+1/n+z1/z2))^0.5
b0+b1*xh-qt(0.975, n-2)*(mse*(1+1/n+z1/z2))^0.5

#47行因变量的实际值:
read.table("SENIC.txt",header=T)[47,2]


xh<-read.table("SENIC.txt",header=T)[112,4]
n<-length(data[,1])
z1<-(xh-mean(x1))^2
z2<-sum((x1-mean(x1))^2)
mse<-sum((y-f(x1))^2)/(n-2)

b0+b1*xh+qt(0.975, n-2)*(mse*(1+1/n+z1/z2))^0.5
b0+b1*xh-qt(0.975, n-2)*(mse*(1+1/n+z1/z2))^0.5

#112行因变量的实际值:
read.table("SENIC.txt",header=T)[112,2]



########################
#另外两个变量同理
########################

summary(lm(y~x2))

summary(lm(y~x3))
