data<-read.table("copiers_full.txt",header=T)

x<-data[,2:4]
#
t(X)
t(x)%*%x

matrix(c(n,sum(x),sum(x),sum(x^2)),2,2)

#LSE
t(x)%*%y

sum(y);sum(x*y)

solve(t(x)%*%x,t(x)%*%y)

coef(lm(y~x))


t(x)%*%x


solve(t(x)%*%x)

y,hat<-x%*%b

fitted(lm(y~x))

help.search("rank of a matrix")

library(Matrix)

H<-

#MAKE 23 BY 23 IDENTITY MATRIX

I<-diag(rep(1,23))

as.vector()

rankMatrix(I-H)


anova(lm(y~x))

sum((y-mwan(y))^2)

J<-matrix(rep(1,23*23),23,23)


t(y)%*%(I-J/n)%*%y






e<-as.vector((I-H)%*%y)

sum(e^2)


t(y)%*%(I-H)%*%y


sum((y,hat-mean(y))^2)


MSE<-sum(e^2)/(n-2)


vcov(lm(y~x))

MSE*solve(t(x),%*%x)

MSE*t()c(1,x.h))%*%solve(t(x)&*&x,c(1,x,h))

predict(lm(y~x),DATA,FRAME(X=X.H),SE.FIT=T)



MSE*t(c(1,x,h))%*%


predict(lm(y~x), data.frame(x=x.h),se.fit=T)$se.fit^2
