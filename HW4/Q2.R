data<-read.table("copier_maintenance.txt",header=T)

x<-data[,2]
y<-data[,1]
n<-length(y)

#Q2.a
g<-2
fit<-lm(y~x)
b0<-summary(fit)$coefficient[1]
b1<-summary(fit)$coefficient[2]
e<-resid(fit)
y.hat<-fitted(fit)
MSE<-sum(e^2)/(n-2)

#B0 UPPERBOUND
b0+qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1/n+mean(x)^2/sum((x-mean(x))^2)))
#B0 LOWERBOUND
b0-qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1/n+mean(x)^2/sum((x-mean(x))^2)))

#B1 UPPERBOUND
b1-qt(1-0.05/(2*g),n-2)*sqrt(MSE/sum((x-mean(x))^2))
#B1 LOWERBOUND
b1+qt(1-0.05/(2*g),n-2)*sqrt(MSE/sum((x-mean(x))^2))



#Q2.c
g<-3
xh1<-3
xh2<-5
xh3<-7

#xh1 confidence interval lower bound
b0+b1*xh1-qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1/n+(xh1-mean(x))^2/sum((x-mean(x))^2)))
#xh1 confidence interval upper bound
b0+b1*xh1+qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1/n+(xh1-mean(x))^2/sum((x-mean(x))^2)))

#XH2 CONFIDENCE INTERVAL lower bOUND
b0+b1*xh2-qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1/n+(xh2-mean(x))^2/sum((x-mean(x))^2)))
#xh2 confidence interval upper bound
b0+b1*xh2+qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1/n+(xh2-mean(x))^2/sum((x-mean(x))^2)))

#xh3 confidence interval lower bound
b0+b1*xh3-qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1/n+(xh3-mean(x))^2/sum((x-mean(x))^2)))
#xh3 confidence interval uooer bound
b0+b1*xh3+qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1/n+(xh3-mean(x))^2/sum((x-mean(x))^2)))




#Q2.d
g<-2
xh4<-4
xh5<-6

#xh4 prediction interval lower bound
b0+b1*xh4-qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1+1/n+(xh4-mean(x))^2/sum((x-mean(x))^2)))
#xh4 prediction interval upper bound
b0+b1*xh4+qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1+1/n+(xh4-mean(x))^2/sum((x-mean(x))^2)))

#xh5 prediction interval lower bound
b0+b1*xh5-qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1+1/n+(xh5-mean(x))^2/sum((x-mean(x))^2)))
#xh5 prediction interval upper bound
b0+b1*xh5+qt(1-0.05/(2*g),n-2)*sqrt(MSE*(1+1/n+(xh5-mean(x))^2/sum((x-mean(x))^2)))



#Q2.e
plot(y~x)
abline(b0,b1)

## BELOW ARE WH Method
> pred<-predict(fit,data.frame(x=1:10),se.fit=T)
> W.H.<-sqrt(2*qf(0.95,df1=2,df2=pred$df))
yfit<-pred$fit
se<-pred$se.fit
lines(1:10, yfit,lty=1)
lines(1:10,yfit-W.H.*se,lty=2)
lines(1:10,yfit+W.H.*se,lty=2)


legend(6,50,c("estimated mean function line","bounds of the confidance band"),lty=c(1,2))


#Q2.f
#ADD ANOTHER LINE(NEED TO DELETE THE PREVIOUS PICTURE FIRST)
abline(0,14,lty=3)
legend(6,50,c("estimated mean function line","bounds of the confidance band","y=14x"),lty=c(1,2,3))
