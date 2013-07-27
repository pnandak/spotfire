
data(airquality)
attach(airquality)

plot(Ozone)
plot(Ozone,type="l")
plot(Ozone,type="h")

date<-ISOdate(1973,Month,Day)

plot(date,Ozone,type="h")
abline(h=120,col="purple",lwd=2)
abline(h=140,col="orange",lwd=2)
abline(h=160,col="red",lwd=2)

title(main="Ozone in NY city, summer 1973")

exceed<-ifelse(Ozone<120,"black",ifelse(Ozone<140,"purple"),ifelse(Ozone<160,"orange","red")

plot(date,Ozone,type="h",col=exceed)
abline(h=120,col="purple",lty=2)
abline(h=140,col="orange",lty=2)
abline(h=160,col="red",lty=2)

title(main="Ozone in NY city, summer 1973")


plot(Ozone~Solar.R)
coplot(Ozone~Solar.R|Temp)
coplot(Ozone~Solar.R|Wind)
coplot(Ozone~Solar.R|Temp*Wind)
coplot(Ozone~Solar.R|Temp*Wind,n=4)


