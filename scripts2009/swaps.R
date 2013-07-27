filename = "C:\\Documents and Settings\\Paolo\\My Documents\\web\\teach\\ma593fal06\\r\\FRB_H15.csv"
raw = read.table(filename,sep=",",skip=6)
matur = c(1,2,3,4,5,7,10,30)
swaps = ts(data = raw[2:9],start=c(2000,7),end=c(2006,11),freq=12,names=matur)

ts.plot(swaps)
pairs(swaps)

swapca = prcomp(swaps)
plot(swapca)
biplot(swapca)
summary(swapca)

par(mfrow=c(2,2))
ylimits <- c(-.7,.7)
plot(matur,swapca$rot[,1],type="l",ylim=ylimits)
plot(matur,swapca$rot[,2],type="l",ylim=ylimits)
plot(matur,swapca$rot[,3],type="l",ylim=ylimits)
plot(matur,swapca$rot[,4],type="l",ylim=ylimits)

remcom = swapca$sdev[4:8]
tot = sum(remcom)
totsq = sum(remcom^2)
len = length(swaps[,1])

sqrt(tot+1.96*sqrt(2*totsq/len))
sqrt(tot-1.96*sqrt(2*totsq/len))

sqrt(sum(swapca$sdev[1:2]))