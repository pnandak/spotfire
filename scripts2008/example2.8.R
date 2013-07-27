################################################################
#  NAME: Chris Bilder                                          #
#  DATE: 7-21-02 for S Programming class                       #
#  UPDATE: 1-6-06 for Boot class                               #
#  PURPOSE: Do the bootstrap example on p. 22-24 of Davison and#
#           Hinkley (1997)                                     #
################################################################


################################################################
# The data

u<-c(138, 93, 61, 179, 48, 37, 29, 23, 30, 2)
x<-c(143, 104, 69, 260, 75, 63, 50, 48, 111, 50)

ux<-data.frame(u, x)
ux


################################################################
# Using my own set of functions
#  Typically, you should just use the boot package functions.  This just provides
#  some nice examples of using "Method" functions.  

my.naive.boot<-function(data, func, R)
{
    n<-length(data[,1])
    res<-matrix(NA, R,1)
    obs.value<-func(data)
    for(i in 1:R) {
        res[i,]<-func(data[sample(1:n, replace=T),])
   }
    res.all<-list(boot=res, obs=obs.value)
    class(res.all)<-"my.boot"
    res.all
}

ratio.means<-function(data) 
{
    mean(data[,2])/mean(data[,1])
}

ratio.means(ux)
temp<-sample(1:10, replace=T)
temp
ux[temp,]

summary.my.boot<-function(res.obj)
{
    mean.stat<-mean(res.obj$boot)
    sd.stat<-sqrt(var(res.obj$boot))
    bias<-mean.stat-res.obj$obs
    cat("Mean of bootstraped statistic values:", round(mean.stat,4), "\n")
    cat("Standard deviation of bootstraped statistic values:", round(sd.stat,4), "\n")
    cat("Estimated bias:", round(bias,4), "\n")
}


plot.my.boot<-function(res.obj)
{
    par(mfrow=c(1,2))
    hist(res.obj$boot, main="Histogram of bootstrap distribution")
    segments(res.obj$obs, -20, res.obj$obs, 5)
    
    boxplot(res.obj$boot, main="Boxplot of bootstrap distribution")
    invisible()
}


##############################################################
#Use my function

#bootstrap it
save<-my.naive.boot(ux, ratio.means, R=999)
class(save)
summary(save)

par(mfrow=c(1,2))
plot(save)



##############################################################
#Canty's boot package functions from Davison and Hinkley

library(boot)

#Need to pass the indices into the function
ratio.means2<-function(data,w) 
{
    #w represents the indices
    d<-data[w,]
    #print(w)
    mean(d[,2])/mean(d[,1])
}


save<-boot(data=ux, statistic=ratio.means2, R=999)
save
class(save)
plot(save)
names(save)
save$statistic

#Find 2.5th percentile and 97.5th percentile
quantile(x = save$t, probs = c(0.025, 0.975))

#A few bootstrap confidence intervals - just to illustrate
boot.ci(boot.out = save, conf = 0.95)

#Part of Figure 2.6's first plot using the subset of the data n=10
par(pty = "m", mfrow=c(1,1))
hist(x = save$t - save$t0, main = "Histogram for t* - t, R=999", freq=FALSE, xlab = "t*-t", ylim = c(0,2.5), xlim = c(-1,2))
curve(dnorm(x, mean = mean(save$t-save$t0), sd = sd(save$t-save$t0)), col = "red", add = TRUE, lty = "dotted")
lines(density(x = save$t - save$t0), col = "blue", lty = "solid") #Use default settings

 
 

##############################################################
# Try another way - somewhat similar to the bootstrap function


#Need to pass the indices into the function
#  Notice how this is different from ratio.means2 - because of order of items
#  need to pass into the function
ratio.means3<-function(indices, data) 
{
    d<-data[indices,]
    #print(indices) #Do as a learning tool only
    mean(d[,2])/mean(d[,1])
}


n<-10
R<-999
numb<-rep(1:n, R) 
indices<-matrix(sample(numb, replace=T), n, R)

save<-apply(indices, 2, ratio.means3, ux)  

mean.stat<-mean(save)
sd.stat<-sqrt(var(save))
bias<-mean.stat-ratio.means(ux)
cat("Mean of bootstraped statistic values:", round(mean.stat,4), "\n")
cat("Standard deviation of bootstraped statistic values:", round(sd.stat,4), "\n")
cat("Estimated bias:", round(bias,4), "\n")


 
