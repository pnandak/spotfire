#######################################################
## Some code examplifying some R functions - including:
## apply, tapply, lapply
## rep
## writting your own functions
#######################################################

f1<-as.factor(rep(c("a","b"),3))
f2<-as.factor(rep(c("A","B","C"),each=2))

da<-1:6
data.frame(da,f1,f2)
mean(da[f1=="a"])
apropos("apply")

tapply(da,f1,mean)
tapply(da,f2,mean)
tapply(da,list(f1,f2),mean)

m1<-matrix(1:20,ncol=4)
apply(m1,1,mean)
apply(m1,2,mean)

my.summary<-function(x){
  return(c(mean(x),sd(x)))
}

apply(m1,2,my.summary)



bodyfat<-read.table("http://www2.imm.dtu.dk/courses/02441/dataweek1/bodyfat.txt",header=TRUE)
summary(bodyfat)
lapply(bodyfat,summary)
sapply(bodyfat,summary)

la<-list(1:20,100:200)
lapply(la,summary)
sapply(la,summary)


####################################################
## Another non-parametric test that can be used
## for multiple things.
##
## Bootstraping to test for difference between means
####################################################
summary(bodyfat)
t1<-t.test(fatpct~gender,bodyfat)
t1

bmale<-bodyfat$fatpct[sample(23,13,replace=TRUE)]
bfemale<-bodyfat$fatpct[sample(23,10,replace=TRUE)]
mean(bmale)-mean(bfemale)

boot.val<-numeric(10000)
for ( i in 1:10000){
  bmale<-bodyfat$fatpct[sample(23,13,replace=TRUE)]
  bfemale<-bodyfat$fatpct[sample(23,10,replace=TRUE)]
  boot.val[i]<-mean(bfemale)-mean(bmale)
}
hist(boot.val,30)

diff(t1$estimate)
sum(boot.val<diff(t1$estimate))
sum(abs(boot.val)>abs(diff(t1$estimate)))

##### End of bootstrap example


