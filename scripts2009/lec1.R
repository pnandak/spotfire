size = 100
x = rnorm(size,mean=.2)
tes = t.test(x)
print(tes)
attributes(tes)
tes$p.value

tstat=mean(x)/sd(x)*sqrt(size)
print(tstat)
2*(1-pt(tstat,df=size-1))

1-pchisq(tstat^2,df=1)