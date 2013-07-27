
# A function to simulate confidence intervals
# from a Normal distribution with mean 1, standard deviation 2

CI = function(n, alpha=0.05) {
Z = rnorm(n, mean=1, sd=2)
return(t.test(Z, conf.level=1-alpha)$conf.int)
}

# Let's create many confidence intervals for a sample of size 20
# and see how often the true mean 1
# lies in the confidence interval.
# It should be roughly 100*(1-alpha)% of the time

confidence.simulation = function(nsample, sample.size=20, alpha=0.05) {
ncontains = 0
for (i in 1:nsample) {
   ci = CI(sample.size, alpha=alpha)
   l = ci[1]
   u = ci[2]
   ncontains = ncontains + (l < 1) * (u > 1)
}
return(ncontains)
}

# In this example, we test the null hypothesis of whether
# a one sample mean is 0 or not when the true mean is 0.
# This should reject the null hypothesis approximately
# 100 * alpha % of the time

hyp.test = function(n, alpha=0.05) {
Z = rnorm(n, mean=0, sd=2)
return(t.test(Z)$p.value < alpha)
}

hyp.simulation = function(nsample, sample.size=20, alpha=0.05) {
nerrors = 0
for (i in 1:nsample) {
   nerrors = nerrors + hyp.test(sample.size, alpha=alpha)
}
return(nerrors)
}

