
# Read data into R
calcium.table <- read.table('http://lib.stat.cmu.edu/DASL/Datafiles/Calcium.html',header=T,skip=28,nrow=21)

# Attach the table so R can find the variables Decrease and Treatment

attach(calcium.table)


# Numerical summaries of the two groups

treated <- Decrease[(Treatment == 'Calcium')]
placebo <- Decrease[(Treatment == 'Placebo')]

print(summary(placebo))
print(summary(treated))

# Also, each piece of information can be extracted individually

min(treated)
max(treated)
median(treated)
print(quantile(treated, probs=c(0.25,0.75)))
sd(treated)
var(treated)


        png("boxplot.png", height=600, width=600)
        
# Plot the descriptive statistics

boxplot(Decrease ~ Treatment, col='orange', pch=23, bg='red')

        dev.off()
        
        png("hist_treat.png", height=600, width=600)
        hist(treated, main='', xlab='Decrease', col='orange')
        dev.off()
        
        png("hist_placebo.png", height=600, width=600)
        hist(placebo, main='', xlab='Decrease', col='orange')
        dev.off()
        
# Get confidence interval for population mean Decrease (ignoring Treatment)

CI <- c(mean(Decrease)-qt(0.975,20)*sd(Decrease)/sqrt(21), mean(Decrease)+qt(0.975,20)*sd(Decrease)/sqrt(21))
print(CI)

# Test whether population mean Decrease is 0 (also gives the same CI!)

print(t.test(Decrease))

# Test for differential effect of Treatment, assuming equal variances

print(t.test(Decrease ~ Treatment, var.equal=T))

# Fitting a regression model

print(summary(lm(Decrease ~ Treatment)))

