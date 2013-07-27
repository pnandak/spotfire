##############################################################
## 1. small sample comparions of means
## two independent samples
## Verzani Example 7.9, p203

## x is placebo
## y is ephedra
x <- c(0,0,0,2,4,5,13,14,14,14,15,17,17)
y <- c(0,6,7,8,11,13,16,16,16,17,18)

boxplot(list(placebo=x,ephedra=y),
        col="gray")

## now test difference of means
t.test(x,y,
       var.equal=TRUE)

## do it again, but relax assumption that the
## variances of x and y are equal
t.test(x,y,
       var.equal=FALSE)

#########################################################
## 2. small sample, comparison of proportions
## independent samples
## Fisher's exact test

## BLG children by mother's sexuality
tab <- c(2,23,0,20)
tab <- matrix(tab,2,2,byrow=T)

## lady tasting tea
TeaTasting <- matrix(c(3, 1, 1, 3),
                     nr = 2,
                     dimnames = list(Guess = c("Milk", "Tea"),
                       Truth = c("Milk", "Tea")))
fisher.test(TeaTasting, alternative = "greater")

## key thing to inspect (for now) is the p-value
## ignore output re odds-ratios etc

########################################################
## 3. comparison of means with dependent samples
## hair loss, twin study, p242 Verzani

Finasteride <- c(5,3,5,6,4,4,7,4,3)
placebo <- c(2,3,2,4,2,2,3,4,2)

t.test(Finasteride,
       placebo,
       paired=TRUE)

