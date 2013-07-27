
## Section 1: Descriptive Statistics

mean(stata.dat$x1)

mean(stata2.dat$x1)
mean(stata2.dat$x1, na.rm=T)


var(stata.dat$x1)


var(stata2.dat$x1)
var(stata2.dat$x1, na.rm=T)

cor(stata.dat[,c("x1", "x5")], use="pairwise.complete.obs")
cor(stata.dat[,c("x1", "x5")], use="complete.obs")

## Section 2: Filtering With Logical Expressions and Sorting

## Page 21
stata.dat$x3 == "yes"
stata.dat$x2 == "none"
stata.dat$x2 == 1
as.numeric(stata.dat$x2) == 1
stata.dat$x1 == 2

stata.dat[stata.dat$x1 == 1 & stata.dat$x2 == "none", ]


## Section 2.1: Sorting

stata.dat

stata.dat[c(10,9,8,7,6,5,4,3,2,1), ]

stata.dat_reorder <- stata.dat[order(stata.dat$x1, stata.dat$x2, decreasing=T), ]

stata.dat_reorder

stata.dat_reorder[order(as.numeric(rownames(stata.dat_reorder))), ]

## Section 3: Working with Real Data

library(car)
library(gmodels)
data(Mroz)

## Section 3.1 Tables and Cross-Tabulations

table(Mroz$wc)
sum(table(Mroz$wc))
table(Mroz$wc)/753
table(Mroz$wc)/sum(table(Mroz$wc))
table(Mroz$wc, Mroz$hc)

CrossTable(Mroz$wc, Mroz$hc, chisq=T)


## Section 3.2: The Linear Model

library(car)
data(Duncan)
lm(prestige ~ income + type, data=Duncan)

mod <- lm(prestige ~ income + type, data= Duncan)

summary(mod)

names(mod)

## Section 3.2.1: Adjusting the Base Category
contrasts(Duncan$type)

contrasts(Duncan$type) <- contr.treatment(3, base=2)

contrasts(Duncan$type)

lm(prestige ~ income + type, data=Duncan)

data(Duncan)
Duncan$type <- relevel(Duncan$type, "prof")

lm(prestige ~ income + type, data=Duncan)

## Section 3.2.2: Predict after lm

mod <- lm(prestige ~ income + type,data=Duncan) 
mod$fitted 
fitted(mod) 

pred <- predict(mod) 
pred.se <- predict(mod, se.fit=T) 
pred.mean.ci <- predict(mod, interval="confidence") 
pred.ind.ci <- predict(mod, interval="prediction") 

newdat <- data.frame( 
	income = 50, 
	type = "bc") 
predict(mod, newdat, interval="confidence") 

newdat <- data.frame( 
	income = c(40, 50, 60), 
	type = c("bc", "bc", "bc")) 

predict(mod, newdat, interval="confidence") 

## Section 3.2.3: Linear Hypothesis Tests

linear.hypothesis(mod, "income = 1") 

linear.hypothesis(mod, "typebc=typewc") 

linear.hypothesis(mod, c("typebc=0", "typewc=0")) 

Anova(mod) 


