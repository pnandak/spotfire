#Section 11 Code.

##Seemingly Unrelated Regression

library(Zelig)
data(grunfeld)

#Take a look at the data
head(grunfeld)

#Let's take a look at our dependent variable
Y <- grunfeld[,c(2,5)]

#And our independent variables 
X1 <- grunfeld[,c(3,4)]
X2 <- grunfeld[,c(6,7)

#Let's run the model

formula <- list(mu1=Ige ~Fge + Cge, mu2 = Iw ~ Fw + Cw)
z.out <- zelig(formula, model="sur", data=grunfeld)

z.out

summary(z.out)

#Simulate some results

x.out <- setx(z.out, x=list(Fge=0, Fw=800))
s.out <- sim(z.out, x=x.out)
summary(s.out)
plot(s.out)

###Multinomial Logit
data(mexico)

z.out <- zelig(as.factor(vote88) ~ age + female, model="mlogit", data=mexico)

z.out

summary(z.out)

x.young <- setx(z.out, age=min(mexico$age))
x.old <- setx(z.out, age=max(mexico$age))
s.out <- sim(z.out, x1=x.old, x=x.young)
summary(s.out)
