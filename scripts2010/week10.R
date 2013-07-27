### NBA data ###

attach(NBA <- read.csv("NBAspread.csv"))
n <- nrow(NBA)

par(mfrow=c(1,2))
hist(NBA$spread[favwin==1], col=5, main="", xlab="spread")
hist(NBA$spread[favwin==0], add=TRUE, col=6)
legend("topright", legend=c("favwin=1", "favwin=0"), fill=c(5,6), bty="n")
boxplot(NBA$spread ~ NBA$favwin, col=c(6,5), horizontal=TRUE, ylab="favwin", xlab="spread")

nbareg <- glm(favwin~spread-1, family=binomial)
s <- seq(0,30,length=100)
fit <- exp(s*nbareg$coef[1])/(1+exp(s*nbareg$coef[1]))
plot(s, fit, typ="l", col=4, lwd=2, ylim=c(0.5,1), xlab="spread", ylab="P(favwin)")

bic <- cbind( extractAIC(nbareg, k=log(553)),
             extractAIC(glm(favwin ~ spread, family=binomial), k=log(553)),
             extractAIC(glm(favwin ~ spread + favhome, family=binomial), k=log(553)))[2,]
ebic <- exp(-.5*(bic-min(bic)))
round(ebic/sum(ebic),2) 

thisweek=c(8,4)
pred <- nbareg$coef[1]*thisweek
exp(pred)/(1+exp(pred))

### German Credit Scoring Data ###

credit <- read.csv("germancredit.csv")
train <- 1:800

# build a model assuming you have credit history in there (a good idea)
null <- glm(GoodCredit~history3, family=binomial, data=credit[train,])
full <- glm(GoodCredit~., family=binomial, data=credit[train,])
reg <- step(null, scope=formula(full), direction="forward", k=log(length(train)))

# Now predictiction: the function defaults to give X'b,
# so you tell it to give you predictions of type="response"
predreg <- predict(reg, newdata=credit[-train,], type="response") 
predfull <- predict(full, newdata=credit[-train,], type="response") 

errorreg <- credit[-train,1]-(predreg >= .5) # 1 is a false negative, -1 is a false positive
errorfull <- credit[-train,1]-(predfull >= .5)

# misclassification rates:
mean(abs(errorreg))
mean(abs(errorfull))




## Here is some fancy image plotting
# totally unnecessary for this class, but pretty nonetheless.
covars <- credit[,2:21]
Y <- credit$Good
index <- credit$history3[-train]=="A32"
locs <- (covars[-train,c(2,8)])[index,]
check <- (covars[-train,1])[index]
resp <- (Y[-train])[index]+1
par(mfrow=c(2,2), mai=c(.3,.3,.2,.1), omi=c(.3,.3,.3,.1))
# create a prediction grid
duration <- 1:72
installment <-1:4
grid <- expand.grid( duration2=duration, installment8=installment)
# predict on the grid
newdata <- cbind(grid, checkingstatus1 = rep("A11", 288), history3="A32")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A11", font=2)
points(locs[check=="A11",], pch=c(4,1)[resp[check=="A11"]])
# Repeat for A12
newdata <- cbind(grid, checkingstatus1 = rep("A12", 288), history3="A32")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A12", font=2)
points(locs[check=="A12",], pch=c(4,1)[resp[check=="A12"]])
# Repeat for A13
newdata <- cbind(grid, checkingstatus1 = rep("A13", 288), history3="A32")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A13", font=2)
points(locs[check=="A13",], pch=c(4,1)[resp[check=="A13"]])
# Repeat for A14
newdata <- cbind(grid, checkingstatus1 = rep("A14", 288), history3="A32")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A14", font=2)
points(locs[check=="A14",], pch=c(4,1)[resp[check=="A14"]])
# Throw on the axis labels
mtext("duration", side=1, font=3, outer=TRUE, line=.5)
mtext("installment", side=2, font=3, outer=TRUE, line=.5)
mtext("1st default borrowers",
      side=3, font=2, outer=TRUE)

index <- credit$history3[-train]=="A34"
locs <- (covars[-train,c(2,8)])[index,]
check <- (covars[-train,1])[index]
resp <- (Y[-train])[index]+1
par(mfrow=c(2,2), mai=c(.3,.3,.2,.1), omi=c(.3,.3,.3,.1))
# create a prediction grid
duration <- 1:72
installment <-1:4
grid <- expand.grid( duration2=duration, installment8=installment)
# predict on the grid
newdata <- cbind(grid, checkingstatus1 = rep("A11", 288), history3="A34")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A11", font=2)
points(locs[check=="A11",], pch=c(4,1)[resp[check=="A11"]])
# Repeat for A12
newdata <- cbind(grid, checkingstatus1 = rep("A12", 288), history3="A34")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A12", font=2)
points(locs[check=="A12",], pch=c(4,1)[resp[check=="A12"]])
# Repeat for A13
newdata <- cbind(grid, checkingstatus1 = rep("A13", 288), history3="A34")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A13", font=2)
points(locs[check=="A13",], pch=c(4,1)[resp[check=="A13"]])
# Repeat for A14
newdata <- cbind(grid, checkingstatus1 = rep("A14", 288), history3="A34")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A14", font=2)
points(locs[check=="A14",], pch=c(4,1)[resp[check=="A14"]])
# Throw on the axis labels
mtext("duration", side=1, font=3, outer=TRUE, line=.5)
mtext("installment", side=2, font=3, outer=TRUE, line=.5)
mtext("Borrowers with multiple defaults",
      side=3, font=2, outer=TRUE)


index <- credit$history3[-train]=="A30"
locs <- (covars[-train,c(2,8)])[index,]
check <- (covars[-train,1])[index]
resp <- (Y[-train])[index]+1
par(mfrow=c(2,2), mai=c(.3,.3,.2,.1), omi=c(.3,.3,.3,.1))
# create a prediction grid
duration <- 1:72
installment <-1:4
grid <- expand.grid( duration2=duration, installment8=installment)
# predict on the grid
newdata <- cbind(grid, checkingstatus1 = rep("A11", 288), history3="A30")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A11", font=2)
points(locs[check=="A11",], pch=c(4,1)[resp[check=="A11"]])
# Repeat for A12
newdata <- cbind(grid, checkingstatus1 = rep("A12", 288), history3="A30")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A12", font=2)
points(locs[check=="A12",], pch=c(4,1)[resp[check=="A12"]])
# Repeat for A13
newdata <- cbind(grid, checkingstatus1 = rep("A13", 288), history3="A30")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A13", font=2)
points(locs[check=="A13",], pch=c(4,1)[resp[check=="A13"]])
# Repeat for A14
newdata <- cbind(grid, checkingstatus1 = rep("A14", 288), history3="A30")
pred <- predict(reg, newdata)
type <-  matrix((exp(pred)/(1+exp(pred))) > 0.5, ncol=4)
image(x=duration, y=installment, z=type, col = c(2,5), xlab="", ylab="")
text(x=65,y=3.5,labels="A14", font=2)
points(locs[check=="A14",], pch=c(4,1)[resp[check=="A14"]])
# Throw on the axis labels
mtext("duration", side=1, font=3, outer=TRUE, line=.5)
mtext("installment", side=2, font=3, outer=TRUE, line=.5)
mtext("Clean history borrowers",
      side=3, font=2, outer=TRUE)
