library(car)
data(Duncan)

plot(Duncan$income, Duncan$prestige, pch=16, col="black", cex=1.5)

plot(prestige ~ income, data=Duncan, type='n', 
    xlab='', ylab='', axes=F, xlim=c(0,100), ylim=c(0,100))

points(Duncan$income, Duncan$prestige, pch=16, col="blue")

plot(Duncan$type, Duncan$income)



axis(1)
axis(2)

x.at <- seq(min(Duncan$income), max(Duncan$income), length=4)
y.at <- seq(min(Duncan$prestige), max(Duncan$prestige), length=4)
plot(prestige ~ income, data=Duncan, type='n', 
    xlab='', ylab='', axes=F)
points(Duncan$income, Duncan$prestige, pch=16, col="blue")
axis(1, at=x.at)
axis(2, at=y.at)


x.at <- round(seq(min(Duncan$income), max(Duncan$income), length=4), 0)
y.at <- round(seq(min(Duncan$prestige), max(Duncan$prestige), length=4), 0)
plot(prestige ~ income, data=Duncan, type='n', 
    xlab='', ylab='', axes=F, xlim=c(7,81), ylim=c(3,97))
points(Duncan$income, Duncan$prestige, pch=16, col="blue")

axis(1, at=c(-10, x.at))
axis(2, at=c(-10, y.at))

box()

mtext("% males earnings > $3500/year", 1, line=-1) 
mtext("% of NORC raters indicating profession as\n 'good' or 'excellent'", 
    2, line=2)

title("Prestige versus Income\n (Duncan data)")


x.at <- round(seq(min(Duncan$income), max(Duncan$income), length=4), 0)
y.at <- round(seq(min(Duncan$prestige), max(Duncan$prestige), length=4), 0)
plot(prestige ~ income, data=Duncan, type='n',
    xlab='', ylab='', axes=F)
axis(1, at=x.at)
axis(2, at=y.at)
box()

mtext("% males earnings > $3500/year", 1, line=3) 
mtext("% of NORC raters indicating profession as\n 'good' or 'excellent'", 
    2, line=2)
title("Prestige versus Income\n (Duncan data)")

Duncan_prof <- Duncan[Duncan$type == "prof", ]
Duncan_bc <- Duncan[Duncan$type == "bc", ]
Duncan_wc <- Duncan[Duncan$type == "wc", ]

points(Duncan_bc$income, Duncan_bc$prestige, col='red',
    pch=15, cex=1.5)
points(Duncan_prof$income, Duncan_prof$prestige, col='black',
    pch=16, cex=1.5)
points(Duncan_wc$income, Duncan_wc$prestige, col='blue',
    pch=17, cex=1.5)



pch.vec <- c(15,16,17)
col.vec <- c('red', 'black', 'blue')
type_num <- as.numeric(Duncan$type)

pch.vec[1]
pch.vec[2]
pch.vec[3]

col.vec[1]
col.vec[2]
col.vec[3]


x.at <- round(seq(min(Duncan$income), max(Duncan$income), length=4), 0)
y.at <- round(seq(min(Duncan$prestige), max(Duncan$prestige), length=4), 0)
plot(prestige ~ income, data=Duncan, type="n", bty="l",
    xlab='', ylab='', axes=F)
axis(1, at=x.at)
axis(2, at=y.at)
box()

mtext("% males earnings > $3500/year", 1, line=3) 
mtext("% of NORC raters indicating profession as\n 'good' or 'excellent'", 
    2, line=2)
title("Prestige versus Income\n (Duncan data)")
pch.vec <- c(15,16,17)
col.vec <- c('red', 'black', 'blue')
type_num <- as.numeric(Duncan$type)
points(Duncan$income, Duncan$prestige, pch=pch.vec[type_num], col=col.vec[type_num], cex=1.5)

legend("bottomright", c("Blue-collar", "Professional", "White-collar"), 
    pch=pch.vec, col=col.vec, cex=1.5, inset=.01)

identify(x=Duncan$income, y=Duncan$prestige, 
    labels=rownames(Duncan), n=2)

locator(1)

mod <- lm(prestige~income, data=Duncan)
abline(mod)


### Three Lines (with arrows showing fitted value for Minister) ###

x.at <- round(seq(min(Duncan$income), max(Duncan$income), length=4), 0)
y.at <- round(seq(min(Duncan$prestige), max(Duncan$prestige), length=4), 0)
plot(prestige ~ income, data=Duncan, type='n',
    xlab='', ylab='', axes=F)
axis(1, at=x.at)
axis(2, at=y.at)
box()

mtext("% males earnings > $3500/year", 1, line=3) 
mtext("% of NORC raters indicating profession as\n 'good' or 'excellent'", 
    2, line=2)
title("Prestige versus Income\n (Duncan data)")
pch.vec <- c(15,16,17)
col.vec <- c('red', 'black', 'blue')
type_num <- as.numeric(Duncan$type)
points(Duncan$income, Duncan$prestige, pch=pch.vec[type_num], col=col.vec[type_num])
legend("bottomright", c("Blue-collar", "Professional", "White-collar"), 
    pch=pch.vec, col=col.vec, inset=.01)


mod2 <- lm(prestige~type + income, data=Duncan)
b <- mod2$coef
a_bc <- b[1]
a_prof <- b[1] + b[2]
a_wc <- b[1] + b[3]

abline(a=a_bc, b=b[4], col=col.vec[1], lty=1, lwd=2)
abline(a=a_prof, b=b[4], col=col.vec[2], lty=2, lwd=2)
abline(a=a_wc, b=b[4], col=col.vec[3], lty=3, lwd=2)


#identify(x=Duncan$income, y=Duncan$prestige, 
#    labels=rownames(Duncan), n=2)

Duncan[rownames(Duncan) == "minister", ]
mod2$fitted[names(mod2$fitted) == "minister"]

arrows(21,87,21,54.05111, code=2, col="gray75", length=.1)
arrows(21,54.0511, 4.04, 54.0511, code=2, col="gray75", length=.1)
text(Duncan$income[rownames(Duncan) == "minister"], 
	Duncan$prestige[rownames(Duncan) == "minister"], "Minister", pos=3)

### Different Slopes of Income by estimating Separate Models ###

mod.bc <- lm(prestige~ income, data=Duncan, subset= Duncan$type == "bc")
mod.prof <- lm(prestige~ income, data=Duncan, subset= Duncan$type == "prof")
mod.wc <- lm(prestige~ income, data=Duncan, subset= Duncan$type == "wc")

x.at <- round(seq(min(Duncan$income), max(Duncan$income), length=4), 0)
y.at <- round(seq(min(Duncan$prestige), max(Duncan$prestige), length=4), 0)
plot(prestige ~ income, data=Duncan, type='n',
    xlab='', ylab='', axes=F)
axis(1, at=x.at)
axis(2, at=y.at)
box()

mtext("% males earnings > $3500/year", 1, line=3) 
mtext("% of NORC raters indicating profession as\n 'good' or 'excellent'", 
    2, line=2)
title("Prestige versus Income\n (Duncan data)")
pch.vec <- c(15,16,17)
col.vec <- c('red', 'black', 'blue')
type_num <- as.numeric(Duncan$type)
points(Duncan$income, Duncan$prestige, pch=pch.vec[type_num], col=col.vec[type_num])
legend("bottomright", c("Blue-collar", "Professional", "White-collar"), 
    pch=pch.vec, col=col.vec, inset=.01)

abline(mod.bc, lty=1, col="red", lwd=2)
abline(mod.prof, lty=2, col="black", lwd=2)
abline(mod.wc, lty=3, col="blue", lwd=2)


### Semi-transparent Confidence Intervals ###

newdata <- data.frame(
	income = rep(seq(min(Duncan$income), max(Duncan$income), length=25), 3), 
	type = rep(c("bc", "prof", "wc"), each=25))

preds <- as.data.frame(predict(mod2, newdata, interval="confidence"))
preds$x <- newdata$income
pred.bc <- preds[1:25, ]
pred.prof <- preds[26:50, ]
pred.wc <- preds[51:75, ]

x.at <- round(seq(min(Duncan$income), max(Duncan$income), length=4), 0)
y.at <- round(seq(min(Duncan$prestige), max(Duncan$prestige), length=4), 0)
plot(prestige ~ income, data=Duncan, type='n',
    xlab='', ylab='', axes=F)
axis(1, at=x.at)
axis(2, at=y.at)
box()

mtext("% males earnings > $3500/year", 1, line=3) 
mtext("% of NORC raters indicating profession as\n 'good' or 'excellent'", 
    2, line=2)
title("Prestige versus Income\n (Duncan data)")
pch.vec <- c(15,16,17)
col.vec <- c('red', 'green', 'blue')
type_num <- as.numeric(Duncan$type)
legend("bottomright", c("Blue-collar", "Professional", "White-collar"), 
    pch=pch.vec, col=col.vec, inset=.01)

with(pred.bc, polygon(x=c(x, rev(x), x[1]), y=c(lwr, rev(upr), lwr[1]), col=rgb(1,0,0, .25), border=NA))
with(pred.bc, lines(x,fit, col="red", lwd=2))
with(pred.prof, polygon(x=c(x, rev(x), x[1]), y=c(lwr, rev(upr), lwr[1]), col=rgb(0,1,0, .25), border=NA))
with(pred.prof, lines(x,fit, col="green", lwd=2))
with(pred.wc, polygon(x=c(x, rev(x), x[1]), y=c(lwr, rev(upr), lwr[1]), col=rgb(0,0,1, .25), border=NA))
with(pred.wc, lines(x,fit, col="blue", lwd=2))
points(Duncan$income, Duncan$prestige, pch=pch.vec[type_num], col=col.vec[type_num])

### Differnet Lines with Interactions ### 

mod3 <- lm(prestige ~ income*type, data=Duncan)
summary(mod3)
preds <- as.data.frame(predict(mod3, newdata, interval="confidence"))
preds$x <- newdata$income
pred.bc <- preds[1:25, ]
pred.prof <- preds[26:50, ]
pred.wc <- preds[51:75, ]

x.at <- round(seq(min(Duncan$income), max(Duncan$income), length=4), 0)
y.at <- round(seq(min(Duncan$prestige), max(Duncan$prestige), length=4), 0)
plot(prestige ~ income, data=Duncan, type='n',
    xlab='', ylab='', axes=F)
axis(1, at=x.at)
axis(2, at=y.at)
box()

mtext("% males earnings > $3500/year", 1, line=3) 
mtext("% of NORC raters indicating profession as\n 'good' or 'excellent'", 
    2, line=2)
title("Prestige versus Income\n (Duncan data)")
pch.vec <- c(15,16,17)
col.vec <- c('red', 'green', 'blue')
type_num <- as.numeric(Duncan$type)
legend("bottomright", c("Blue-collar", "Professional", "White-collar"), 
    pch=pch.vec, col=col.vec, inset=.01)

with(pred.bc, polygon(x=c(x, rev(x), x[1]), y=c(lwr, rev(upr), lwr[1]), col=rgb(1,0,0, .25), border=NA))
with(pred.bc, lines(x,fit, col="red", lwd=2))
with(pred.prof, polygon(x=c(x, rev(x), x[1]), y=c(lwr, rev(upr), lwr[1]), col=rgb(0,1,0, .25), border=NA))
with(pred.prof, lines(x,fit, col="green", lwd=2))
with(pred.wc, polygon(x=c(x, rev(x), x[1]), y=c(lwr, rev(upr), lwr[1]), col=rgb(0,0,1, .25), border=NA))
with(pred.wc, lines(x,fit, col="blue", lwd=2))
points(Duncan$income, Duncan$prestige, pch=pch.vec[type_num], col=col.vec[type_num])



#### Check Boxes for Turning Elements on and off ####
library(rpanel)
mypanel <- function(panel){
	x.at <- round(seq(min(Duncan$income), max(Duncan$income), length=4), 0)
	y.at <- round(seq(min(Duncan$prestige), max(Duncan$prestige), length=4), 0)
	plot(Duncan$income, Duncan$prestige, type='n',, 
	    xlim=c(7,81), ylim=c(-10,105), xlab='', ylab='', axes=F)
	axis(1, at=x.at)
	axis(2, at=y.at)
	box()
	mtext("% males earnings > $3500/year", 1, line=3) 
	mtext("% of NORC raters indicating profession as\n 'good' or 'excellent'", 
	    2, line=2)
	title("Prestige versus Income\n (Duncan data)")
	pch.vec <- c(16,16, 16)
	col.vec <- c('red', 'green', 'blue')
	type_num <- as.numeric(Duncan$type)
	if(panel$ci[1])with(pred.bc, polygon(x=c(x, rev(x), x[1]), y=c(lwr, rev(upr), lwr[1]), col=rgb(1,0,0, .25), border=NA))
	if(panel$ci[2])with(pred.prof, polygon(x=c(x, rev(x), x[1]), y=c(lwr, rev(upr), lwr[1]), col=rgb(0,1,0, .25), border=NA))
	if(panel$ci[3])with(pred.wc, polygon(x=c(x, rev(x), x[1]), y=c(lwr, rev(upr), lwr[1]), col=rgb(0,0,1, .25), border=NA))
	if(panel$lines[1])with(pred.bc, lines(x,fit, col="red", lwd=2))
	if(panel$lines[2])with(pred.prof, lines(x,fit, col="green", lwd=2))
	if(panel$lines[3])with(pred.wc, lines(x,fit, col="blue", lwd=2))
	if(panel$points[1])with(Duncan, points(income[type == "bc"], prestige[type == "bc"], pch=pch.vec[1], col=col.vec[1]))
	if(panel$points[2])with(Duncan, points(income[type == "prof"], prestige[type == "prof"], pch=pch.vec[2], col=col.vec[2]))
	if(panel$points[3])with(Duncan, points(income[type == "wc"], prestige[type == "wc"], pch=pch.vec[3], col=col.vec[3]))
	panel}

panel <- rp.control(lines = c(F,F,F), ci = c(F,F,F), points = c(F,F,F))
rp.checkbox(panel, points, labels = c("Blue Collar", "Professional", "White Collar"), title= "Points", 
	action = mypanel)
rp.checkbox(panel, lines, labels = c("Blue Collar", "Professional", "White Collar"), title= "Fitted Lines", 
	action = mypanel)
rp.checkbox(panel, ci, labels = c("Blue Collar", "Professional", "White Collar"), title= "Confidence Intervals", 
	action = mypanel)

rp.radiogroup(panel, lines, 	initval = c(0,1), labels=c("off","on"), action=mypanel)
rp.radiogroup(panel, ci, c(0,1), labels=c("off","on"), action=mypanel)
rp.radiogroup(panel, points, c(0,1), labels=c("off","on"), action=mypanel)

### Side-by-side Barplots ###
library(car)
data(Ornstein)
sectors <- table(Ornstein$sector)
barplot(sectors, las=2)

sectors.prop <- sectors/sum(sectors)
barplot(sectors.prop, las=2)

sec.list <- by(Ornstein$sector, list(Ornstein$nation), table)
sec.tab <- do.call(rbind, sec.list)
barplot(sec.tab, col=c("red", "blue", "green", "orange"), beside=T)
legend(locator(1), legend=levels(Ornstein$nation), fill=c("red", "blue", "green", "orange"))

sec.prop <- prop.table(sec.tab, 1)
barplot(sec.prop, beside=T, col=c("red", "blue", "green", "orange"))
legend(locator(1), legend=levels(Ornstein$nation), fill=c("red", "blue", "green", "orange"))


### Tables to LaTeX ###

mod.income <- lm(prestige ~ income, data= Duncan)
mod.education <- lm(prestige ~ education, data=Duncan)
mod.type <- lm(prestige ~ type, data= Duncan)
mod.full <- lm(prestige ~ type + education + income, data = Duncan)

library(apsrtable)
apsrtable(mod.income, mod.education, mod.type, mod.full, 
	model.names = c("Income", "Education", "Type", "Full"), Sweave=F, 
	caption = "Duncan Models", label = "tab:duncanmods")

apsrtable(mod.income, mod.education, mod.type, mod.full, 
	model.names = c("Income", "Education", "Type", "Full"), Sweave=T)


### Tables to Word ###

cat(apsrtable(mod.income, mod.education, mod.type, mod.full, 
	model.names = c("Income", "Education", "Type", "Full"), Sweave=T), 
     file="duncan_mods.txt")

### Printing APA Style ###

shuffle <- function(b,se,p, df.res, two.sided=T, digits, sep= ",", apa=T){
	nb <- names(b)
	nse <- sapply(1:length(b), function(x)paste(rep("", each=x), collapse=""))
	stars <- sapply(1:length(p), function(x)paste(rep("*", x), collapse=""))
	stars <- c("", stars)
	sig <- do.call(cbind, lapply(p, function(x)(2^(two.sided)*pt(abs(b/se), df.res, lower.tail=F)) < x))
	sig <- apply(sig, 1, sum)+1
	b <- sprintf(paste("%.", digits, "f", sep=""), b)
	se <- sprintf(paste("%.", digits, "f", sep=""), se)
	if(apa){
	newb <- gsub("^-0", "-", gsub("^0", "", b))
	} else{
		newb <- b
	}
	newb <- paste(newb, stars[sig], sep="")
	if(apa){
	newse <- gsub("^-0", "-", gsub("^0\\.", "\\.", se))
	} else{
		newse <- se
	}
	newse <- paste("(", newse, ")", sep="")
	mat <- matrix(rbind(newb, newse), ncol=1)
	rownames(mat) <- as.vector(rbind(nb, nse))
	mat <- cbind(sep, mat)
	colnames(mat) <- c("", "")
	noquote(mat)
}


shuffle(mod.full$coef,sqrt(diag(vcov(mod.full))), c(.05,.01,.001), 
	mod.full$df.residual, two.sided=F, digits=3, apa=T)

shuffle(mod.full$coef,sqrt(diag(vcov(mod.full))), c(.05,.01,.001), 
	mod.full$df.residual, two.sided=F, digits=3, apa=F)

