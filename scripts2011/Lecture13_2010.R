library(mice)
library(foreign)
library(Amelia)
library(Zelig)
library(lattice)

sub <- read.dta("nes04_sub.dta")
anes.mice <- mice(sub)
nes.mids <- lm.mids(leftright ~ age + as.numeric(education) + gender + as.numeric(hhincome), data=anes.mice)

anes.am <- amelia(sub, noms = c("gender", "lastpres"), ords = c("hhincome", "education"))
nes.zel <- zelig(leftright ~ age + as.numeric(education) + gender + as.numeric(hhincome), data=anes.am$imputations, 
	model="normal")

summary(pool(nes.mids))
summary(nes.zel)


data(africa)
africa$year - mean(africa$year)
a.out <- amelia(x = africa, cs = "country", ts = "year", logs = c("gdp_pc", "trade", "population"), polytime=3)
summary(a.out)

mice(africa)
data(africa)
africa$loggdp_pc <- log(africa$gdp_pc)
africa$logpop <- log(africa$population)
africa$logtrade <- log(africa$trade)
africa <- africa[,-c(2,7)]
africa$year2 <- africa$year^2
africa$year3 <- africa$year^3
a.mice <- mice(africa, method = c("","~exp(loggdp_pc)", "","~exp(logtrade)", "",  "pmm", "", "pmm", "~I(year^2)", "~I(year^3)"))
data(africa)
africa$loggdp_pc <- log(africa$gdp_pc)
africa$logpop <- log(africa$population)
africa$logtrade <- log(africa$trade)
africa <- africa[,-c(2,7)]
a.mice <- mice(africa, method = c("","~exp(loggdp_pc)", "","~exp(logtrade)", "",  "pmm", "", "pmm"))

am.imp1 <- a.out$imputations[[1]]
mice.imp1<- complete(a.mice, 1)
am.imp2 <- a.out$imputations[[2]]
mice.imp2 <- complete(a.mice, 2)
am.imp3 <- a.out$imputations[[3]]
mice.imp3 <- complete(a.mice, 3)
am.imp4 <- a.out$imputations[[4]]
mice.imp4 <- complete(a.mice, 4)
am.imp5 <- a.out$imputations[[5]]
mice.imp5 <- complete(a.mice, 5)



xyplot(trade ~ year | country, data=am.imp1, 
	panel=function(x,y,subscripts){
		panel.lines(x,y, lty=1, col="black", lwd=1.5)
		panel.lines(x, am.imp2$trade[subscripts], col="black", lwd=1.5)
		panel.lines(x, am.imp3$trade[subscripts], col="black", lwd=1.5)
		panel.lines(x, am.imp4$trade[subscripts], col="black", lwd=1.5)
		panel.lines(x, am.imp5$trade[subscripts], col="black", lwd=1.5)
		panel.lines(x, (mice.imp1$trade)[subscripts], col="red", lwd=1.5, lty=2)
		panel.lines(x, (mice.imp2$trade)[subscripts], col="red", lwd=1.5, lty=2)
		panel.lines(x, (mice.imp3$trade)[subscripts], col="red", lwd=1.5, lty=2)
		panel.lines(x, (mice.imp4$trade)[subscripts], col="red", lwd=1.5, lty=2)
		panel.lines(x, (mice.imp5$trade)[subscripts], col="red", lwd=1.5, lty=2)
})


poetate <- read.spss("isq99.por", use.value.labels=T, to.data.frame=T)
ptsub <- poetate[,c(1,2,3,4,5,6,7,8,9,17,19,20,21)]
pt.am <- amelia(ptsub, cs="IDORIGIN", ts="YEAR", splinetime=4)
pt.mice <- mice(ptsub)
comp <- list()
for(i in 1:5){
	comp[[i]] <- complete(pt.mice, i)
}
for(i in 1:5){
	comp[[i]]$lagAI <- tscslag(comp[[i]], "AI", "IDORIGIN", "YEAR")
	pt.am$imputations[[i]]$lagAI <- tscslag(pt.am$imputations[[i]], "AI", "IDORIGIN", "YEAR")
}

class(comp) <- "mi"
mice.mod <- zelig(AI ~ lagAI + DEMOC3 + CWARCOW + IWARCOW2 + MIL2 + LEFT + BRIT + PCGNP+ LPOP, data=comp, model="normal")
zel.mod <- zelig(AI ~ lagAI + DEMOC3 + CWARCOW + IWARCOW2 + MIL2 + LEFT + BRIT + PCGNP+ LPOP, data=pt.am$imputations, model="normal")

poetate$lagAI <- tscslag(poetate, "AI", "IDORIGIN", "YEAR")
poetate.lw <- poetate[, c("AI","lagAI","DEMOC3","CWARCOW","IWARCOW2","MIL2","LEFT","BRIT","PCGNP","LPOP")]
poetate.lw <- na.omit(poetate.lw)
lw.mod <- lm(AI ~ lagAI + DEMOC3 + CWARCOW + IWARCOW2 + MIL2 + LEFT + BRIT + PCGNP+ LPOP, data=poetate.lw)


