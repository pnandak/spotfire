#-----------------------------------------------------------------------
#--- FIRST SESSION
#-----------------------------------------------------------------------

# Operator "<-"
x <- 2
x
x + 1
myfunction <- function(p) log(p / (1 - p))
myfunction(0.4)
x <- rnorm(5)
x
res <- summary(x)
res

# Vectors
c(25, 0, 7, 248)
c("A", "A", "E", "FG")
as.factor(c("A", "A", "E", "FG"))
x <- c(25, 0, 7, 248)
log10(x + 1)
y <- -(1:4)
y
x + y
x * y
c(x, y)

# Matrices
cbind(x)
rbind(x)
cbind(x, y)
rbind(x, y)
matrix(x, ncol = 1)
matrix(x, nrow = 1)
matrix(c(x, y), ncol = 2)
matrix(c(x, y), nrow = 2, byrow = TRUE)
z <- cbind(x, y)
z
rownames(z)
colnames(z)
rownames(z) <- 1:4
colnames(z) <- c("v1", "v2")
z
z <- cbind(x, y)
z
dimnames(z)
dimnames(z) <- list(1:4, c("v1", "v2"))
z
matrix(c(x, y), ncol = 2, dimnames = list(1:4, c("v1", "v2")))
z <- cbind(v1 = x, v2 = y)
z

# Data frames
data.frame(x)
data.frame(nbanim = x)
y <- c("A", "A", "E", "FG")
mydata <- data.frame(nbanim = x, f1 = I(y), f2 = y)
mydata
summary(mydata)
mydata$nbanim
mydata$f1
mydata$nbanim[c(2, 4)]
mydata$nbanim[-c(1, 3)]
mydata$nbanim[mydata$nbanim > 0]
mydata$nbanim[mydata$f1 == "A" & mydata$nbanim > 0]
mydata$nbanim[(mydata$f1 == "A" | mydata$f1 == "E") & mydata$nbanim > 0]
mydata$nbanim[mydata$f1 != "FG" & mydata$nbanim > 0]

mydata[mydata$f1 == "A" & mydata$nbanim > 0, ]
mydata[mydata$nbanim > 0, c(1, 3)]
mydata[mydata$nbanim > 0, c("nbanim", "f2")]
mydata[1:2, c("nbanim", "f2")]
mydata[1:2, -3]

mydata$sizeclass <- ifelse(mydata$nbanim < 100, "SMALL", "LARGE")
mydata
mydata$sizeclass <- ifelse(mydata$nbanim < 20, "SMALL",
    ifelse(mydata$nbanim >= 20 & mydata$nbanim < 100, "MEDIUM", "LARGE"))
mydata

#-----------------------------------------------------------------------
#--- DATA IMPORTATION/EXPORTATION
#-----------------------------------------------------------------------

## Data importation
# text files
db <- "C:/Stat_Ilri_2005_06/Data/import.txt"
mydata <- read.table(
    file = db,
    header = TRUE,
    sep = ";"
    )
mydata
# access tables with RODBC
db <- "C:/Stat_Ilri_2005_06/Data/import.mdb"
channel <- odbcConnectAccess(db)
mydata <- sqlFetch(
    channel = channel,
    sqtable = "T_Data"
    )
mydata
mydata <- sqlQuery(
    channel = channel,
    query = "SELECT * FROM T_Data" 
    )
mydata
mydata <- sqlQuery(
    channel = channel,
    query = "SELECT * FROM T_Data WHERE var3 > 0" 
    )
mydata
odbcClose(channel)
# excel tables with RODBC
db <- "C:/Stat_Ilri_2005_06/Data/import.xls"
channel <- odbcConnectExcel(db)
mydata <- sqlFetch(
    channel = channel,
    sqtable = "Data"
    )
mydata
mydata <- sqlQuery(
    channel = channel,
    query = "select * from [Data$]" 
    )
mydata
odbcClose(channel)

## Data exportation
# text files
db <- "C:/Stat_Ilri_2005_06/Data/mydata.txt"
write.table(
    x = mydata,
    file = db,
    quote = FALSE,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE
    )
# access tables with RODBC
# An Acess database must exist before opening the connection
db <- "C:/Stat_Ilri_2005_06/Data/mydata.mdb"
channel <- odbcConnectAccess(db)
sqlSave(
  channel = channel,
  dat = mydata,
  tablename = "T_Data",
  # if "T_Data" already exists, use "safer = FALSE"
  # to remove the existing table
  rownames = FALSE
  )
odbcClose(channel)    
mydata <- query ("T_Data")
mydata
mydata <- query ("Data")
mydata

#-----------------------------------------------------------------------
#--- PROBABILITY DISTRIBUTIONS
#-----------------------------------------------------------------------

x <- seq(-5, 15, by = 0.1)
y <- dnorm(x, mean = 5, sd = 2)
xyplot(y ~ x, type = "l", ylab = "pdf", col = "red")

b <- 0.012
varb <-  0.000055
z <- b / varb^0.5
z
2 * pnorm(z, lower.tail = FALSE)
[1] 0.1056454
pnorm(z, lower.tail = FALSE)
[1] 0.05282271

alpha <- 0.05
qnorm(alpha / 2, lower.tail = FALSE)

y <- rnorm(n = 100, mean = 5, sd = 2)
y
histogram(y, n = 30, col = "orangered")

b <- c(2.8, 1.4) 
varb <- matrix(c(2, 0.7, 0.7, 0.5), ncol = 2)
varb
mvrnorm(n = 3, mu = b, Sigma = varb)
var(mvrnorm(n = 10000, mu = b, Sigma = varb))

#-----------------------------------------------------------------------
#--- DESCRIPTIVE STATISTICS - CONTINUOUS DATA
#-----------------------------------------------------------------------

## 1 variable
# dataset metab_energ.xls
mydata <- query("Data") 
mydata
# descriptive statistics
mean(mydata$energ)
var(mydata$energ)
sd(mydata$energ) # = var(mydata$energ)^0.5
summary(mydata$energ)
quantile(mydata$energ) # min, first quartile, median, third quartile, max
quantile(x = mydata$energ, probs = 0.5)
median(mydata$energ)
quantile(x = mydata$energ, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
# graphics
histogram(formula = ~ energ, data = mydata,
    n = 30, xlab = "Metabolic energy (kcal/kg)")
densityplot(formula = ~ energ, data = mydata)
stripplot(formula = ~ energ, data = mydata,
    jitter = TRUE, factor = 10)
bwplot(formula = ~ energ, data = mydata)
qqmath(formula = ~ energ, distrib = qnorm, data = mydata)

## 1 variable x 1 or several factors
# descriptive statistics
aggregate(
    data.frame(energ = mydata$energ),
    by = list(poultry = mydata$poultry),
    FUN = mean
    )
aggregate(
    data.frame(energ = mydata$energ),
    by = list(
        poultry = mydata$poultry,
        country = mydata$country
        ),
    FUN = mean
    )
tapply(
    X = mydata$energ,
    INDEX = mydata$poultry,
    FUN = mean
    )
tapply(
    X = mydata$energ,
    INDEX = list(mydata$country, mydata$poultry),
    FUN = mean
    )
aggstat(
    formula = energ ~ poultry,
    data = mydata,
    FUN = mean
    )
aggstat(
    formula = cbind(energ, log(energ)) ~ poultry,
    data = mydata,
    FUN = mean
    )
aggstat(
    formula = energ ~ poultry + country,
    data = mydata,
    FUN = mean
    )
# graphics
histogram(formula = ~ energ | poultry,
    data = mydata, n = 30, layout = c(1, 2))
qqmath(formula = ~ energ | poultry, distribution = qnorm, data = mydata)
qq(formula = poultry ~ energ, data = mydata)
densityplot(formula = ~ energ | poultry,
    data = mydata, layout = c(1, 2))
stripplot(formula = ~ energ | poultry,
    data = mydata, layout = c(1, 2),
    jitter = TRUE, factor = 10)
stripplot(formula = energ ~ poultry,
    data = mydata, jitter = TRUE)
stripplot(formula = energ ~ poultry,
    data = mydata, jitter = TRUE,
    groups = poultry, col = c("red", "blue"))
bwplot(formula = ~ energ | poultry,
    data = mydata, layout = c(1, 2))
bwplot(formula = energ ~ poultry,
    data = mydata)
bwplot(formula = energ ~ poultry | country,
    data = mydata)
dotplot(formula = energ ~ poultry | country,
    data = mydata)

## Several variables
# dataset zebus.xls
mydata <- query("Data") 
mydata
# covariances and correlations
cov(mydata$meat1q, mydata$fat) 
cor(mydata$meat1q, mydata$fat)
round(cov(mydata[, 3:8]), digits = 2)
round(cor(mydata[, 3:8]), digits = 2)
# graphics
xyplot(formula = fat ~ meat1q, data = mydata)
pairs(mydata[, 3:8])

#-----------------------------------------------------------------------
#--- DESCRIPTIVE STATISTICS - CATEGORICAL DATA
#-----------------------------------------------------------------------

## dataset pathosen.xls
mydata <- query("Data") 
mydata

## Descriptive statistics
#table
table(mydata$disease)
N <- sum(table(mydata$disease))
round(100 * table(mydata$disease) / N, digits = 2)
table(mydata$year, mydata$disease)
table(mydata$year, mydata$disease, mydata$poultry)
# xtabs
xtabs(formula = ~ disease, data = mydata)
xtabs(formula = ~ year + disease, data = mydata)
# ctabs
ctabs(formula = disease ~ 1, data = mydata)
ctabs(formula = disease ~ year, chi2 = FALSE, data = mydata)
ctabs(formula = disease ~ poultry + year, chi2 = FALSE, data = mydata)

## Graphics
# barchart
res <- ctabs(formula = disease ~ 1, data = mydata)
res@counts
barchart(formula = n ~ disease, data = res@counts)
res <- ctabs(formula = disease ~ year, data = mydata)
barchart(formula = n ~ disease | year, layout = c(1, 2), data = res@counts)
res <- ctabs(formula = disease ~ poultry + year, chi2 = FALSE, data = mydata)
barchart(formula = n ~ disease | year + poultry, layout = c(2, 3), data = res@counts)

#-----------------------------------------------------------------------
#--- COMPARISON OF MEANS (SIMPLE TESTS)
#-----------------------------------------------------------------------

## Independent samples
# dataset wgt30cam
mydata <- query("Data") 
mydata
bwplot(formula = weight ~ supp, data = mydata)
t.test(formula = weight ~ supp, var.equal = TRUE, data = mydata)
histogram(formula = ~ weight | supp, n = 30, data = mydata)
qqmath(formula = ~ weight | supp, data = mydata)
shapiro.test(mydata$weight[mydata$supp == "CTRL"])
shapiro.test(mydata$weight[mydata$supp == "SUPP"])
aggstat(formula = weight ~ supp, data = mydata, FUN = sd)
bartlett.test(formula = weight ~ supp, data = mydata)
t.test(formula = weight ~ supp, data = mydata)
wilcox.test(formula = weight ~ supp, data = mydata)

## Paired data
# dataset expepg
mydata <- query("Data") 
mydata
bwplot(formula = epg ~ date | group, data = mydata)
bwplot(formula = log(epg + 1) ~ date | group, data = mydata)
mydata1 <- mydata[mydata$date == "DATE1", ]
mydata2 <- mydata[mydata$date == "DATE2", ]
newdata <- merge(mydata1, mydata2, by = c("idanim", "group"))
newdata
datatmp <- newdata[newdata$group == "CTRL", ]
t.test(x = log(datatmp$epg.x + 1), y = log(datatmp$epg.y + 1),
	paired = TRUE)
datatmp <- newdata[newdata$group == "TREAT", ]
t.test(x = log(datatmp$epg.x + 1), y = log(datatmp$epg.y + 1),
	paired = TRUE)
datatmp <- newdata[newdata$group == "CTRL", ]
wilcox.test(x = log(datatmp$epg.x + 1), y = log(datatmp$epg.y + 1),
	paired = TRUE)
datatmp <- newdata[newdata$group == "TREAT", ]
wilcox.test(x = log(datatmp$epg.x + 1), y = log(datatmp$epg.y + 1),
	paired = TRUE)


#-----------------------------------------------------------------------
#--- COMPARISON OF PROPORTIONS (SIMPLE TESTS)
#-----------------------------------------------------------------------

## Independent samples
# dataset trial_p_1
mydata <- query("Data")
mydata
# chi-squared test
prop.test(x = mydata$y, n = mydata$n)
X <- cbind(mydata$y, mydata$n - mydata$y)
X
prop.test(X)
chisq.test(X)
res <- chisq.test(X)
attributes(res)
res$p.value
res$observed
res$expected
# dataset trial_p_2
mydata <- query("Data")
mydata
# chi-squared test
X <- cbind(mydata$y, mydata$n - mydata$y)
X
chisq.test(X)
chisq.test(X, simulate.p.value = TRUE, B = 10000)
# Fisher's exact test
fisher.test(X)
# dataset trial_p_1
mydata <- query("Data_Binary")
mydata
# chi-squared test
res <- xtabs(formula =  ~ group + y, data = mydata)
res
summary(res)
chisq.test(res)
ctabs(formula = y ~ group, data = mydata)

## Paired data
# dataset trial_p_3
mydata <- query("Data")
mydata
X <- as.matrix(mydata[, 2:3])
X
mcnemar.test(X)
# dataset trial_p_3
mydata <- query("Data_Binary")
mydata
mcnemar.test(x = mydata$firstDate, y = mydata$secondDate)

#-----------------------------------------------------------------------
#--- LINEAR REGRESSION
#-----------------------------------------------------------------------

## Dataset marketsas
mydata <- query("Data")
mydata
## Simple regression
xyplot(formula = opcost ~ volume, data = mydata)
fm1 <- lm(formula = opcost ~ volume, data = mydata)
fm1
coef(fm1)
vcov(fm1) 
xyplot(
    formula = opcost ~ volume,
    data = mydata,
    panel = function(x,y){
        panel.xyplot(x, y)
        panel.abline(fm1, col = "red") # same as: panel.lmline(x, y, col = "red")
        }
    )
summary(fm1)
anova(fm1)
fitted(fm1)
predict(fm1, se.fit = TRUE)
residuals(fm1)
rstandard(fm1)
rstudent(fm1)
plot(fm1)
xyplot(
    formula = rstudent(fm1) ~ fitted(fm1),
    panel = function(x,y){
        panel.xyplot(x, y)
        panel.abline(0, col = "grey")
        panel.loess(x, y, col = "blue")
        }    
    )
influence.measures(fm1)
summary(influence.measures(fm1))
xyplot(formula = opcost ~ volume | type, data = mydata)
## Ancova
xyplot(
    formula = opcost ~ volume | type, data = mydata,
    panel = function(x,y, ...){
        panel.xyplot(x, y)
        panel.abline(fm1, col = "blue", lty = 2)
        panel.lmline(x, y, col = "red")
        }    
    )
# equal intercepts, equal slopes
summary(fm1)
# different intercepts, equal slopes
fm2 <- lm(formula = opcost ~ type + volume, data = mydata)
summary(fm2)
# equal intercepts, different slopes
fm3 <- lm(formula = opcost ~ type * volume - type, data = mydata)
summary(fm3)
fm3bis <- lm(formula = opcost ~ type:volume, data = mydata)
summary(fm3bis)
# different intercepts, different slopes
fm4 <- lm(formula = opcost ~ type * volume, data = mydata)
summary(fm4)
fm4bis <- lm(formula = opcost ~ type + type:volume, data = mydata)
summary(fm4bis)
# F-tests between nested models
# we observe an apparent contradiction
# between the first and the last two results
anova(fm1, fm2)
anova(fm1, fm3)
anova(fm2, fm4)
anova(fm3, fm4)
# but fm2, fm3, and fm4 have close AIC values
# more data are needed to better assess 
# the effect of "type"
AIC(fm1, fm2, fm3, fm4)
## Multiple regression
fm5 <- lm(formula = opcost ~ cattle + calves + hogs + sheep, data = mydata)
summary(fm5)
xyplot(
    formula = rstudent(fm5) ~ fitted(fm5),
    panel = function(x,y){
        panel.xyplot(x, y)
        panel.abline(0, col = "grey")
        }    
    )
plot(fm5)
histogram(rstudent(fm5), n = 30)

#-----------------------------------------------------------------------
#--- ANOVA
#-----------------------------------------------------------------------

## 1 factor
# dataset diet
mydata <- query("Data")
mydata
bwplot(formula = adg ~ diet1, data = mydata)
aggstat(formula = adg ~ diet1, data = mydata, FUN = mean)
aggstat(formula = adg ~ diet1, data = mydata, FUN = sd)
# model fitting
fm <- aov(formula = adg ~ diet1, data = mydata)
anova(fm)
coef(fm)
vcov(fm)
predict(fm, se.fit = TRUE)
plot(fm)
TukeyHSD(fm)

## 2 factors
# dataset wgt30cam
mydata <- query("Data")
mydata
bwplot(formula = weight ~ supp | birth, data = mydata)
aggstat(formula = weight ~ birth + supp, data = mydata, FUN = mean)
plot.design(weight ~ birth + supp, data = mydata, fun = "mean")
interaction.plot(mydata$birth, mydata$supp, mydata$weight)
fm1 <- aov(formula = weight ~ birth + supp + birth:supp, data = mydata)
anova(fm1)
fm2 <- aov(formula = weight ~ birth + supp, data = mydata)
coef(fm2)
vcov(fm2)
plot(fm2)
predict(fm2, se.fit = TRUE)
X <- expand.grid(birth = c("1OFFS", "2OFFS"), supp = c("CTRL", "SUPP"))
X
predfm2 <- predict(fm2, newdata = X, se.fit = TRUE)
predfm2
cbind(X, predfm2$fit, predfm2$se.fit)
drop1(fm2, test = "F")

#-----------------------------------------------------------------------
#--- Generalized linear models
#-----------------------------------------------------------------------

## Logistic regression
# dataset ovdeathsen (binary data)
mydata <- query("BinaryData")
mydata
ctabs(formula = death ~ birth + treatment, data = mydata)
fm1 <- glm(formula = death ~ treatment * birth, family = binomial(link = logit), data = mydata)
summary(fm1)
anova(fm1, test = "Chisq")
fm2 <- update(fm1, formula = death ~ treatment + birth)
summary(fm2)
X <- expand.grid(birth = c("1OFFS", "2OFFS"), treatment = c("CTRL", "DREN"))
X
predfm2 <- predict(fm2, type = "response", newdata = X, se.fit = TRUE)
cbind(X, predfm2$fit, predfm2$se.fit)
fm3 <- update(fm1, formula = death ~ treatment)
fm4 <- update(fm1, formula = death ~ birth)
fm5 <- update(fm1, formula = death ~ 1)
anova(fm1, fm2, test = "Chisq")
anova(fm2, fm5, test = "Chisq")
AIC(fm1, fm2, fm3, fm4, fm5)
# dataset ovdeathsen (binomial data)
mydata <- query("BinomialData")
mydata
fm2 <- glm(formula = cbind(death1, death0) ~ treatment + birth, family = binomial(link = logit), data = mydata)
summary(fm2)
predfm2 <- predict(fm2, type = "response", se.fit = TRUE)
cbind(mydata, predfm2$fit, predfm2$se.fit)

## Poisson regression
# dataset deathrates
mydata <- query("Data")
mydata
mydata$rate <- mydata$nbdeath / mydata$trisk
mydata
dotplot(formula = rate ~ treatment | sex, data = mydata)
fm1 <- glm(formula = nbdeath ~ sex * treatment + offset(log(trisk)), family = poisson(link = log), data = mydata)
summary(fm1)
anova(fm1, test = "Chisq")
fitted(fm1)
predict(fm1, type = "response")
fm1$offset
fitted(fm1) / exp(fm1$offset)

#-----------------------------------------------------------------------
#--- Linear mixed models
#-----------------------------------------------------------------------

## Dataset gen_1
mydata <- query("Data")
mydata
dotplot(
    formula = male ~ adg,
    data = mydata,
    groups = female
    )

## Linear mixed model
fm1 <- lme(fixed = adg ~ 1, random = ~ 1 | male/female, data = mydata)
summary(fm1)
intervals(fm1)
vcov(fm1)
VarCorr(fm1)
ranef(fm1)
ranef(fm1, level = 1)
ranef(fm1, level = 2)
coef(fm1, level = 1)
coef(fm1, level = 2)

#-----------------------------------------------------------------------
#--- PAIRWISE MULTIPLE COMPARISONS
#-----------------------------------------------------------------------

## Adjustement of p-values
# dataset trial_p_1
mydata <- query("Data")
mydata
# omnibus chi-squared test
prop.test(mydata$y, mydata$n)
# pairwise chi-squared tests
res <- data.frame(
    group1 = c(1, 1, 1, 2, 2, 3),
    group2 = c(2, 3, 4, 3, 4, 4)
    )
res 
P <- 0
for(i in 1:nrow(res)){
    datatmp <- mydata[as.numeric(res[i, ]), ]
    P[i] <- prop.test(datatmp$y, datatmp$n)$p.value
    }
P.holm <- p.adjust(P)
P.bonf <- p.adjust(P, method = "bonferroni")
res$P <- P ; res$P.holm <- P.holm ; res$P.bonf <- P.bonf 
res
# direct pairwise chi-squared tests
pairwise.prop.test(mydata$y, mydata$n, p.adjust.method = "none")
pairwise.prop.test(mydata$y, mydata$n)
pairwise.prop.test(mydata$y, mydata$n, p.adjust.method = "bonferroni")

#-----------------------------------------------------------------------
#--- MAPPING
#-----------------------------------------------------------------------

mydata <- read.shape("C:/Stat_Ilri_2005_06/Data/African countries.shp", dbf.data = TRUE)
attributes(mydata)
mydata$att.data
plot(mydata)

myatt <- mydata$att.data
myatt$color <- ifelse(myatt$PAYS == "Ethiopie", "orangered", "lightyellow")
plot(mydata, fg = myatt$color)

plot(mydata, fg = myatt$color, axes = FALSE, xlab = "", ylab = "")
centr <- get.Pcent(mydata)
text(x = centr[17, 1], y = centr[17, 2], "Ethiopia", cex = 0.9) 

myatt$pop <- round(myatt$POP_94 / 1e6, digit = 2)
res <- plot(mydata, auxvar = myatt$pop, col = "red", nclass = 5) 
res
lev <- levels(cut(myatt$pop, breaks = round(res$breaks, digit = 2)))
legend(-3e6, 0, legend = lev, fill = res$ramp,
    title = "Millions of hab.", cex = 0.7)
