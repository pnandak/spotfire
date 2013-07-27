###
### Gov 2001 replication
### Egorov et al
###

setwd("/Users/danieltrmasterson/Documents/HKS/Courses Spring '11/GOV 2001/Rep Paper")






data <- read.dta("Dataset_Dec2008.dta")

names(data)
unique(data[,33]) ### 160 countries



###############  Generating variables
data1994 <- data[data$year > "1993",]
data$polity2_8092 <- data$polity2
data$polity2_8092 <- mean(data$polity2)

summary(data$polity2_8092)

data$polity2_8092[ data$year < 1993 ] <- summary(data$polity2)[4]
data$logoilres92_democ9307 <- data$logoilres92*data$democ9307
data$logoilres92_polity29307 <- data$logoilres92*data$polity29307
data$logoilres92_polity2_8092 <- data$logoilres92*data$polity2_8092

summary(data$logoilres92_democ9307)

## With Oil Price
data$logoilprice=log(data$oilprice)
data$logoilresvalue <- data$logoilres + data$logoilprice

data$logoilprodvalue <- data$logoilprod + data$logoilprice
data$logoilprodvalue_democ <- data$logoilprodvalue*data$democ

data$logoilresvalue92 <- data$logoilresvalue
data$logoilresvalue92 <- c(NA,data$logoilresvalue92[+1])
data$logoilresvalue92[data$year == 1980 ] <- c(data$logoilresvalue92[+15])

data$loglandarea92 <- data$loglandarea
data$loglandarea92 <- c(NA,data$loglandarea[+1])
data$loglandarea92[data$year == 1980 ] <- c(data$loglandarea[+15])

data$logoilresvalue_democ9307 <- data$logoilresvalue92*data$democ9307

## polity2 interactions

data$logoilresvalue_polity <- data$logoilresvalue*(data$polity2)
data$logoilres_polity=data$logoilres*(data$polity2) 
data$logoilprod_polity=data$logoilprod*(data$polity2)
data$logoilprodvalue_polity=data$logoilprodvalue*(data$polity2)

data$logoilres_pr=data$logoilres*data$pr 
data$logoilres_cl=data$logoilres*data$cl

data$logoilres_pr <- data$logoilres * data$pr

###
###  Subsetting year group and polity groups
###

data1992 <- data[ which(data$year >= 1992), ]
data1992.ND <- data1992[ which(data1992$polity292 < 6), ]
data1992.autocrat <- data1992[ which(data1992$polity292 < 1), ]
data1992.impdem <- data1992[ which(data1992$polity292 < 9 & data1992$polity292 > 0), ]
data1992.democrat <- data1992[ which(data1992$polity292 > 8), ]
data1992.ND2 <- data1992[ which(data1992$polity292 < 9), ]
data1992.democ92 <- data1992[ which(data1992$democ92 < 9), ]

data1993 <- data[ which(data$year == 1993), ]
data1993.nondemoc <- data[ which(data$year == 1993 & data$polity292<=8), ]
data1993.democ <- data[ which(data$year == 1993 & data$polity292 > 8), ]




##############
############## Dealing with the Arab Middle East
##############

data1992 <- data[ which(data$year >= 1992), ]
data2006 <- data[ which(data$year == 2006), ]
compare <- cbind(data2006$country, data2006$mflead, data2006$polity2)


head(data1992)
names(data1992)
unique(data1992$country)

dim(data1992)
data1992$arab <- 0
data1992$WBincome <- 0


arab <- data1992$country == "Algeria"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Bahrain"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Egypt, Arab Rep."
data1992[arab,]$arab <- 1
arab <- data1992$country == "Iraq"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Jordan"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Kuwait"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Lebanon"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Libya"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Morocco"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Oman"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Qatar"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Saudi Arabia"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Syrian Arab Republic"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Tunisia"
data1992[arab,]$arab <- 1
arab <- data1992$country == "United Arab Emirates"
data1992[arab,]$arab <- 1
arab <- data1992$country == "Yemen, Rep."
data1992[arab,]$arab <- 1


sum(data1992$arab);sum(data1992$arab)/16


arabworld <- data1992[data1992$arab == 1,]
dim(arabworld)
unique(arabworld$country)

arabworld9405 <- arabworld[ which(arabworld$year >= 1994), ]
arabworld9405 <- arabworld9405[ which(arabworld9405$year <= 2005), ]

length(arabworld9405$year)
length(arabworld9405$year)/16
unique(arabworld9405$country)


### Coding West Bank income data drawn from World Bank sources

WB.data <- c(1228.834333, 1245.626114, 1225.61809, 1320.21038, 1428.822831,
	1501.020962, 1369.193121, 1126.818867, 978.5614627, 1002.89514,
	1029.052021, 1056.296491)
WB.data.length <- rep(WB.data, 16)

arabworld9405$WBincome <- WB.data.length


###### East Jerusalem House Demolitions drawn from B'Tselem data

Jslamhousedemo <- c(12, 48, 29, 25, 17, 16, 30, 17, 9, 32, 36, 63, 53,70,44,62)
Jslamhousedemo.length <- rep(Jslamhousedemo, 16)

arabworld$Jslamhousedemo <- Jslamhousedemo.length

## Plot of correlation between WB income 
## and East Jeruslame house demolitions by year

trellis.device(device="pdf",file="WBincome-Housedemos.pdf",color=FALSE,width=8,height=8)

plot(x = arabworld9405$WBincome, 
	y = arabworld$Jslamhousedemo[arabworld$year >= 1994 & arabworld$year <= 2005 ],
	pch = 1, xlab = "West Bank Income ($US per capita)",
	ylab = "Demolitions of Palestinian Homes in East Jerusalem",
	main = "Proxies for the Intensity of the Israeli-Palestinian Conflict", sub="Sources: World Bank (x-axis) and B'Tselem (y-axis)")
WBline <- lm(arabworld$Jslamhousedemo[arabworld$year >= 1994 & arabworld$year <= 2005 ] ~ arabworld9405$WBincome)
abline(WBline$coef[1],WBline$coef[2], lty=3)
text(x = arabworld9405$WBincome, 
	y = arabworld$Jslamhousedemo[arabworld$year >= 1994 & arabworld$year <= 2005 ], labels=arabworld9405$year, pos=3, cex= 0.72, offset = -0.9, font=1)
legend(x=1305, y=70, lty=1, legend="OLS Correlation of Y on X", cex=0.8)	

dev.off()

# Correlation Coefficient
cor(x = arabworld9405$WBincome, 
	y = arabworld$Jslamhousedemo[arabworld$year >= 1994 & arabworld$year <= 2005 ])



###################
###################   WB income effect on Arab media freedom?
###################


arabworld9405$WB2 <- arabworld9405$WBincome ^ 2
arabworld9405$logWB <- log(arabworld9405$WBincome)


### Model 1 from paper + add logWBincome + country fixed FX

model1 <- lm(mflead ~ logWB + logoilres + logoilres_polity + polity2 
		+ loggdppcppp + logpoptotal + loggovgdp + factor(id), 
			data=arabworld9405)
summary(model1)


### Model 2  from paper + add WBincome + country fixed FX

model2 <- lm(mflead ~ WBincome + logoilresvalue + logoilresvalue_polity 
		+ polity2 + loggdppcppp + logpoptotal + loggovgdp + factor(id), 
		data=arabworld9405)
summary(model2)


### Model 3 from paper + add logWBincome without country fixed FX

model3 <- lm(mflead ~ logWB + logoilres + logoilres_polity + polity2 
		+ loggdppcppp + logpoptotal + loggovgdp, 
			data=arabworld9405)
summary(model3)


### Model 4  from paper + add WBincome without country fixed FX

model4 <- lm(mflead ~ WBincome + logoilresvalue + logoilresvalue_polity 
		+ polity2 + loggdppcppp + logpoptotal + loggovgdp, 
		data=arabworld9405)
summary(model4)


### Model 5 Home demolitions

model5 <- lm(mflead ~ Jslamhousedemo + logoilres + logoilres_polity + polity2 
		+ loggdppcppp + logpoptotal + loggovgdp + factor(id), 
			data=arabworld)
summary(model5)



################
########

# Drawing from Figure 1
# Ho, Imai, King, and Stuart (2007)
# MatchIt version 2.2-11


#############
### FIGURES
#############

names(data1992)

##
data1992plot <- as.data.frame(na.omit(cbind(data1992$mflead, data1992$logoilresvalue, data1992$logoilresvalue_polity, data1992$polity2, data1992$loggdppcppp, data1992$logpoptotal, data1992$loggovgdp, data1992$arab, data1992$popurb,  data1992$landarea, data1992$popdensity)))

head(data1992plot)
colnames(data1992plot) <- c("mflead","logoilresvalue","logoilresvalue_polity","polity2","loggdppcppp","logpoptotal","loggovgdp","arab","popurb","landarea","popdensity")
head(data1992plot)

class(data1992plot)

#2560 observations before na.omit
dim(data1992plot)

length(na.omit(data1992$popurb))
length(na.omit(data1992$landarea))
length(na.omit(data1992$popdensity))


## Testing for Arab effect on Media Freedom

lm.all1 <- lm(mflead ~ logoilresvalue + logoilresvalue_polity + polity2 + loggdppcppp + logpoptotal + loggovgdp + arab + popurb + landarea + popdensity, data=data1992plot)

print("Full group: linear model")

temp <- matchit(arab ~ logoilresvalue + logoilresvalue_polity + polity2 + loggdppcppp + logpoptotal + loggovgdp + popurb + landarea + popdensity, data=data1992plot)
		
matched <- match.data(temp)

lm.m1 <- lm(mflead ~ logoilresvalue + logoilresvalue_polity + polity2 + loggdppcppp + logpoptotal + loggovgdp + arab + popurb + landarea + popdensity, data=matched)
print("Matched group: linear model")

summary(lm.all1)
summary(lm.m1)


## Robust Standard Errors do not differ very much
summary(lm.all1) ## MF - all
sqrt(diag(hccm(lm.all1, type="hc1")))


summary(lm.m1)	## MF - matched
sqrt(diag(hccm(lm.m1, type="hc1")))




#################
###### Check Balance for Model 1 Matching

## Multivariate balance

	data0 <- matched[matched$arab==0,]
	data1 <- matched[matched$arab==1,]

	dim(data0)
	dim(data1)

	balance1 <- mean(data1)-mean(data0) ; balance1
	sdbalance1 <- sd(data1)-mean(data0) ; sdbalance1

dim(matched)
dim(na.omit(matched))


test <- imbalance(group=matched$arab, data=matched,
	drop=c("arab","mflead","distance",
	"logoilresvalue_polity","loggdppcppp","logoilresvalue","loggovgdp","polity2"))
test

## Univariate balance

imbalance(group=data1992plot$arab, data=data1992plot,
	drop=c("arab","mflead","distance"))

imbalance(group=matched$arab, data=matched,
	drop=c("arab","mflead","distance"))



##############


plot.pts <- seq(from=min(data1992plot$polity2),to=max(data1992plot$polity2),by=0.01)
plot.pts2 <- seq(from=-10, to=10, by=0.01)

write.table(data1992plot, file="Figure1Data.txt", row.names=FALSE)


############
##  Plotting Model 1
 
trellis.device(device="pdf",file="olspanel-masterson.pdf",color=FALSE,width=16,height=8)


par(mar=c(2, 2, 2, 2) + 0.1, cex.lab=0.7, cex.axis=0.5,
    mgp=c(1,0.5,0), cex.main=0.8, cex=1, mfrow=c(1,2), bg="white")
plot(jitter(data1992plot$polity2[data1992plot$arab==1],2),jitter(data1992plot$mflead[data1992plot$arab==1],2),pch="A",
     xlim=range(data1992plot$polity2), ylim = c(-10,90),
     xlab="Polity IV", ylab = "Media Freedom", cex=0.8, main="Before Matching")
points(jitter(data1992plot$polity2[data1992plot$arab==0],2),data1992plot$mflead[data1992plot$arab==0],pch=".", cex=1.3)
abline(h=0,lty=3,col="grey")
legend("topleft", inset=0.05, legend=c("A    Arab Country-Years",".     Non-Arab Country-Years"), cex=0.65)
plot(matched$polity2[matched$arab==1], matched$mflead[matched$arab==1],
     pch="A", xlab="Polity IV", ylab="Media Freedom", xlim=range(data1992plot$polity2),
     ylim = c(-10,90), cex=0.8, main="After Matching")
points(jitter(matched$polity2[matched$arab==0],2), jitter(matched$mflead[matched$arab==0],2), pch="+", cex=0.8)
points(jitter(data1992plot$polity2[temp$weights==0 & data1992plot$arab==0],2),
       jitter(data1992plot$mflead[temp$weights==0 & data1992plot$arab==0],2),
       pch=".", col="darkgrey", cex=1.3)
abline(h=0,lty=3,col="grey")
legend("topleft", inset=0.05, legend=c("A    Arab Country-Years",
         "+    Matched Non-Arab Country-Years",".     Non-Matched Non-Arab Country-Years"), cex=0.65)
                  
dev.off()
?legend

#################
## Indentifying Matches

matched[,1:2]
dim(matched)
matched[,1:8]
matched

data1992[93:101,1:4]
# Armenia 2004-2007
# Australia 1992-1996
data1992[569,1:4]
data1992[571,1:4]
data1992[573:575,1:4]
data1992[577:578,1:4]
#Czechoslovakia in 2000, 2002, 2004, 5, and 6

data1992[596,1:4]
#Djibouti in 1995
#DEU in 1992 and '93 
data1992[665,1:4]
#ECU in 2000 and 2004-2007
data1992[669:676,1:4]
#Egypt from 1992-1995

data1992[676,]
#Egypt


