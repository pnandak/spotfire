# Lecture 8
 library(plm)
 source("http://thiloklein.de/R/myfunctions.R")


# --- Ex 3: Panel Data, Random Effects, Fixed Effects and First Differences -----------------------------
 wage <- read.csv("http://thiloklein.de/R/Lent/wagepan.csv",header=T)
 str(wage)

 waget <- pdata.frame(wage, c("nr","year"))
 head(waget)
 summary(waget)


# --- Ex 3: a) ---
# Obtain summary statistics for lwage, educ, black, hisp, exper, married and union.

 wage$nr <- as.factor(wage$nr)
 wage$year <- as.factor(wage$year)

 wage.sub <- subset(wage, select=c("lwage","educ","black","hisp","exper","married","union"))
 s.panel
 for(i in 1:7){
   s.panel(wage.sub[,i], wage$nr, names(wage.sub)[i])
 }



# --- Ex 3: b) ---
# Estimate a wage equation. Use simple OLS. Comment on the results. In particular, 
# is this a panel data estimator?

 lm3b <- lm(lwage ~ year + educ + black + hisp + exper + married + union, data=wage)
 shccm(lm3b)



# --- Ex 3: c) ---
# In order to get an indication whether the errors are correlated over time, we will 
# look at the correlations of the residuals over time.

 e <- lm3b$res
 e_1 <- unlist( by(e, wage$nr, function(x) c(NA, x[-length(x)])) )
 e_2 <- unlist( by(e, wage$nr, function(x) c(NA, NA, x[-c(7:8)])) )
 e_3 <- unlist( by(e, wage$nr, function(x) c(NA, NA, NA, x[-c(6:8)])) )
 e_4 <- unlist( by(e, wage$nr, function(x) c(rep(NA,4), x[-c(5:8)])) )

 C <- cbind(e, e_1, e_2, e_3, e_4)
 head(C)
 library(Hmisc)
 # cor(C, method="pearson", use="complete")
 # cor(C, method="pearson", use="pairwise.complete")
 rcorr(C)

 # or simply:
 acf(e)


# --- Digression: Correlation Matrix with significance levels ---
 corstars <- function(x){
   x <- as.matrix(x)
   R <- rcorr(x)$r
   p <- rcorr(x)$P
   mystars <- ifelse(p < .01, "***", ifelse(p < .05, "** ", ifelse(p < .1, "*  ", "   ")))
   R <- format(round(R, 3))
   Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
   rownames(Rnew) <- colnames(x)
   colnames(Rnew) <- colnames(x)
   Rnew[upper.tri(Rnew,diag=TRUE)] <- ""
   Rnew <- data.frame(Rnew)[-1,-length(colnames(x))]
   return(Rnew)
 } 
corstars(C)


# --- Digression: Graphical Representation of Correlation Matrix (CORRGRAM) ---
 # install.packages("corrgram")
 library(corrgram)
 corrgram(C, order=FALSE, lower.panel=panel.ellipse,
  upper.panel=panel.pts, text.panel=panel.txt,
  diag.panel=panel.minmax,
  main="Individual Level Part-Worths") 


# --- Digression: Graphical Representation of Correlation Matrix (PLOTCORR) ---
 install.packages("ellipse")
 library(ellipse)
 corC <- cor(C, use="complete")
 ord <- order(corC[1,])
 xc <- corC[ord, ord]
 colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
 plotcorr(corC, col=colors[5*xc + 6], type = "lower")



# --- Ex 3: d) ---
# Adjust the standard errors for the correlation of the residuals over time per individual. 
# Are these standard errors different from the simple OLS ones? 

 coeftest(lm3b)
 clx(lm3b, 1, wage$nr)

 # Alternatively:
 lm3d.ols <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="pooling", index=c("nr","year"))
 coeftest(lm3d.ols) # -> same as coeftest(lm3b)
 # Method "arellano" also allows for general heteroskedasticity / serial correlation structure:
 # "Panel data econometrics in R: the plm package", page 31.
 coeftest(lm3d.ols, vcov=pvcovHC(lm3d.ols, method="arellano")) # -> same as clx(lm3b, 1, wage$nr)


# --- Ex 3: e) ---
# Estimate the model as in b), allowing for random and fixed unobserved individual effects.

 ## OLS:
 lm3e.ols <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="pooling", effect="individual", index=c("nr","year"))
 # coeftest(lm3e.ols)
 coeftest(lm3e.ols, vcov=pvcovHC(lm3e.ols, method="arellano"))


 ## Random Effects:
 lm3e.re <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="random", effect="individual", index=c("nr","year"))
 # coeftest(lm3e.re)
 coeftest(lm3e.re, vcov=pvcovHC(lm3e.re, method="arellano"))


 ## Fixed Effects
 lm3e.fe <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="within", effect="individual", index=c("nr","year"))
 coeftest(lm3e.fe)
 # coeftest(lm3e.fe, vcov=pvcovHC(lm3e.fe, method="arellano"))



# --- Ex 3: f) ---
# What do you conclude from the Hausman-test result? 

 phtest(lm3e.fe, lm3e.re)



# --- Ex 3: g) ---
# Estimate the model in first differences. Check the autocorrelation structure of the 
# residuals in the first-differenced model.

 lm3e.fd <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="fd", effect="individual", index=c("nr","year"))
 coeftest(lm3e.fd) 

 e <- lm3e.fd$res
 acf(e)



# --- Ex 3: i) ---
# Estimate the between estimator. What does this estimator collect?

 lm3e.b <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="between", effect="individual", index=c("nr","year"))
 coeftest(lm3e.b) 

