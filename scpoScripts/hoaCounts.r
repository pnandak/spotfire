# Count model examples using HOAdata.org data:
# Poisson, Negative Binomial, Zero-Inflated Negative Binomial
# Chris Adolph

# Clear memory
rm(list=ls())

# Load libraries
library(MASS)
library(tile)
library(simcf)
library(pscl)
#source("avp.r")

# Load data
# WARNING:  To do Goodness of Fit tests, including LR, AIC, BIC, etc.,
# you must use the
# complete data file h100.csv throughout
file <- "h100.csv";
#file <- "hoa100s_nozeros.csv";
data <- read.csv(file,header=TRUE);

outfile = "_with_zeros.pdf"
#outfile = "_without_zeros"

# Process data for model
y <- data$file9501
x <- cbind(data$mdvalue,data$yearhom2)
model <- file9501 ~ mdvalue + yearhom2
modeldata <- as.data.frame(cbind(y,x))
names(modeldata) <- c("file9501","mdvalue","yearhom2")

# Fit Poisson model using optim
llk.poisson <- function(param,y,x) {
  os <- rep(1,length(x[,1]))
  x <- cbind(os,x)
  b <- param[ 1 : ncol(x) ]
  xb <- x%*%b
  -sum( xb*y - exp(xb) );
     # optim is a minimizer, so min -ln L(param|y)
}
ls.result <- lm(y~x)  # use ls estimates as starting values
stval <- ls.result$coefficients  # initial guesses
pois.result <- optim(stval,llk.poisson,method="BFGS",hessian=T,y=y,x=x)
                   # call minimizer procedure
pe.pois <- pois.result$par   # point estimates
vc.pois <- solve(pois.result$hessian)  # var-cov matrix
se.pois <- sqrt(diag(vc.pois))    # standard errors
#ll.pois <- -pois.result$value  # likelihood at maximum

glm.pois <- glm(file9501~mdvalue+yearhom2, modeldata, family=poisson)
ll.pois <- logLik(glm.pois)

# Set up counterfactuals (range over homevalues for average neighborhood size
valueseq <- log(seq(50000,250000,1000))
xhyp <- cfMake(model,modeldata,nscen = length(valueseq))
for (i in 1:length(valueseq))
  xhyp <- cfChange(xhyp, "mdvalue", x=valueseq[i], scen=i)

# Simulate results
sims <- 10000
simbetas <- mvrnorm(sims,pe.pois,vc.pois) # draw parameters
yhyp <- loglinsimev(xhyp, simbetas)

# Transform expected counts to expected rates
yhyp$pe <- (yhyp$pe*1000)/mean(exp(modeldata$yearhom2))
yhyp$lower <- (yhyp$lower*1000)/mean(exp(modeldata$yearhom2))
yhyp$upper <- (yhyp$upper*1000)/mean(exp(modeldata$yearhom2))

# Set up lineplot trace of result for plotting using tile
trace1 <- lineplot(x = exp(valueseq)/1000,
                   y = yhyp$pe,
                   lower = yhyp$lower,
                   upper = yhyp$upper,
                   ci = list(mark="shaded"),
                   extrapolate = list(data=modeldata,cfact=xhyp$x,
                                      omit.extrapolated=FALSE),
                   col = "blue",
                   plot = 1
                   )

# Plot traces using tile
tc <- tile(trace1,
           RxC = c(1,1),
           limits = c(50,250,0,10.5),
           output = list(wide=4.5,outfile=paste("Poisson",outfile,sep=""),type="pdf"),
           xaxistitle = list(labels="Median home price in $1000s"),
           yaxistitle = list(labels="Annual expected filings per 1000 homes"),
           plottitle = list(labels="HOA foreclosure filings and home values"),
           gridlines = list(type="xy"),
           frame=TRUE
           )

################################################
# Re-analyze using negative binomial regression
nb.result <- glm.nb(file9501~mdvalue+yearhom2,modeldata)
pe.nb <- nb.result$coefficients
vc.nb <- vcov(nb.result)
se.nb <- sqrt(diag(vc.nb))
ll.nb <- logLik(nb.result)
theta.nb <- nb.result$theta
setheta.nb <- nb.result$SE.theta

# Simulate results (we re-use xhyp from above)
sims <- 10000
simbetas <- mvrnorm(sims,pe.nb,vc.nb) # draw parameters
yhyp <- loglinsimev(xhyp, simbetas)

# Transform expected counts to expected rates
yhyp$pe <- (yhyp$pe*1000)/mean(exp(modeldata$yearhom2))
yhyp$lower <- (yhyp$lower*1000)/mean(exp(modeldata$yearhom2))
yhyp$upper <- (yhyp$upper*1000)/mean(exp(modeldata$yearhom2))

# Set up lineplot trace of result for plotting using tile
trace1 <- lineplot(x = exp(valueseq)/1000,
                   y = yhyp$pe,
                   lower = yhyp$lower,
                   upper = yhyp$upper,
                   ci = list(mark="shaded"),
                   extrapolate = list(data=modeldata,cfact=xhyp$x,
                                      omit.extrapolated=FALSE),
                   col = "blue",
                   plot = 1
                   )

# Plot traces using tile
tc <- tile(trace1,
           RxC = c(1,1),
           limits = c(50,250,0,10.5),
           output = list(wide=4.5,outfile=paste("NB",outfile,sep=""),type="pdf"),
           xaxistitle = list(labels="Median home price in $1000s"),
           yaxistitle = list(labels="Annual expected filings per 1000 homes"),
           plottitle = list(labels="HOA foreclosure filings and home values"),
           gridlines = list(type="xy"),
           frame=TRUE
           )


###########################
# Re-analyze using Zero-Inflated Negative Binomial

# Reload data
file <- "h100.csv";
data <- read.csv(file,header=TRUE);
outfile = "conditional_count"

# Process data for model
y <- data$file9501
x <- cbind(data$mdvalue,data$yearhom2)
model <- file9501 ~ mdvalue + yearhom2
modeldata <- as.data.frame(cbind(y,x))
names(modeldata) <- c("file9501","mdvalue","yearhom2")

# Estimate ZINB using zeroinfl in the pscl library
zinbmodel <- file9501 ~ mdvalue + yearhom2 | mdvalue + yearhom2
zinb.result <- zeroinfl(zinbmodel, modeldata, dist="negbin")  # to do ZIP, leave out dist argument

# Extract results, taking care to keep track of both equations
pe.zinb.count <- zinb.result$coefficients$count
pe.zinb.zeros <- zinb.result$coefficients$zero
pe.zinb <- c(pe.zinb.count,pe.zinb.zeros)
vc.zinb <- vcov(zinb.result)
se.zinb <- sqrt(diag(vc.zinb))
se.zinb.count <- se.zinb[1:length(pe.zinb.count)]
se.zinb.zeros <- se.zinb[ (length(pe.zinb.count)+1) : length(se.zinb) ]
ll.zinb <- logLik(zinb.result)
theta.zinb <- zinb.result$theta
setheta.zinb <- zinb.result$SE.theta

# Simulate results  (we re-use xhyp from above)
# Option 1:  E(count | not a structural zero)
# { other options include unconditional E(count) and Pr(Structural Zero) }
sims <- 10000
simparam <- mvrnorm(sims,pe.zinb,vc.zinb) # draw parameters
simbetas <- simparam[,1:length(pe.zinb.count)]
simgammas <- simparam[,(length(pe.zinb.count)+1) : length(se.zinb) ]
yhyp <- loglinsimev(xhyp, simbetas)

# Transform expected counts to expected rates
yhyp$pe <- (yhyp$pe*1000)/mean(exp(modeldata$yearhom2))
yhyp$lower <- (yhyp$lower*1000)/mean(exp(modeldata$yearhom2))
yhyp$upper <- (yhyp$upper*1000)/mean(exp(modeldata$yearhom2))

# Set up lineplot trace of result for plotting using tile
trace1 <- lineplot(x = exp(valueseq)/1000,
                   y = yhyp$pe,
                   lower = yhyp$lower,
                   upper = yhyp$upper,
                   ci = list(mark="shaded"),
                   extrapolate = list(data=modeldata,cfact=xhyp$x,
                                      omit.extrapolated=FALSE),
                   col = "blue",
                   plot = 1
                   )

# Plot traces using tile
tc <- tile(trace1,
           RxC = c(1,1),
           limits = c(50,250,0,10.5),
           output = list(wide=4.5,outfile=paste("ZINB_",outfile,sep=""),type="pdf"),
           xaxistitle = list(labels="Median home price in $1000s"),
           yaxistitle = list(labels="Annual expected filings per 1000 homes"),
           plottitle = list(labels="HOA foreclosure filings and home values"),
           gridlines = list(type="xy"),
           frame=TRUE
           )

# Goodness of fit tests

# NB vs Pois: LR
NBvsPoisson.lr.test <- 2*(ll.nb - ll.pois)
NBvsPoisson.lr.test.p <- pchisq(NBvsPoisson.lr.test,df=1,lower.tail=FALSE)

# NB vs Pois: BIC
NBvsPoisson.bic <- - 2*(ll.nb - ll.pois) + 1*log(nrow(x))

# ZINB vs NB: Vuong
print(vuong(nb.result, zinb.result))

# ZINB vs NB:  BIC
ZINBvsNB.bic <- - 2*(ll.zinb - ll.nb) + 3*log(nrow(x))
