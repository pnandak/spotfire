## Using tile to summarize a probit model of democracy
## Chris Adolph
## 2/4/2010
##
## Note that the model below is grossly oversimplified---
## ignoring, for example, all dynamic and panel features
## of the data, and employing an essentially arbitrary
## specification.  The point is to illustrate the use
## of the simcf and tile packages for post-estimation
## model interpretation and presentation, which would
## carry over with little change to better models.

# Clear memory
rm(list=ls())

# Load libraries
library(MASS)
library(car)
library(tile)
library(simcf)

# Load democracy data
alldata <- read.csv("AclpLim.csv", header=TRUE, na.strings=".")

# Create a dataframe with only the model data; listwise delete
data <- cbind(alldata$REG, alldata$GDPW, alldata$EDT, alldata$NEWC,
              alldata$BRITCOL, alldata$STRA, alldata$ELF60, alldata$OIL,
              alldata$MOSLEM)
data <- as.data.frame(data)
names(data) <-  c("REG", "GDPW", "EDT", "NEWC", "BRITCOL",
                  "STRA", "ELF60", "OIL", "MOSLEM")
data <- na.omit(data)

# Recode a few variables
data$GDPW <- data$GDPW/1000
data$REG <- recode(data$REG,"1=0;0=1")

# Put together response and covariate matrices
y <- data$REG
x <- data[,2:ncol(data)]
model <- REG ~ GDPW + EDT + NEWC + BRITCOL + STRA + ELF60 + OIL + MOSLEM

# Fit probit model using optim
llk.probit <- function(param,y,x) {
  os <- rep(1,length(x[,1]))
  x <- cbind(os,x)
  b <- param[ 1 : ncol(x) ]
  xb <- x%*%b
  -sum( y*log(pnorm(xb)) + (1-y)*log(1-pnorm(xb)) );
               # optim is a minimizer, so min -ln L(param|y)
}
ls.result <- lm(model,data)  # use ls estimates as starting values
stval <- ls.result$coefficients  # initial guesses
probit.result <- optim(stval,llk.probit,method="BFGS",hessian=T,y=y,x=as.matrix(x))
                   # call minimizer procedure
pe <- probit.result$par   # point estimates
vc <- solve(probit.result$hessian)  # var-cov matrix
se <- sqrt(diag(vc))    # standard errors
ll <- -probit.result$value  # likelihood at maximum

# Simulate results using simcf package
sims <- 1000
simbetas <- mvrnorm(sims,pe,vc) # draw parameters

# Simulate Pr(Dem) for British Colonies of varied GDP
hypGDP <- seq(0.5,35,0.5)
coloniesScen <- cfMake(model, data, nscen=length(hypGDP))
for (i in 1:length(hypGDP)) {
  coloniesScen <- cfChange(coloniesScen, "BRITCOL", 1, scen=i)
  coloniesScen <- cfChange(coloniesScen, "GDPW", hypGDP[i], scen=i)
}
coloniesSimDemoc <- probitsimev(coloniesScen, simbetas, ci=0.95)

# Simulate Pr(Dem) for non-British Colonies of varied GDP
hypGDP <- seq(0.5,35,0.5)
nonColoniesScen <- cfMake(model, data, nscen=length(hypGDP))
for (i in 1:length(hypGDP)) {
  nonColoniesScen <- cfChange(nonColoniesScen, "BRITCOL", 0, scen=i)
  nonColoniesScen <- cfChange(nonColoniesScen, "GDPW", hypGDP[i], scen=i)
}
nonColoniesSimDemoc <- probitsimev(nonColoniesScen, simbetas, ci=0.95)

# Prepare lineplot traces for tile package plotting
# Summarize model for various levels of GDP:  British Colonies only
trace1 <- lineplot(x=hypGDP,
                   y=coloniesSimDemoc$pe,
                   lower=coloniesSimDemoc$lower,
                   upper=coloniesSimDemoc$upper,
                   ci=list(mark=c("shaded","dashed")),
                   col="blue",
                   extrapolate=list(formula=model,
                                    data=data,
                                    cfact=coloniesScen$x,
                                    omit.extrapolated=FALSE),
                   plot=c(1,3)
                   )

# Summarize model for various levels of GDP:  non-British Colonies only
trace2 <- lineplot(x=hypGDP,
                   y=nonColoniesSimDemoc$pe,
                   lower=nonColoniesSimDemoc$lower,
                   upper=nonColoniesSimDemoc$upper,
                   ci=list(mark=c("shaded","dashed")),
                   col="brown",
                   extrapolate=list(formula=model,
                                    data=data,
                                    cfact=nonColoniesScen$x,
                                    omit.extrapolated=FALSE),
                   plot=c(2,4)
                   )

# Make rugs for Colonies
XrugColonies <- rugTile(x=data$GDPW[data$BRITCOL==1],
                        type="jitter",
                        col="blue",
                        plot=c(1,3)
                        )

# Make rugs for non-Colonies
XrugNonColonies <- rugTile(x=data$GDPW[data$BRITCOL==0],
                           type="jitter",
                           col="brown",
                           plot=c(2,4)
                           )

# Plot everything with tile
tile(# Input all traces we want to plot
     trace1,
     trace2,
     XrugColonies,
     XrugNonColonies,
     
     # Choose various plotting options
     RxC=c(2,2),
     gridlines=list(type="xy"),
     limits=c(0.5,35,0,1),
     
     xaxis=list(at=c(1,5,10,20,35), log=c(0,0,10,10)),
     xaxistitle=list(labels=c("GDP (pc, constant international prices)")),
     
     yaxis=list(at=c(0,0.2,0.4,0.6,0.8,1)),
     yaxistitle=list(type="row",labels=c("Pr(Democracy)",
                                         "Pr(Democracy)")),
     
     maintitle=list(fontface="bold",
       labels=c("Probability of Democracy Given Development & Colonial Origin")
       ),
     
     rowtitle=list(labels=c("Linear","Log")),
     columntitle=list(labels=c("Former British Colonies",
                               "Other Countries")),
     
     frame=TRUE,
     output = list(width=8,outfile="democDemo",type="pdf")
     )

