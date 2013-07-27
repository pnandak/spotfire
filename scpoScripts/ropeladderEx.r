# Linear, Poisson, and Negative Binomial regression using UScrime data

# Uses ropeladders to show how the expected crime rate varies in
# response to changes in 7 covariates under each of four estimation 
# methods.  

# Plot 1 shows a four plot set up, one plot per method.  
# This approach highlights differences in effects across covariates

# Plot 2 squeezes all four ropeladders into a single plot.
# This approach gives equal attention to differences 
# across covariates and models

# Plot 3 creates a plot for each covariate.
# This approach highlights differences across models.

# Load data and libraries; set up specification
library(tile)
library(simcf)
library(MASS)
data(UScrime)
model <- (y ~ log(M) + So + log(Ed) + log(Po1) + log(Po2)
              + log(LF) + log(M.F) + log(Pop) + log(NW) +log(U1)
              + log(U2) + log(GDP) + log(Ineq) + log(Prob) +
              log(Time))

# Estimate Linear regression model
lm1.res <- lm(model, data = UScrime)
lm1.pe <- lm1.res$coefficients        # point estimates
lm1.vc <- vcov(lm1.res)               # var-cov matrix

# Estimate Robust and resistant regression model
mm1.res <- rlm(model, data = UScrime, method="MM")
mm1.pe <- mm1.res$coefficients        # point estimates
mm1.vc <- vcov(mm1.res)               # var-cov matrix

# Estimate Poisson model
po1.res <- glm(model, family=poisson, data = UScrime)
po1.pe <- po1.res$coefficients         # point estimates
po1.vc <- vcov(po1.res)                # var-cov matrix

# Estimate Negative Binomial model
nb1.res <- glm.nb(model, data = UScrime)
nb1.pe <- nb1.res$coefficients         # point estimates
nb1.vc <- vcov(nb1.res)                # var-cov matrix

# Initialize 7 different scenarios to mean values of covariates
xscen <- cfMake(model, data=UScrime, nscen=7)

# Configure scenario 1:  Raise Probability of Imprisonment by 1/2 sd
xscen <- cfName(xscen, "Pr(Prison) +0.5 sd", scen=1)
xscen <- cfChange(xscen, "Prob", 
                  x = mean(UScrime$Prob) + 0.5*sd(UScrime$Prob), 
                  scen=1)

# Configure scenario 2:  Raise Police Spending by 1/2 sd
xscen <- cfName(xscen, "Police Spending +0.5 sd", scen=2)
xscen <- cfChange(xscen, "Po1", 
                  x = mean(UScrime$Po1) + 0.5*sd(UScrime$Po1),
                  scen=2)

# Configure scenario 3:  Raise Unemployment (Age 35-39) by 1/2 sd
xscen <- cfName(xscen, "Unemployment (t-2) +0.5 sd", scen=3)
xscen <- cfChange(xscen, "U2", 
                  x = mean(UScrime$U2) + 0.5*sd(UScrime$U2),
                  scen=3)

# Configure scenario 4:  Raise Non-white population by 1/2 sd
xscen <- cfName(xscen, "Non-White Pop +0.5 sd", scen=4)
xscen <- cfChange(xscen, "NW", 
                  x = mean(UScrime$NW) + 0.5*sd(UScrime$NW), 
                  scen=4)

# Configure scenario 5:  Raise Male Pop by 1/2 sd
xscen <- cfName(xscen, "Male Pop +0.5 sd", scen=5)
xscen <- cfChange(xscen, "M",  
                  x = mean(UScrime$M) + 0.5*sd(UScrime$M), 
                  scen=5)

# Configure scenario 6:  Raise Education by 1/2 sd
xscen <- cfName(xscen, "Education +0.5 sd", scen=6)
xscen <- cfChange(xscen, "Ed", 
                  x = mean(UScrime$Ed) + 0.5*sd(UScrime$Ed),
                  scen=6)

# Configure scenario 7:  Raise Inequality by 1/2 sd
xscen <- cfName(xscen, "Inequality +0.5 sd", scen=7)
xscen <- cfChange(xscen, "Ineq", 
                  x = mean(UScrime$Ineq) + 0.5*sd(UScrime$Ineq), 
                  scen=7)



# Simulate conditional expectations for these counterfactuals
sims <- 10000

# Linear regression simulations
simbetas.lm <- mvrnorm(sims, lm1.pe, lm1.vc)   
lm1.qoi <- linearsimfd(xscen, simbetas.lm, ci=0.95)

# Robust regression simulations
simbetas.mm <- mvrnorm(sims, mm1.pe, mm1.vc)      
mm1.qoi <- linearsimfd(xscen, simbetas.mm, ci=0.95)

# Poisson simulations
simbetas.po <- mvrnorm(sims, po1.pe, po1.vc)     
po1.qoi <- loglinsimfd(xscen, simbetas.po, ci=0.95)

# Negative Binomial simulations
simbetas.nb <- mvrnorm(sims, nb1.pe, nb1.vc)      
nb1.qoi <- loglinsimfd(xscen, simbetas.nb, ci=0.95)

# Create ropeladder traces of first differences from each model
trace1 <- ropeladder(x=lm1.qoi$pe,
                     lower=lm1.qoi$lower,
                     upper=lm1.qoi$upper,
                     labels=row.names(xscen$x),
                     plot=1
                     )

trace2 <- ropeladder(x=mm1.qoi$pe,
                     lower=mm1.qoi$lower,
                     upper=mm1.qoi$upper,
                     plot=2
                     )

trace3 <- ropeladder(x=po1.qoi$pe,
                     lower=po1.qoi$lower,
                     upper=po1.qoi$upper,                   
                     plot=3
                     )

trace4 <- ropeladder(x=nb1.qoi$pe,
                     lower=nb1.qoi$lower,
                     upper=nb1.qoi$upper,
                     plot=4
                     )

rug1 <- rugTile(x = UScrime$y - mean(UScrime$y),
                plot = 1:4
                )
                
vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "solid",
                      plot = 1:4
                      )


# Create Plot 1 (focus on covariates) and save to pdf
tc <- tile(trace1, trace2, trace3, trace4,
           rug1, vertmark,
           #output = list(file = "ropeladderEx1"),          
           xaxistitle = list(labels="E(crime rate per 100,000)"),
           topaxis= list(at = mean(UScrime$y)*c(0.5, 1, 1.5, 2) 
                              - mean(UScrime$y),
                         labels = c("0.5x","1x","1.5x","2x"),
                         add = rep(TRUE,4)
                         ),
           topaxistitle = list(labels="E(crime rate) / average"),
           plottitle = list(labels1 = "Linear",
                            labels2 = "Robust",
                            labels3 = "Poisson",
                            labels4 = "Neg Bin"),
           gridlines=list(type="t")
           )


# Plot 2 squeezes all four ropeladders into a single plot.
# This approach gives equal attention to differences 
# across covariates and models

# Revise traces to place on same plot
trace1$plot <- trace2$plot <- trace3$plot <- trace4$plot <- 1
vertmark$plot <- 1

# Revise traces to make symbols different
trace1$pch <- 19
trace2$pch <- 15
trace3$pch <- 17
trace4$pch <- 23

# Add sublabels to each trace
trace1$sublabels <- "linear"
trace2$sublabels <- "robust"
trace3$sublabels <- "poisson"
trace4$sublabels <- "negbin"

# Widen space between entries to make labels visible
trace1$entryheight <- 0.25

# Shift sublabels to left side of plot to avoid overlap
trace1$sublabelsX <- 0.07
trace2$sublabelsX <- 0.07
trace3$sublabelsX <- 0.07
trace4$sublabelsX <- 0.07

# Add boxes around the results for each covariate 
# when traces are plotted to the same graph 
# (could add to any of the traces)
trace1$shadowrow <- TRUE


# Create Plot 2 and save to pdf 
tc <- tile(trace1, trace2, trace3, trace4,
           vertmark,
           limits = c(-230, 460),
           width=list(null=4),
           #output = list(file="ropeladderEx2"),          
           xaxistitle = list(labels="E(crime rate per 100,000)"),
           topaxis= list(at = mean(UScrime$y)*c(0.75, 1, 1.25, 1.5) 
                              - mean(UScrime$y),
                         labels = c("0.75x","1x","1.25x","1.5x"),
                         add = TRUE),
           topaxistitle = list(labels="E(crime rate) / average"),
           gridlines=list(type="t")
           )



# Plot 3 creates a plot for each covariate.
# This approach highlights differences across models.

# Collect in matrix form all first differences and confidence 
# intervals across models (columns) and covariates (rows)
allPE <- cbind(lm1.qoi$pe, mm1.qoi$pe, po1.qoi$pe, nb1.qoi$pe)
allLOWER <- cbind(lm1.qoi$lower, 
                  mm1.qoi$lower, 
                  po1.qoi$lower, 
                  nb1.qoi$lower)
allUPPER <- cbind(lm1.qoi$upper, 
                  mm1.qoi$upper, 
                  po1.qoi$upper,  
                  nb1.qoi$upper)

# Create a trace for each covariate of the 
# different models' estimates
# (Save these traces in a vector of traces; 
#  note double bracket indexing)

collectedtraces <- vector("list", nrow(allPE))

for (i in 1: nrow(allPE)) {
  collectedtraces[[i]] <- ropeladder(x = allPE[i,],
                                     lower = allLOWER[i,],
                                     upper = allUPPER[i,],
                                     shadowbox = TRUE,
                                     plot = i
                                     )
  }

# Add ropeladder labels to first and fifth plots
# (The first ropeladder in each row of plots; 
#  note double bracket indexing)
collectedtraces[[1]]$labels <- 
  collectedtraces[[5]]$labels <- 
    c("Linear", "Robust & Resistant", 
      "Poisson", "Negative Binomial")

# Revise vertical mark to plot on all seven plots
vertmark$plot <- 1:7

tc <- tile(collectedtraces, vertmark,
           RxC = c(2,4),
           limits = c(-230, 460),
           width = list(spacer=3),
           #output = list(file="ropeladderEx3"),       
           xaxis = list(at = c(-200, 0, 200, 400)),   
           xaxistitle = list(labels="E(crime rate per 100,000)"),
           topaxis= list(at = mean(UScrime$y)*c(0.75, 1, 1.25, 1.5) 
                              - mean(UScrime$y),
                         labels = c(".75x","1x","1.25x","1.5x"),
                         add = rep(TRUE,4)),
           topaxistitle = list(labels="E(crime rate) / average"),
           plottitle = list(labels1 = "Pr(Prison)",
                            labels2 = "Police Spending",
                            labels3 = "Unemployment",
                            labels4 = "Non-White Pop",
                            labels5 = "Male Pop",
                            labels6 = "Education",
                            labels7 = "Inequality"),
           gridlines=list(type="top"))
