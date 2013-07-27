####################################################################
## R commands to accompany class, PS 150C/350C
## $Id: class0411.R 96 2007-04-13 01:20:52Z jackman $	
##
## logit model, with calculation and graphical display of
## predicted probabilities
##
## Jackman-Sniderman data from 2006 JOP article, Table 1
## dependent variable is supporting the proposition that
## more must be done to help the unemployed
## (y_i = 1 if support, 0 otherwise).
## predictors: left-right orientation (trichotomy);
## political sophistication (0 to 1 scale)
##  
## simon jackman, dept of political science
## stanford university
####################################################################

## read data
data <- read.csv(file="JackmanSniderman2006_Table1.txt")

## cleanup
data$yNum <- as.numeric(data$y=="Support")

## look at data, note summary statistics
summary(data)

## logit models, binary response (Bernoulli, not covariate class/binomial)
logit1 <- glm(yNum ~ ideoPos,
              data=data,
              family=binomial)

## add interaction with sophistication
logit2 <- glm(yNum ~ soph*ideoPos,
              data=data,
              family=binomial)

## likelihood ratio test of two models
anova(logit1,logit2,test="Chi")

## fake data for predicted probabilities and pictures
xseq <- seq(0,1,length=100)  ## grid of data for sophistication
fakeData <- expand.grid(list(soph=xseq,
                             ideoPos=levels(data$ideoPos)))

## predicted log-odds and standard errors,
## running fakeData back thru logit2 model
phat <- predict(logit2,
                newdata=fakeData,
                se.fit=TRUE)

## the logit function, transforms from reals to unit probability interval
logitFunc <- binomial()$linkinv

fakeData$fit <- logitFunc(phat$fit)
fakeData$lo <- logitFunc(phat$fit - 1.96*phat$se.fit)
fakeData$up <- logitFunc(phat$fit + 1.96*phat$se.fit)


## now make nice pictures
pdf(file="JackmanSnidermanLogitPredictions.pdf",
    w=8.5,h=11)
par(mfrow=c(3,1),   ## 3 rows, 1 column of sub graphs
    pty="s",
    las=1,
    mar=c(4,2,4,2))

for(i in 1:3){
  ## create a dummy plotting region
  ## we will fill manually
  plot(x=c(0,1),y=c(0,1),
       type="n",                   ## no actual plotting
       xlab="Political Sophistication",
       ylab="Pr[Support Increase]",
       axes=FALSE,
       xlim=c(0,1),xaxs="i",
       ylim=c(0,1),yaxs="i")

  axis(1,pos=-.025)
  axis(2)
  
  ## subset to the data for this level of ideoPos
  ok <- fakeData$ideoPos == levels(fakeData$ideoPos)[i]

  ## 95% confidence envelope as a shaded region
  polygon(x=c(xseq,rev(xseq)),
          y=c(fakeData$lo[ok],rev(fakeData$up[ok])),
          col=gray(.75),
          border=FALSE)

  ## overlay predicted probabilities for all groups
  ## 
  for(j in 1:3){
    ok <- fakeData$ideoPos == levels(fakeData$ideoPos)[j]
    if(j==i)
      lines(xseq,fakeData$fit[ok],lwd=2)
    else
      lines(xseq,fakeData$fit[ok],lty=2,col=gray(.45))
  }

  ## overlay quantiles of sophistication conditional on this
  ## level of sophistication
  q <- quantile(data$soph[data$ideoPos==levels(fakeData$ideoPos)[i]],
                c(.05,.25,.50,.75,.95),
                na.rm=TRUE) 
  rug(q)

  text(x=q,y=.075,cex=.75,
       c("5%","25%","50%","75%","95%"))

  ## title for the graph
  title(toupper(paste("IDEOLOGY:",
                      levels(fakeData$ideoPos)[i])
                ),
        line=0)
}
dev.off()



