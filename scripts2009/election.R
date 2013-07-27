# 2008 Election Simulator
# Nov. 3, 2008
# Nov. 2, 2008
# Oct. 24, 2008
# Bret Larget

# This code should be cleaned up considerably with more useful comments
# and better data management, but the election is tomorrow!

# This R program contains a number of function useful for simulating,
# predicting, and displaying results for the 2008 presidential
# election.

# The code assumes the existence of:
#   election.R (this file)
#   polls2008.txt (a file with poll results)
#   states2008.txt (a file with 2004 results and 2008 Electoral votes)

############################################################
# Quick examples:

#   source("election.R")
#   plotPollData("WI")
#   simulateElection(1)
#   simulateElection(10000)
#   plotStateData()  # creates file states.pdf with results for each state
############################################################

# The poll results were found using links on:
# http://www.realclearpolitics.com/
# and
# http://www.fivethirtyeight.com/  (for DC)

# Maine and Nebraska actually divide the states into congressional
# districts hold separate elections that theoretically can votes
# separately without a winner take all.  The code here ignores this
# (as it is very likely that all parts of Maine will go for Obama and
# all parts of Nebraska for McCain).

# The graphical functions require the library(maps) to draw maps showing the
# election outcome.  Access this library with the command
# > install.packages("maps")
# which only needs to be done once.

# The file polls2008.txt contains all poll results from September, 2008 or later
# by state conducted by either Rasmussen or SurveyUSA.  For
# SurveyUSA, I can get raw counts of likely voters.  For Rasmussen
# (including FOX/Rasmussen), I multiply the percentage by the number
# of likely voters.  If there are undecideds among the likely voters,
# this will inflate the counts somewhat.
# 
# Both SurveyUSA and Rasmussen score very well on http://www.fivethirtyeight.com/
# which scores and ranks polling organizations by their accuracy.

# The calculations in this file assume that the proportion of voters
# favoring either Obama or McCain have been steady since September 1,
# and that these two polling organizations are able to distinguish
# separate likely voters from their larger sample.  The predictions could fail if:
# (1) Voters change preferences over time;
# (2) Many "unlikely voters" actually vote;
# (3) Respondents do not tell the truth.
# (4) If the many non-respondents differ from the respondents.

# 
# Initialize
#

# use install.library("maps") if not already installed
require(maps)

# Read in the data
# Requires the files "polls2008.txt" and "states2008.txt" to be in the working directory.
pollInfo = read.table("polls2008.txt",header=T)
stateInfo = read.table("states2008.txt",header=T)

# Add DateX to the pollInfo data frame.
# DateX counts the number of days after August 31, 2008 for the date of the poll.
# For polls conducted over multiple days (most SurveyUSA polls), I use the last date.

pollInfo$DateX = with(pollInfo, 30*(Month=="Oct") + 61*(Month=="Nov") + Date)

# Find quantiles of a posterior distribution for plotting individual polls
# by effectively adding a total of 4 voters to each of the two candidates.
# Quantiles are converted to percentages.

pollInfo$a = with(pollInfo,100*qbeta(0.025,Obama+4,McCain+4))
pollInfo$b = with(pollInfo,100*qbeta(0.975,Obama+4,McCain+4))

# Summarize the percentage of voters for each candidate for each separate poll.
pollInfo$p.Obama = with(pollInfo, 100*Obama/(Obama+McCain))
pollInfo$p.McCain = with(pollInfo, 100*McCain/(Obama+McCain))

# Use actual vote totals from the 2004 election
# to calculate prior distributions for the 2008 election.
# Effectively, use the proportion of votes among Republican and Democratic
# to assign a total of eight "extra" voters to each sample.

stateInfo$dem2004 = with(stateInfo, Kerry2004/(Kerry2004+Bush2004))
stateInfo$demPrior = with(stateInfo, 8*dem2004)
stateInfo$repPrior = with(stateInfo, 8*(1-dem2004))

# Merge the poll and state information into a single data frame.
allInfo = merge(pollInfo,stateInfo)

#
# For predictions, use the combination of all polls.
#

totalPollInfo = data.frame(Obama=with(allInfo, sapply(split(Obama,State),sum)),
                           McCain=with(allInfo, sapply(split(McCain,State),sum)),
                           State=with(allInfo,levels(State)))

#
# Merge the state information and the combined poll information into a single data frame.
# Compute combined relative proportions for each candidate and prior information based on the 2004 elections.
# Compute the probability that each candidate will win each state.

totalInfo = merge(stateInfo,totalPollInfo)
totalInfo$prop.Obama = with(totalInfo, Obama/(Obama+McCain))
totalInfo$prop.McCain = with(totalInfo, McCain/(Obama+McCain))
totalInfo$prob.Obama = with(totalInfo, 1 - pbeta(0.5,Obama + demPrior,McCain + repPrior))
totalInfo$prob.McCain = with(totalInfo, pbeta(0.5,Obama + demPrior,McCain + repPrior))

# Create a rounded summary of totalInfo for display

displayTotalInfo = with(totalInfo, data.frame(State=State,AB=AB,Votes=Votes,Bush2004=Bush2004,Kerry2004=Kerry2004,demPrior=round(demPrior,2),
  repPrior=round(repPrior,2),Obama=Obama,McCain=McCain,pct.Obama=round(100*prop.Obama,2),pct.McCain=round(100*prop.McCain,2),
  prob.Obama=round(prob.Obama,4),prob.McCain=round(prob.McCain,4)))

sink("totalInfo.txt")
print(displayTotalInfo)
sink()

#
# probWinByState
#

probWinByState = function(dCount=totalInfo$Obama,rCount=totalInfo$McCain,dPrior=totalInfo$demPrior,rPrior=totalInfo$repPrior) {
  p = 1 - pbeta(0.5,dCount+dPrior,rCount+rPrior)
  return(data.frame(p.Obama=p,p.McCain=1-p,State=totalInfo$State,Votes=totalInfo$Votes))
}

win = probWinByState()

# plotPosterior
#
# Plots a beta density with parameters a1 and a2
# Used by other functions to plot state-by-state probability distributions.

plotPosterior = function(a1,a2,scale=T,state="",ev=0) {
  mu = a1/(a1+a2)
  sig2 = a1*a2/(a1+a2)^2/(a1+a2+1)
  sig = sqrt(sig2)
  if(scale) {
    lo = mu - 4*sig
    hi = mu + 4*sig
  }
  else {
    lo = 0
    hi = 1
  }
  u = seq(lo,hi,length=1001)
  y = dbeta(u,a1,a2)
  plot(u,y,type="l",xlab="% for Obama",ylab="",las=1)
  abline(h=0)
  rep = (u<0.5)
  dem = (u>0.5)
  polygon(c(0,u[rep],0.5),c(0,y[rep],0),col="red")
  polygon(c(0.5,u[dem],1),c(0,y[dem],0),col="blue")
  abline(v=0.5,lwd=2)
  p = 1 - pbeta(0.5,a1,a2)
  title(paste(state," (",ev,")",sep=""))
  my = max(y)
  text(lo,my,paste("P(McCain wins) =",round(1-p,4)),cex=1.0,adj=0,col="red")
  text(hi,my,paste("P(Obama wins) =",round(p,4)),cex=1.0,adj=1,col="blue")
  p1 = a1/(a1+a2)
  title(paste("#McCain =",round(a2,1),", #Obama =",round(a1,1),", %McCain =",round(1-p1,3),", %Obama =",round(p1,3)),line=0.3,cex.main=0.7)
  invisible(p)
}

plotPollData = function(ab,...) {
  subset = with( allInfo, AB==ab)
  ylim = with(allInfo, c(min(c(50,a[subset])),max(c(50,b[subset]))))
  with(allInfo, plot(DateX[subset],p.Obama[subset],xlim=c(0,70),ylim=ylim,axes=F,xlab="Date",ylab="Relative % for Obama",pch=16,main=with(totalInfo,State[AB==ab]),...) )
  axis(1,at=c(1,15,31,45,62),labels=c("Sep 1","Sep 15","Oct 1","Oct 15","Nov 1"))
#  axis(2,at=seq(20,90,10),las=2)
  axis(2,at=pretty(ylim),las=2)
  abline(h=50,col="gray",lty=2,lwd=3)
  abline(h=100*totalInfo$prop.Obama[totalInfo$AB==ab],col="green",lty=3,lwd=3)
  with(allInfo, segments(DateX[subset],a[subset],DateX[subset],b[subset]))
  return(invisible())
}

###
plotStateData = function(pdf=T) {
  if(pdf)
    pdf("states.pdf",paper="letter",height=9.5,width=7.5)
  par(mfrow=c(2,1),mar=c(6,5,3,0))
  st = with( allInfo, levels(State) )
  for(i in 1:length(st)) {
    with(totalInfo, plotPollData(ab=(AB[State==st[i]])))
    r = with(totalInfo, State==st[i])
    with(totalInfo, plotPosterior(Obama[r] + demPrior[r],McCain[r] + repPrior[r],scale=T,state=State[r],ev=Votes[r]))
  }
  if(pdf)
    dev.off()
  invisible()
}

plotStateData()

# simulateElection
#
# Example use.
# > simulateElection(100) will plot the first election
# results and report the outcomes for all 100 simulated elections.

simulateElection = function(nElect=1,plot=T,x=totalInfo,pdf=F) {
  if(pdf)
    pdf("usa.pdf",paper="letter")
  n = nrow(x)
  obamaA = with(x, Obama + demPrior)
  mccainA = with(x, McCain + repPrior)
  winner = rep("noOne",nElect)
  demTotal = rep(0,nElect)
  repTotal = rep(0,nElect)
  for(i in 1:nElect) {
    p = rbeta(n,obamaA,mccainA)
    obamaEV = with(x, sum(Votes[p>0.5]))
    mccainEV = with(x, sum(Votes[p<0.5]))
    if(i==1 && plot) {
      map('state')
      map('state',region=x$state[p>0.5],fill=T,col="blue",add=T)
      map('state',region=x$state[p<0.5],fill=T,col="red",add=T)
      if(p[x$state=="alaska"]>0.5)
        text(-120,30,"Alaska",col="blue")
      else
        text(-120,30,"Alaska",col="red")
      if(p[x$state=="hawaii"]>0.5)
        text(-120,28,"Hawaii",col="blue")
      else
        text(-120,28,"Hawaii",col="red")
      text(-77,30,paste("Obama",obamaEV),col="blue",adj=0)
      text(-77,28,paste("McCain",mccainEV),col="red",adj=0)
    }
    if(obamaEV>269)
      winner[i] = "Obama"
    else if(mccainEV>269)
      winner[i] = "McCain"
    else
      winner[i] = "tie"
    demTotal[i] = obamaEV
    repTotal[i] = mccainEV
  }
  if(pdf)
    dev.off()
  print(paste("# of Elections:",nElect))
  print(paste("# McCain wins:",sum(winner=="McCain")))
  print(paste("# Obama wins:",sum(winner=="Obama")))
  print(paste("# Ties:",sum(winner=="tie")))
  print(paste("# Mean McCain Electoral Votes:",round(mean(repTotal),1)))
  print(paste("# Mean Obama Electoral Votes:",round(mean(demTotal),1)))
  
  invisible(list(winner=winner,demTotal=demTotal,repTotal=repTotal))
}

doSims = function(nElect=10000,pdf=F) {
  e = simulateElection(nElect=nElect,pdf=pdf)
  out = data.frame(ev=c(e$demTotal,e$repTotal),candidate=factor(c(rep("Obama",length(e$winner)),rep("McCain",length(e$winner)))))
  if(pdf)
    pdf("sim.pdf",paper="letter")
  else
    quartz()
  print( histogram(~ev | candidate, data=out,layout=c(1,2),col="blue",xlab="Electoral Votes",nint=25) )
  if(pdf)
    dev.off()
  return(invisible(e))
}
