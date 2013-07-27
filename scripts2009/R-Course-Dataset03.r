################################################################################
#
# R-Course: Exercises with data set 3
#
# Data source: Nationale Daueruntersuchung der schweizerischen Fliessgewässer
#              NADUF (http://www.naduf.ch)
#              Converted to ASCII format by Ursula Schoenenberger, Eawag
#
################################################################################


# =======================================
# Part A: Data Manipulation and Graphics:
# =======================================


# Read data set 3:
# ----------------

filename <- "R-Course-Dataset03.dat"
header   <- read.table(file=filename,sep="\t",nrows=1)
units    <- read.table(file=filename,skip=1,sep="\t",nrows=1)
data     <- read.table(file=filename,skip=2,sep="\t")
colnames <- character(0)
for ( i in 1:ncol(header) ) colnames[i] <- as.character(header[1,i])
names(data) <- colnames
     
data$Datum <- as.Date(as.character(data$Datum),format="%d.%m.%Y")
data$Uhrzeit <- as.numeric(substr(as.character(data$Uhrzeit),1,2))+
                as.numeric(substr(as.character(data$Uhrzeit),4,5))/60


# Get size and variable names:
# ----------------------------

dim(data)
names(data)


# Get stations:
# -------------

stations <- levels(data$Kuerzel)


# Plot temperature time series at all stations:
# ---------------------------------------------

ncol.plot <- 3
par.default <- par(no.readonly=T)
par(mfrow=c(as.integer(length(stations)/ncol.plot+0.999),ncol.plot),
    mar=c(2.5,4,2,1),  # bottom, left, top, right
    xaxs="i",yaxs="i")
for ( i in 1:length(stations) )
{
   plot(data[data$Kuerzel==stations[i],"Datum"]-7,
        data[data$Kuerzel==stations[i],"Temperatur"],
        main=stations[i],ylab="Temperature",
        xlim=c(min(data$Datum,na.rm=T),max(data$Datum,na.rm=T)),
        ylim=c(min(data$Temperatur,na.rm=T),max(data$Temperatur,na.rm=T)),
        cex=0.8)
}
par(par.default)


# ===============================
# Part B: Statistical Techniques:
# ===============================


# Read data set 3:
# ----------------

filename <- "R-Course-Dataset03.dat"
header   <- read.table(file=filename,sep="\t",nrows=1)
units    <- read.table(file=filename,skip=1,sep="\t",nrows=1)
data     <- read.table(file=filename,skip=2,sep="\t")
colnames <- character(0)
for ( i in 1:ncol(header) ) colnames[i] <- as.character(header[1,i])
names(data) <- colnames
     
data$Datum <- as.Date(as.character(data$Datum),format="%d.%m.%Y")
data$Uhrzeit <- as.numeric(substr(as.character(data$Uhrzeit),1,2))+
                as.numeric(substr(as.character(data$Uhrzeit),4,5))/60


# Get summary statistics:
# -----------------------

summary(data)


# =================================
# Part C: Packages and Programming:
# =================================


# Read data set 3:
# ----------------

read.naduf <- function(filename) 
{
   header   <- read.table(file=filename,sep="\t",nrows=1)
   units    <- read.table(file=filename,skip=1,sep="\t",nrows=1)
   data     <- read.table(file=filename,skip=2,sep="\t")
   colnames <- character(0)
   for ( i in 1:ncol(header) ) colnames[i] <- as.character(header[1,i])
   names(data) <- colnames
     
   data$Datum <- as.Date(as.character(data$Datum),format="%d.%m.%Y")
   data$Uhrzeit <- as.numeric(substr(as.character(data$Uhrzeit),1,2))+
                   as.numeric(substr(as.character(data$Uhrzeit),4,5))/60
                  
   return(data)
}

data <- read.naduf("R-Course-Dataset03.dat")


# Temperature sine regression at all stations (see plotting above):
# -----------------------------------------------------------------

ncol.plot <- 3
par.default <- par(no.readonly=T)
par(mfrow=c(as.integer(length(stations)/ncol.plot+0.999),ncol.plot),
    mar=c(2.5,4,2,1),  # bottom, left, top, right
    xaxs="i",yaxs="i")
date.base <- as.Date("1900-01-01")
fit.par <- list()
for ( i in 1:length(stations) )
{
   x.min <- min(data$Datum,na.rm=T)
   x.max <- max(data$Datum,na.rm=T)
   x <- data[data$Kuerzel==stations[i],"Datum"]-7
   y <- data[data$Kuerzel==stations[i],"Temperatur"]
   plot(x,y,
        main=stations[i],ylab="Temperature",
        xlim=c(x.min,x.max),
        ylim=c(min(data$Temperatur,na.rm=T),max(data$Temperatur,na.rm=T)),
        cex=0.8)
   avail <- !is.na(y)
   x <- as.numeric(x[avail]-date.base)
   y <- y[avail]
   fit.par[[stations[i]]] <- NA
   if ( max(x)-min(x) > 10*365.24 )  # regression only if data over 10 years
   { 
      fit.res <- nls(y~Tm+Tgrad*x+0.5*Ta*sin(2*pi/365.25*(x+offset)),
                     start=list(Tm=10,Ta=10,offset=-90,Tgrad=-0.0001))
      #print(summary(fit.res))
      x.calc <- seq(as.numeric(x.min-date.base),as.numeric(x.max-date.base),by=1)
      y.calc <- predict(fit.res,newdata=list(x=x.calc))
      fit.par[[stations[i]]]  <- cbind(summary(fit.res)$parameters,
                                       confint(fit.res,level=0.95))
      lines(x.calc+date.base,y.calc)
   }
}
par(par.default)

gradT.est   <- rep(NA,length(fit.par))
gradT.lower <- rep(NA,length(fit.par))
gradT.upper <- rep(NA,length(fit.par))
for ( i in 1:length(fit.par) )
{
   if ( !is.na(fit.par[[i]][1]) )
   {
      gradT.est[i]   <- fit.par[[i]]["Tgrad","Estimate"]*365.25
      gradT.lower[i] <- fit.par[[i]]["Tgrad","2.5%"]*365.25
      gradT.upper[i] <- fit.par[[i]]["Tgrad","97.5%"]*365.25
   }
}
plot(1:length(fit.par),gradT.est,
     main="Slopes of Sine Regression",axes=F,
     ylab="Temp.grad [deg/year]",xlab="",
     xlim=c(0,length(fit.par)+1),
     ylim=c(min(c(0,gradT.lower),na.rm=T),max(gradT.upper,na.rm=T)))
axis(1,at=1:length(stations),labels=stations)
axis(2)
lines(c(0,length(fit.par)+1),c(0,0))
lines(c(0,length(fit.par)+1),c(mean(gradT.est,na.rm=T),mean(gradT.est,na.rm=T)))
for ( i in 1:length(fit.par) )
{
   if ( !is.na(gradT.est[i]) )
   {
      lines(c(i,i),c(gradT.lower[i],gradT.upper[i]))
   }
}


