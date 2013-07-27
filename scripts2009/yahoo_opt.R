require(fCalendar)
require(fImport)

## workaround for R 2.1.1:

Sys.timezone <- function ()
as.vector(Sys.getenv("TZ"))

yahoo.getOption <- function(ticker="QQQQ",maturity="2005-12",file="tempfile01",method="internal",get.short.rate=TRUE) {
#################################################################################################
## get quotes for stock options from Yahoo ###
#################################################################################################
## ticker   .. Ticker symbol for underlying stock
## maturity .. maturity date of option in format YYYY-MM
## file     .. name of temporary file used to store data
## method   .. used by 'download.file'
#################################################################################################

        source <- "http://finance.yahoo.com/q/op?"
        url <- paste(source,"s=",ticker,"&m=",maturity,sep="")
        download.file(url = url, destfile = file, method = method)
      
        raw <- scan(file,what="character",sep=">",quote="");
                   
        ## throw away file
        
        unlink(file)

        start <- grep("<body",raw)
        raw <- raw[start:length(raw)]
        
        table.starts <- grep("<table",raw)
        table.ends <- grep("</table",raw)
        
        heads <- grep("<td.*yfnc_tablehead.*",raw)
        subtit <- grep("<table.*yfncsubtit",raw)
        expire <- grep("Expire at close",raw)
        
        if(!all(length(heads) == 16,length(subtit) == 1,length(expire) == 2)) stop("Unexpected data format")
        
        info.range <- c(subtit,min(table.ends[table.ends > subtit]))
        call.range <- c(heads[8]+2, min(table.ends[table.ends > heads[8]]))
        put.range <- c(heads[16]+2, min(table.ends[table.ends > heads[16]]))
               
        info.raw <- raw[info.range[1]:info.range[2]]
        call.raw <- raw[call.range[1]:call.range[2]]
        put.raw <- raw[put.range[1]:put.range[2]]
        
        b.pos <- grep("<b$",info.raw)
        sm.pos <- grep("<small$",info.raw)
        
        stock.name <- gsub("</b","",info.raw[b.pos[1]+1])
        Stock.price <- as.double(gsub("</b","",info.raw[b.pos[2]+1]))
        Quote.date <- gsub("On |:","",info.raw[sm.pos[1]+1])
        Strike.date <- gsub("^ *|</td","",raw[expire[1]+1])
        
        
        strike.timeDate <- timeDate(strptime(Strike.date,format="%a, %b %d, %Y"), zone="NewYork")
        # Quote.date.timeDate <- timeDate(Quote.date,format="%b %d", FinCenter="NewYork")
        Quote.date.timeDate <- timeDate(strptime(date(),format="%a %b %d %H:%M:%S %Y"))
        
        TTM <- ceiling(as.double(difftime(strike.timeDate,Quote.date.timeDate,units="day")))
                        
        ## remove HTML junk and commas
        
        call.raw <- gsub("<.*$|,","",call.raw)
        put.raw <- gsub("<.*$|,","",put.raw)
        
        ## discard blank and empty elements
        
        call.raw <- call.raw[-grep("[[:blank:]]|^$",call.raw)]
        put.raw <- put.raw[-grep("[[:blank:]]|^$",put.raw)]
        
        ## reshape to matrix
        
        call.mat <- matrix(call.raw,ncol=8,byrow=TRUE)
        put.mat <- matrix(put.raw,ncol=8,byrow=TRUE)


        if(nrow(call.mat) == 0 || nrow(put.mat) == 0) {
            warning(paste("No data available for ticker",ticker,", maturity", maturity))
            return(NULL)
        }
        
        tc <- function(x) type.convert(x,na.string=c("N/A",""))
        
        ## convert types, make data frames
        
        call.df <- data.frame(Strike = tc(call.mat[-1,1]), Symbol = tc(call.mat[-1,2]), Last = tc(call.mat[-1,3]), Chg = tc(call.mat[-1,4]),
                                Bid = tc(call.mat[-1,5]), Ask = tc(call.mat[-1,6]), Vol = tc(call.mat[-1,7]), Open.Int = tc(call.mat[-1,8]))

        put.df <- data.frame(Strike = tc(put.mat[-1,1]), Symbol = tc(put.mat[-1,2]), Last = tc(put.mat[-1,3]), Chg = tc(put.mat[-1,4]),
                                Bid = tc(put.mat[-1,5]), Ask = tc(put.mat[-1,6]), Vol = tc(put.mat[-1,7]), Open.Int = tc(put.mat[-1,8]))
                                
        if(get.short.rate) {
            Short.rate <- get.short.rate(TTM)       
        } else {
            Short.rate <- NA
        }
        
        
        return(list(call=call.df,put=put.df,Stock.ticker=ticker,Quote.date=Quote.date.timeDate,Strike.date=strike.timeDate,
        Stock.name=stock.name,Stock.price=Stock.price,TTM=TTM,Short.rate=Short.rate))

            
}


get.short.rate <- function(TTM) {
###############################################################
## get constant maturity short rate for maturity = TTM (in days)
################################################################
## uses crude piecewise linear interpolation
## TTM ... vector of maturities in days
###################################################

   cmr1m <- fredImport("DGS1MO")  # 1-Month Treasury constant maturity rate
   cmr1y <- fredImport("DGS1")  # 1-Year Treasury constant maturity rate
   cmr2y <- fredImport("DGS2")  # 2-Year Treasury constant maturity rate

    r1m <- cmr1m@data[dim(cmr1m@data)[1],"DGS1MO"]
    r1y <- cmr1y@data[dim(cmr1y@data)[1],"DGS1"]
    r2y <- cmr2y@data[dim(cmr2y@data)[1],"DGS2"]

    helper <- function(TTM) {
       if(TTM <= 360) {
         b <- (12 * r1m - r1y)/11
         a <- (r1y - r1m)/330
       } else {
         b <- 2 * r1y - r2y
         a <- (r2y - r1y)/360
       }
       return(a*TTM + b)
    }
    return(sapply(TTM,helper))
}

yahoo.getMaturities <- function(ticker="QQQQ",file="tempfile01",method="internal") {
#################################################################################################
## get maturities for which option quotes are available from Yahoo ###
#################################################################################################
## ticker   .. Ticker symbol for underlying stock
## file     .. name of temporary file used to store data
## method   .. used by 'download.file'
#################################################################################################


        source <- "http://finance.yahoo.com/q/op?"
        url <- paste(source,"s=",ticker,sep="")
        download.file(url = url, destfile = file, method = method)
      
        raw <- scan(file,what="character",sep=">",quote="");
                   
        ## throw away file
        
        unlink(file)

        start <- grep("<body",raw)
        raw <- raw[start:length(raw)]
        
        expire <- grep("View By Expiration",raw)
        table.starts <- grep("<table",raw)
        
   
        if(!length(expire) == 1) stop("Unexpected data format")
        
        expire.range <- c(expire,min(table.starts[table.starts > expire]))
        expire.raw <- raw[expire.range[1]:expire.range[2]]
          ## remove HTML junk and commas
        expire.raw <- gsub("<.*$|,","",expire.raw)
        
        pattern <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [[:digit:]]+.*"
        expire.index <- grep(pattern,expire.raw)
        expire.raw <- expire.raw[expire.index]
            ## insert century "20"  (05 -> 2005, etc..)
        
        conv <- function(x) {
            xs <- unlist(strsplit(x," "))
            num <- match(xs[1],c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
            return(paste("20",xs[2],"-",sprintf("%02i",num),sep=""))
        }
                
        return(sapply(expire.raw,conv))
     
            
}



combine.data <- function(x) {
#######################################################################
## Reformat object returned by 'yahoo.getOption' to a single data frame
#######################################################################

      dfrm <- rbind(data.frame(x$call,Type="call"),data.frame(x$put,Type="put"))
      #dfrm <- dfrm[,!(colnames(dfrm) %in% c("Bid","Ask","Chg"))]
      dfrm$Quote.date <- format(x$Quote.date,"%Y-%m-%d")
      dfrm$Strike.date <- format(x$Strike.date,"%Y-%m-%d")
      dfrm$Underlying <- x$Stock.ticker
      dfrm$Stock.price <- x$Stock.price
      dfrm$TTM <- x$TTM
      dfrm$Short.rate <- x$Short.rate
      dfrm$Moneyness.fw <- dfrm$Strike*exp(-dfrm$Short.rate/100*dfrm$TTM/365)/dfrm$Stock.price
      rownames(dfrm) <- 1:dim(dfrm)[1]
      return(dfrm)

}



yahoo.getAllOptions <- function(tickers=c("QQQQ"),File=NULL,imp.vol=TRUE) {
##############################################################################################
## Combines 'yahoo.getOption', 'yahoo.getMaturities' and 'combine.data':
## Downloads option quotes for all available maturities and stores them in a single data frame
##############################################################################################
## tickers  .. vector of ticker symbols for which to get quotes
## File .. if not NULL store data frame to a file
## imp.vol .. if true implied Black-Scholes volatilities are calculated and added to the data frame
##############################################################################################



    df.total <- data.frame()


    for(i in 1:length(tickers)) {
        ticker <- tickers[i]
        mat <- yahoo.getMaturities(ticker)
        if(length(mat) < 1) stop(paste("Cannot Continue: No valid maturities found for ticker",ticker))
        short.rate <- get.short.rate(1:1000)  
        for(j in 1:length(mat)) {
            fresh <- yahoo.getOption(ticker=ticker,maturity=mat[j],get.short.rate=FALSE)
            fresh$Short.rate <- short.rate[fresh$TTM]
            if(!is.null(fresh))
                df.total <- rbind(df.total,combine.data(fresh))
        }
    }


    if(imp.vol) df.total$Imp.vol <- imp.vol(df.total)       ## add implied volatilities

    if(!is.null(File)) {
        write.table(df.total,File,append=TRUE,row.names=FALSE,col.names=FALSE)
        return(NULL)
    }
    return(df.total)

}



plot.smile <- function(x,type="call",scale="strike",normed=TRUE) {
################################################################################################
##  Plot smiles (Implied Volatility vs. Strike) for each Time-to-maturity (TTM) thats available.
################################################################################################
##  x .. data frame as returned by 'yahoo.getAllOptions'
##  type  .. call/put
##  scale .. One of "strike", "moneyness" (= exp(-r*TTM)*Strike/Stock_price), "logmoneyness"
##  normed .. TRUE -> same scaling for all plots
################################################################################################

    x <- x[(x$Type == type) & !is.na(x$Imp.vol),]
    TTM.level <- unique(x$TTM)
    limits <- NULL
    if(normed && scale=="strike") limits <- list(xlim=range(x$Strike),ylim=range(x$Imp.vol,na.rm=TRUE))
    if(normed && scale=="moneyness") limits <- list(xlim=range(x$Moneyness.fw),ylim=range(x$Imp.vol,na.rm=TRUE))
    if(normed && scale=="logmoneyness") limits <- list(xlim=range(log(x$Moneyness.fw)),ylim=range(x$Imp.vol,na.rm=TRUE))

    single.plot <- function(x,scale="strike",limits=NULL) {
        if(scale == "strike") xx <- x$Strike
        if(scale == "moneyness") xx <- x$Moneyness.fw
        if(scale == "logmoneyness") xx <- log(x$Moneyness.fw)
        if(is.null(limits)) limits <- list(xlim=range(xx),ylim=range(x$Imp.vol,na.rm=TRUE))
        ylow <- limits$ylim[1] - (limits$ylim[2]-limits$ylim[1])*.1
        
        #browser()
        plot(xx,x$Imp.vol,type="b",xlab=scale,ylab="Implied Volatility",ylim=c(ylow,limits$ylim[2]),xlim=limits$xlim)
        loess.fit <- loess(x$Imp.vol ~ xx,weights=sqrt(x$Vol))
        k.seq <- seq(from=min(xx),to=max(xx),length=200)
        lines(k.seq,predict(loess.fit,k.seq),col=2,lty="dotted")
        if(scale == "strike") abline(v=x$Stock.price,col=3,lty="dashed")
        if(scale == "moneyness") abline(v=1,col=3,lty="dashed")
        if(scale == "logmoneyness") abline(v=0,col=3,lty="dashed")
        mtext(paste("TTM:", x$TTM, "(days)"),line=.5)

        volume.scaled <- x$Vol/max(x$Vol,na.rm=TRUE)*(limits$ylim[2] - ylow)*.25 + ylow
        segments(xx,ylow,xx,volume.scaled,col=4,lwd=2)
        #mtext("volume",4,adj=.1,col=4,srt=90)
        text(min(xx),ylow + (limits$ylim[2] - ylow)*.025,"volume",srt=90,col=4,pos=4)
        # axis(4,at=range(volume.scaled,na.rm=TRUE),labels=c(0,max(x$volume,na.rm=TRUE)),cex=.7,col=4)
    }
    
    nx <- floor(sqrt(length(TTM.level)))
    ny <- ceiling(length(TTM.level)/nx)
    old.par <- par(mfrow=c(nx,ny))
    
    by(x,x$TTM,single.plot,scale=scale,limits=limits)    
    
    par(old.par)
    #browser()
}





plot3d.smile <- function(opt.all,type="call",strike.range=c(-Inf,Inf),TTM.range=c(-Inf,Inf),strike.scale="Strike",TTM.scale="TTM",plot.type="persp",n=60,fit.diagnostics=FALSE) {
######################################################################################################
##  Plot 3d smiles (Volatility Surface consisting of Implied Volatility vs. Strike + Time-to-maturity
######################################################################################################
##  opt.all .. data frame as returned by 'yahoo.getAllOptions' or 'combine.data'
##  type         .. call/put
##  strike.range .. vector of length 2;  c(20,50) means include all options with strike prices between 20 and 50
##  strike.range .. vector of length 2; c(10,100) means include all options with time-to-maturity between 10 and 100 days
##  strike.scale .. One of "Strike", "Moneyness" (= exp(-r*TTM)*Strike/Stock_price), "Log-Moneyness"
##  TTM.scale    .. One of "TTM","Log-TTM" 
##  plot.type    .. One of "persp", "lattice"; two different types of plots
##  n            .. density of grid, applies both for strike and TTM grid
##  fit.diagnostics .. if TRUE plot diagnostics for the smoothing used
################################################################################################


      if(!require(locfit)) stop("Cannot load package 'locfit'")

      # select valid data
      is.valid <- opt.all$Type == type & opt.all$TTM >= TTM.range[1] & opt.all$TTM <= TTM.range[2] & opt.all$Strike >= strike.range[1] & opt.all$Strike <= strike.range[2] & !is.na(opt.all$Imp.vol)
      if(!any(is.valid)) stop("No data point inside the specified range")
      opt.use <- opt.all[is.valid,]

      # transform scales
      if(TTM.scale == "Log-TTM") {
          opt.use$TTM.tf <- log(opt.use$TTM)
      } else {
         opt.use$TTM.tf <- opt.use$TTM
      }

      if(strike.scale == "Moneyness") {
          opt.use$strike.tf <- opt.use$Moneyness.fw
      } 

      if(strike.scale == "Log-Moneyness") {
          opt.use$strike.tf <- log(opt.use$Moneyness.fw)
      }

      if(strike.scale == "Strike") {
         opt.use$strike.tf <- opt.use$Strike
      }


      # calculate smooth surface
      opt.fit <- locfit(Imp.vol ~ strike.tf + TTM.tf, data = opt.use, alpha=.7,deg=2) # ,weights=sqrt(opt.call$Vol))

      if(fit.diagnostics) {
          old.par <- par(mfrow=c(2,2))
          res <- residuals(opt.fit,data=opt.use)
          qqnorm(res)
          plot(opt.use[,"strike.tf"],res)
          plot(opt.use[,"TTM.tf"],res)
          plot.new()
          par(old.par)
          windows()
      }


      strike.range <- range(opt.use$strike.tf)
      TTM.range <- range(opt.use$TTM.tf)

      strike.grid <- seq(from=strike.range[1],to=strike.range[2],length=n)
      TTM.grid <- seq(from=TTM.range[1],to=TTM.range[2],length=n)
      full.grid <- expand.grid(strike.tf=strike.grid,TTM.tf=TTM.grid)

      opt.pred <- predict(opt.fit,full.grid)


      if(plot.type ==  "persp") {
          persp(x=strike.grid,y=TTM.grid,z=matrix(opt.pred,nrow=n),xlab=strike.scale,ylab=TTM.scale,zlab="Implied Volatility",main="Volatility Smile",
                expand=.6,shade=TRUE,col=2,phi=20,theta=30,ticktype="detailed")
      }

      if(plot.type == "lattice") {

          if(!require(lattice)) stop("Cannot load package 'lattice'")
          aspect <- list(z=-35,x=-70,y=0)

          plot1 <- cloud(Imp.vol ~ strike.tf + TTM.tf, data = opt.use,screen=aspect,cross=TRUE,col=1,
                      scales=list(arrows=FALSE),xlab=strike.scale,ylab=TTM.scale,zlab="Volatility",main="Implied Volatility: Raw Data")
          plot2 <- wireframe(opt.pred ~ full.grid$strike.tf + full.grid$TTM.tf, shade=TRUE,pretty=TRUE,screen=aspect,
                      scales=list(arrows=FALSE),xlab=strike.scale,ylab=TTM.scale,zlab="Volatility",main="Implied Volatility: Smooth Interpolation")

          print(plot1, split=c(1,1,2,1),more=TRUE)
          print(plot2, split=c(2,1,2,1))
      }

}


#### Misc. functions


pc.parity <- function(x,rate=NA) {
## check put-call parity

    if(is.na(rate)) rate <- x$Short.rate/100

    common <- intersect(x$put$Strike,x$call$Strike)
    cmp <- x$call[match(common,x$call$Strike),"Last"] - x$put[match(common,x$put$Strike),"Last"] - x$Stock.price + exp(-rate*x$TTM)*common
    return(list(Strike = common,cpp.res = cmp))

}


bs.call <- function(volatility,strike.price,Stock.price,TTM,rate) {
## returns Black-Scholes European call option price. Time unit for rate, TTM and volatility is YEARS !! 

return(Stock.price*pnorm((log(Stock.price/strike.price) + (rate + volatility^2/2)*TTM)/(volatility*sqrt(TTM))) - 
      strike.price*pnorm((log(Stock.price/strike.price) + (rate - volatility^2/2)*TTM)/(volatility*sqrt(TTM))) * exp(-TTM*rate))

}

bs.put <- function(volatility,strike.price,Stock.price,TTM,rate) {
## returns Black-Scholes European call option price. Time unit for rate, TTM and volatility is YEARS !! 

return(strike.price*pnorm(-(log(Stock.price/strike.price) + (rate - volatility^2/2)*TTM)/(volatility*sqrt(TTM)))*exp(-TTM*rate) -
        Stock.price*pnorm(-(log(Stock.price/strike.price) + (rate + volatility^2/2)*TTM)/(volatility*sqrt(TTM))))
  

}


imp.vol <- function(x,upper=10) {

    bs.res <- function(vol,row) {
        ret <- NA
        if(row$Type == "call")
            ret <- bs.call(vol,strike.price=row$Strike,Stock.price=row$Stock.price,TTM=row$TTM/365,rate=row$Short.rate/100) - row$Last
        if(row$Type == "put")
            ret <-  bs.put(vol,strike.price=row$Strike,Stock.price=row$Stock.price,TTM=row$TTM/365,rate=row$Short.rate/100) - row$Last
        return(ret)
    }

    find.root <- function(row) {
        if(bs.res(0,row)*bs.res(upper,row) >= 0)
             vol <- NA
        else 
            vol <- uniroot(bs.res, interval = c(0,upper),row=row)$root
            
        return(vol)
    }

    vols <- vector("numeric",length=nrow(x))
    for(i in 1:nrow(x))
        vols[i] <- find.root(x[i,])
   
    return(vols)
    
}
