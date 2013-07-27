##  The purpose of this set of functions is to produce
##  plots of regression coefficients, possibly for
##  multiple models, in a dot plot with error bars
##  format. The plots are produced using the
##  grid package in R. The idea is that you
##  could supply a list of models to a function, and
##  it would somewhat automatically draw a plot with
##  multiple panels, with one independent variable
##  exhibited in each panel. In what follows, we will
##  do so using zelig models.

require(grid)
require(Zelig)

##The main function is plot.reg. It is called as follows  
plot.reg <- function(Y,
                                        #a list composed by three or five matrices, all k x m.
                                        #where k is the number of independent variables
                                        #and m is the number of models
                                        #the matrices are: estimate, lo and hi;
                                        #and optionally lo1 and hi1
                                        #lo and hi are the low and upper bounds of the
                                        #confidence intervals
                                        #similarly
                                        #lo1 and hi1 are the inner confidence intervals
                                        #which will be plotted as cross hairs
                     Y2=NULL,
                                        #specified just as Y
                                        #so one can plot the models in pairs (see examples)
                     legend=NULL,
                                        #if there both Y and Y2 are specified, legend
                                        #is an optional character vector of length 2
                                        #giving the legends for Y and Y2
                     print=TRUE,        # print the plot or just create the object
                     refline=NA,         # a vector with the reference lines for each independent variable
                                        # put NA if you don't want ref lines
                     hlast=.1,          # the amount of space (in proportion) left at the bottom of the graph
                                        # for the x-axis labels
                     lwd.fact=1,
                                        # a multiplier for the line width and character size
                     length.arrow=unit(0,"mm"),
                                        # length of the cross hair
                     widths=c(.45,.45,.1),
                                        # widths in proportion of the graph.
                                        # (space for the independent variable labels,
                                        # space for the panels,
                                        # space for the y-axis labels)
                     rot.label.y=0,     # rotation of the independent variable labels
                     just.label.y="right", # justification of the  independent variable labels
                     pos.label.y=.97,   # x position of the independent variable labels
                     pch.size=0.5,      # size of the symbols
                     h.grid=FALSE,       # plot horizontal grid
                     v.grid=FALSE,       # plot vertical grid
                     expand.factor=0.25, # factor by which to extend the plot range
                     expand.factor2=0.1, # factor by which to extend the plot range
                     leg.mult=.7,  #rel size of legend
                     yaxis.at=NULL, ## list with y axis tick-mark points,
                     ylabel=NULL, ## list with y axis labels
                     ##with length equal to the number of plots
                     ...                # other options passed to the grid.Vdotplot function
                     ) {
  ## the function gets the variable names from Y$estimate rownames
  label.vec.vars <- rownames(Y$estimate)
  ## number of independent variables
  n.plots <- nrow(Y$estimate)
  if ((!is.null(yaxis.at))&(length(yaxis.at)!=n.plots)) {
    stop("length of yaxis.at must equal the number of plots")
  }
  hbet <- .01 # amound of vertical space between plots
  hit <- (1-hlast-hbet*n.plots)/n.plots #height of each plot
  sp.now <- 0 #i f sp.now > 0, the x-axis with labels is plotted
  index <- seq(1,n.plots*2,2) # index of the plots and between spaces
  grid.newpage() # create a new page
  ##a frame graphical object
  ## it has k*2 vertical slots (one for each variable + one for each space between plots)
  fg <- frameGrob(layout=grid.layout(n.plots*2,
                    ## and 3 horizontal slots (space for ind. variables labels,
                    ## space for plots, space for yaxis labels)      
                    3,widths=unit(widths,"npc"),
                    heights=unit(c(rep(c(hit,hbet),n.plots-1),hit,hlast),"npc"))) 
  ## loop to create panels
  ## j indexes independent variables
  j <- 1
  for (i in index) {  ## i is the vertical slot position
    #create a dataframe with the data to plot now
    Y.now <- data.frame(estimate=Y$estimate[j,],
                        lo=Y$lo[j,],
                        hi=Y$hi[j,],
                        lo1=Y$lo1[j,],
                        hi1=Y$hi1[j,])
    ##similartly for Y2
    if (!is.null(Y2)) Y2.now <- data.frame(estimate=Y2$estimate[j,],
                                           lo=Y2$lo[j,],
                                           hi=Y2$hi[j,],
                                           lo1=Y2$lo1[j,],
                                           hi1=Y2$hi1[j,])
    else Y2.now <- NULL
    ## if it is the bottom row, set sp.now to a positive value
    if (i==max(index)) sp.now <- .1    
    ##are we drawing a reference line?
    drawRef <- !is.na(refline[j])
    ##place the plot
    ##the actual plot object is created by the function grid.Vdotplot
    fg <- placeGrob(fg,
                    grid.Vdotplot(Y.now,
                                  Y2.now,
                                  sp=c(.1,sp.now),draw=FALSE,lwd.fact=lwd.fact,
                                  refline=ifelse(drawRef,refline[j],0) ## if refline is NA put anything in place
                                  ,drawRef=drawRef,
                                  length.arrow=length.arrow,
                                  pch.size=pch.size,
                                  h.grid=h.grid,
                                  v.grid=v.grid,
                                  expand.factor=expand.factor,
                                  expand.factor2=expand.factor2,
                                  aty=yaxis.at[[j]],
                                  labely=ylabel[[j]],
                                  ...) 
                    ,col=2,row=i)
    ##the independent variables labels
    fg <- placeGrob(fg,textGrob(x=pos.label.y,label.vec.vars[j]
                                ,rot=rot.label.y,gp=gpar(cex=.75*lwd.fact),just=just.label.y
                                ),col=1,row=i)
    j <- j+1
  }
  ## if Y2 exists and a legend is specified, draw it using the legendGrob function
  if (!is.null(Y2)&!is.null(legend)) {
    fg <- placeGrob(fg,legendGrob(c(21,21),legend,cex=leg.mult,fill=c("black","white")),col=1,row=i+1)
  }
  if (print) {
    grid.draw(fg)
  } else {
    ## if we are not printing, return the graphical object
    fg
  }
}



### grid.Vdotplot is what actually draws the plots
### the arguments are explained in the plot.reg function
grid.Vdotplot <- function(Y,Y2=NULL,x=NULL,sp=c(.1,.1),draw=TRUE,refline=0,label.x=NULL,drawRef=TRUE,lwd.fact=0.35,length.arrow=0,pch.size=0.5,h.grid,v.grid,y1y2sep=.1,expand.factor,expand.factor2,aty=NULL,labely=NULL) {
  ## function to plot point estimates
  estimates.grob <- function(x, #x coordinates
                             Y, #Y$estimate has the y coordinates
                             fill="black" #color to fill the symbol
                             ) {
    ## pointsGrob is a grid function
    pointsGrob(x,Y$estimate,pch=21,size=unit(pch.size,"char"),gp=gpar(fill=fill,lwd=lwd.fact))
  }
  ## function to plot confidence intervals
  ci.grob <- function(ylo,yhi,x,lwd=2.5,name="ci",plot.arrow=FALSE) {
    ##do we want the cross hairs at the ends?
    if (plot.arrow) {
      arrow.now <- arrow(angle=90,length=length.arrow,ends="both")
    } else {
      arrow.now <- NULL
    }
    ## use the segmentsGrob function of grid to plot the error bars
    segmentsGrob(x0=x,x1=x,y0=ylo,y1=yhi,
                 default.units="native",
                 name=name,gp=gpar(lwd=lwd)
                 ,arrow=arrow.now
                 )
  }

  if (is.null(aty)) {
    ##  tick-mark not supplied
    
    ## calculate y axis ticks (and labels)
    ## create a vector with all values in the plot
    aty <- unique(c(unlist(Y),unlist(Y2),refline))
    ## if there is a refline, we want to make the plot symmetric around it
    ##aty <- pretty(aty,5,min.n=5,high.u.bias=5)
    if (!is.na(refline)) {
      ##maximum distance
      ##mdist <- max(abs(aty-refline),na.rm=TRUE)    
      aty <- pretty(aty,2,min.n=2,high.u.bias=1)
      ##cat(aty,"is aty a \n")    
      aty <- unique(sort(c(aty,2*refline-aty)))
      ##cat(aty,"is aty b \n")    
    }
    else {
      aty <- pretty(aty,5,min.n=5,high.u.bias=5)
    }
    ## take out the highest and the lowest value, to minimize whitespace
    if (length(aty)>5)  {
      aty <- aty[-c(1,length(aty))]
      r.y <- range.e(aty,expand.factor) ## expand the range, so as to include everything
    } else {
      r.y <- range.e(aty,expand.factor2)
    }
    ##cat(aty,"is aty c \n")
    ##aty <- c(aty,-max(abs(aty)),max(abs(aty)))  
    ##r.y <- range(aty)
    ## make sure we draw the horizontal grid in every interval in the plot
    ## but not outside the plot area
    ## some manual adjustment might be necessary
    aty <- ifelse(aty<min(r.y)|aty>max(r.y),NA,aty)
  } else {
    r.y <- range.e(aty,expand.factor) ## expand the range, so as to include everything
  }
  ## x axis. might have to change this. the default is simply
  ## an index of the models in the x - axis
  ## later we possibly want to make this continuous
  if (is.null(x)) x <- 1:nrow(Y)
  ## save x values
  x.o <- x
  l.x <- length(x)
  ## if there is no label, we create one with the index
  if (is.null(label.x)) label.x <- paste("(",x.o,")",sep="")  
  ## if there is a second set of values create x2=x+e and decrease x to x-e
  if (!is.null(Y2)) {
    x2 <- x+y1y2sep
    x <- x-y1y2sep
  }
  ## horizontal grid
  if (h.grid) {
    hgrid <-   segmentsGrob(x0=unit(rep(0,length(aty)),"npc"),
                            x1=unit(rep(1,length(aty)),"npc"),
                            y0=unit(aty,"native"),
                            y1=unit(aty,"native"),
                            gp=gpar(lty="dotted",lwd=lwd.fact,col="lightgrey"))
  } else {
    hgrid <- NULL
  }
  ## vertical grid
  if (v.grid) {
    vgrid <-   segmentsGrob(y0=unit(rep(0,l.x),"npc"),
                            y1=unit(rep(1,l.x),"npc"),
                            x0=unit(x.o,"native"),
                            x1=unit(x.o,"native"),
                            gp=gpar(lty="dotted",lwd=lwd.fact,col="lightgrey"))
  } else {
    vgrid <- NULL
  }
  
  ## ref line  
  if (drawRef)  {
    refline <- segmentsGrob(x0=unit(0.01,"npc"),x1=unit(.99,"npc"),y0=unit(refline,"native"),y1=unit(refline,"native"),gp=gpar(lwd=1*lwd.fact,lty="dashed",col="grey20"))
  } else {
    refline <- NULL
  }
  ## store ci
  ci1a <- NULL  
  ci1b <- NULL
  ci2a <- NULL
  ci2b <- NULL
  points2 <- NULL
  ## if ncol(Y)=5 there are overlapping CIs. the second one here.
  if(ncol(Y)==5) ci1b <-         ci.grob(Y$hi1,Y$lo1,x,lwd=.8*lwd.fact,name="ci1b",plot.arrow=TRUE)
  ## the first one here.
  ci1a <- ci.grob(Y$hi,Y$lo,x,lwd=1.2*lwd.fact,name="ci1a")
  if (!is.null(Y2)) {
    ## if ncol(Y2)=5 there are overlapping CIs. the second one here.    
    if(ncol(Y2)==5) ci2b <- ci.grob(Y2$hi1,Y2$lo1,x2,lwd=.8*lwd.fact,name="ci2b",plot.arrow=TRUE)
    ## the first one here.    
    ci2a <- ci.grob(Y2$hi,Y2$lo,x2,lwd=1.2*lwd.fact,name="ci2a")
    ## point estimates here
    points2 <- estimates.grob(x2,Y2,fill="white")
  }
  if (is.null(labely)) {
    labely <- aty
    ##print(paste("labely is ",labely,is.null(labely)))    
  }
  gplot <- with(Y,
                ## gTree is a graphical object with the whole plot
                gTree(
                      children=gList(
                        hgrid,vgrid,
                        refline,
                        ci1a,
                        ci1b,
                        estimates.grob(x,Y),
                        ## if Y2
                        ci2a,
                        ci2b
                        ,points2
                        ## box/rectangle around the plot area
                        ##,rectGrob(gp=gpar(lwd=.5*lwd.fact))
                        ## plot x axis if sp2>0 (we name it xaxis, so we can refer to it later)
                        ,if(sp[2]!=0) xaxisGrob(at=x.o,label=label.x,name="xaxis",
                                gp=gpar(cex=0.8*lwd.fact,lwd=0.6*lwd.fact))
                        ## plot x axis with no labels if it is not the bottom plot
                        ,if(sp[2]==0) xaxisGrob(at=x.o,label=rep("",length(x.o)),
                                gp=gpar(cex=0.8*lwd.fact,lwd=0.6*lwd.fact))
                        ## plot y-axis if sp1>0
                        ,if(sp[1]!=0) yaxisGrob(at=aty,label=labely,
                                gp=gpar(cex=0.7*lwd.fact,lwd=0.6*lwd.fact),main=FALSE,name="yaxis")),
                      ## definition of the viewport (plot area)
                      vp=viewport(width=unit(1, "npc"), 
                        height=unit(1, "npc"),
                        xscale=c(0.5,nrow(Y)+0.5),
                        yscale=r.y
                        ##yscale=c(-.1,.1)
                        ,clip=FALSE)
                      ))
  if (draw==TRUE) {
    ##draw the plot
    grid.newpage()
    fg <- frameGrob(layout=grid.layout(2,2,widths=unit(c(sp[1],1-sp[1]),"npc"),heights=unit(c(1-sp[2],sp[2]),"npc")))
    fg <- placeGrob(fg,gplot,col=2,row=1)
    grid.draw(fg)
  } else {
    gplot
  }
}

##function to create legend (adapted from Murrell R Graphics book)
legendGrob <- function(pch, ## what symbol to use
                       labels, ## the text
                       hgap = unit(0.1, "lines"), #horizontal gap                       
                       vgap = unit(0.5, "lines"), #vertical gap
                       default.units = "lines", #default units 
                       vp = NULL, #what viewport to use
                       cex=1, #character expansion
                       fill=NULL)
{
  nkeys <- length(labels)
  gf <- frameGrob(vp = vp)
  for (i in 1:nkeys) {
    if (i == 1) {
      symbol.border <- unit.c(vgap, hgap,
                              vgap, hgap)
      text.border <- unit.c(vgap, unit(0,
                                       "npc"), vgap, hgap)
    }
    else {
      symbol.border <- unit.c(vgap, hgap,
                              unit(0, "npc"), hgap)
      text.border <- unit.c(vgap, unit(0,
                                       "npc"), unit(0, "npc"), hgap)
    }
    gf <- packGrob(gf, pointsGrob(0.5, 0.5,
                                  pch = pch[i],gp=gpar(cex=cex,fill=fill[i])), col = 1, row = i, border = symbol.border,
                   width = unit(1, "lines"), height = unit(1,
                                               "lines"), force.width = TRUE)
 gf <- packGrob(gf, textGrob(labels[i],
                             x = 0, y = 0.5, just = c("left", "centre"),gp=gpar(fontsize=8)),
 col = 2, row = i, border = text.border)
 }
 gf
 }

##function to plot the legend
grid.legend <- function(pch, labels, frame = TRUE,
                         hgap = unit(1, "lines"), vgap = unit(1, "lines"),
                         default.units = "lines", draw = TRUE, vp = NULL) {  
  gf <- legendGrob(pch, labels, frame, hgap,
                    vgap, default.units, vp)
   if (draw)
     grid.draw(gf)
   gf
 }



range.e <- function(x,xp=.1) {
  ##expand the range by a fixed proportion
  r <- range(x,na.rm=TRUE)
  r.e <- (r[2]-r[1])*xp
  c(r[1]-r.e,r[2]+r.e)
}






### These are functions to get coefficients and standard errors from Zelig models
### They are not well documented yet
get.coef <- function(x,level=NULL,SD=NULL,list=FALSE,exclude=NULL,get.se=FALSE) {
  if (is.null(level)&is.null(SD)) stop("must specify either level or set SD to true")
  if (!is.null(level)&!is.null(SD)) stop("must specify only one either level or  SD")  
  res <- NULL
  if (!is.list(x[[1]])) x <- list(x)
  for (i in 1:length(x)) {
    if (x[[i]]$zelig %in%c("ls","logit","probit","ologit")) {
      if (!is.null(SD)) {
        res.now <- sd.coef(x[[i]],get.se=get.se)
      } else {
        res.now <- s.coef(x[[i]],level,get.se=get.se)
      }
    } else if (x[[i]]$zelig %in%c("normal.bayes","logit.bayes","probit.bayes")) {
      res.now <- p.coef(x[[i]]$coefficients,level)
    } else stop("model not supported")
    if (!is.null(exclude)) {
      td <- NULL
      for (e.i in 1:length(exclude)) {
        td <- c(td,grep(exclude[e.i],rownames(res.now)))
      }
      if (length(td)>0)  res.now <- res.now[-td,]
    }
    ##the next line breaks plotReg
    ##i <- ifelse(!is.character(names(x)[i]),i,names(x)[i])
    res <- rbind(res,cbind(i,res.now))
  }
  colnames(res)[1] <- "model"
  covariate <- rownames(res)
  rownames(res) <- NULL
  res <- data.frame(res)
  res$covariate <- covariate
  res$model <- factor(res$model,ordered=TRUE)
  if (list==FALSE) {
      res
    } else {
     mat2list(res)
    }
}

mat2list <- function(res,o.models=NULL,o.covariates=NULL) {
  if (is.null(o.covariates)) o.covariates <- 1:length(unique(res$covariate))
  if (is.null(o.models)) o.models <- 1:length(unique(res$model))
  Y <- vector(mode="list")
  rnames <- reshape(subset(res,select=c(estimate,model,covariate)),idvar="covariate",direction="wide",timevar="model")[,1]
  Y$estimate <- as.matrix(reshape(subset(res,select=c(estimate,model,covariate)),idvar="covariate",direction="wide",timevar="model")[o.covariates,-1])[,o.models]
  rownames(Y$estimate) <- rnames[o.covariates]
  Y$lo <-  as.matrix(reshape(subset(res,select=c(CIlo2,model,covariate)),idvar="covariate",direction="wide",timevar="model")[o.covariates,-1])[,o.models]
  Y$hi <-  as.matrix(reshape(subset(res,select=c(CIhi2,model,covariate)),idvar="covariate",direction="wide",timevar="model")[o.covariates,-1])[,o.models]
  Y$lo1 <-  as.matrix(reshape(subset(res,select=c(CIlo,model,covariate)),idvar="covariate",direction="wide",timevar="model")[o.covariates,-1])[,o.models]
  Y$hi1 <-  as.matrix(reshape(subset(res,select=c(CIhi,model,covariate)),idvar="covariate",direction="wide",timevar="model")[o.covariates,-1])[,o.models]
  colnames(Y$hi1) <- colnames(Y$lo1) <- colnames(Y$hi) <- colnames(Y$lo) <- colnames(Y$estimate) <- NULL  
  Y
}

qsum <- function(x,level=.95)  {
  l.lo <- (1-level)/2
  c(mean(x),
    quantile(x,
             probs=c(
               l.lo,
               1-l.lo)
             )
    )
}

sd.coef <- function(x,get.se=FALSE) {
  s.now <- summary(x)
  estimate <- s.now$coefficients[,1]
  SE <- s.now$coefficients[,2]
  ci.lo <- estimate-SE*1
  ci.hi <- estimate+SE*1
  ci.lo2 <- estimate-SE*2
  ci.hi2 <- estimate+SE*2
  if (get.se) {
    res <- cbind(estimate,ci.lo2,ci.lo,ci.hi,ci.hi2,SE)
    colnames(res) <- c("estimate","CIlo2","CIlo","CIhi","CIhi2","SE")
  } else {
    res <- cbind(estimate,ci.lo2,ci.lo,ci.hi,ci.hi2)
    colnames(res) <- c("estimate","CIlo2","CIlo","CIhi","CIhi2")
  }
  res
}

p.coef <- function(x,level=.95) {
  res <- t(apply(x,2,qsum,level))
  colnames(res) <- c("estimate","CIlo","CIhi")
  res
}

s.coef <- function(x,level=c(.9,.95),get.se=FALSE) {
  r.now <- abs(qnorm((1-level[1])/2))
  s.now <- summary(x)
  estimate <- s.now$coefficients[,1]
  SE <- s.now$coefficients[,2]
  ci.lo <- estimate-SE*r.now
  ci.hi <- estimate+SE*r.now
  r.now <- abs(qnorm((1-level[2])/2))
  ci.lo2 <- estimate-SE*r.now
  ci.hi2 <- estimate+SE*r.now
  if (get.se) {
    res <- cbind(estimate,ci.lo2,ci.lo,ci.hi,ci.hi2,SE)
    colnames(res) <- c("estimate","CIlo2","CIlo","CIhi","CIhi2","SE")
  } else {
    res <- cbind(estimate,ci.lo2,ci.lo,ci.hi,ci.hi2)
    colnames(res) <- c("estimate","CIlo2","CIlo","CIhi","CIhi2")
  }
  res
}


is.zelig.list <- function(x) {
  if (class(x)%in%c("strata","list")) {
    tmp <- table(sapply(x,function(x) x$zelig))
    if (length(tmp)>1) {
      stop("all zelig models must be the same")
    }
  } else {
    stop("x is not a list of zelig models")
  }
}

plot.zelig.list <- function(x,y=NULL,level=.95,rot=0,print=TRUE,...) {
  is.zelig.list(x)
  c1.list <- get.coef(x,level=level,list=TRUE)
  if (is.null(y)) {
    tmp <- plot.reg(c1.list,label.x=names(x),print=FALSE
                    ,...)
  } else {
    is.zelig.list(y)
    c2.list <- get.coef(y,level=level,list=TRUE)
    tmp <- plot.reg(c1.list,c2.list,label.x=names(x),print=FALSE
                    ,...)        
  }
  tmp <- editGrob(tmp,gPath("xaxis","labels"),rot=rot,just="right")
  if (print) {
    grid.draw(tmp)
  } else {
    tmp
  }
}
