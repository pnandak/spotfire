avp <- function(ya,
                yp=NULL,
                beta=NULL,
                x=NULL,
                fnform=NULL,
                cutpoints=NULL,
                nperbin=NULL,
                sizefactor=10,
                usr=NULL,
                at=NULL,
                xlab=NULL,
                ylab=NULL,
                cex.lab=1,
                cex.xax=.75,
                cex.yax=.75,
                cex.axis=.75,
                output = list(outfile=NULL,high=4.5,wide=4,type="pdf"),
                lab = list(x = NULL, y=NULL, str=NULL, col=NULL, cex=NULL),
                leg = list(leg=FALSE,corner=1,x=NULL,y=NULL,lty=ltylist,lwd=lwdlist,cex=1,col=collist,str="",calib.x=1,calib.y=1,calib.tsp=1,calib.lsp=1,calib.ysp=1),
                color="gray",
                pch=21,
                collist="gray",
                ltylist="solid",
                lwdlist=1,
                closeplot=T,
                addtoplot=F
                ) {

  output.def <- list(outfile=NULL,high=6,wide=7,type="pdf")
  lab.def <- list(x = NULL, y=NULL, str=NULL, col=NULL, cex=NULL)
  leg.def <- list(leg=FALSE,corner=1,x=NULL,y=NULL,lty=ltylist,lwd=0,cex=1,col=collist,str="",calib.x=1,
                  calib.y=1,calib.tsp=1,calib.lsp=1,calib.ysp=1)
  output <- union.asym.list(output,output.def)
  lab <- union.asym.list(lab,lab.def)
  leg <- union.asym.list(leg,leg.def)



    if (1-addtoplot) {
    
      if (1-is.null(output$outfile)) {
        if (output$type=="pdf") 
          pdf(output$outfile,width=output$wide,height=output$high, pointsize=14)
        if ((output$type=="postscript")||(output$type=="ps")||(output$type=="eps"))
          postscript(output$outfile,paper="letter",pointsize = 14,width=output$wide,height=output$high,horizontal = FALSE, onefile = TRUE)
      }
      
      plot.new()
    
   
    par(usr=usr,tcl=-0.1,mgp=c(2,0.35,0))
    axis(2,at=at$y,labels = T, tick = T, line = 0, outer = F, font = NA, fg = "black",las=1,cex.axis=cex.axis);
    par(usr=usr,tcl=-0.1,mgp=c(2,0.15,0))
    axis(1,at=at$x,labels = T, tick = T, line = 0, outer = F, font = NA, fg = "black",cex.axis=cex.axis);

    title(ylab=ylab,mgp=c(2,.15,0),cex.lab=cex.lab);
    title(xlab=xlab,mgp=c(1.25,.15,0),cex.lab=cex.lab);
    }
    
  # Calculate y-hat if needed
  if (is.null(yp)) {
    if (fnform=="logit") {
      yp = 1/(1+exp(-x%*%as.matrix(beta)))
      pcp = sum( ya*(yp>.5) + (1-ya)*(yp<.5) ) /length(yp)
      print(pcp)
    }
    if (fnform=="linear") {
      yp = x%*%as.matrix(beta)
    }
    if (fnform=="probit") {
      yp = pnorm(x%*%as.matrix(beta))
    }
  }

  
  
    
  # Set up bins
    if (is.null(cutpoints)) {
      if (is.null(nperbin)) {
        nperbin <- trunc(length(ya)/10)
      }
      yp.bin <- NULL
      ya.bin <- NULL
      n.bin <- NULL
      yall <- sortmc(cbind(yp,ya),1)
      yp <- yall[,1]
      ya <- yall[,2]
      done <- 0
      while (1-done) {
          if (length(yp)>nperbin) {
            ya.bin <- c(ya.bin,mean(ya[1:nperbin]))
            yp.bin <- c(yp.bin,mean(yp[1:nperbin]))
            n.bin <- c(n.bin,nperbin)
            ya <- ya[(nperbin+1):length(ya)]
            yp <- yp[(nperbin+1):length(yp)]
          } else {
            ya.bin <- c(ya.bin,mean(ya))
            yp.bin <- c(yp.bin,mean(yp))
            n.bin <- c(n.bin,length(ya))
            done <- 1
          }
        }
    } else {
      yp.bin <- rep(0,length(yp))
      ya.bin <- rep(0,length(ya))
      n.bin <- rep(0,length(ya))
      for (i in 1:length(cutpoints)) {
        cond <- ((yp<cutpoints[i+1]) * (yp>=cutpoints[i]))
        yp.bin[i] <- mean(selif(yp,cond))
        ya.bin[i] <- mean(selif(ya,cond))
        n.bin[i] <- sum(cond)
      }
    }
  
  # Plot with desired color & symbol

  lines(x=c(usr[1],usr[2]),y=c(usr[3],usr[4]))
  points(yp.bin,ya.bin,pch=pch,col=color,cex=sqrt(n.bin)*sizefactor)
  text(yp.bin,ya.bin,labels=n.bin,col=color,cex=cex.lab*.5)

    # Add labels
    if (1-is.null(lab$x))
      {
        for (j in 1:length(lab$x))
          {
                                        #add autoplace option
            text(lab$x[j],lab$y[j],lab$str[j],col=lab$col[j],cex=lab$cex[j]);
          };
      };
    
    if (leg$leg) legend.simple(
                               loc = list(corner=leg$corner,x=leg$x,y=leg$y),
                               type="l",
                               lnes=list(lty=leg$lty,lwd=leg$lwd,cex=leg$cex,col=leg$col,str=leg$str),
                               calib = list(x=leg$calib.x,y=leg$calib.y,tsp=leg$calib.tsp,lsp=leg$calib.lsp,ysp=leg$calib.ysp)
                               )

box()

    if (closeplot) {
        if (1-is.null(output$outfile))
          { dev.off()
            
          }
      }
  }


# Asymetric union of two lists
# duplicate components are taken from list a only 
union.asym.list <- function(a,b)
  {
    if (is.list(a) & is.list(b))
        {
          c <- a;
          uabn <- union(names(a),names(b));
          if (length(uabn)!=length(names(a)))
            {
              for (i in (length(c)+1):(length(uabn)))
                {
                  c <- c(c,NA);
                  names(c)[i] <- uabn[i];
                  c[i] <- b[uabn[i]];
                }
              #c <- c(c,b[setdiff(names(a),names(b))]);
            }
        } else {
          "Error in union.asym.list:  Input not a list";
          stop;
        }
    c;
  }



# Sort a matrix on multiple columns
sortmc <- function(Mat, Sort) 
{ 
  m <- do.call("order", as.data.frame(Mat[, Sort])) 
  Mat[m, ] 
}



# Generic legend
legend.simple <- function(loc = list(corner=1,x=NULL,y=NULL),
                          type="s",
                          syms=list(pch=1,cex=1,col="black",str=""),
                          lnes=list(lty=0,lwd=1,cex=1,col="black",str=""),
                          calib = list(x=1,y=1,tsp=1,lsp=1,ysp=1)
                          )
  {
    loc.def = list(corner=1,x=NULL,y=NULL)
    syms.def = list(pch=1,cex=1,col="black",str="")
    lnes.def = list(lty=0,lwd=0,cex=1,col="black",str="")
    calib.def = list(x=1,y=1,tsp=1,lsp=1,ysp=1)
    loc <- union.asym.list(loc,loc.def)
    syms <- union.asym.list(syms,syms.def)
    lnes <- union.asym.list(lnes,lnes.def)
    calib <- union.asym.list(calib,calib.def)
    
    usr <- par("usr");
    xb <- usr[1]; xe <- usr[2]; xw <- xe-xb;
    yb <- usr[3]; ye <- usr[4]; yw <- ye-yb;
    if (is.null(loc$x)|is.null(loc$y))
      {
        if (loc$corner==1) {
          startx <- 0.05*calib$x*xw + xb;
          starty <- ye - 0.05*calib$y*yw ;
        }
        if (loc$corner==2) {
          startx <- xe - 0.25*calib$x*xw;
          starty <- ye - 0.05*calib$y*yw;
        }
        if (loc$corner==3) {
          startx <- xe - 0.25*calib$x*xw;
          starty <- 0.20*calib$y*yw + yb;
        }
        if (loc$corner==4) {
          startx <- 0.05*calib$x*xw + xb;
          starty <- 0.20*calib$y*yw + yb;
        }
      } else {
        startx <- loc$x;
        starty <- loc$y;
      }

    lsp <- 0.05*(calib$lsp)*xw;
    tsp <- 0.12*(calib$tsp)*xw;
    ysp <- 0.07*(calib$ysp)*yw;
    
    if (type=="s")
      {
        for (i in 1:length(syms$pch))
          {
            points(startx,starty,pch=syms$pch[i],col=syms$col[i],cex=syms$cex[i]);
            text(startx+tsp,starty,labels=syms$str[i],col=syms$col[i],cex=syms$cex[i]);
            starty <- starty-ysp;
          }
        
      }
    if (type=="l")
      {
        for (i in 1:length(lnes$lty))
          {
            lines(c(startx,startx+lsp),c(starty,starty),lwd=lnes$lwd[i],col=lnes$col[i],lty=lnes$cex[i]);
            text(startx+tsp+lsp,starty,labels=lnes$str[i],col=lnes$col[i],cex=lnes$cex[i]);
            starty <- starty-ysp;
          }
      }

    if (type=="b")
      {
        for (i in 1:length(syms$pch))
          {
            points(startx,starty,pch=syms$pch[i],col=syms$col[i],cex=syms$cex[i]);
            text(startx+tsp+lsp,starty,labels=syms$str[i],col=syms$col[i],cex=syms$cex[i]);
            starty <- starty-ysp;
          }
        for (i in 1:length(lnes$lty))
          {
            lines(c(startx,startx+lsp),c(starty,starty),lwd=lnes$lwd[i],col=lnes$col[i],lty=lnes$cex[i]);
            text(startx+tsp+lsp,starty,labels=lnes$str[i],col=lnes$col[i],cex=lnes$cex[i]);
            starty <- starty-ysp;
          }
      }
  };

# Select rows of a matrix based on a condition c
selif <- function(x,c)
{
  x <- cbind(x,t(t(c)));
  w <- x[x[,ncol(x)]!=0];
  dim(w) <- c(length(w)/ncol(x),ncol(x));
  w[,1:(ncol(w)-1)];
};


# Sort a matrix on multiple columns
sortmc <- function(Mat, Sort) 
{ 
  m <- do.call("order", as.data.frame(Mat[, Sort])) 
  Mat[m, ] 
}  
                
