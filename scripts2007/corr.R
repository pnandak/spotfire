##this is extensively based on Murrel's functions
  
plot.corr <- function(x,breaks=NULL,reorder=TRUE,colors="Spectral",absc=TRUE) {
  ##colors come from Rbrewer
  ##possible options: 
  ##breaks=seq(floor(min(tmp)*10)/10,floor(max(tmp[tmp!=1])*10)/10,.1)
  if (is.null(breaks)) {
    m1 <- ceiling(max(x[round(x,10)!=1])*10)/10
    breaks <- seq(floor(min(x)*10)/10,m1,.1)
    if (length(breaks)>9) {
      ## check this code
      breaks <- seq(floor(min(x)*5)/5,ceiling(max(x[x!=1])*5)/5,.2)
    }
  }
  require(RColorBrewer)
  if (colors=="gray") {
    colors <- gray((1:(length(breaks)-1))/length(breaks))  
  } else {
    colors <- brewer.pal(length(breaks)-1,colors)
  }
  nr <- nrow(x)
  if (reorder) {
    require(grid)
    require(cba)
    ## highly correlated variables (positively or negatively)
    ## closer together
    if (absc) distx <- dist(abs(x))
    else distx <- dist(x)
    ord <- seriation(distx,method="Optimal")
    x <- x[ord,ord]
  }
  ##if (is.null(colors)) colors <-  gray((((breaks-min(breaks))/diff(range(breaks)))[-length(breaks)]))
    else if (length(colors)!=(length(breaks)-1)) stop ("length of colors must be of length(breaks)-1")
  color.mat <- matrix(
                      as.character(
                                   cut(c(x),breaks=breaks
                                       ,labels=colors,include.lowest=TRUE
                                       ,right=FALSE)
                                   ),nr,nr)
  color.mat[lower.tri(x,diag=TRUE)] <- "white"
  grid.newpage()
  labels <- textGrob((colnames(x)),y=unit(0:nr /nr+ (1/nr)*.4,"npc"),x=unit(0:(nr) / (nr) +1/(nr+1),"npc"),just=c("right","bottom"))
  ribbonlegend <- ribbonLegend(breaks=round(breaks,2),
                               cols=colors, 
                               scale=range(breaks),
                               gp=gpar(cex=0.7),margin=unit(0,"lines"),
                               margin2=unit(1-(nr-1)/nr,"npc"))
  color.mat.borders <- matrix("white",nr,nr)
  color.mat.borders[lower.tri(color.mat.borders,diag=TRUE)] <- "white"
  image <- imageGrob(nr, nr,fill=(color.mat), name="imageGrob",
                     byrow=FALSE,col=color.mat.borders)  
  layout <- grid.layout(1,3,widths=unit(c(.15,.725,.125),c("npc","npc")))
  fg <- frameGrob(layout=layout)
  fg <- placeGrob(fg,image,col=2)
  fg <- placeGrob(fg, ribbonlegend,col=3)
  fg <- placeGrob(fg, labels,col=2)
  grid.draw(fg)  
}

  
calcBreaks <- function(nlevels, breaks, scale) {
  if (is.null(breaks)) {
    seq(min(scale), max(scale), diff(scale)/nlevels)
  } else {
    breaks
  }
}

ribbonVps <- function(nlevels, breaks, margin, scale,margin2) {
  breaks <- format(signif(calcBreaks(nlevels, breaks, scale), 
                          3))
  vpTree(
         viewport(name="layout", layout=
                  grid.layout(3, 4,
                              widths=unit.c(margin, unit(3, "lines"),
                                max(unit(0.8, "lines") + 
                                    stringWidth(breaks)), margin),
                              ##heights=unit.c(margin, unit(1, "null"), margin))),
                              heights=unit.c(margin2, unit(1, "null"),unit(0, "lines") ))),                  
         vpList(viewport(layout.pos.col=2, layout.pos.row=2,
                         yscale=scale, name="ribbon"),
                viewport(layout.pos.col=3, layout.pos.row=2,
                         yscale=scale, name="labels")))
}

ribbonKids <- function(nlevels, breaks, cols, scale) {
  breaks <- calcBreaks(nlevels, breaks, scale)
  nb <- length(breaks)
  ##rescale breaks to fill vp
  breaks.npc <- breaks-min(breaks)
  breaks.npc <- breaks.npc/max(breaks.npc)
  tickloc.npc <- breaks.npc+.01##[-c(1, nb)]
  tickloc <- breaks##[-c(1, nb)]
  ##gList(rectGrob(y=unit(breaks.npc, "npc"),   
  gList(rectGrob(y=unit(breaks.npc[-1], "npc"),
                 height=unit(diff(breaks.npc), "npc"),
                 just="top",
                 ##just="bottom",
                 gp=gpar(fill=cols),
                 vp=vpPath("layout", "ribbon")),
##         segmentsGrob(x1=unit(0.5, "lines"),
##                      y0=unit(tickloc.npc, "npc"),
##                      y1=unit(tickloc.npc, "npc"),
##                      vp=vpPath("layout", "labels")),
        textGrob(x=unit(0.8, "lines"),
                 y=unit(tickloc.npc, "npc"),
                 just="left", 
                 label=format(signif(tickloc, 3)),
                 vp=vpPath("layout", "labels")))
}



ribbonLegend <- function(nlevels=NULL, breaks=NULL, cols, 
                         scale=range(breaks), 
                         margin=unit(0.5, "lines"), margin2=unit(0.5, "lines"),
                         gp=NULL, vp=NULL, name=NULL) {
  gTree(
        nlevels=nlevels, breaks=breaks, cols=cols, scale=scale, 
        children=ribbonKids(nlevels, breaks, cols, scale),
        childrenvp=ribbonVps(nlevels, breaks, margin, scale,margin2=margin2),
        gp=gp, vp=vp, name=name, cl="ribbonLegend")
}

widthDetails.ribbonLegend <- function(x) { 
  sum(layout.widths(viewport.layout(x$childrenvp[[1]]))) 
} 


makeImageRect <- function(nrow, ncol, fill, byrow,col=NULL) {
  xx <- (1:ncol)/ncol
  yy <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(xx, nrow)
    top <- rep(yy, each=ncol)
  } else {
    right <- rep(xx, each=nrow)
    top <- rep(yy, ncol)
  }  
  rectGrob(x=right, y=top, 
           width=1/ncol, height=1/nrow, 
           just=c("right", "top"), 
           gp=gpar(col=col, fill=fill,lwd=0),           
           name="image")
}

imageGrob <- function(nrow, ncol, fill, byrow=TRUE,
                      name=NULL, gp=NULL, vp=NULL,col=NULL) { 
  igt <- gTree(nrow=nrow, ncol=ncol, 
               col=col, byrow=byrow,
               children=gList(makeImageRect(nrow, ncol, 
                 fill, byrow,col)),
               gp=gp, name=name, vp=vp, 
               cl="imageGrob") 
  igt
}

grid.imageGrob <- function(...) {
  igt <- imageGrob(...)
  grid.draw(igt)
}


editDetails.imageGrob <- function(x, specs) { 
  if (any(c("ncol", "nrow", "byrow") %in% names(specs))) { 
    x <- addGrob(x, makeImageRect(x$nrow, x$ncol,
                                  x$cols, x$byrow))
  } 
  if (any(c("cols") %in% names(specs))) { 
    x <- editGrob(x, "image", gp=gpar(fill=x$cols))
  } 
  x 
} 
