
rm(list=ls())

file <- "iver.csv";
data <- read.csv(file,header=TRUE);
attach(data)

y <- povred
x <- lnenp




# Here's an effort at a color lightener that could use work
lighten <- function(col,
                    pct=0.75,
                    alpha=1){
  if (abs(pct)>1) {
    print("Warning:  Error in Lighten; invalid pct")
    pcol <- col2rgb(col)/255
  } else {
    col <- col2rgb(col)/255
    if (pct>0) {
      pcol <- col + pct*(1-col)
    } else {
      pcol <- col*pct
    }
  }
  pcol <- rgb(pcol[1],pcol[2],pcol[3],alpha)
  pcol
}

# Sort a matrix on multiple columns
sortmc <- function(Mat, Sort, decreasing=F) 
{
  if (decreasing) direction <- -1 else direction <- 1
  m <- do.call("order", as.data.frame(direction*Mat[, Sort,drop=FALSE]))
  Mat[m, ,drop=FALSE] 
}

# Helper functions:  MM-estimator fitting
mmest.fit <- function(y,x,ci) {
  require(MASS)
  dat <- sortmc(cbind(y,x),2,decreasing=F)
  x <- dat[,2]
  y <- dat[,1]
  result <- rlm(y~x,method="MM")
  print(result)
  fit <- list(x=x)
  fit$y <- result$fitted.values
  fit$lower <- fit$upper <- NULL
  if (length(na.omit(ci))>0)
    for (i in 1:length(ci)) {
      pred <- predict(result,interval="confidence",level=ci[i])
      fit$lower <- cbind(fit$lower,pred[,2])
      fit$upper <- cbind(fit$upper,pred[,3])
    }
  fit
}

library(grid)

pdf("testgrid_sub.pdf",horizontal=FALSE,width=5,height=5)

usr <- c(log(1.5),log(8),20,100)

overlay <- grid.layout(nrow=3,
                       ncol=2,
                       widths=c(1,5),
                       heights=c(1,5,1),
                       respect=T)

pushViewport(viewport(layout=overlay)
             )


pushViewport(viewport(layout.pos.col=2,
                      layout.pos.row=1,
                      xscale=c(0,1),
                      yscale=c(0,1),
                      gp=gpar(fontsize=12),
                      name="xtitle",
                      clip="on"
                      )
             )

grid.text("Main title",
          x=unit(0.5,"npc"),
          y=unit(0.5,"npc"),
          gp=gpar(fontface="bold")
          )


upViewport(1)

pushViewport(viewport(layout.pos.col=1,
                      layout.pos.row=2,
                      xscale=c(0,1),
                      yscale=c(0,1),
                      gp=gpar(fontsize=12),
                      name="ytitle",
                      clip="on"
                      )
             )

grid.text("Y-axis label",
          x=unit(0.15,"npc"),
          y=unit(0.5,"npc"),
          rot=90
          )



upViewport(1)

pushViewport(viewport(layout.pos.col=2,
                      layout.pos.row=3,
                      xscale=c(0,1),
                      yscale=c(0,1),
                      gp=gpar(fontsize=12),
                      name="xtitle",
                      clip="on"
                      )
             )

grid.text("X-axis label",
          x=unit(0.5,"npc"),
          y=unit(0.25,"npc")
          )


upViewport(1)




pushViewport(viewport(layout.pos.col=2,
                      layout.pos.row=2,
                      xscale=c(usr[1],usr[2]),
                      yscale=c(usr[3],usr[4]),
                      gp=gpar(fontsize=12),
                      name="mainplot",
                      clip="on"
                      )
             )

grid.xaxis(at = c(0,0.5,2,4,5,6,8), # Where to put ticks
                   label = TRUE,      # Argh!  Only takes logical values
                   main = TRUE#,       # Top (TRUE) or bottom (FALSE)
#                   gp = gpar(),       # Any gpars to change
                   )
#grid.draw(xaxis)

grid.rect(gp=gpar(linejoin="round"))

upViewport(1)


pushViewport(viewport(layout.pos.col=2,
                      layout.pos.row=2,
                      xscale=c(usr[1],usr[2]),
                      yscale=c(usr[3],usr[4]),
                      gp=gpar(fontsize=12),
                      name="mainplot",
                      clip="off"
                      )
             )


yaxis <- yaxisGrob(at = c(20,40,60,80,100),   # Where to put ticks
          label = TRUE,      # Argh!  Only takes logical values
          main = TRUE        # Top (TRUE) or bottom (FALSE)
          #gp = gpar(),      # Any gpars to change
          )
grid.draw(yaxis)

xaxis <- xaxisGrob(at = log(c(2,4,5,6)),   # Where to put ticks
          label = TRUE,      # Argh!  Only takes logical values
          main = TRUE        # Top (TRUE) or bottom (FALSE)
          #gp = gpar(),      # Any gpars to change
          )

xaxis <- editGrob(xaxis,
                  gPath("labels"),
                  label=c(2,4,5,6)
                  )

grid.draw(xaxis)

upViewport(1)

                      
dev.off()


