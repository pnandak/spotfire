#Note: For this graph, we first created a Stata dataset, which is available at 
     #http://svn.cluelessresearch.com/tables2graphs/schwindt_table2.dta
   #Download this file and save it into your R working
   #directory (or change your working directory to that where the data is located--e.g.
   #setwd("C:\\Documents and Settings\\Your Name\\My Documents\\....")

library(lattice)
library(foreign)
library(car)

ltheme <- canonical.theme(color = FALSE)     ## in-built B&W theme  
ltheme$strip.background$col <- "lightgrey" ## change strip bg  
lattice.options(default.theme = ltheme)      ## set as default  

##load the stata file with the estimates
data <- read.dta("schwindt_table2.dta")
##looks like this
##        variable count    period      legislature total proportion
## 1   Agriculture    28      1995        Argentina   980 0.02857143
## 10       Health    51      1999        Argentina  1503 0.03393213
## 20 Child/Family    13 1994-1998 Colombia-Chamber   629 0.02066773
## 30  Agriculture    18 1998-2002 Colombia-Chamber   623 0.02889246

##recode the period variable (First Period/Second Period) into a factor ordered in this fashion
##order is important for controlling the ordering of lattice plots
data$period <- factor(recode(data$period,"'1995'='First Period';'1994-1998'='First Period';'1999'='Second Period';'1998-2002'='Second Period'"),ordered=TRUE )

##create a factor out of the legislature variable.
data$legislature <- factor(data$legislature,
                           levels=c("Argentina","Costa Rica","Colombia-Chamber","Colombia-Senate"),
                           ordered=TRUE)

## we relabel the vector, so it displays the years and the N in each year
## the extra spaces centers justification manually
levels(data$legislature) <- c('                       Argentina\n 1995 (N=246/980); 1999 (N=257/1503)',
                              '                            Costa Rica\n1994-1998 (N=57/929); 1998-2002 (N=57/1183)',
                              '                       Colombia Chamber\n1994-1998 (N=139/629); 1998-2002 (N=165/623)',
                              '                       Colombia Senate\n1994-1998 (N=87/620); 1998-2002 (N=94/514)')


##we create the theme variable out of  "variable" in data. It is a factor ordered by the mean proportion of bills
## in each theme across legislatures
data$theme <- factor(data$variable)
data$theme <- reorder(data$theme,data$proportion)

## we want the tick marks and labels in the x-axis
## in these proportions
prop.vec <- c(.01,.02,.04,.08,.16,.32,.64)

## but we are plotting in the log2 scale, so we calculate it
axN <- log2(prop.vec)

## the plot
p <-
  dotplot(theme~proportion|legislature,
          ## the formula is factor.within.panels~variable.toplot|factor.across.panels
          data=data, ## where the variables come from
          groups=period, ## groups are the categories within factor.within.panels
                                        #denoted by color or symbol type
          layout = c(1,4), ## layout: columns,rows
          scales=list(cex=0.65,##relative size of the axis labels
            x=list(at=prop.vec,##where to plot ticks/labels
              log=2##plotting in the log2 scale
              ),alternating=3), #we want axis labels in the top and bottom, so we set alternating to 3
          par.strip.text=list(lines=2.5,cex=0.8),##the width (2.5lines) and relative size of the strip text
          panel=function(...) { ## panel function defines what to do in each panel
            panel.abline(v=axN,col="lightgrey") ## we want vertical lines at the x-axis tick/label marks
            panel.abline(h=1:9,col="lightgrey",lty=2) ## and horizontal lines at the categories within panels
            panel.xyplot(...) ## then we let lattice do the actual plotting
          },
          as.table=TRUE, ## inverts the order (top plot first) if true
          pch=c(1,3) ##the symbols for each category within panels (groups)
          ,cex=0.8,##relative size of the fonts
          xlab="Proportion, plotted in the log_2 scale"##x title,
          ,auto.key=list(space="bottom",columns=2) ## legend at the bottom in two columns
          )

## so far, the plot is in memory, assigned to "p"
##we now open the pdf device, print it and close it
#trellis.device(file="schwindt_fig.pdf",device="pdf",color=FALSE,width=6,height=8) #use this to create pdf
#trellis.device(file="schwindt_fig.png",device="png",color=FALSE,width=600,height=800) #use this to create png
print(p)
#graphics.off()
