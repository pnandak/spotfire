## Using tile to make some scatterplots
## Chris Adolph
## 2/4/2010
##

rm(list=ls())

library(car)
library(tile)

# Load data
data <- read.csv("iver.csv", header=TRUE)
attach(data)

# First, collect all the data inputs into a series of "traces"

# The actual scattered points
trace1 <- scatter(x = enp, # X coordinate of the data
                  
                  y = povred, # Y coordinate of the data
                  
                  labels = cty, # Labels for each point

                  # Plot symbol for each point
                  pch = recode(system,"1=17;2=15;3=16"),

                  # Color for each point
                  col = recode(system,"1='blue';2='darkgreen';3='red'"),

                  # Offset text labels
                  labelyoffset = -0.035,  # on npc scale

                  # Fontsize
                  fontsize = 9,

                  # Marker size
                  size = 1,  # could be vector for bubble plot

                  # Add a robust fit line and CI
                  fit = list(method = "mmest", ci = 0.95),

                  # Which plot(s) to plot to
                  plot = 1
                  )

# The rugs with marginal distributions
rugX1 <- rugTile(x=enp, type="lines", plot = 1)

rugY1 <- rugTile(y=povred, type="lines", plot = 1)

# A legend
legendSymbols1 <- pointsTile(x=  c(1.8,   1.8,       1.8),
                            y=  c(78,     74,      70),
                            pch=c(17,     15,      16),
                            col=c("blue", "darkgreen", "red"),
                            fontsize = 9,
                            size=1,
                            plot=1
                            )

legendLabels1 <- textTile(labels=c("Majoritarian",
                                  "Proportional",
                                  "Unanimity"),
                         x=  c(2.05,      2.05,       2.05),
                         y=  c(78,     74,      70),
                         pch=c(17,     15,      16),
                         col=c("blue", "darkgreen", "red"),
                         fontsize = 9,
                         plot=1
                         )

# Now, send that trace to be plotted with tile
tile(trace1,                # Could list as many traces here as we want
     rugX1,
     rugY1,
     legendSymbols1,
     legendLabels1,
      
     # Some generic options for tile
     RxC = c(1,1),
     height = list(plot="golden"),

     limits=c(1.6, 7.5, 0, 82), # Limits of plotting region
     
     # x-axis controls
     xaxis=list(log = TRUE,
       at = c(2,3,4,5,6,7)
       ),
     
     xaxistitle=list(labels=c("Effective number of parties")),

     # y-axis controls
     yaxistitle=list(labels=c("% lifted from poverty by taxes & transfers")),

     # Plot titles
     plottitle=list(labels=("Party Systems and Redistribution"))
     )



# Try adding another plot.  Note these plots have nothing to do with
# each other.  We're just jamming them together to show it can be
# done using tile

data(Duncan)
attach(Duncan)

# Create labels, symbols, and colors for points
labels <- as.vector(row.names(Duncan))
markers <- (type=="prof")*17 + (type=="wc")*15 + (type=="bc")*16
col <- recode(type,"'prof'='blue';'wc'='green';'bc'='red'")

# Create scatterplot trace
prestigeXeducation <- scatter(x = education,
                              y = prestige,
                              labels = labels,
                              pch = markers,
                              col = col,
                              labelyoffset = -0.035,
                              fontsize = 9,
                              size = 0.5,
                              plot = 2,
                              fit = list(method="mmest")
                              )

# Create legend traces
legendSymbols2 <- pointsTile(x=c(2,      2,       2),
                             y=c(88,     82,      77),
                             pch=c(17,     15,      16),
                             col=c("blue", "green", "red"),
                             fontsize = 9,
                             size = 0.5,
                             plot=2
                             )

legendLabels2 <- textTile(labels=c("Professional",
                                   "White collar",
                                   "Blue collar"),
                          x=c(14,      14,       14),
                          y=c(88,     82,      77),
                          pch=c(17,     15,      16),
                          col=c("blue", "green", "red"),
                          fontsize = 9,
                          size = 0.5,
                          plot=2
                          )

# Create rug traces
rugX2 <- rugTile(x=education, type="dots", plot=2)

rugY2 <- rugTile(y=prestige, type="dots", plot=2)


# Modify some traces from the first plot for appearance
trace1$size <- 0.5
trace1$labelyoffset <- -0.045
legendSymbols1$size <- 0.5
legendLabels1$x <- rep(2.2, 3)


# Plot all traces using tile
tile(trace1,                         # All traces to plot
     prestigeXeducation,             # in any order
     legendSymbols1, legendLabels1,
     legendSymbols2, legendLabels2,                 
     rugX1, rugY1,
     rugX2, rugY2,

     # Output device controls
     output = list(width=10),

     # Limits of plotting region, by plot, as a matrix
     # with one row per plot, and four columns:
     # xmin, xmax, ymin, ymax
     limits=matrix(c(1.6, 7.5, 0, 82,   
                     0, 100, 0, 100),
                   nrow=2,ncol=4,byrow=TRUE),

     # Other inputs
     RxC=c(1,2),
     
     xaxis = list(log1 = TRUE, 
                  at1 = c(2,3,4,5,6,7)
                  ),

     xaxistitle = list(labels1 = "Effective number of parties",
                       labels2 = "Income (% of males making > $3500 in $1950)"
                       ),

     yaxistitle = list(labels1 = "% lifted from poverty by taxes & transfers",
                       labels2 = "Prestige (% rated good, excellent)"
                       ),

     plottitle = list(labels1 = "Parties & Redistribution",
                      labels2 = "Income and Occupational Prestige"
                      ),

     maintitle = list(labels = "Two completely unrelated scatterplots tiled together"),

     # Set aspect ratio as golden rectangle.  Could be a number or "square"
     height=list(plot="golden")
     )


