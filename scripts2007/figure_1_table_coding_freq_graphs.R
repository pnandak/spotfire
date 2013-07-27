library(foreign)#allow for import of Stata datasets

#note: to run this code, you must first download the dataset "tables2graph.dta" from
   #http://svn.cluelessresearch.com/tables2graphs/tables2graphs.dta, and save it into your R working
   #directory (or change your working directory to that where the data is located--e.g.
   #setwd("C:\\Documents and Settings\\Your Name\\My Documents\\....")
    
article.coding <- read.dta("tables2graphs.dta")
attach(article.coding)

counts <- table(subtag, type, exclude = c("", ""))#create contingency table of type versus 
    #either graph or table

graph.counts <-  counts[,1] #pull out 1st column (graphs) into single vector 
table.counts <- counts[,2]  #pull out 2nd column (tables) into single vectors
all.counts <- graph.counts+table.counts # sum of graphs and tables, by type
all.counts.sorted <- sort(all.counts, decreasing = T) #sort sum, from highest to lowest, by type
graph.counts.sorted <- graph.counts[order(all.counts, decreasing = T)]#sort graph type, by order in total of graphs and tables
table.counts.sorted <- table.counts[order(all.counts, decreasing = T)]#sort table type, ......
#get combined totals and within graphs and tables
total.counts <- sum(all.counts)
total.graphs <- sum(graph.counts)
total.tables <- sum(table.counts)

#create vector for labeling y-axes: place in  order of all counts sorted
print(all.counts.sorted)
label.vec <- c("Summary Stats", "Estimates and Uncertainties",
    "Predicted Values", "Non-numeric", "Other", "Mathematical" 
    )

#2-PLOTS: PERCENTAGE OF ALL GRAPHS AND TABLES COMBINED,
#    GRAPHS/(GRAPHS + TABLES)

#pdf("table_graph_freq.pdf", height = 3.5, width  = 8)#use this command to create pdf
#png(file="table_graph_freq.png",height=500,width=740)#use this to create png for web display
layout(rbind(c(1,2)), #since we only want to include y-axis lables on 1st plot, use layout command to   
    widths = rbind(c(5,3)), respect = F) #create unequal plot regions: (5,3) gives the ratio of width of left plot to right
##layout.show(2) #use this command to see sizes of plot regions; but it must be commented out
    #in order to create PDF

#LEFT PLOT: PERCENTAGE OF ALL GRAPHS AND TABLES
#1) percent of total within each type
per.total <- 100*(all.counts.sorted/sum(counts))#plot this on left graph

y.axis <-(length(all.counts):1)
y.size <- 1
x.size<- 1.2
point.size <-1
par(mar=c(2, 12, 4, 1))

plot(per.total, y.axis, xlim = c(0,
    40), ylim = c(min(y.axis)-.1, max(y.axis) +.1), type = "p", axes = F, xlab = "", ylab = "", pch = 19, 
    main = "", cex = point.size, xaxs = "i")
segments(0,y.axis,40,y.axis,lty="dotted")#draw lines
box(bty = "o")
axis(1, at=seq(0,40, by=10), labels = c(0, 10, 20, 30, "40%"),
    cex = 1, las = 1, tick = T, cex.axis = x.size,
    line = 0, mgp = c(2,.7,0))
axis(2, at = y.axis, labels = label.vec, tick = T, las  = 1, 
    line =0, cex.axis = y.size,  mgp = c(2,.7,0)) #hadj centers axis labels
mtext("Percentage of Graphs\nand Tables Combined,\nby Category", 3, line = .8, cex =1, font = 2)

#RIGHT PLOT: GRAPHS/(GRAPHS+TABLES)
graphs.per.all <- 100*(graph.counts.sorted/(graph.counts.sorted+table.counts.sorted))
par(mar=c(2, 1, 4, 2))

plot(graphs.per.all, y.axis, ylim = c(min(y.axis)-.1, max(y.axis) +.1), 
    xlim = c(0,100) ,type = "p", axes = F, xlab = "", ylab = "", pch = 19, 
    main = "", cex = point.size, xaxs = "i")
segments(0,y.axis,100,y.axis,lty="dotted")#draw lines
box(bty = "o")
axis(1, at=seq(0,100, by=25), labels = c(0, 25, 50, 75, "100%"), 
    cex = 1, las = 1, tick = T, cex.axis = x.size,
    line = 0, mgp = c(2,.7,0))
axis(2, at = y.axis, labels = F, tick = F)  
mtext("Percentage of\nGraphs Within\nEach Category", 3, line = .8, cex =1, font = 2)
#add duplicate points for dots that fall right on axes (using xpd = TRUE)
    #mathematical (1) = 100
    #estimates and uncertainties (2) = 0?????
points(graphs.per.all[6],1, xpd = T, pch = 19)#mathematical
points(graphs.per.all[2],5, xpd = T, pch = 19)#estimates and uncertainties

#dev.off() #use this to end creation of pdf or png
