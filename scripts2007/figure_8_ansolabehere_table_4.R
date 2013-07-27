###################################################
###  Ansolabehere
###################################################

## You will need to "source" the file plotReg.R
## (where the main graph functions are)
## in order to produce this plot
## You can either put plotReg.R
## in the path where R is, which you can find by
## typing
getwd()
## or put the full path to the file in the next line
source("plotReg.R")
## if you are connected to the internet you can download it directly
## from our website
source("http://svn.tables2graphs.com/tables2graphs/Rcode/Final%20Code%20for%20Website/plotReg.R")


##point estimates, in a n.variables, n.variables x n.models
coef.matrix <- matrix(c(-.039, NA, .048, -.133, .071, -.795, 1.47, 
                        -.036, NA, .036, -.142, .07, -.834, 1.70, 
                        -.051, NA, .017, .05, .011, -.532, .775, 
                        -.037, -.02, .047, -.131,.072, -.783, 1.45,
                        -.034, -.018, -.035, -.139, .071, -.822, 1.68,
                        -.05, -.023, .016,-.049, .013, -.521, .819),nr=7)
## R2 of the models
R2<-  c(0.910,  0.910,  0.940,  0.910,  0.910,  0.940)

##standard error matrix, n.variables x n.models
se.matrix <- matrix(c(.003, NA, .011, .013, .028, .056, .152, .003, NA, .012, .014, .029, .059, .171, .003, NA,
                      .01, .013, .024, .044, .124, .003, .005, .011, .013, .028, .055, .152, .003, .005, .021, .014,
                      .029, .059, .17, .003,.006, .01, .013, .024, .044, .127),nr=7)

##variable names
varnames<- c("% of county\nregistration", "Law change", "Log population", "Log median\nfamily income",
             "% population with\nh.s. education" ,"% population\nAfrican American" ,"Constant")



##create a csv file with the table, so we can create the latex table with confidence intervals
coef.sheet <- NULL
for(i in 1:7) {
  coef.sheet <- rbind(coef.sheet,coef.matrix[i,])
  ##coef.sheet <- rbind(coef.sheet,se.matrix[i,])
  coef.sheet <- rbind(coef.sheet,paste("[",coef.matrix[i,]-2*se.matrix[i,],",",coef.matrix[i,]+2*se.matrix[i,],"]",sep=""))
}
rownames(coef.sheet) <- rep(varnames,each=2)
rownames(coef.sheet)[seq(2,nrow(coef.sheet),2)] <- ""
colnames(coef.sheet) <- rep(c("Full Sample","Excluding counties\nw. partial registration","Full sample w. \nstate year dummies"),2)
write.csv(coef.sheet,file="Ansolabehere.csv")

##exclude intercept
coef.matrix<-coef.matrix[-(7),]
se.matrix<-se.matrix[-7,]

## each panel has at most six models, plotted in pairs.
## in each pair, solid circles will be the models with "law change" in the specification
## empty circles, those without "law change"

##we are making a list, define it first as empty
Y1 <- vector(length=0,mode="list")
#estimates with law change (in the 4th to 6th columns)
Y1$estimate <- coef.matrix[,4:6]
##95% confidence intervals
Y1$lo <- coef.matrix[,4:6]-qnorm(0.975)*se.matrix[,4:6]
Y1$hi <- coef.matrix[,4:6]+qnorm(0.975)*se.matrix[,4:6]
##90% confidence intervals
Y1$lo1 <- coef.matrix[,4:6]-qnorm(0.95)*se.matrix[,4:6]
Y1$hi1 <- coef.matrix[,4:6]+qnorm(0.95)*se.matrix[,4:6]
##name the rows of Y1 estimate
rownames(Y1$estimate) <- varnames[-7] ##no intercept


#estimates without law change
Y2 <- vector(length=0,mode="list")
Y2$estimate <- coef.matrix[,1:3]
Y2$lo <- coef.matrix[,1:3]-qnorm(.975)*se.matrix[,1:3]
Y2$hi <- coef.matrix[,1:3]+qnorm(.975)*se.matrix[,1:3]
Y2$lo1 <- coef.matrix[,1:3]-qnorm(.95)*se.matrix[,1:3]
Y2$hi1 <- coef.matrix[,1:3]+qnorm(.95)*se.matrix[,1:3]
rownames(Y2$estimate) <- varnames[-7]


##Plot estimates in a single columns
pdf(file="ansolabehere_fig.pdf",width=4,height=9)
## create the graph (do not print it yet)
tmp <- plot.reg(Y1,Y2,#the lists
                #the model labels
                label.x=c("Full Sample","Excluding counties\nw. partial registration",
                  "Full sample w. \nstate year dummies"),
                ## reference lines
                refline=c(0,0,0,0,0,0),
                ## space left in the bottom (for the x-axis labels)
                hlast=.12,
                ## print the graph?
                print=FALSE,
                ## line width / character size multiplier
                lwd.fact=.8,
                ## length of the cross- hairs
                length.arrow=unit(0,"mm"),
                ## legend
                ##legend=c("without law change","with law change"),
                ## widths: variable names, plot size, y-axis
                widths=c(.6,.3,.1),
                ## rotation of the variable name labes
                rot.label.y=0,
                ## justification of the variable name labels
                just.label.y="right",
                ## position (x-axis) of the variable name labels)
                pos.label.y=0.95,
                ## size of the symbol
                pch.size=0.3,expand.factor=.4,expand.factor2=0.1,
                ##legend
                legend=c("With law change dummy","Without law change dummy"),leg.mult=.4
                )
## we rotate the labels of the x-axis 45 degrees. The grid utilities allow
## this modification "on the fly", and it is easy if you are careful at naming the paths
tmp <- editGrob(tmp,gPath("xaxis","labels"),rot=45,just="right")
##tmp is the object we have just created,"xaxis" is the name of element in the object with the x-axis
##elements, and "labels" is the actual object in xaxis that we want to rotate
##just is the justification of the text
grid.draw(tmp) ## print the graph
graphics.off() ## close the graphics windows


### post to webpage

png(file="ansolabehere_fig.png",height=800,width=400,pointsize=1)
## create the graph (do not print it yet)
tmp <- plot.reg(Y1,Y2,#the lists
                #the model labels
                label.x=c("Full Sample","Excluding counties\nw. partial registration",
                  "Full sample w. \nstate year dummies"),
                ## reference lines
                refline=c(0,0,0,0,0,0),
                ## space left in the bottom (for the x-axis labels)
                hlast=.2,
                ## print the graph?
                print=FALSE,
                ## line width / character size multiplier
                lwd.fact=1.1,
                ## length of the cross- hairs
                length.arrow=unit(0,"mm"),
                ## legend
                ##legend=c("without law change","with law change"),
                ## widths: variable names, plot size, y-axis
                widths=c(.6,.3,.1),
                ## rotation of the variable name labes
                rot.label.y=0,
                ## justification of the variable name labels
                just.label.y="right",
                ## position (x-axis) of the variable name labels)
                pos.label.y=0.95,
                ## size of the symbol
                pch.size=0.5,expand.factor=.4,expand.factor2=0.1,
                ##legend
                legend=c("With law change dummy","Without law change dummy"),leg.mult=.4
                )
## we rotate the labels of the x-axis 90 degrees. The grid utilities allow
## this modification "on the fly", and it is easy if you are careful at naming the paths
tmp <- editGrob(tmp,gPath("xaxis","labels"),rot=90,just="right")
##tmp is the object we have just created,"xaxis" is the name of element in the object with the x-axis
##elements, and "labels" is the actual object in xaxis that we want to rotate
##just is the justification of the text
grid.draw(tmp) ## print the graph
graphics.off()
