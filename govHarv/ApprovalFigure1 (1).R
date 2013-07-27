#
# This makes a very nice ApprovalFigure
#

data.approval <- read.table(file="http://jsekhon.fas.harvard.edu/gov1000/approval.asc",header=T)

#print the variables in the "approval" dataframe.
print(names(data.approval))

#print raw approval
print(data.approval$approval)


# Convert the "approval" variable into a time series using the
# function ts().  ts() takes many arguments.  We using the following three variable form:
#1) "data": the variable to convert
#2) "start": the date on which the first observation of the variable begins
#3) "freqency": the number of observations per unit of time (by default year)

approval.ts <- ts(data.approval$approval,start=c(1953, 1),frequency=12)

#let's print approval with time information:
print(approval.ts)

#unemployment with time information
unrate.ts <- ts(data.approval$unrate,start=c(1953, 1),frequency=12)

#inflation with time information
inflation.ts <- ts(data.approval$inflation,start=c(1953, 1),frequency=12)

#the plot() command create the basic figure to which other information
#can be added.  The plot command takes various arguments, we are here
#using only three of them:
#1) "x": the variable to plot
#2) "ylab": the label for the y-axis
#3) "xlab": the label for the x-axis

plot(approval.ts,ylab="Percentage",xlab="Date",ylim=c(0,90));

#let's label when various presidents were in office.  We are going to
#use the text() command.  This command can take various arguments, but
#we use the simplist three argument version:
#1) "x": the x coordinate on the plot to print the text
#2) "y": the y coordinate on the plot to print the text
#3) "labels": the text to be printed

ytext <- 90
text(x=1954,y=ytext,labels="Eisenhower");

#Kennedy
yy <- 85;
xx <- 1961;
text(x=xx+1,y=ytext,labels="Kennedy");

#let's also make an arrow for when Kennedy takes office.  This will
#use the arrrow() command.  This command can take many arguments but we
#use 7 of them:
#1) "x0": the x coordinate to being the arrow
#2) "y0": the y coordinate to begin the arrow
#3) "x1": the x coordinate to END the arrow
#4) "y1": the y coordinate to END the arrow
#5) "length": the size of the arrow head in inches (the default is 0.25)
#6) "col": the color of the line
#7) "lty": the type of line. lty=1 is a solid line, other numbers are various types of dashed lines.

arrows(xx,yy,xx,0,length=0.1,col="blue",lty=2);


#Johnson (November 22, 1963)
xx <- 1963 + 11/12;
arrows(xx,yy,xx,0,length=0.1,col="blue",lty=2);
text(x=xx+2.5,y=ytext,labels="Johnson");

#Nixon resigned on August 8, 1974 (effective the next day)
xx <- 1969;
arrows(xx,yy,xx,0,length=0.1,col="blue",lty=2);
text(x=xx+1,y=ytext,labels="Nixon");

#Ford August 9, 1974
xx <- 1974 + 8/12;
arrows(xx,yy,xx,0,length=0.1,col="blue",lty=2);
text(x=xx+1,y=ytext,labels="Ford");

#Carter
xx <- 1977;
arrows(xx,yy,xx,0,length=0.1,col="blue",lty=2);
text(x=xx+1,y=ytext,labels="Carter");

#Regan
xx <- 1981;
arrows(xx,yy,xx,0,length=0.1,col="blue",lty=2);
text(x=xx+1,y=ytext,labels="Regan");

#Bush
xx <- 1989;
arrows(xx,yy,xx,0,length=0.1,col="blue",lty=2);
text(x=xx+1,y=ytext,labels="Bush");

#Clinton
xx <- 1993;
arrows(xx,yy,xx,0,length=0.1,col="blue",lty=2);
text(x=xx+1,y=ytext,labels="Clinton");

#the lines() command is VERY similar to the plot() command but is used
#to add lines to a prexisting plot.

#let's add unemployment
lines(unrate.ts,col="red",lty=2)

#let's add inflation
lines(inflation.ts,col="black",lty=4)

legend(1953,40,c("app","unemp","infl"),col=c("black","red","black"),lty=c(1,2,4))

#Let's create another plot which makes it easier to compare movements
#in unemployment with movement with approval.  Let's do this by
#changing the scale of unemployment to make it more comparable with
#approval.

#pause before procedding to the next plot
par(ask=T)

#let's pick a scale factor so we can plot unemployment on a scale similar to approval
scale  <- mean(approval.ts)/mean(unrate.ts)
unrate2.ts  <- unrate.ts*scale

plot(approval.ts,ylab="Percentage",xlab="Date",ylim=c(0,100));

#let's add unemployment
lines(unrate2.ts,col="red",lty=2)

#We now need to add an axis for the unemployment scale.  The axis()
#command takes many options, we use 4 of them
#1) "side": which side of the plot to draw the axis 4 is the right side of the plot
#2) "col": the color of the axis
#3) "at": where to plot the ticks on the axis on the scale of the
#   plot() command.  Which in our case means on the scale of approval.
#4) "labels": the labels to add to the axis

#let's add an axis for unemployment on the right.  

#First we need to decide where to make dashes:
dashes    <- c(0,2,4,6,8,10)

#where are these dashes on the scale of approval?
dashes.at <- dashes*scale

axis(4, col="red",at=dashes.at,labels=dashes)

##

# HOMEWORK HINTS 5(a): how long has an administration been in office?  Use
# the cumsum() function.  This is the cummulative sum function.  It
# calculates the cummulative sum of a variable.  For example,
cumsum(data.approval$kennedy)

#So the plot of approval versus time for the kennedy administration
#and Johnson administrations looks like:

#if kennedy admin
kennedy.index  <- data.approval$kennedy==1
johnson.index  <- data.approval$johnson==1

#only select if it is the kennedy OR johnson administration
index  <- data.approval$kennedy==1 | data.approval$johnson==1

x  <- data.approval$kennedy
x[kennedy.index]  <- cumsum(data.approval$kennedy)[kennedy.index]
x[johnson.index]  <- cumsum(data.approval$johnson)[johnson.index]


plot(x[index],data.approval$approval[index], ylab="Approval Percentage",xlab="Time in Months")


# HOMEWORK HINTS 5(c): Does approval move randomly?  Let's plot the
# histogram of the change in approval.  For more information about
# histograms see Wonnacott and Wonnacott.  A histogram is plotting
# using the "hist()" function

change.approval  <- data.approval$approval-data.approval$lagApproval
hist(change.approval)


