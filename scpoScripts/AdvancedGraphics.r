
#------------
#BOXPLOT
#------------
#create example
country<-c("Botswana", "Ghana", "Guinea", "Kenya", "Malawi", "Nigeria"
, "Sudan", "Swaziland", "Tanzania", "Togo", "Uganda", "Zimbabwe",
"Zambia")
mscore<-c(42.5,37.3,64.8,58.8,49.5,81.8,80.8,72.6,49,74.5,79.6,39.7,59.6)
type<-c(0,0,0,1,0,1,1,0,0,0,1,1,0)
data<-cbind(country, mscore, type)

data
     country     mscore type
 [1,] "Botswana"  "42.5" "0"
 [2,] "Ghana"     "37.3" "0"
 [3,] "Guinea"    "64.8" "0"
 [4,] "Kenya"     "58.8" "1"
 [5,] "Malawi"    "49.5" "0"
 [6,] "Nigeria"   "81.8" "1"
 [7,] "Sudan"     "80.8" "1"
 [8,] "Swaziland" "72.6" "0"
 [9,] "Tanzania"  "49"   "0"
[10,] "Togo"      "74.5" "0"
[11,] "Uganda"    "79.6" "1"
[12,] "Zimbabwe"  "39.7" "1"
[13,] "Zambia"    "59.6" "0"


#change this one...i'm not sure which is 0
type3<-ifelse(type==0, "No Ethnic War", "Ethnic War")

#boxplot
boxplot(mscore~type3)

#add means
means<-tapply(mscore, type3, mean)
points(1:2, means, pch=19)


#---------------------
#DOTPLOT
#---------------------
#create data
carbrand<-c("corolla", "civic", "matrix", "torus", "focus", "accord", "A4", "BMW 3-series")
country<-c("japanese", "japanese","japanese","domestic",  "domestic", "japanese","european", "european")
price<-c(15,16,18,18.5, 14,21,33,35)

cartemp<-data.frame(cbind(carbrand, country))     #these columns are "character"
cardata<-data.frame(cbind(cartemp, price))        #price is numeric


#simple dotplot
dotchart(cardata[,3], labels=cardata[,1], main="car prices")

#sort by price
sortprice<-cardata[order(cardata$price),]
dotchart(sortprice[,3], labels=sortprice[,1], main="sort by price")

#have different color for each group
                                                      #first assign col=black for every case
color1<-ifelse(sortprice[,2]=="japanese", "red","black")     #if country="japanese", assign col=red
color2<-ifelse(sortprice[,2]=="domestic", "blue",color1)     #now if country="domestic", assign col=blue
color3<-ifelse(sortprice[,2]=="european", "green",color2)    #now if country="european", assign col=green

#examine the coding:
table(sortprice[,2], color3)   #see how colors are assigned for diff. country type

dotchart(sortprice[,3], labels=sortprice[,1], groups=sortprice[,2], main="sort by price", color=color3)


#---------------------
#SCATTERPLOT MATRIX
#---------------------

library(lattice)

x1<-rnorm(100, 5,1)
x2<-0.3*x1+rnorm(100)
x3<-0.75*x1+rnorm(100)
y<-0.6*x1+0.4*x2+0.3*x3+rnorm(100)

spdata<-data.frame(cbind(y,x1,x2,x3))

splom(~spdata[1:4])

#fit loess curve (non-parametric fit)
splom(~spdata[1:4], panel=function (x, y)
{panel.xyplot(x,y); panel.loess(x,y)}
)


#---------------------
#3D PLOT
#---------------------
# 3D Scatterplot
library(scatterplot3d)
scatterplot3d(x1,x2,y, main="3D Scatterplot")

#to add vertical lines
scatterplot3d(x1,x2,y, pch=16, highlight.3d=TRUE, type="h",main="3D Scatterplot")

#add vertical lines 
s3d<-scatterplot3d(x1,x2,y, pch=16, highlight.3d=TRUE, type="h",main="3D Scatterplot")

#fit a plane
#without the vertical lines
s3d<-scatterplot3d(x1,x2,y, highlight.3d=FALSE,main="3D Scatterplot")
fitplane<-lm(y~x1+x2)
s3d$plane3d(fitplane)

