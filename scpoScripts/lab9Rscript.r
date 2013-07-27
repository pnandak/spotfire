#source("C:/Users/Iris/Documents/PS231A/lab9/lab9Rscript.r", echo=T)

#sink the console to destinated location
sink("C:/Users/Iris/Documents/PS231A/lab9/Lab9outApril18.txt", type="output")

#LAB 9
#TOPICS COVERED
#PART I: ANOVA
#PART II: OLS REGRESSION: BIVARIATE & MULTIVARIATE
#PART III: INTERPRETE REGRESSION OUTPUTS
#PART IV: REGRESSION IN PRACTICE
    #   CREATE DUMMIES
    #   HIGHER ORDER TERM
    #   INTERACTION

#============================
#PART I: ANOVA
#============================

#Laura's handout on refugee camp and children's weight
y1<-c(52,41,52,39)
y2<-c(40,28)
y3<-c(38, 33, 27,33,39)
y4<-c(48,36,38,38,48,49,36,38,47)

depvar<-c(y1,y2,y3,y4)
grp<-rep(c(1,2,3,4), c(4,2,5,9))

summary(aov(depvar~factor(grp)))       #use factor() to indicate grouping

#BSS=432; WSS=576; TSS=432+576=1008
# 42.9% of variance in dep variable can be explained by the grouping

#===============================
#PART II: BIVARIATE REGRESSION
#===============================

#-------------------
#create fake data
#-------------------
set.seed(007)
gpa<-runif(100, 1, 4)
set.seed(777)
error<-rnorm(100, 0,50)
sat<-300*gpa+error

summary(gpa)
summary(sat)

#-------------------
#run regression
#-------------------
#run a bivariate regression (i.e. only one indep variable)
regout<-lm(sat~gpa)

#print out the details
print(summary(regout),digits=3)

#find out what are the outputs
names(summary(regout))

#-------------------
#graphics 
#-------------------

#plot x and y
plot(gpa, sat)

#add regression line to the plot
abline(regout$coef, col="red", lwd=3)

#note the regression line passes through the mean of x and y
abline(v=mean(gpa), col="blue", lwd=3, lty=2)
abline(h=mean(sat), col="green",lwd=3, lty=2)

#-------------------
#examine the residuals
#-------------------

#regout$fit returns the fitted (predicted) values
#regout$residuals returns the residuals (i.e. diff between Y observed and y predicted)
olsexam<-cbind(sat, gpa, error, regout$fit, regout$residuals) 
colnames(olsexam)<-c("True/Observed SAT", "GPA", "Error", "Y-hat", "Residuals")
olsexam[1:5,]

#~~~~~~~~~~~~~~~~
#RECALL
#the true model:
#Sat=300*gpa+error
#ols model:
#Y-hat=-4.33 +301.5*X
#Residuals=Yobserved-Yhat

#~~~~~~~~~~~~~~~~

#-------------------
#using regression to do prediction
#-------------------

xp3<-data.frame(gpa=3)     #feed in x value. ask what's y-hat
predict(regout, xp3)

xp4<-data.frame(gpa=4)     #what's the change in SAT score when GPA increased by 1pt?
predict(regout, xp4)       #301.5, i.e. same size as b

xp2<-data.frame(gpa=2)     #how about GPA dropped to 2?
predict(regout, xp2)       #note: linear change!

xp35<-data.frame(gpa=3.5)    #how about GPA changed from 3 to 3.5?
predict(regout, xp35)-predict(regout, xp3)       #change=301.5*.5


#===================================
#PART III: UNDERSTAND THE REGRESSION OUTPUTS
#===================================
cov(regout$residuals, gpa)     #by construction, these 2 are true
sum(regout$residuals)
mean(regout$residuals)

#calculate regression by hand

#b=cov(x,y)/var(x)
b<-cov(sat,gpa)/var(gpa)

#a=ybar-b*xbar
a<-mean(sat)-b*mean(gpa)

#correlation: cov(x,y)/Sx*Sy
cov(sat,gpa)/(sd(sat)*sd(gpa))

#adjusted R-sq
#Total Sum of Squares=
#Regression Sum of Squares (Explained Sum of Squares)
#Error Sum of Squares (Residual Sum of Squares)

tss<-sum((sat-mean(sat))^2)
ess<-sum((regout$residual-mean(regout$residual))^2)   #Error Sum of Squares; 
                                                      #mean of  residuals=0   
ess2<-sum((sat-regout$fit)^2)                           #which is the same as above
rsq<-1-(ess/tss)                                      #R-square

#how to calculate standard error?

seb<-sqrt(var(regout$residual)/((length(gpa)-2)*var(gpa)))
#t-stat is simply: b/se(b)
b/seb

#===================================
#MULITIPLE REGRESSION 
#===================================

#now add TWO other indep variables
set.seed(999)
hourtv<-runif(100, 0, 10)
set.seed(9999)
male<-round(runif(100,0,1))
summary(hourtv)
sat2<-300*gpa-20*hourtv+5*male+error

satdata<-data.frame(cbind(sat2, gpa, hourtv, male))

regout2<-lm(sat2~gpa+hourtv+male, data=satdata)
summary(regout2)

xp1<-data.frame(gpa=4, hourtv=10, male=1)     #feed in x values. ask what's y-hat
predict(regout2, xp1)

xp2<-data.frame(gpa=4, hourtv=11, male=1)     #what happens when hourtv=1?
predict(regout2, xp2)

#===================================
#PART IV: MULITIPLE REGRESSION IN PRACTICE---REAL DATA
#===================================
#read in data: NES 2001 FILE
#convert.factor=FALSE asks R not to bring over the data dictionary

library(foreign)

nesda<-read.dta("C:/Users/Iris/Documents/PS231A/03740-0001-Data.dta", convert.factor=FALSE)
dim(nesda) #check dimension
names(nesda) #check out the var names

attach(nesda)

#variables we need for this model
#v023010: gwb ft  (dep variable, ranges 0 to 100)
#v023131: education 7 categories
#v023153: sex
#v023036: party id

#create new variables, attach to dataset
nesda$gwbft<-V023010
nesda$educ7<-V023131
nesda$sex<-V023153
nesda$pid<-V023036 #1=dem; 2=rep; 3=ind; 4=otherparty; 5=no pref

attach(nesda)

summary(gwbft)
table(educ7)
table(sex)

#---------------------------------------------------
#CASE 1: WHEN INDEP IS CATEGORICAL
#CREATING DUMMIES FOR CATEGORICAL INDEP VARIABLE
#---------------------------------------------------

#regress gwbft on sex
summary(lm(gwbft~sex))        #hard to interpret since male=1; female=2

#use survey weight
summary(lm(gwbft~sex, weights=V020101))        #hard to interpret since male=1; female=2

#EXAMPLE: BINARY INDEP VARIABLE (2 CATEGORIES)
#two ways to treat binary indep variable: 
#first approach: use factor()
summary(lm(gwbft~factor(sex)))     #sex=1 automatically becomes the reference group

#second approach: change sex to dummy variable
male<-ifelse(sex==1,1,0)

summary(lm(gwbft~male)) #using dummy i created


#ANOTHER EXAMPLE: WHEN INDEP VARIABLE HAS MORE THAN ONE CATEGORY
summary(lm(gwbft~factor(pid)))       #default: 1st category=reference category

#what are these coefficients?
#in bivariate case--these are just the diff in means!
tapply(gwbft, pid, function(gwbft){mean(gwbft, na.rm=T)}) #examine mean gwbft by pid

summary(lm(gwbft~I(pid==1)+I(pid==2)+I(pid==3)+I(pid==4)+I(pid==5)))  #will fail!

#remember to put j-1 dummies: the excluded group is the reference category
summary(lm(gwbft~I(pid==2)+I(pid==3)+I(pid==4)+I(pid==5)))  #works fine!                                                        
                                                        #now DEM is the reference group


#---------------------------------------------------
#CASE 2: WHEN INDEP IS CONTINUOUS
#---------------------------------------------------

#treat education as continuous
summary(lm(gwbft~educ7))      #note R automatically skips NAs


#-------------------------------------------------
#higher order term to capture non-linearity
#-------------------------------------------------
#we have theoretical reason to believe that relationship between edu and gwbft is non-linear
#higher edu tend to be more likely to be rep; yet we know some highly- educated professors/professionals are pro-Democrat
#want to use a quadratic term to capture the curvelinear relationship
#create a higher order term--educ7^2

educ7sq<-educ7^2

summary(lm(gwbft~educ7+educ7sq))      #can either create a new higher order term
summary(lm(gwbft~educ7+I(educ7^2)))    #or use this format to tell R to create it 

#-------------------------------------------------
#interaction between 2 indep variables
#-------------------------------------------------
#say we have two variables: sex and education
#first we find out if these 2 independently affects Y
#sometimes we have reason to believe that interaction of 2 indep variables exists as well
#perhaps men like gwb more; educated people like gwb more; AND educated men like gwb EVEN MORE 

#CASE 1: interaction between education and sex?

summary(lm(gwbft~educ7+male))       
summary(lm(gwbft~educ7+male+educ7*male))      #interaction between educ and male?
summary(lm(gwbft~male+educ7+I(male*educ7)))   #same as above; tell R to create an interaction 
                                               #no stat sig interaction effect

#CASE 2: interaction between education and DEM-identifier?
#create dummy for Dem-identifiers
#for those who identified themselves as Democrats demdummy=1; 0 otherwise

demdummy<-ifelse(pid==1, 1,0)

#check my work
table(pid, demdummy)

#what's the relationship between gwbft and a) education; b) Dem-identifer?
demedu<-lm(gwbft~educ7+demdummy)   
summary(demedu)     #those who are more educated; those who ID with Democrats like GWB less

#ask for prediction
xp1<-data.frame(educ7=6, demdummy=1)   #for those who are DEM & BA grad (educ7=6) 
                                       #what's their predicted gwbft?
predict(demedu, xp1)                   #predicted gwbft=49

#now add interaction term--education and dem-identification
demedu2<-lm(gwbft~educ7+demdummy+educ7*demdummy)      #add interaction term
summary(demedu2)                                      #interaction is stat sig at .01 level!

xp2<-data.frame(educ7=6, demdummy=1)     #repeat the above step
predict(demedu2, xp2)                    #note that the predicted of Y differ from above!
                                         #predicted gwbft=47.6 
                                         #lower than before becoz of the negative interaction


#putting all together into ONE model
finalreg<-lm(gwbft~male+educ7+I(educ7^2)+factor(pid))

xp3<-data.frame(educ7=6, male=0, pid=1)     #what's predicted score for a female DEM, college-edu
predict(finalreg, xp3)                      #yhat=50.6

xp4<-data.frame(educ7=6, male=1, pid=1)     #what if the person is a male instead?
predict(finalreg, xp4)

xp4-xp3   # b for male

maledem<-predict(finalreg, data.frame(educ7=6, male=0, pid=1)) #let's compare 2 college edu males
malerep<-predict(finalreg, data.frame(educ7=6, male=0, pid=2)) #one is DEM, another is REP

maledem-malerep  #diff in predicted gwbft: party identification is far more important than gender



#-------------------------------------------------
#fancy graphics to display your results!!
#-------------------------------------------------
par(mfrow=c(1,2))

#plot graphs
barplot(table(educ7), ylim=c(0,500))

#subset Democrates and Republicans
dems<-nesda[pid==1,]
reps<-nesda[pid==2,]

#plot gwbft by education for DEM 
plot(jitter(dems$educ7), jitter(dems$gwbft), pch=1, col="light blue", xlab="Education 7 categories", ylab="GWB FT")

#overlay gwbft by education for REP on top
points(jitter(reps$educ7), jitter(reps$gwbft), pch=5, col="pink")


#since we're plotting education (x) and gwbft (Y) on a 2-dimensional graph
#we need to fix the other parameters, i.e. male and pid
#to get the predicted values for an average MALE DEM

pred.frame<-data.frame(educ7=seq(1,7,1), male=1, pid=1)
pc<-predict(finalreg, newdata=pred.frame, interval="conf", level=.975)   #change conf interval
matlines(pred.frame$educ7, pc, lty=c(1,3,3), lwd=c(2,2,2),col=c("blue", "black", "black"))

#any party difference?
#to get the predicted values for an average MALE REP

rep.pred.frame<-data.frame(educ7=seq(1,7,1), male=1, pid=2)
rep.pc<-predict(finalreg, newdata=rep.pred.frame, interval="conf", level=.975)
matlines(pred.frame$educ7, rep.pc, lty=c(1,3,3), lwd=c(2,2,2),col=c("red", "black", "black"))





#Reference:
#For a great "book" on regression:
#http://cran.r-project.org/doc/contrib/Faraway-PRA.pdf