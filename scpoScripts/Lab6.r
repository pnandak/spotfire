#++++++++++++++++++++++++++++++++++++
#LAB 6
#++++++++++++++++++++++++++++++++++++

#TOPICS COVERED:
#1) APPLY() FAMILY
#2) FUNCTION()
#3) BY()
#4) FOR-LOOP


######################################
# WARM UP
# Loops --SIMPLE ILLUSTRATION
######################################
vec2<-11:20           #VEC2 IS A VECTOR OF 10 ELEMENTS

#EXAMPLE 1
for(i in 1:length(vec2)){
  print(vec2[i])      #PRINT OUT THE VALUE OF iTH ELEMENT
}

a<-NULL                 #CREATE AN EMPTY OBJECT FIRST 
                        #THE RESULT FROM LOOPING WILL BE STORED IN THIS OBJECT

#EXAMPLE 2: 
for(i in 1:length(vec2)){
  a[i]<-vec2[i]+10    #ADD 10 TO EACH ELEMENT
}

#EXAMPLE 3: EXAMPLE 2 is NOT equivalent to EXAMPLE 3!!!
for(i in 1:length(vec2)){
  a[i]<-i+10          #ADD 10 TO EACH iTH INDEX
}

#EXAMPLE 4
for(i in 1:length(vec2)){
  a[i]<-vec2[i]+vec2[i+1]   #1ST ELEMENT+2ND ELEMENT ETC.
}

######################################



#=====================
#I CREATED A FAKE DATASET FOR ILLUSTRATION
#SEE END OF DOCUMENT ON HOW I CONSTRUCT THE DATA
# THIS FAKE DATASET CONTAINS HWK/MIDTERM/FINAL SCORES FOR STUDENTS IN PS3
#=====================


ps3<-read.table("C:/Users/Iris/Documents/PS231A/lab6data.txt")
attach(ps3)


#find out the mean, SD and IQR for each homework
#the slow way is to repeat the calculation for EACH homework, for example:
mean(ps3$hw1, na.rm=T)
mean(ps3$hw2, na.rm=T)
mean(ps3$hw3, na.rm=T)
mean(ps3$hw4, na.rm=T)
mean(ps3$hw5, na.rm=T)

sd(ps3$hw1, na.rm=T)
sd(ps3$hw2, na.rm=T)
sd(ps3$hw3, na.rm=T)
sd(ps3$hw4, na.rm=T)
sd(ps3$hw5, na.rm=T)

IQR(ps3$hw1, na.rm=T)
IQR(ps3$hw2, na.rm=T)
IQR(ps3$hw3, na.rm=T)
IQR(ps3$hw4, na.rm=T)
IQR(ps3$hw5, na.rm=T)


#the efficient way is to use apply and function
#let's extract out the homework scores
hwkscores<-ps3[,3:7]

#WE CAN DO THESE....
mean(hwkscores, na.rm=T)
sd(hwkscores, na.rm=T)
IQR(hwkscores, na.rm=T)  #doesn't work

#0R WE CAN USE APPLY FUNCTION TO GET ALL THE STATS WE NEED
#AND STORE THEM IN ONE OBJECT
# REMEMBER: 2=DO BY COLUMN
hwkstats<-apply(hwkscores, 2, function(hwkscores){
    hwkmean<-mean(hwkscores, na.rm=T)
    hwksd<-sd(hwkscores, na.rm=T)
    hwkiqr<-IQR(hwkscores, na.rm=T)
    list(hwkmean=hwkmean, hwksd=hwksd, hwkiqr=hwkiqr)     
                                      #LIST--ASK R TO RETAIN & OUTPUT RESULTS
    })
    

#to find out the avg score of homeworks for EACH student
#I.E. WE'RE AVERAGING SCORES ACROSS EACH STUDENT
#APPLY (X, 1=BY ROW, MEAN); (X, 2=BY COLUMN, MEAN)
ps3$hwkavg<-apply(hwkscores, 1, function(hwkscores){
    mean(hwkscores, na.rm=T)
    })

attach(ps3)    
#which section does the best?
#to find out the avg homework score BY section
secavg<-tapply(hwkavg, factor(section), function(hwkavg){
  secmean<-mean(hwkavg)
  secsd<-sd(hwkavg)
  seciqr<-IQR(hwkavg)
  list(secmean=secmean, secsd=secsd, seciqr=seciqr)
})
   

#====================================
#by()--GET STATISTICS BY GROUP
#====================================

by(hwkavg, factor(section), mean)

by(hwkavg, factor(section), sd)


#====================================
#LOOP
#====================================

#METHOD 1
#FIND OUT WHO FAILED Homeworks
ps3$failhwk<-ifelse(hwkavg<50, "failed", "passed")

#FIND OUT WHO FAILED MIDTERM
ps3$failmid<-ifelse(midterm<50, "failed", "passed")

#FIND OUT WHO FAILED Final
ps3$failfinal<-ifelse(final<50, "failed", "passed")


#METHOD 2 
#CAN ALSO WRITE A LOOP TO DO THE SAME
#CBIND THE THREE COLUMNS WE NEED--AVG-HWK/MIDTERM/FINAL SCORE

qty3<-cbind(hwkavg, midterm, final) 

# NOW CREATE A EMPTY MATRIX TO STORE THE ANTICIPATED OUTPUT
pfstatus<-matrix(ncol=3, nrow=45)

for (i in 1:3){
pfstatus[,i]<-ifelse(qty3[,i]<50,"failed", "passed")
}

# NOW COMPARE THE RESULTS OF TWO APPROACHES
compare2<-cbind(ps3$failhwk, ps3$failmid, ps3$failfinal, pfstatus)
colnames(compare2)<-c("ifelse-failhwk", "ifelse-failmid", "ifelse-failfinal", "loop-failhwk", "loop-failmid", "loop-failfinal")
compare2[1:15,]
        


   
#===========================
#Revisit Homework 2
#===========================    

library(foreign)
wvs<-read.dta("C:/Users/Iris/Documents/PS231A/wvs2000_v6.dta", convert.factor=FALSE)
dim(wvs) #q1 find dimension

us<-wvs[wvs$v2==11,] #subsetting data

dim(us) #q2

attach(us)

names(us)

#Q4 asks you to create a new variable "socialscale" index based on v27-v31
#see Laura's homework solution

q4<-data.frame(cbind(v27, v28, v29, v30, v31))

#Laura then proceeded to do a table() for each variable
#long way: write table() for each variable
#short cut: use lapply

lapply(q4, table)          #produce five tables in one line!!

#first we have to change the 9 into NA
#we can write 5 ifelse statements ifelse(v27==9, NA, v27) etc...
#or we can just write one function with apply() 

newvar<-apply(q4, 2,function(q4){
        ifelse(q4==9, NA, q4)
        })

compare<-cbind(q4, newvar)        
compare[50:75,]



#there are two ways to create a socialscale
#one is to sum---if there's NA, the respondent will NOT get a score for socialindex. The problem is we end up throwing away cases (not good!)
#a better way: how about an average based on valid responses? 
#i.e. do mean(na.rm=T)

sumsocial<-apply(newvar, 1, sum)
table(sumsocial)

compare<-cbind(q4, newvar, sumsocial)        
compare[50:75,]

#limitation of using sum is that we end up losing 30 cases
sum(is.na(sumsocial))

#a better way is to compute the average
sumsocial2<-apply(newvar, 1, mean)    #doesn't work coz NA

sumsocial2<-apply(newvar, 1, function(newvar){
          mean(newvar, na.rm=T)
          })   

table(sumsocial2)

#for fun--let's count how many NAs per respondent
#using apply()
sumna<-apply(newvar, 1, function(newvar){
      sum(is.na(newvar))
      })

compare<-cbind(q4, newvar, sumna)        
compare[50:75,]




#==========================
#THE SCRIPT I USED TO CREATE THE FAKE PS3 DATA
#==========================


hw1<-round(runif(45, min=20, max=100))
hw2<-round(runif(45, min=30, max=100))
hw3<-round(runif(45, min=25, max=100))
hw4<-round(runif(45, min=10, max=100))
hw5<-round(runif(45, min=15, max=100))
midterm<-round(runif(45, min=0, max=100))
final<-round(runif(45, min=35, max=100))
section<-rep(c(101:103), each=15)
caseid<-1001:1045

ps3<-cbind(caseid, section, hw1, hw2, hw3, hw4, hw5, midterm, final)

ps3[3,3]<-NA
ps3[10,3]<-NA
ps3[11,4]<-NA
ps3[11,4]<-NA
ps3[18,4]<-NA
ps3[18,6]<-NA
ps3[18,7]<-NA
ps3[38,7]<-NA
ps3[41,6]<-NA

write.table(ps3, "C:/Users/Iris/Documents/PS231A/lab6data.txt")

