#SUBSETTING AND INDEXING ARE TWO IMPORTANT SKILLS FOR SUCCESSFUL PROGRAMMING

#read in data
data<-read.table("C:/Users/Iris/Documents/PS231A/lab4data.txt")

attach(data)
#print 5th-10th observations, all columns
data[5:10,]

#print 5th-10th observations, only 2nd-3rd column
#two ways to do that: either use indexing
data[5:10, 2:3]
#or to use column names
data[5:10, c("partyid", "job approval")]

#find out which cases have NA for job approval
is.na(data[,3])
#count up how many NA
sum(is.na(data[,3]))
#find out the position of the NA
which(is.na(data[,3]))        #remember this tells you the position;
                              #hence "7" == 7th case!!

#now ask R to print out the ENTIRE record for the case(s) that has NA in the job approval variable
data[is.na(data[,3]),]

#ask R to print out cases that have the value "3" for the partyid variable
data[data[,2]==3,]

#alternative way--using variable name instead
data[partyid==3,]

#ask R to print out cases that do NOT have the value "3" for the partyid variable
data[data[,2]!=3,]

#alternative way--using variable name instead
data[partyid!=3,]

#ask R to print out the ENTIRE record for the cases that give >50 in the GWBft variable
data[data[,4]>50,]      #print out 12 rows

#Note that the enitre record gets NA when GWBft=NA
#To eliminate the NA row, add the extra condition !is.na
data[data[,4]>50 & !is.na(data[,4]),]      #print out 11 rows

#ask R to print out JUST the caseid and partyid for the cases that give >50 in the GWBft variable
data[data[,4]>50 & !is.na(data[,4]), c("caseid", "partyid")]     #print out 11 rows

#find cases where partyid=3
data[partyid==3,]
#print out observations where partyid=3 and job approval=7
data[partyid==3 & job.approval==7,]     #note R also prints out the row where job.approval=NA
data[partyid==3 & job.approval==7 &!is.na(job.approval),]     #this takes care of the extra row

#find cases where partyid=3 and job.approval has values between 3 and 5
#also to get rid of the case where job.approval=NA
data[partyid==3 & job.approval>=3 & job.approval<=5 & !is.na(job.approval),]

#find cases where partyid=3 OR job.approval=3
# | means "or"
data[partyid==3 | job.approval==3,]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#APPENDIX
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#HERE'S HOW I CREATED THE DATASET FOR DEMONSTRATION

row1<-round(runif(20, 1,5),0)
row2<-round(runif(20,1,9),0)
row3<-round(runif(20, 0, 100), 0)
caseid<-101:120

data<-data.frame(cbind(caseid, row1, row2, row3))
colnames(data)<-c("caseid", "partyid", "job approval", "GWBft")

attach(data)

data[,3]<-ifelse(data[,3]==9, NA, data[,3])
data[4,4]<-NA

write.table(data, "C:/Users/Iris/Documents/PS231A/lab4data.txt")
