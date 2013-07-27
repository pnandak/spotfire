#run R as batch mode
#source("C:/Users/Iris/Documents/PS231A/lab7/Lab7.r", echo=T)

#sink the console to destinated location
sink("C:/Users/Iris/Documents/PS231A/Lab7outMarch21.txt", type="output")


#=========================
#LAB 7
#=========================

#TOPICS COVERED
  #MULTI-LEVEL DATA
  #CONTEXTUAL ANALYSIS
  #MERGE FILES
  #AGGREGATE DATA ACROSS LEVELS


#BACKGROUND
#WE OFTEN START WITH INDIVIDUAL (LOWER) UNIT DATA
#IN ADDITION, WE WANT TO EXAMINE CONTEXTUAL EFFECT
#FOR EXAMPLE, WE ARE INTERESTED IN THE RELATIONSHIP BETWEEN RACE AND TURNOUT
#AT THE INDIVIDUAL LEVEL, ONE CAN EXAMINE THE RELATIONSHIP BETWEEN RACE AND TURNOUT
#IN ADDITION, ONE CAN HYPOTHESIZE THE RACIAL COMPOSITION OF ONE'S NEIGHBORHOOD HAS IMPACT ON TURNOUT
#I.E. TO MODEL:     TURNOUT=RESPONDENT'S RACE + RACIAL COMPOSITION OF NEIGHBORHOOD
#USUALLY WE WANT TO HAVE LOWER GEOGRAPHIC UNIT (e.g. Census Tract) TO PROXY NEIGHBORHOOD
#DUE TO DATA LIMITATION: THIS NES PUBLIC DATA ONLY HAS INFORMATION ON RESPONDENTS'STATE OF RESIDENCE
#WE WILL JUST ADD RACIAL COMPOSITION AT STATE LEVEL FOR ILLUSTRATION

#====================================================
#(Individual level data) NES DATA---2002 ELECTION
#OBTAINED FROM WWW.ICPSR.UMICH.EDU
#====================================================

library(foreign)

nesda<-read.dta("C:/Users/Iris/Documents/PS231A/03740-0001-Data.dta", convert.factor=FALSE)
dim(nesda) #check dimension
names(nesda) #check out the var names

attach(nesda)

#VARIABLE V021202--RESPONDENT'S CURRENT STATE OF RESIDENCE    (codebook p.56)
table(V021201)

#VARIABLE V025016---DID R VOTE IN THE 2002 ELECTION? (CODEBOOK P.340)
#1=voted; 5=did NOT vote
table(V025016)

#create a new variable "turnout"--1=yes; 0=no.

nesda$turnout<-ifelse(V025016==1, 1, 0)

table(nesda$turnout)           #reverse of V025016


#VARIABLE V023150 for race/ethnicity of respondent
table(V023150)

#TOO MANY CATEGORIES, LET'S COLLAPSE INTO 5 GROUPS

library(car)
nesda$race5<-recode(V023150, "1=1; 2=2; 3=5; 4=3; 5=4; 12=1; 13=1; 14=1; 15=1; 23=2; 24=2; 25=2; 34=5; 35=5; 45=5; 77=5")

#new categories: 1) black; 2) asian ; 3) hispanic; 4) white; 5) other/native american

#check my work
table(V023150, nesda$race5)

#LET'S BEGIN WITH SOME BASIC DESCRIPTIVE STATISTICS
#WHAT IS THE RELATIONSHIP BETWEEN RACE AND TURNOUT?
table(nesda$turnout, nesda$race5)             #always check for cell size!!
                                              #as we don't want to draw conclusion based      
                                              #on small n
racetab<-prop.table(table(nesda$turnout, nesda$race5),2)

colnames(racetab)<-c("black", "asian", "hisp", "white", "others")
rownames(racetab)<-c("no", "yes")

#it seems like whites have the highest turnout among all racial groups

#===================================================
#LET'S TURN TO STATE LEVEL RACIAL COMPOSITION
#WANT TO APPEND CENSUS RACIAL COMPOSITION AT STATE LEVEL TO INDIVIDUAL
#DATA OBTAINED FROM CENSUS SF3 FILE
#THE UNIT OF ANALYSIS IS INDIVIDUAL
#===================================================

table(V021201)    #NES only sampled respondents in 48 states

#READ IN CENSUS DATA
library(foreign)
censusda<-read.dta("C:/Users/Iris/Documents/PS231A/lab7/stateracialcomposition.dta" )

#ONE THING TO WATCH OUT--SOME VARIALES MAY BE IN CHARACTER FORMAT INSTEAD OF NUMERIC
#IS.CHARACTER IS A LOGICAL STATEMENT THAT ASKS WHETHER A VARIABLE IS CHARACTER OR NOT 

is.character(censusda[,2])

#SINCE V021201 IS NUMERIC, WE HAVE TO CREATE A NEW VARIABLE THAT IS ALSO NUMERIC to MERGE FILES
censusda$statecode<-as.numeric(censusda[,2])

#NOTE THAT THE TWO DATA MUST BE IN DATA.FRAME MODE
#MERGE (X.DATA, Y.DATA, BY.X=, BY.Y=)
mergeda<-merge(nesda, censusda, by.x="V021201", by.y="statecode")

dim(nesda)  #734 COLUMNS

dim(censusda) #15 COLUMNS

dim(mergeda) #i.e. 734 (columns)+15(new columns)-1(common column)


#EXAMINE WHETHER INDIVIDUAL TURNOUT IS RELATED TO STATE-LEVEL RACIAL COMPOSITION
#IN PARTICULAR, DO WHITES HAVE HIGHER TURNOUT IN MORE RACIALLY HOMOGENEOUS STATE?

attach(mergeda)

#CREATE A NEW VARIABLE TO CAPTURE THE % OF NON-WHITE POPULATION
mergeda$pctmin<-(totalpop-nhwhite)/totalpop

summary(mergeda$pctmin)

library(car)

#DUE TO SMALL N, I CLASSIFY THE STATES INTO 2 GROUPS---HIGH NON-WHITE POP; LOW NON-WHITE POP
mergeda$pctmin2<-recode(mergeda$pctmin, "0:0.25='lo'; 0.2500001:0.8='high'")


table(mergeda$pctmin2, mergeda$V021201)

#EXAMINE TURNOUT BY STATE RACIAL COMPOSITION
tapply(turnout, mergeda$pctmin2, function(turnout) {
  mean(turnout, na.rm=T)
})

#EXAMINE TURNOUT BY STATE RACIAL COMPOSITION & RACIAL GROUPS
table(mergeda$pctmin2, race5, turnout)   #CHECK CELL SIZE

tab2<-tapply(turnout, list(mergeda$pctmin2, race5), function(turnout) {
  mean(turnout, na.rm=T)
})

colnames(tab2)<-c("black", "asian", "hisp", "white", "others")


#==============================
#NOW WE WANT TO REVERSE THE PROCESS
#WE WANT TO AGGREGATE INDIVIDUAL CHARACTERISTICS BY STATE
#OBTAIN THE STATE MEAN TURNOUT LEVEL
#THEN MERGE THE STATE MEANS BACK TO CENSUS FILE
#NOW THE NEW UNIT OF ANALYSIS IS STATE
#==============================

#OBTAIN MEAN TURNOUT LEVEL
statemean<-tapply(turnout, V021201, function(turnout){
    mean(turnout, na.rm=T)
 })

#GET STATE CODES
unistate<-unique(factor(V021201))

#PUT THE STATE CODES AND MEAN TURNOUT LEVEL INTO ONE NEW FILE
statemeanda<-data.frame(unistate, statemean)

#MERGE THE NEW FILE, STATEMEANDA, WITH OUR EXISTING CENSUS FILE
newcensusda<-merge(censusda, statemeanda, by.x="statecode", by.y="unistate")

dim(newcensusda)  #ONLY 48 STATES!! WHILE THE ORIGINAL CENSUS FILE HAS 52 OBSERVATIONS

cbind(newcensusda$geo.name, newcensusda$statemean)

#ASK R TO OUTPUT ALL ROWS IN CENSUS FILE, EVEN THE STATES THAT DON'T EXIST IN NES
newcensusda2<-merge(censusda, statemeanda, by.x="statecode", by.y="unistate", all.x=T)

dim(newcensusda2)  #52 OBSERVATIONS

cbind(newcensusda2$geo.name, newcensusda2$statemean)