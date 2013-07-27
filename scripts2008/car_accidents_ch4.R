#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-29-02                                                    #
# Purpose: Find the logit model for the car accident data           #
#                                                                   #
# NOTES:                                                            #
#####################################################################



n.table<-array(c(350, 26, 150, 23, 60, 19, 112, 80), dim = c(2, 2, 2), 
        dimnames = list(Eject=c("No", "Yes"), 
        Injury=c("not severe", "severe"), Type=c("Collison", "Rollover")))
n.table



#Convert data - BE VERY CAREFUL ABOUT THE ORDER OF THE DATA IN CAR ACCIDENT
type.eject.injury<-as.data.frame(as.table(n.table))
type.eject.table<-xtabs(Freq ~ Eject + Type, data = type.eject.injury)
type.eject<-as.data.frame(type.eject.table)
injury.yes<-type.eject.injury[type.eject.injury$Injury == "severe",]
car.accident<-data.frame(type.eject, severe = injury.yes$Freq)
car.accident


#################################################################

mod.fit.a<-glm(formula = severe/Freq ~ Type + Eject, data = car.accident, weight=Freq, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit.a)

#Check what the X matrix looks like
model.matrix(mod.fit.a)



#Check for conditional independence
mod.fit.o<-glm(formula = severe/Freq ~ Type, data = car.accident, weight=Freq, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit.o)

#Check what the X matrix looks like
model.matrix(mod.fit.o)

round(1-pchisq(mod.fit.o$deviance - mod.fit.a$deviance, mod.fit.o$df.residual - mod.fit.a$df.residual),4)



#########################################################################
#Another way to get data into a data.frame (more difficult)
collision<-c("Collison", "Collison", "Collison", "Collison", 
            "Rollover", "Rollover", "Rollover","Rollover")
eject<-c("No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")
injury<-c("not severe", "not severe", "severe", "severe",
         "not severe", "not severe", "severe", "severe")
count<-c(350, 26, 150, 23, 60, 19, 112, 80)
car.accident<-data.frame(collision, eject, injury, count)
car.accident

#convert back
xtabs(count ~ eject + injury + collision, data = car.accident)


##########################################################################
# Larger table example

n.table<-array(data = c(350, 26, 50, 20, 150, 23, 60, 19, 
    60, 10, 112, 80), dim = c(2,3,2), dimnames=list(Eject = 
    c("No", "Yes"), Injury = c("not severe", "medium severe", 
    "severe"), Type = c("Collison", "Rollover")))

n.table

or.xy.1<-n.table[1,1,1]*n.table[2,2,1]/(n.table[1,2,1]*n.table[2,1,1])
or.xy.2<-n.table[1,1,2]*n.table[2,2,2]/(n.table[1,2,2]*n.table[2,1,2])
or.xy.3<-n.table[1,2,1]*n.table[2,3,1]/(n.table[1,3,1]*n.table[2,2,1])
or.xy.4<-n.table[1,2,2]*n.table[2,3,2]/(n.table[1,3,2]*n.table[2,2,2])
or.xy.5<-n.table[1,1,1]*n.table[2,3,1]/(n.table[1,3,1]*n.table[2,1,1])
or.xy.6<-n.table[1,1,2]*n.table[2,3,2]/(n.table[1,3,2]*n.table[2,1,2])

cat("Estimated ORs for Z=1 is:", round(or.xy.1,2), round(or.xy.3,2), round(or.xy.5,2), "\n")
cat("Estimated ORs for Z=2 is:", round(or.xy.2,2), round(or.xy.4,2), round(or.xy.6,2), "\n")
mantelhaen.test(n.table, correct=F)























#
