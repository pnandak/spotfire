##  Chapter 12 
##  Page 211ff
##
## Data from a competition experiment in which the biomass of control plants is compared with biomass of plants grown in conditions where competition was reduced in one of four different ways.
## There are two treatments in which the roots of neighbouring plants were cut (to 5cm depth or 10 cm depth) and two treeatments in which the shoots of neighbouring plants were clipped (25% or 50% of the neighbours cut back to ground level.


comp<-read.table("book/competition.txt",header=T)
attach(comp)
names(comp)
plot(biomass~clipping)
model1<-aov(biomass~clipping)
summary(model1)

summary.lm(model1)

contrasts(clipping)


clip2<-clipping
levels(clip2)
levels(clip2)[4:5]<-"root"
levels(clip2)

model4<-aov(biomass~clip2)
anova(model1,model4)

clip3<-clip2
levels(clip3)[2:3]<-"shoot"
levels(clip3)

model5<-aov(biomass~clip3)
anova(model4,model5)

clip4<-clip3
levels(clip4)[2:3]<-"pruned"
levels(clip4)

model6<-aov(biomass~clip4)
anova(model5,model6)

summary.lm(model6)
