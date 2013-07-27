library(lme4);
library(foreign);
library(ggplot2);
library(ltm);

#Part 1: Simple Hierarchical Hox (2003), see also http://www.stat.ucla.edu/~jeroen/multilevel/

myData <- read.spss("popular2.sav", to.data.frame=T);

model1 <- lmer(popular~1+(1|class), data=myData);
model2 <- lmer(popular~extrav+sex+texp+(1|class), data=myData);
model3 <- lmer(popular~extrav+sex+texp+(1+extrav+sex|class), data=myData);
anova(model1,model2,model3);

model4 <- lmer(popular~extrav+sex+texp+(1+extrav|class), data=myData);
anova(model3,model4);

plot(fitted(model4),residuals(model4));
dotplot(ranef(model4,postVar=T));



###### Part 2: Repeated Measures: Sleepstudy Data

summary(sleepstudy);
ggplot(sleepstudy) + aes(Days,Reaction) + geom_point() + geom_smooth(method="lm", se=F) + facet_wrap(~Subject);

fm0 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
fm1 <- lmer(Reaction ~ Days + (1 + Days|Subject), sleepstudy)  # includes covariance between random effects
fm2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy) #no convariance between random effects
anova(fm0, fm1, fm2)

dotplot(ranef(fm1,postVar=T));



###### part 3: Item response theory:

## convert the wide data to long data format.
myData <- LSAT;
longdata <- reshape(myData,  varying=list(1:5), v.names="Correct", direction="long");
names(longdata) <- c("Item","Correct","Subject");

## rasch model:
myModel <- lmer(Correct~(1|Subject)+(1|Item),family=binomial,data=longdata);
dotplot(ranef(myModel,postVar=T))[[2]]; #difficulty parameters

#should be identical to (however, different scaling of parameters):
raschModel <- rasch(LSAT);

#check for difficulty estimates
estimates1 <- ranef(myModel,postVar=T)$Item
estimates2 <- coef(raschModel)[,1];
cor(estimates1,estimates2);

#check for ability estimates
estimates1 <- ranef(myModel,postVar=T)$Subject
estimates2 <- factor.scores(raschModel, resp.patterns = raschModel$X)$score.dat$z1;
cor(estimates1,estimates2);



####### part 4: crossed levels.
#
emotions <- read.csv("emotions.csv");
myModel1 <- lmer(Correct~1+(1|Actor)+(1|Emotion)+(1|Subject)+factor(Intensity), family=binomial,data=emotions);

summary(myModel);
dotplot(ranef(myModel1,postVar=T))[[1]];
dotplot(ranef(myModel1,postVar=T))[[2]];
dotplot(ranef(myModel1,postVar=T))[[3]];

myModel2 <- lmer(Correct~1+(1|Actor)+(1|Emotion)+(1|Subject)+factor(Intensity)+Schizo, family=binomial,data=emotions);
anova(myModel1,myModel2);






