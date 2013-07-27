############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  8-18-06                                                           #
# PURPOSE: Example on p. 441 of KNN                                        #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

table11.4<-read.table(file = "C:\\chris\\UNL\\STAT870\\Instructor_CD\\Data Sets\\Chapter 11 Data Sets\\CH11TA04.txt", 
                  header = FALSE, col.names = c("state", "mathprof", "parents", "homelib", "reading", "tvwatch", "absences"), sep = "")
#Check first few observations
head(table11.4)

mod.fit1<-lm(formula = mathprof ~ homelib, data = table11.4)
summary(mod.fit1)
examine.mod.multiple.final(mod.fit.obj = mod.fit1, first.order = 1)  
table11.4[c(4,8,35,36),]


mod.fit2<-lm(formula = mathprof ~ homelib + I(homelib^2), data = table11.4)
summary(mod.fit2)
examine.mod.multiple.final(mod.fit.obj = mod.fit2, first.order = 1)  
table11.4[c(4,8,11,36),]


plot(x = table11.4$homelib, y = table11.4$mathprof, xlab = "homelib (% with 3 or more reading materials)", 
     ylab = "mathprof (mathematics proficiency)", main = "mathprof vs. homelib", 
     panel.first = grid(col = "gray", lty = "dotted"))
curve(expr = mod.fit1$coefficients[1] + mod.fit1$coefficients[2]*x, col = "red", lty = "solid", lwd = 2, add = TRUE,
      from = min(table11.4$homelib), to = max(table11.4$homelib)) 
curve(expr = mod.fit2$coefficients[1] + mod.fit2$coefficients[2]*x + mod.fit2$coefficients[3]*x^2, 
      col = "darkred", lty = "dashed", lwd = 2, add = TRUE, from = min(table11.4$homelib), to = max(table11.4$homelib)) 
identify(x = table11.4$homelib, y = table11.4$mathprof, labels = table11.4$state)
legend(locator(1), legend = c("first-order LS", "second-order LS"), lwd = 2, lty = c("solid", "dashed"), col = c("red", "darkred"), bty = "n")


#See p. 157 of Venables and Ripley (2002)
library(MASS)


#NOTE: SAS will minimize the 21st ordered residual where R will do the 20th (floor((n + 1)/2))
#      so this is why estimates are a little different (SAS gets b0 = 77.75 and b1 = 2.25)
mod.fit.lms<-lqs(formula = mathprof ~ homelib, data = table11.4, method = "lms")
summary(mod.fit.lms)  #Not helpful
mod.fit.lms
names(mod.fit.lms)

mod.fit.lms2<-lqs(formula = mathprof ~ homelib + I(homelib^2), data = table11.4, method = "lms")
mod.fit.lms2
summary(mod.fit.lms2)  #Not helpful

#LTS
mod.fit.lts2<-lqs(formula = mathprof ~ homelib + I(homelib^2), data = table11.4, method = "lts")
mod.fit.lts2


#Just second-order models
plot(x = table11.4$homelib, y = table11.4$mathprof, xlab = "homelib (% with 3 or more reading materials)", 
     ylab = "mathprof (mathematics proficiency)", main = "mathprof vs. homelib", 
     panel.first = grid(col = "gray", lty = "dotted"))
curve(expr = mod.fit2$coefficients[1] + mod.fit2$coefficients[2]*x + mod.fit2$coefficients[3]*x^2, 
      col = "darkred", lty = "dashed", lwd = 2, add = TRUE, from = min(table11.4$homelib), to = max(table11.4$homelib)) 
curve(expr = mod.fit.lms2$coefficients[1] + mod.fit.lms2$coefficients[2]*x + mod.fit.lms2$coefficients[3]*x^2, 
      col = "darkgreen", lty = "dashed", lwd = 2, add = TRUE, from = min(table11.4$homelib), to = max(table11.4$homelib)) 
curve(expr = mod.fit.lts2$coefficients[1] + mod.fit.lts2$coefficients[2]*x + mod.fit.lts2$coefficients[3]*x^2, 
      col = "blue", lty = "dashed", lwd = 2, add = TRUE, from = min(table11.4$homelib), to = max(table11.4$homelib)) 
identify(x = table11.4$homelib, y = table11.4$mathprof, labels = table11.4$state)
legend(locator(1), legend = c("second-order LS", "second-order LMS", "second-order LTS"), lwd = 2, lty = c("dashed", "dashed", "dashed"), 
       col = c("darkred", "darkgreen", "blue"), bty = "n")





#Huber 
mod.fit.huber<-rlm(formula = mathprof ~ homelib, data = table11.4)
summary(mod.fit.huber)
mod.fit.huber2<-rlm(formula = mathprof ~ homelib + I(homelib^2), data = table11.4)
summary(mod.fit.huber2)

#
