####################################################################
## setup code for hw4, ps151b 2006
####################################################################

## read data
data <- read.csv(file="03209-0001-Data.csv",
                 header=T)

## some re-coding to make the homework go easier
## V132: The member states of the European Union should have one
## common foreign policy towards countries outside the European Union
## 1 is yes
## 2 is no
## NA is DK

data$V132 <- factor(data$V132,
                    levels=c(1,2,NA),
                    labels=c("Yes","No","DK"),
                    exclude=NULL)

## nationality
data$V6 <- factor(data$V6,
                  levels=1:17,
                  labels=c("France",
                    "Belgium",
                    "The Netherlands",
                    "Germany (West)",
                    "Italy",
                    "Luxembourg",
                    "Denmark",
                    "Ireland",
                    "United Kingdom",
                    "Greece",
                    "Spain",
                    "Portugal",
                    "Germany (East)",
                    "Norway (not included)",
                    "Finland",
                    "Sweden",
                    "Austria"))


## now, a cross-tabulation
tab <- xtabs(~ V6 + V132,
             data=data,
             drop.unused.levels=TRUE)
             

## this should be enough to get you started...
