###################################################
### Using R as a calculator: 
###################################################
2*10
10 + 13 - 21; 2^3
1 - 2*3
(1 - 2)*3


###################################################
### Assignment: 
###################################################
x <- 5
x
(x <- 5)
10*x + 2
x
x <- 2
x
x < -5


###################################################
### Functions: 
###################################################
date
date()
ourFUN <- function(x) {
   x + 5
}
ourFUN(x = 2)
anotherFUN <- function(x, y = 5) {
   x + y
}
anotherFUN(x = 2)
anotherFUN(x = 2, y = 7)
lastFUN <- function(z, ...) {
   ourFUN(z) - anotherFUN(z, ...)
}
lastFUN(z = 2)
lastFUN(z = 2, y = 10)


###################################################
### Vectors: 
###################################################
c(2, 3, 5, 2, 7, 1)
c(TRUE, FALSE, FALSE, TRUE)
c('cat', 'dog', 'bird', 'horse')
x<-c(2, NA, 5, NA, NA, 7)
y<-c(10,15,12)
c(y, x)
c(1:3, 'cat')
seq(5)
seq(-5)
seq(from = 2, to = 10)
2:10
seq(from = 10, to = 3)
10:3
5.7:20.2
10.5:3.25
seq(from= -1, to = 1, by = 0.2)
seq(from = 9, to = 1, by = -2)
seq(from = 5.7, to = 11.2, by = 0.4)
seq(from = 2, to = 15, length = 7)


###################################################
### Data frames: 
###################################################
ourdf <- data.frame(id = 101:110, 
   sex = sample(c("M", "F"), size = 10, replace=TRUE),
   age = sample(20:50, size=10, replace=TRUE),
   tx = sample(c("Drug", "Placebo"), size=10, replace=TRUE),
   diabetes = sample(c(TRUE, FALSE)))
ourdf


###################################################
### Read in motivating data set: 
###################################################
pbc <- read.table("pbc.csv", header = TRUE, 
   sep = ",", na.strings = "")


###################################################
### Attributes of the read-in data set: 
###################################################
dim(pbc)
names(pbc)

library(Hmisc)
contents(pbc)

head(pbc)


###################################################
### Factors: 
###################################################
5:1
factor(5:1)
c("cat", "horse", "dog", "cat", "dog", "dog")
factor(c("cat", "horse", "dog", "cat", "dog", "dog"))
factor(c(TRUE, NA, FALSE, TRUE, FALSE, FALSE, NA))
mean(factor(1:5))

factor(c(3, 3, 1, 2, 2), levels = 1:4)

factor(c("White", "Black", "Other", "White", "Other", "Black"))
factor(c("White", "Black", "Other", "White", "Other", "Black"),
   levels = c("White", "Black", "Other"))

factor(c(1, 1, 2, NA, 1, 2), labels = c("Case", "Control"))


###################################################
### Factor practice exercise 1: 
###################################################
x <- c("A", "SA", "D", "D", "SA", "SA", "SA", "A", "N", "SD")
y <- factor(x, levels = c("SD", "D", "N", "A", "SA"), 
   labels = c("Strongly disagree", "Disagree", "Neutral", 
      "Agree", "Strongly agree"))
y


###################################################
### Ordered factors: 
###################################################
x <- c("A", "SA", "D", "D", "SA", "SA", "SA", "A", "N", "SD")
y <- factor(x, levels = c("SD", "D", "N", "A", "SA"), 
   labels = c("Strongly disagree", "Disagree", "Neutral", 
      "Agree", "Strongly agree"),
   ordered = TRUE)
y
y < "Neutral"


###################################################
### More on factors: 
###################################################
test <- factor(c("positive", "negative", "negative"))
levels(test)
levels(test) <- c("positive", "negative")
levels(test)
levels(test) <- list(Positive = "positive", Negative = "negative")
test
levels(test) <- list(Undetermined = "Undetermined",
   Positive = "Positive", Negative = "Negative")
test
levels(test) <- list(Combined = c("Undetermined", "Positive"),
   Negative = "Negative")
test


###################################################
### Practice exercise 2: 
###################################################
z <- y
levels(z) <- list("Disagree" = c("Strongly disagree", "Disagree"),
   "Neutral" = "Neutral", "Agree" = c("Agree", "Strongly agree"))
z


###################################################
### Hmisc package upData() function: 
###################################################
pbc <- upData(pbc,
   lowernames = TRUE,
   stage = factor(stage, levels = 1:4),
   ageyrs = age/365.25,
   fuyrs = fudays/365.25,
   censored = factor(status),
   labels = c(ageyrs="Age", 
      fuyrs = "Follow Up",
      censored = "Collapsed Survival Status", 
      fudays = "Follow Up",
      status="Original Survival Status",
      drug = "Treatment",
      sex = "Gender",
      age = "Age",
      ascites = "Presence of Ascites", 
      bili = "Serum Bilirubin",
      chol = "Serum Cholesterol",
      album = "Serum Albumin",
      stage = "Histological stage of disease"),
   units = c(fudays = "days",
      fuyrs = "years",
      age = "days",
      ageyrs = "years",
      bili = "mg/dL", chol = "mg/dL", album = "mg/dL"),
   levels = list(status = c("Censored", 
         "Censored due to liver treatment", "Dead"),
      censored = list("Censored" = c(0,1), "Dead" = 2),
      drug = c("D-penicillamine", "Placebo")))
contents(pbc)


###################################################
### Alternative upData() invocation: 
###################################################
pbc <- read.table("pbc.csv", header = TRUE,  sep = ",", na.strings = "", stringsAsFactors = FALSE)
contents(pbc)
pbc <- upData(pbc,
   lowernames = TRUE,
   status = factor(status, levels = 0:2, labels = c("Censored", "Censored due to liver treatment", "Dead")),
   censored = status,
   drug = factor(drug, levels = 1:2, labels = c("D-penicillamine", "Placebo")),
   sex = factor(sex, levels = c("Female", "Male")),
   ascites = factor(ascites, levels = c("No", "Yes")),
   stage = factor(stage, levels = 1:4),
   ageyrs = age/365.25,
   fuyrs = fudays/365.25,
   labels = c(ageyrs="Age", 
      fuyrs = "Follow Up",
      censored = "Collapsed Survival Status", 
      fudays = "Follow Up",
      status="Original Survival Status",
      drug = "Treatment",
      sex = "Gender",
      age = "Age",
      ascites = "Presence of Ascites", 
      bili = "Serum Bilirubin",
      chol = "Serum Cholesterol",
      album = "Serum Albumin",
      stage = "Histological stage of disease"),
   units = c(fudays = "days",
      fuyrs = "years",
      age = "days",
      ageyrs = "years",
      bili = "mg/dL", chol = "mg/dL", album = "mg/dL"),
   levels = list(censored = list("Censored" = c("Censored", "Censored due to liver treatment"), "Dead" = "Dead")))
contents(pbc)


###################################################
### Tables of categorical variables: 
###################################################
with(pbc, table(sex))
with(pbc, table(sex, drug))
with(pbc, table(sex, drug, censored))

with(pbc, ftable(sex, drug, censored))
with(pbc, ftable(sex, censored, drug))

with(pbc, table(signif(chol, digits = 1), exclude = NULL))
with(pbc, table(as.character(drug), exclude = NULL))
with(pbc, table(stage, exclude = 4))
with(pbc, table(as.character(stage), exclude = 4))


###################################################
### Table practice exercise: 
###################################################
with(pbc, table(fuyrs > 50, status))


###################################################
### Univariate summary statistics: 
###################################################
with(pbc, mean(ageyrs))
with(pbc, median(ageyrs))
with(pbc, min(ageyrs))
with(pbc, max(ageyrs))
with(pbc, range(ageyrs))
with(pbc, sd(ageyrs))
with(pbc, quantile(ageyrs))
with(pbc, quantile(ageyrs, probs = seq(0, 1, by = 0.1)))

with(pbc, mean(chol))
with(pbc, mean(chol, na.rm = TRUE))


###################################################
### Univariate summary stats Practice Exercise: 
###################################################
with(pbc, mean(ageyrs, na.rm = TRUE) - 1.96*sd(ageyrs, na.rm = TRUE))
with(pbc, mean(ageyrs, na.rm = TRUE) + 1.96*sd(ageyrs, na.rm = TRUE))


###################################################
### More on missing values: 
###################################################
x <- c(9, 5, 12, NA, 2, NA, NA, 1)
x + 5
x > 2
x == NA

is.na(x)
x > 2 & !is.na(x)


###################################################
### More on missing values Practice Exercise: 
###################################################
with(pbc, table(is.na(ascites)))


###################################################
### Group-wise summary stats of continuous variables: 
###################################################
summary.formula(bili ~ censored + sex, data = pbc,
   method = "response")
summary.formula(bili ~ censored + sex, data = pbc,
   method = "response", fun = range)
summary.formula(bili ~ censored + sex, 
   data = pbc, method = "response",
   fun = function(x) {
   c(Median = median(x), Min = min(x), Max = max(x))
   })
summary.formula(chol ~ drug + ascites, data = pbc,
   method = "response", fun = median)
summary.formula(fuyrs ~ drug + ascites, data = pbc,
   method = "response", fun = median)


###################################################
### Group-wise summary stats of continous variables Practice Exercise: 
###################################################
summary.formula(ageyrs ~ sex + censored + stage, 
   data = pbc, method = "response",
   fun = function(x) {round(quantile(x), 2)})


###################################################
### Group-wise summary stats of continuous variables, cont'd: 
###################################################
summary.formula(bili ~ ascites, 
   data = pbc, method = "cross", fun = median)
summary.formula(bili ~ drug + censored + sex, 
   data = pbc, method = "cross",
   fun = function(x) {
      c(Mean = mean(x), SD = sd(x),
      Median = median(x), Min = min(x), Max = max(x))
   })
summary.formula(bili ~ censored + sex, 
   data = pbc, method = "cross",
   fun = function(x) {
      c(Mean = mean(x), SD = sd(x),
      Median = median(x), Min = min(x), Max = max(x))
   })


###################################################
### Automatic summaries of both continuous and categorical variables: 
###################################################
summary.formula(sex ~ ageyrs + chol + drug + stage, 
   data=pbc, method="reverse")
summary.formula(sex ~ ageyrs + chol + drug + stage, 
   data=pbc, method="reverse", overall = TRUE)
summary.formula(~ ageyrs + chol + drug + stage + sex, 
   data=pbc, method="reverse")
summary.formula(sex ~ ageyrs + chol + drug + stage, test = TRUE,
   data=pbc, method="reverse")

print(summary.formula(sex ~ ageyrs + chol + drug + stage, 
   overall = TRUE, method="reverse", data=pbc), 
   digits = 3, npct = "both", pctdig = 2,
   exclude1 = FALSE, long = TRUE)


###################################################
### Histogram: 
###################################################
with(pbc, hist(ageyrs, xlab = label(ageyrs),
   main = paste("Histogram of", label(ageyrs))))


###################################################
### Boxplot: 
###################################################
with(pbc, boxplot(album, ylab = label(album), 
   main = paste("Boxplot of", label(album))))


###################################################
### Boxplot with stripchart: 
###################################################
with(pbc, {
   boxplot(album, outline = FALSE,
      ylab = label(album),
      main = paste("Boxplot of", label(album)))
   stripchart(album, vertical = TRUE, method = "jitter", 
      pch = 1, add = TRUE)
})


###################################################
### Side-by-side boxplot with stripchart: 
###################################################
with(pbc, {
   boxplot(album ~ stage, outline = FALSE, varwidth = TRUE, 
      xlab = label(stage), ylab = label(album),
      main = paste("Boxplot of", label(album), "by\n",
         label(stage)))
   stripchart(album ~ stage, method = "jitter", pch = 1,
      vertical = TRUE, add = TRUE)
})


###################################################
### Pairs plot: 
###################################################
pairs(subset(pbc, select = c(ageyrs, bili, chol, album)))


###################################################
### Scatterplot: 
###################################################
with(pbc, plot(album ~ bili, 
      xlab = label(bili), ylab = label(album),
      main = paste("Plot of", label(album), "vs.",label(bili))))


###################################################
### Scatterplot with plsmo() curve: 
###################################################
with(pbc, {
   plot(album ~ bili, 
      xlab = label(bili), ylab = label(album),
      main = paste("Plot of", label(album), "vs.",  label(bili)))
   plsmo(x = bili, y = album, add = TRUE)
})


###################################################
### Barplot: 
###################################################
with(pbc, barplot(prop.table(table(censored)),
   ylim = c(0, 1.0), xlab = label(censored),
   ylab = "Proportion", main = paste("Barplot of", label(censored)))) 


###################################################
### Dotplot: 
###################################################
with(pbc, dotchart(prop.table(table(censored)),
   lcolor = "black",
   xlim = c(0, 1.0), xlab = "Proportion", 
   main = paste("Dotplot of", label(censored))))


###################################################
### Two-way barplot: 
###################################################
with(pbc, barplot(prop.table(table(censored, drug)),
   beside = TRUE, legend.text = levels(pbc$censored),
   ylim = c(0, 1.0), xlab = label(censored),
   ylab = "Proportion", 
   main = paste("Barplot of", label(censored), "\n across",
       label(drug)))) 


###################################################
### Two-way dotplot: 
###################################################
with(pbc, dotchart(prop.table(table(censored, drug)),
   lcolor = "black",
   xlim = c(0, 1.0), xlab = "Proportion", 
   main = paste("Dotplot of", label(censored), "\n across",
      label(drug))))


###################################################
### Alternative two-way dotplot: 
###################################################
with(pbc, dotchart(prop.table(table(censored, drug))[, "D-penicillamine"],
   lcolor = "black",
   xlim = c(0, 1.0), xlab = "Proportion", 
   main = paste("Dotplot of", label(censored), "\n across",
      label(drug))))
par(new = TRUE)
with(pbc, dotchart(prop.table(table(censored, drug))[, "Placebo"],
   lcolor = "black",
   xlim = c(0, 1.0), pch = 4))
legend(x = 1, y = 0.25, xjust = 1, yjust = 0, 
   legend = levels(pbc$drug), pch = c(1, 4))


###################################################
### Scatterplot with different points and lines: 
###################################################
with(pbc, plot(album ~ chol, 
   type = "n", xlab = label(chol), ylab = label(album),
   main = paste("Plot of", label(album), "vs. \n",
      label(chol), "across", label(drug))))
with(subset(pbc, drug == "D-penicillamine"), 
   points(album ~ chol, pch = 4))
with(subset(pbc, drug == "Placebo"), 
   points(album ~ chol))
with(pbc, {
   plsmo(chol, album, group = drug, add = TRUE,
      lty = c(1,2))
   legend(x = 1100, y = 2, xjust = 1, yjust = 0,
      legend = levels(drug), lty = c(1,2), 
      pch = c(4, 1))
})


###################################################
### Multiple plots: 
###################################################
xrange <- with(pbc, range(chol, na.rm = TRUE))
yrange <- with(pbc, range(album, na.rm = TRUE))
par(mfrow = c(1, 3), oma = c(0, 0, 5, 0))
with(subset(pbc, drug == "D-penicillamine"), {
   plot(album ~ chol, pch = 16,
      xlim = xrange, 
      ylim = yrange,
      xlab = label(chol), ylab = label(album),
      main = levels(drug)[1])
   plsmo(chol, album, add = TRUE)
})
with(subset(pbc, drug == "Placebo"), {
   plot(album ~ chol, pch = 16,
      xlim = xrange, 
      ylim = yrange,
      xlab = label(chol), ylab = label(album),
      main = levels(drug)[2])
   plsmo(chol, album, add = TRUE)
})
with(pbc, {
   plot(album ~ chol, pch = 16,
      xlim = xrange, 
      ylim = yrange,
      xlab = label(chol), ylab = label(album),
      main = "Overall")
   plsmo(chol, album, add = TRUE)
   mtext(text = paste("Plot of", label(album), "vs. \n",
      label(chol), "across", label(drug)), 
      side = 3, outer = TRUE)
})
par(mfrow = c(1, 1), oma = c(0,0,0,0))


