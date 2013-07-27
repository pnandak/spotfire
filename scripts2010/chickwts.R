## Chickwts.R   Alexander.Ploner@ki.se  2007-09-24
##
## Short analysis of the chicken weight data, see ?chickwts
## Demonstrates how a script file can work

# Move to a prepared directory
olddir = getwd()
setwd("~/USERS/Rcourse/example/chickwts")

# We want to do a Tukey pairwise t-test later
require(multcomp)

# Set up a local copy of the data
# Generally, a load() or attach() or read.table() or similar
data(chickwts)
# Make variables generally available
attach(chickwts)

# Do a linear model
lm_cw = lm(weight ~ feed, chickwts)

# Do the Tukey test
tuk_cw = glht(lm_cw, mcp(feed="Tukey"))

# We store the actual results if we need to process them further
save(lm_cw, tuk_cw, file="chickwtsResult.RData")

# We redirect useful text output to a file
sink("chickwtsOutput.Rout")
print(table(feed))
print(tapply(weight, feed, summary))
print(summary(lm_cw))
print(summary(tuk_cw))
sink()

# We save useful graphs for further study/reporting
pdf("chickwtsDiag.pdf")
par(mfrow=c(2,2))
plot(lm_cw)
dev.off()
pdf("chickwtsSortedBoxplots.pdf", width=8, height=8)
meds  = tapply(weight, feed, median)
smeds = sort(meds)
feed2 = factor(feed, levels=names(smeds), ordered=TRUE)
boxplot(weight ~ feed2, xlab="Feed", ylab="Weight")
dev.off()

# Clean up
detach(chickwts)
setwd(olddir)
