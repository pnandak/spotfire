##
## Rcourse06.R
##
## Demonstration code for "Modern Applied Statistics Using R"
##
## Alexander.Ploner@ki.se 2007-10-15
##
##########################################################################


##---------------- Multiple testing ----------------------------##

# Set up data; see lecture for details
attach("~/stockholm-U133A-50%.Rdata")
# What we got?
ls(2)
dim(stk.x)                 # Expression data
str(stk.clinical)          # Phenotypic/clinical data
status = stk.clinical$stat # TRUE = dead/relapse within 5 years
table(status)

# Do this fast: compute gene-wise t-statistics
# OCplus available at Bioconductor - requires akima (CRAN)
require(OCplus)
tt = tstatistics(stk.x, status)[,1]
# The corresponding p-values under normal theory
pp = 2*pt(-abs(tt), nrow(stk.clinical)-2)

# Show it graphically
par(mfrow=c(1,2))
hist(tt, main="t-statistics", xlab="", prob=TRUE, ylim=c(0, 0.5), xlim=c(-5,5))
curve(dnorm(x), add=TRUE, lwd=2, col="red")
hist(pp, main="p-values", prob=TRUE, xlab="")

# Testing CDC2
# Magic knowledge: this gene id is CDC2
ndx = which(rownames(stk.x)=="203213_at")
CDC2   = stk.x[ndx,]
t.test(CDC2~status, var.equal=TRUE)

# How many genes "significant" at the usual levels?
table(pp<=0.05)
table(pp<=0.01)

# More magic: assumes package hgu133a (Bioconductor) installed
# This part is not really relevant for multiple testing, but it shows
# how biological information can be added to the naked expression data
ndx = rank(pp)<=10     # top ten genes
require(hgu133a)
symb = mget(rownames(stk.x), hgu133aSYMBOL)  # get names
tg = data.frame(Names=unlist(symb[ndx]), tstat=round(tt[ndx],2), pval=round(pp[ndx],6))
tg = tg[order(-abs(tg$tstat)),] # sort the table
tg

# Multiplicity correction of p-values: Bonferroni, Bonferroni-Holmes, FDR
pval = pp
pval.bonf = p.adjust(pp, "bonferroni")
pval.bonf = pmin(pp*length(pp), 1)     # equivalent!
pval.holm = p.adjust(pp, "holm")       # default correction
pval.fdr  = p.adjust(pp, "fdr")
# Compare
table(pval < 0.05)       # Too many!
table(pval.bonf < 0.05)  # Very few - too few?
table(pval.holm < 0.05)  # Same
table(pval.fdr < 0.05)   # More - different level of confidence, though

# Again the old magic: table of 10 top-raking genes
ndx = rank(pp) <= 10
tg = data.frame(Names=unlist(symb[ndx]), tstat=round(tt[ndx],2),
                pval=round(pval[ndx],6), 
                bonf = round(pval.bonf[ndx],3),
                holms=round(pval.holm[ndx],3),
                fdr = round(pval.fdr[ndx],3))
tg = tg[order(-abs(tg$tstat)),]
tg

# Compare graphically; note how Bonferroni and Bonferroni-Holm are 
# totally the same
plot(abs(tt), -log10(pval), xlab="|t-statistic|", ylab="-log10(p-value)")
points(abs(tt), -log10(pval.bonf), col="red")
points(abs(tt), -log10(pval.holm), col="blue", pch=19, cex=0.5)
points(abs(tt), -log10(pval.fdr), col="green")
abline(h=-log10(0.05), lty=2)
legend("topleft", legend=c("Raw p-values","Bonferroni","Holm","FDR"),
col=c("black","red","blue","green"), pch=20)


##----------------- Multiple testing: permutation based --------------------##

# Compute the global FDR based on permutation null distribution 
# (no normality assumption)
set.seed(3144) # Controlled random permutations
# An unusual function that *both* computes and plots
# This may compute a while
eoc = EOC(stk.x, status, main="", sensitivity.show=FALSE)
# What have we computed?
str(eoc)
eoc[1:10,]  # t-stats, p-values *and* FDRs
# Graphical decoration: add the t-statistics to the x-axis
rug(abs(eoc[,1])) 

# Shortlist: the genes at FDR < 0.01
topDE(eoc, 0.01)
# Estimated proportion of genes *not* differentially 
# expressed (true null hypotheses)
p0(eoc)

# Short sanity check: how do the permutation-based pvalues relate to the 
# p-values computed under assumption of normality?
plot(pval, eoc[,"pvalue"], log="xy") # logarithmically scaled 
abline(0,1) # good agreement, but "normal" pvalues systematically smaller!


# Now local fdr compared to global FDR above
# this should limit the influence of strongly regulated genes
set.seed(3144)
res1d = fdr1d(stk.x, status, p0=p0(eoc)) # Use the same p0 for comparison
# What we got?
summary(res1d)
plot(res1d)
# Fewer top-genes!
topDE(res1d,0.01)



