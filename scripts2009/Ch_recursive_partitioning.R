###################################################
### chunk number 1: setup
###################################################
rm(list = ls())
if (!file.exists("tables")) dir.create("tables")
set.seed(290875)
options(prompt = "R> ", continue = "+  ",
    width = 63, # digits = 4, 
    SweaveHooks = list(leftpar = function() 
        par(mai = par("mai") * c(1, 1.05, 1, 1))))
HSAURpkg <- require("HSAUR")
if (!HSAURpkg) stop("cannot load package ", sQuote("HSAUR"))
rm(HSAURpkg)
a <- Sys.setlocale("LC_ALL", "C")
book <- TRUE
refs <- cbind(c("AItR", "SI", "CI", "ANOVA", "MLR", "GLM", 
                "DE", "RP", "SA", "ALDI", "ALDII", "MA", "PCA", 
                "MDS", "CA"), 1:15)
ch <- function(x, book = TRUE) {
    ch <- refs[which(refs[,1] == x),]
    if (book) {
        return(paste("Chapter~\\\\ref{", ch[1], "}", sep = ""))
    } else {
        return(paste("Chapter~\\\\ref{", ch[2], "}", sep = ""))
    }
}


###################################################
### chunk number 2: RP-setup
###################################################
library("vcd")
library("lattice")
library("randomForest")
library("party")
ltheme <- canonical.theme(color = FALSE)     ## in-built B&W theme  
ltheme$strip.background$col <- "transparent" ## change strip bg  
lattice.options(default.theme = ltheme) 
mai <- par("mai")
options(SweaveHooks = list(nullmai = function() { par(mai = rep(0, 4)) },
                           twomai = function() { par(mai = c(0, mai[2], 0, 0)) },
                           threemai = function() { par(mai = c(0, mai[2], 0.1, 0)) }))


###################################################
### chunk number 3: RP-Forbes-na
###################################################
data("Forbes2000", package = "HSAUR")
Forbes2000 <- subset(Forbes2000, !is.na(profits))


###################################################
### chunk number 4: RP-Forbes-rpart
###################################################
library("rpart")
forbes_rpart <- rpart(profits ~ assets + marketvalue + sales, 
                      data = Forbes2000)


###################################################
### chunk number 5: RP-Forbes-initial
###################################################
plot(forbes_rpart, uniform = TRUE, margin = 0.1, branch = 0.5, 
     compress = TRUE)
text(forbes_rpart)


###################################################
### chunk number 6: RP-Forbes-cp
###################################################
print(forbes_rpart$cptable)
opt <- which.min(forbes_rpart$cptable[,"xerror"])


###################################################
### chunk number 7: RP-Forbes-prune
###################################################
cp <- forbes_rpart$cptable[opt, "CP"]
forbes_prune <- prune(forbes_rpart, cp = cp)


###################################################
### chunk number 8: RP-Forbes-plot
###################################################
layout(matrix(1:2, nc = 1))
plot(forbes_prune, uniform = TRUE, margin = 0.1, branch = 0.5, 
     compress = TRUE)
text(forbes_prune)
rn <- rownames(forbes_prune$frame)
lev <- rn[sort(unique(forbes_prune$where))]
where <- factor(rn[forbes_prune$where], levels = lev)
n <- tapply(Forbes2000$profits, where, length)
boxplot(Forbes2000$profits ~ where, varwidth = TRUE,
        ylim = range(Forbes2000$profit) * 1.3, 
        pars = list(axes = FALSE), 
        ylab = "Profits in US dollars")
abline(h = 0, lty = 3)
axis(2)
text(1:length(n), max(Forbes2000$profit) * 1.2, 
     paste("n = ", n))


###################################################
### chunk number 9: RP-seed-again
###################################################
set.seed(290875)


###################################################
### chunk number 10: RP-glaucoma-rpart
###################################################
data("GlaucomaM", package = "ipred")
glaucoma_rpart <- rpart(Class ~ ., data = GlaucomaM, 
              control = rpart.control(xval = 100))
glaucoma_rpart$cptable
opt <- which.min(glaucoma_rpart$cptable[,"xerror"])
cp <- glaucoma_rpart$cptable[opt, "CP"]
glaucoma_prune <- prune(glaucoma_rpart, cp = cp)


###################################################
### chunk number 11: RP-glaucoma-plot
###################################################
layout(matrix(1:2, nc = 1))
plot(glaucoma_prune, uniform = TRUE, margin = 0.1, branch = 0.5, 
     compress = TRUE)
text(glaucoma_prune, use.n = TRUE)
rn <- rownames(glaucoma_prune$frame)
lev <- rn[sort(unique(glaucoma_prune$where))]
where <- factor(rn[glaucoma_prune$where], levels = lev)
mosaicplot(table(where, GlaucomaM$Class), main = "", xlab = "", 
           las = 1)


###################################################
### chunk number 12: RP-glaucoma-cp
###################################################
nsplitopt <- vector(mode = "integer", length = 25)
for (i in 1:length(nsplitopt)) {
    cp <- rpart(Class ~ ., data = GlaucomaM)$cptable
    nsplitopt[i] <- cp[which.min(cp[,"xerror"]), "nsplit"]
}
table(nsplitopt)


###################################################
### chunk number 13: RP-glaucoma-bagg
###################################################
trees <- vector(mode = "list", length = 25)
n <- nrow(GlaucomaM)
bootsamples <- rmultinom(length(trees), n, rep(1, n)/n)
mod <- rpart(Class ~ ., data = GlaucomaM, 
             control = rpart.control(xval = 0))
for (i in 1:length(trees))
    trees[[i]] <- update(mod, weights = bootsamples[,i])


###################################################
### chunk number 14: RP-glaucoma-splits
###################################################
table(sapply(trees, function(x) as.character(x$frame$var[1])))


###################################################
### chunk number 15: RP-glaucoma-baggpred
###################################################
classprob <- matrix(0, nrow = n, ncol = length(trees))
for (i in 1:length(trees)) {
    classprob[,i] <- predict(trees[[i]], 
                             newdata = GlaucomaM)[,1]
    classprob[bootsamples[,i] > 0,i] <- NA
}


###################################################
### chunk number 16: RP-glaucoma-avg
###################################################
avg <- rowMeans(classprob, na.rm = TRUE)
predictions <- factor(ifelse(avg > 0.5, "glaucoma", "normal"))
predtab <- table(predictions, GlaucomaM$Class)
predtab


###################################################
### chunk number 17: RP-glaucoma-sens
###################################################
round(predtab[1,1] / colSums(predtab)[1] * 100)


###################################################
### chunk number 18: RP-glaucoma-spez
###################################################
round(predtab[2,2] / colSums(predtab)[2] * 100)


###################################################
### chunk number 19: RP-glaucoma-baggplot
###################################################
library("lattice")
gdata <- data.frame(avg = rep(avg, 2), 
    class = rep(as.numeric(GlaucomaM$Class), 2),
    obs = c(GlaucomaM[["varg"]], GlaucomaM[["vari"]]),
    var = factor(c(rep("varg", nrow(GlaucomaM)), 
                   rep("vari", nrow(GlaucomaM)))))
panelf <- function(x, y) {
           panel.xyplot(x, y, pch = gdata$class)
           panel.abline(h = 0.5, lty = 2)
       }
print(xyplot(avg ~ obs | var, data = gdata, 
       panel = panelf,
       scales = "free", xlab = "", 
       ylab = "Estimated Class Probability Glaucoma"))


###################################################
### chunk number 20: RP-glaucoma-rf
###################################################
library("randomForest")
rf <- randomForest(Class ~ ., data = GlaucomaM)


###################################################
### chunk number 21: RP-glaucoma-rf-oob
###################################################
table(predict(rf), GlaucomaM$Class)


###################################################
### chunk number 22: RP-glaucoma-ctree
###################################################
library("party")
glaucoma_ctree <- ctree(Class ~ ., data = GlaucomaM)


###################################################
### chunk number 23: RP-glaucoma-ctree-plot
###################################################
plot(glaucoma_ctree)


