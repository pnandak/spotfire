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
### chunk number 2: PCA-heptathlon-recode
###################################################
data("heptathlon", package = "HSAUR")
heptathlon$hurdles <- max(heptathlon$hurdles) - 
    heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - 
    heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - 
    heptathlon$run800m


###################################################
### chunk number 3: PCA-heptathlon-scatter
###################################################
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])


###################################################
### chunk number 4: PCA-options65
###################################################
w <- options("width")
options(width = 65)


###################################################
### chunk number 5: PCA-heptathlon-cor
###################################################
round(cor(heptathlon[,-score]), 2)


###################################################
### chunk number 6: PCA-optionsw
###################################################
options(width = w$width)


###################################################
### chunk number 7: PCA-heptathlon-pca
###################################################
heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)
print(heptathlon_pca)


###################################################
### chunk number 8: PCA-heptathlon-summary
###################################################
summary(heptathlon_pca)


###################################################
### chunk number 9: PCA-heptathlon-a1
###################################################
a1 <- heptathlon_pca$rotation[,1]
a1


###################################################
### chunk number 10: PCA-heptathlon-scaling
###################################################
center <- heptathlon_pca$center
scale <- heptathlon_pca$scale


###################################################
### chunk number 11: PCA-heptathlon-s1
###################################################
hm <- as.matrix(heptathlon[,-score])
drop(scale(hm, center = center, scale = scale) %*% 
     heptathlon_pca$rotation[,1])


###################################################
### chunk number 12: PCA-heptathlon-s1
###################################################
predict(heptathlon_pca)[,1]


###################################################
### chunk number 13: PCA-heptathlon-pca-plot
###################################################
plot(heptathlon_pca)


###################################################
### chunk number 14: PCA-heptathlon-sdev
###################################################
sdev <- heptathlon_pca$sdev
prop12 <- round(sum(sdev[1:2]^2)/sum(sdev^2)*100, 0)


###################################################
### chunk number 15: PCA-heptathlon-PCscatter eval=FALSE
###################################################
## biplot(heptathlon_pca, col = c("gray", "black"))


###################################################
### chunk number 16: PCA-heptathlon-PCscatter
###################################################
tmp <- heptathlon[, -score]
rownames(tmp) <- abbreviate(gsub(" \\(.*", "", rownames(tmp)))
biplot(prcomp(tmp, scale = TRUE), col = c("black", "lightgray"), xlim =
c(-0.5, 0.7))


###################################################
### chunk number 17: PCA-scorecor
###################################################
cor(heptathlon$score, heptathlon_pca$x[,1])


###################################################
### chunk number 18: PCA-heptathlonscore
###################################################
plot(heptathlon$score, heptathlon_pca$x[,1])


