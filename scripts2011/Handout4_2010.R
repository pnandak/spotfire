CrossTable2 <- function(x,y){
require("gmodels")
    xt <- CrossTable(x,y, prop.r=F, prop.t=F, prop.chisq=F, format="SPSS",
        chisq=T)
    print(xt)
}

CrossTable2(Duncan$ed.cat, Duncan$type)


CrossTable2 <- function(a,b){
require("gmodels")
    xt <- CrossTable(x=a,y=b, prop.r=F, prop.t=F, prop.chisq=F, format="SPSS",
        chisq=T)
    print(xt)
}

library(car)
data(Duncan)
mod <- lm(prestige ~ income + education + type, data=Duncan)

robse <- function(model){
    require("car")
    coefs <- coefficients(model)
    rses <- sqrt(diag(hccm(model, "hc0")))
    tstat <- coefs/rses
    pvals <- 2*(1-pt(abs(tstat), model$df.residual))
    mat <- cbind(coefs, rses, tstat, pvals)
    rownames(mat) <- names(coefs)
    colnames(mat) <- c("Coefficient", "Robust_SE", "t-stat", "p-value")
    round(mat,5)
}

robse2a <- function(model){
    require("car")
    coefs <- coefficients(model)
    rses <- sqrt(diag(hccm(model, "hc0")))
    tstat <- coefs/rses
    pvals <- 2*(1-pt(abs(tstat), model$df.residual))
    mat <- cbind(coefs, rses, tstat, pvals)
    rownames(mat) <- names(coefs)
    colnames(mat) <- c("Coefficient", "Robust_SE", "t-stat", "p-value")
    print(summary(model))
    print(round(mat,5))
}

robse2b <- function(model){
    require("car")
    coefs <- coefficients(model)
    rses <- sqrt(diag(hccm(model, "hc0")))
    tstat <- coefs/rses
    pvals <- 2*(1-pt(abs(tstat), model$df.residual))
    mat <- cbind(coefs, rses, tstat, pvals)
    rownames(mat) <- names(coefs)
    colnames(mat) <- c("Coefficient", "Robust_SE", "t-stat", "p-value")
    print(round(mat,5))
    print(summary(model))
}

robsum2a <- robse2a(mod)
robsum2b <- robse2b(mod)


my.array <- array(1:24, dim=c(4,3,2))




robse2c <- function(model){
    require("car")
    coefs <- coefficients(model)
    rses <- sqrt(diag(hccm(model, "hc0")))
    tstat <- coefs/rses
    pvals <- 2*(1-pt(abs(tstat), model$df.residual))
    mat <- cbind(coefs, rses, tstat, pvals)
    rownames(mat) <- names(coefs)
    colnames(mat) <- c("Coefficient", "Robust_SE", "t-stat", "p-value")
    return(list(robust = round(mat,5), orig=summary(mod)))  
}

robsumc <- robse2c(mod)
x <- 2
ifelse(x > 1, "X is greater than 1", "X is not greater than 1")

x <- 0
ifelse(x > 1, "X is greater than 1", "X is not greater than 1")

x <- 2
{if(x > 1){
    "X is greater than 1"
    }
    else{
        "X is not greater than 1"
        }
}



x <- c(-1,0,1,2,3,4)
ifelse(x > 1, "X is greater than 1", "X is not greater than 1")

{if(x > 1){
    "X is greater than 1"
    }
    else{
        "X is not greater than 1"
        }
}





robse <- function(model){
    require("car")
    coefs <- coefficients(model)
    rses <- sqrt(diag(hccm(model, "hc0")))
    tstat <- coefs/rses
    pvals <- 2*(1-pt(abs(tstat), model$df.residual))
    one.star <- which(pvals >= 0.01 & pvals < 0.05)
    two.star <- which(pvals >= 0.001 & pvals < 0.01)
    three.star <- which(pvals < 0.001)
    star.vec <- rep("", length(coefs))
    star.vec[one.star] <- "*"
    star.vec[two.star] <- "**"
    star.vec[three.star] <- "***"
    mat <- data.frame(Coefficients = round(coefs, 5), Robust_SE = round(rses, 5), 
        t_stat = round(tstat, 5), p_value = round(pvals, 5), "sig" = star.vec)
    print(mat)
    cat("* p < 0.05, ** p < 0.01, *** p < 0.001\n")
    invisible(mat)
}

robsum <- robse(mod)
robsum



robse <- function(model, two.sided=T){
    require("car")
    coefs <- coefficients(model)
    rses <- sqrt(diag(hccm(model, "hc0")))
    tstat <- coefs/rses
    {if(two.sided == TRUE){
        pvals <- 2*(1-pt(abs(tstat), model$df.residual))
        }
        else{
            pvals <- (1-pt(abs(tstat), model$df.residual))
        }
    }
    one.star <- which(pvals >= 0.01 & pvals < 0.05)
    two.star <- which(pvals >= 0.001 & pvals < 0.01)
    three.star <- which(pvals < 0.001)
    star.vec <- rep("", length(coefs))
    star.vec[one.star] <- "*"
    star.vec[two.star] <- "**"
    star.vec[three.star] <- "***"
    mat <- data.frame(Coefficients = round(coefs, 5), Robust_SE = round(rses, 5), 
        t_stat = round(tstat, 5), p_value = round(pvals, 5), "sig" = star.vec)
    print(mat)
    {if(two.sided == T){
        cat("* p < 0.05, ** p < 0.01, *** p < 0.001 (two-sided)\n")    
        }
        else{
            cat("* p < 0.05, ** p < 0.01, *** p < 0.001 (one-sided)\n")
        }
    }
    invisible(mat)
}

robsum <- robse(mod)
robsum


1:10
2:4

for(apple in 1:10){
    print(mean(rnorm(100)))
}
set.seed(10)
mat2 <- matrix(rnorm(1000), ncol=10)


colmeans <- rep(NA, 10)
for(lemon in 1:10){
    colmeans[lemon] <- mean(mat2[, lemon])
}

rowmeans <- rep(NA, 100)
for(orange in 1:100){
    rowmeans[orange] <- mean(mat2[orange, ])
}

for(j in 2:ncol(mat2)){
    tmp <- lm(mat2[,1] ~ mat2[,j])
    print(summary(tmp))
}


mylmres <- list()
k <- 1
for(j in 2:ncol(mat2)){
    mylmres[[k]] <- lm(mat2[,1] ~ mat2[,j])
    k <- k+1
}

r2s <- rep(NA, length(mylmres))
for(i in 1:length(mylmres)){
    r2s[i] <- summary(mylmres[[i]])$r.squared
}
r2s


mylmres <- list()
r2s <- rep(NA, length(2:ncol(mat2)))
k <- 1
for(j in 2:ncol(mat2)){
    mylmres[[k]] <- lm(mat2[,1] ~ mat2[,j])
    r2s[k] <- summary(mylmres[[k]])$r.squared
    k <- k+1
}


colmeans <- apply(mat2, 2, mean)
rowmeans <- apply(mat2, 1, mean)


mylmres <- apply(mat2[,2:ncol(mat2)], 2, function(banana)lm(mat2[,1] ~ banana))

r2s <- apply(mat2[,2:ncol(mat2)], 2, function(banana)summary(lm(mat2[,1] ~ banana))$r.squared)

sapply(apply(mat2[,2:ncol(mat2)], 2, function(banana)lm(mat2[,1] ~ banana)), 
    function(x)summary(x)$r.squared)



r2s <- sapply(mylmres, function(x)summary(x)$r.squared)

sapply(apply(mat2[,2:ncol(mat2)], 2, function(banana)lm(mat2[,1] ~ banana)), 
    function(x)summary(x)$r.squared)


library(car)
data(Duncan)

mean(Duncan$education[Duncan$type == "bc"])
mean(Duncan$education[Duncan$type == "prof"])
mean(Duncan$education[Duncan$type == "wc"])

untype <- as.character(unique(Duncan$type))


means <- rep(NA, length(untype))
for(i in 1:length(untype)){
    means[i] <- mean(Duncan$education[Duncan$type == untype[i]])
    names(means)[i] <- untype[i]
}


ag <- aggregate(Duncan$education, list(Duncan$type), mean, na.rm=T)

inc.dum <- cut(Duncan$income, 2)
ag2 <- aggregate(Duncan$education, list(Duncan$type, inc.dum), mean)
ag2


by1 <- by(Duncan$education, list(Duncan$type), mean)

by1.vec <- c(by1)
by1.vec

by2 <- by(Duncan[,c("prestige", "income", "education")], 
    list(Duncan$type), 
    function(x)apply(x, 2, mean))

c(by2)
    
by2a <- as.data.frame(c(by2))

t(as.data.frame(c(by2)))