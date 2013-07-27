

# ------------------------------------ #
# Housing starts data.
#
# large R^2 meaningless
# missing variable the cause of autocorelated errors.
# ------------------------------------ #

housing<-read.table("HousingStarts.txt",header=TRUE,sep="\t")
attach(housing)
plot(P,H, pch=17,cex=1.5)

lm1 <- lm(H~P)
summary(lm1)
png("housing2,png",height=600,width=800)
plot(rstandard(lm1), pch=17,cex=1.5)
dev.off()

library(car)
durbin.watson(lm1, alternative="positive")

pairs(housing)
lm2 <- lm(H~P+D)
summary(lm2)
png("housing3,png",height=600,width=800)
plot(rstandard(lm2),pch=17, cex=1.5)
dev.off()

durbin.watson(lm2, alternative="positive")


# ------------------------------------ #
# Consumer Expenditure vs. Money Stock
#
# ------------------------------------ #

stock <- read.table("ConsumerExpenditure.txt",header=TRUE,sep="\t")

plot(stock$stock, stock$expendit,pch=17,cex=1.5)


lm1 <- lm(expendit~stock,data=stock)
summary(lm1)
plot(rstandard(lm1), pch=17,cex=1.5)
Y<-stock$expendit
X<-stock$stock
n<-length(Y)

while(TRUE){
    lm1 <- lm(Y~X)
    dw<-durbin.watson(lm1, alternative="positive")
    
    cat("\n\n ------------------------------------------------- \n \n")
    print(summary(lm1))
    print(dw)
    if(dw$p>0.05) break
    Y <- Y[2:n]-dw$r*Y[1:(n-1)]
    X <- X[2:n]-dw$r*X[1:(n-1)]     
}
