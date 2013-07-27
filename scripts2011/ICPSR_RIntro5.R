####################
## Intro to R     ##
## Lecture 5      ##
## Dave Armstrong ##
## ICPSR Sum Prog ##
## 6/30/2009      ##
####################

## Page 2
r2s <- sapply(mlres, function(x)summary(x)$r.squared)

sapply(apply(mat2[,2:ncol(mat2)], 2, function(banana)lm(mat2[,1] ~ banana)), 
    function(x)summary(x)$r.squared)


library(car)
data(Duncan)

mean(Duncan$education[Duncan$type == "bc"])
mean(Duncan$education[Duncan$type == "prof"])
mean(Duncan$education[Duncan$type == "wc"])

untype <- as.character(unique(Duncan$type))

## Page 3

means <- rep(NA, length(untype))
for(i in 1:length(untype)){
    means[i] <- mean(Duncan$education[Duncan$type == untype[i]])
    names(means)[i] <- untype[i]
}


ag <- aggregate(Duncan$education, list(Duncan$type), mean)

inc.dum <- cut(Duncan$income, 2)
ag2 <- aggregate(Duncan$education, list(Duncan$type, inc.dum), mean)
ag2

## Page 4

by1 <- by(Duncan$education, list(Duncan$type), mean)

by1.vec <- c(by1)
by1.vec

by2 <- by(Duncan[,c("prestige", "income", "education")], 
    list(Duncan$type), 
    function(x)apply(x, 2, mean))

## Page 5
c(by2)
    
by2a <- as.data.frame(c(by2))

t(as.data.frame(c(by2)))

## Page 6

library(lattice)
library(car)
data(Duncan)

xyplot(prestige ~ income, data=Duncan)

xyplot(prestige ~ income, data=Duncan, pch=16, col="black")


xyplot(prestige ~ income | type, data=Duncan, pch=1, col="black", 
    as.table=T)

##Page 7
pch.vec <- c(1,2,4)
col.vec <- c("black", "red", "blue")

xyplot(prestige ~ income, data=Duncan, 
    pch=pch.vec[as.numeric(Duncan$type)], 
    col=col.vec[as.numeric(Duncan$type)])


xyplot(prestige ~ income, data=Duncan, panel=function(x,y){
    panel.points(x,y, pch=pch.vec[as.numeric(Duncan$type)], 
        col=col.vec[as.numeric(Duncan$type)])
        }
)


mod2 <- lm(prestige~type + income, data=Duncan)
b <- mod2$coef
a_bc <- b[1]
a_prof <- b[1] + b[2]
a_wc <- b[1] + b[3]


## Page 8
xyplot(prestige ~ income, data=Duncan, panel=function(x,y){
    panel.points(x,y, pch=pch.vec[as.numeric(Duncan$type)], 
        col=col.vec[as.numeric(Duncan$type)])
    panel.abline(a=a_bc, b=b[4], lty=1, col="black")
    panel.abline(a=a_prof, b=b[4], lty=2, col="red")
    panel.abline(a=a_wc, b=b[4], lty=3, col="blue")
        },
    key=list(space="top", 
        points=list(pch=pch.vec, col=col.vec), 
        text=list(c("Blue Collar", "Professional", "White Collar")), 
        lines=list(lty=c(1,2,3), col=col.vec))
)

## Page 9
levels(Duncan$type) <- c("Blue Collar", "Professional", "White Collar")

xyplot(prestige ~ income | type, data=Duncan, as.table=T, 
    panel=function(x,y,subscripts){    
        panel.text(50,50, paste("packet # = ", packet.number(), sep=""), cex=2)
}
)



ints <- c(a_bc, a_prof, a_wc)
slope <- b[4]

## Page 10
xyplot(prestige ~ income | type, data=Duncan, as.table=T, 
    panel=function(x,y){    
        panel.points(x,y, pch=1, col="black")
        panel.abline(a=ints[packet.number()], b=slope)
}
)


xyplot(prestige ~ income | type, data=Duncan, as.table=T, 
    panel=function(x,y,subscripts){    
        panel.points(x,y, pch=1, col="black")
        panel.points(Duncan$education[subscripts], y, col="red", pch=2)
} 
)

xyplot(prestige ~ income | type, data=Duncan, as.table=T, 
    xlim=c(0,100), 
    panel=function(x,y,subscripts){    
        panel.points(x,y, pch=1, col="black")
        panel.points(Duncan$education[subscripts], y, col="red", pch=2)
}
)

xyplot(prestige ~ income | type, data=Duncan, as.table=T, 
    xlim=c(0,100), 
    panel=function(x,y,subscripts){    
        panel.points(x,y, pch=1, col="black")
        panel.points(Duncan$education[subscripts], y, col="red", pch=2)
}, 
    key=list(space="top", points=list(pch=c(1,2), col=c("black", "red")), 
        text=list(c("Income", "Education")))

)

## Page 11
library(Rcmdr)

scatter3d(Duncan$income, Duncan$prestige, Duncan$education, surface=F)
scatter3d(Duncan$income, Duncan$prestige, Duncan$education, surface=T, 
    fit="linear")


scatter3d(Duncan$income, Duncan$prestige, Duncan$education, surface=F,
    grou=Duncan$type)

## Page 12
mod3d <- lm(prestige ~ income + education + type, data=Duncan)
summary(mod3d)

scatter3d(Duncan$income, Duncan$prestige, Duncan$education, surface=T, 
    groups=Duncan$type, fit="linear", parallel=T)

scatter3d(Duncan$income, Duncan$prestige, Duncan$education, surface=T, 
    groups=Duncan$type, fit="linear", parallel=F)
