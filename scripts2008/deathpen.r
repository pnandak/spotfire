library(ctest) # Not required with R versions >= 0.99.
dp <- c(53, 414, 11, 37, 0, 16, 4, 139)
dp <- array(dp, dim=c(2,2,2))
dimnames(dp) <- list(DeathPen=c("yes","no"),
                     Defendant=c("white","black"),
                     Victim=c("white","black"))
ftable(dp, row.vars=c("Victim","Defendant"), col.vars="DeathPen")
mantelhaen.test(dp)
mantelhaen.test(dp,correct=FALSE)
