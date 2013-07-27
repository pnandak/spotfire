##source("corr.R")
corr <- matrix(c(1,-0.38,-.5,.11,.75,-.44,.34,-.57,-.13,.12,.8,-.49,
                 rep(0,1),1,.37,-.38,-.22,-.01,-.54,-.09,.66,-.42,-.45,.55,
                 rep(0,2),1,-.24,-.49,.33,-.66,.14,.46,-.08,-.28,.52,
                 rep(0,3),1,.51,-.43,.71,-.27,.1,-.51,-.19,.06,
                 rep(0,4),1,-.56,.49,-.76,.14,-.18,.48,-.20,
                 rep(0,5),1,-.27,.14,-.16,.61,-.06,.01,
                 rep(0,6),1,-.18,-.24,-.22,.17,-.2,
                 rep(0,7),1,-.48,.08,-.37,.02,
                 rep(0,8),1,-.64,-.168,.63,
                 rep(0,9),1,.38,-.41,
                 rep(0,10),1,-.51,
                 rep(0,11),1),12,12)
corr <- corr+t(corr)
diag(corr) <- 1


vnames <- c("Redistribution","Inequality","Partisanship","Turnout","Unionization","Veto points","Electoral system","Left fragmentation","Right overrepresentation","Per capita income","Female LF participation","Unemployment")
rownames(corr) <- colnames(corr) <- vnames

## pdf(file="corr.pdf") ## uncomment to create a pdf
png(file="extra_4_iversen_table_a2.png", width = 600, height = 600) ## uncomment to create a png
plot.corr(corr)
graphics.off() ## close all R graphic windows
