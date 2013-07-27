# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 5.5(© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------
source("base-www.R")
#
succo <- read.table("succo.dat", head=TRUE) 
succo[,"negozio"] <- factor(succo[,"negozio"])
attach(succo)
#
set.seed(123)
n  <- nrow(succo)
n1 <- round(n*0.75)
n2 <- n-n1
permuta<- sample(1:n,n)
stima <- sort(permuta[1:n1])
verifica <- sort(permuta[(n1+1):n])
v <- c(1,3:7,9:10)

library(polspline)
mars2  <- polyclass(succo$scelta[stima], succo[stima,v[-1]])
pmars2 <- ppolyclass(succo[verifica,v[-1]], mars2)
#
matrice.confusione(pmars2[,2]>0.5,succo[verifica,"scelta"])
pause("tabella 5.8")
#
lift.roc(pmars2[,2], as.numeric(succo[verifica,1]=="MM"))
pause("fig 5.12")
#
detach.all()
