life<-source("c:\\allwork\\rsplus\\chap4lifeexp.dat")$value
#
#
attach(life)
#
#
life.fa1<-factanal(life,factors=1,method="mle")
life.fa1
#
life.fa2<-factanal(life,factors=2,method="mle")
life.fa2
#
life.fa3<-factanal(life,factors=3,method="mle")
life.fa3
#
#
scores<-factanal(life,factors=3,method="mle",scores="regression")$scores
#
#
druguse.cor<-source("c:\\allwork\\rsplus\\chap4druguse.dat")$value
#
#
druguse.fa<-lapply(1:6,function(nf) factanal(covmat=druguse.cor,factors=nf,method="mle",n.obs=1634))
#
#
pred<-druguse.fa[[6]]$loadings%*%t(druguse.fa[[6]]$loadings)+
diag(druguse.fa[[6]]$uniquenesses)
#
#
round(druguse.cor-pred,digits=3)
#
pred<-druguse.fa[[3]]$loadings%*%t(druguse.fa[[3]]$loadings)+
diag(druguse.fa[[3]]$uniquenesses)
round(druguse.cor-pred,digits=3)
#
pred<-druguse.fa[[4]]$loadings%*%t(druguse.fa[[4]]$loadings)+
diag(druguse.fa[[4]]$uniquenesses)
round(druguse.cor-pred,digits=3)




