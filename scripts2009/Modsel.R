setClass('mod.sel',
representation(LogLik.One='numeric', LogLik.Two='numeric', 
Numerator='numeric', Denominator='numeric', Vuong='numeric', VuongP='numeric', 
Clarke='numeric', ClarkeP='numeric',AIC.One='numeric',AIC.Two='numeric',
BIC.One='numeric',BIC.Two='numeric'))

mod.sel <- function(x,z) {
    y <- x$y
    N <- length(y)
    fam.1 <- family(x)$family
    fam.2 <- family(z)$family                                                              
    fit.mod.1 <- fitted(x)
    fit.mod.2 <- fitted(z)
    xb.1 <- predict.glm(x)
    xb.2 <- predict.glm(z)
    loglik.1 <- switch(fam.1,
        poisson = (xb.1*y-exp(xb.1)-log(gamma(y+1))),
        binomial = y*log(fit.mod.1)+(1-y)*log(1-fit.mod.1),
        gaussian = (-log(2*pi*sum((residuals(x))^2)/N))/2 - 
            (1/2)*((residuals(x))/sqrt(sum((residuals(x))^2)/N))^2)
    loglik.2 <- switch(fam.2,
        poisson = (xb.2*y-exp(xb.2)-log(gamma(y+1))),
        binomial = y*log(fit.mod.2)+(1-y)*log(1-fit.mod.2),
        gaussian = (-log(2*pi*sum((residuals(z))^2)/N))/2 - 
            (1/2)*((residuals(z))/sqrt(sum((residuals(z))^2)/N))^2)
    pdim <- x$rank
    if (fam.1 %in% c("gaussian", "Gamma", "inverse.gaussian"))
        pdim <- pdim+1
    qdim <- z$rank
    if (fam.2 %in% c("gaussian", "Gamma", "inverse.gaussian"))
        qdim <- qdim+1
    loglik.1.sum <- pdim-AIC(x)/2
    loglik.2.sum <- qdim-AIC(z)/2
    lldiff <- loglik.1-loglik.2
    adjll.1 <- loglik.1-(((pdim)/(2*N))*log(N))
    adjll.2 <- loglik.2-(((qdim)/(2*N))*log(N))
    adjdiff <- ((adjll.1-adjll.2)>0) 
    Clarke.test <- binom.test(sum(adjdiff),N,p=.5)
    Clarke <- Clarke.test$statistic
    Clarke.p <- Clarke.test$p.value
    vuong.den <- (sqrt(mean(lldiff^2)-((mean(lldiff))^2)))*sqrt(N)
    vuong.num <- (loglik.1.sum-loglik.2.sum-
        (((pdim/2)*log(N))-((qdim/2)*log(N))))
    Vuong <- vuong.num/vuong.den
    Vuong.p <- 2*(1-pnorm(abs(Vuong)))
    aic.1 <- AIC(x)
    aic.2 <- AIC(z)
    bic.1 <- -2*loglik.1.sum+log(N)*pdim
    bic.2 <- -2*loglik.2.sum+log(N)*qdim
    result <- new('mod.sel', LogLik.One=loglik.1.sum, LogLik.Two=loglik.2.sum, 
        Numerator=vuong.num, Denominator=vuong.den, Vuong=Vuong, 
        VuongP=Vuong.p, Clarke=Clarke, ClarkeP=Clarke.p, AIC.One=aic.1, AIC.Two=aic.2,
        BIC.One=bic.1, BIC.Two=bic.2)
    class(result) <- 'mod.sel'
    result
    }

setMethod('summary', signature(object='mod.sel'),
    definition=function(object, ...){
    ll1 <- round(object@LogLik.One,digits=3)
    ll2 <- round(object@LogLik.Two,digits=3)
    vnum <- object@Numerator
    vden <- object@Denominator
    vts <- round(object@Vuong,digits=3)
    vp <- round(object@VuongP,digits=3)
    cts <- round(object@Clarke)
    cp <- round(object@ClarkeP,digits=3)
    aic1 <- round(object@AIC.One,digits=3)
    aic2 <- round(object@AIC.Two,digits=3)
    bic1 <- round(object@BIC.One,digits=3)
    bic2 <- round(object@BIC.Two,digits=3)
    colone <- rbind(ll1,aic1,bic1)
    coltwo <- rbind(ll2,aic2,bic2)
    table <- cbind(colone,coltwo)
    colnames(table) <- c('Model One', ' Model Two')
    rownames(table) <- c('LogLik    ', 'AIC', 'BIC')
    cat('\nSelection Criteria\n\n')
    print(table)
    #table <- rbind(vnum,vden,vts, vp, cts, cp)
    cat('\n\nModel Section Tests*\n\n')
    colone <- rbind(vts,cts)
    coltwo <- rbind(vp,cp)
    table <- cbind(colone,coltwo)
    colnames(table) <- c('Statistic', ' P-value')
    rownames(table) <- c('Vuong ', 'Clarke  ')
    print(table)
    cat('\n*The log-likelihoods for model \ntwo are subtracted from the \nlog-likehoods for model one.\n\n')
    }
)
