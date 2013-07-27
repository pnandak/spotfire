# una versione modificata della funzione persp.polymars
# del "package polspline" di R.

persp.polymars<-
function (x, predictor1, predictor2, response, n = 33, xlim, 
    ylim, xx, contour.polymars, main, intercept, ...) 
{
    if (class(x) != "polymars") 
        stop("x is not a polymars object")
    pmars.model <- x
    if (missing(xx)) 
        xx <- pmars.model$ranges.and.medians[3, ]
    if (missing(xlim)) 
        xlim <- c(pmars.model$ranges.and.medians[1, predictor1], 
            pmars.model$ranges.and.medians[2, predictor1])
    if (missing(ylim)) 
        ylim <- c(pmars.model$ranges.and.medians[1, predictor2], 
            pmars.model$ranges.and.medians[2, predictor2])
    if (missing(predictor1) || missing(predictor2)) {
        stop("You must specify 2 predictor numbers\n")
    }
    if (pmars.model$responses != 1 && missing(response)) {
        cat("Response should be specified  (default: response =1)\n")
    }
    if (missing(response)) 
        response <- 1
    if (response > pmars.model$responses || response < 0) {
        stop("response arguement = ", response, "is out of range\n")
    }
    if (sum(as.integer(predictor1 == pmars.model$model[, 1])) == 
        0) {
        stop("Predictor 1 not in model\n")
    }
    if (sum(as.integer(predictor2 == pmars.model$model[, 1])) == 
        0) {
        stop("Predictor 2 not in model\n")
    }
    X <- seq(xlim[1], xlim[2], (xlim[2] - xlim[1])/(n - 1))
    y <- seq(ylim[1], ylim[2], (ylim[2] - ylim[1])/(n - 1))
    meshX <- rep(X, n)
    meshY <- rep(y, n)
    meshY <- sort(meshY)
    pred.values <- matrix(nrow = n^2, ncol = ncol(pmars.model$ranges.and.medians), 
        data = xx, byrow = TRUE)
    for (i in 1:(n^2)) pred.values[i, predictor1] <- meshX[i]
    for (i in 1:(n^2)) pred.values[i, predictor2] <- meshY[i]
    Z <- predict.polymars(pmars.model, pred.values, intercept = intercept)[, 
        response]
    Z <- matrix(Z, ncol = n, byrow = FALSE)
    xtitle <- paste("Predictor", predictor1)
    ytitle <- paste("Predictor", predictor2)
    if (pmars.model$responses > 1) {
        if (missing(main) && (!contour.polymars)) {
            ztitle <- paste("Response", response)
        }
        if (missing(main) && (contour.polymars)) {
            ztitle <- paste("Contour of response", response)
        }
    }
    else {
        if (missing(main) && (!contour.polymars)) 
            ztitle <- "Response"
        if (missing(main) && contour.polymars) 
            ztitle <- paste("Contour of response")
    }
    if (!contour.polymars) {
        persp(X, y, Z,   ticktype="detailed",...)
    }
    else {
        contour(X, y, Z, xlab = xtitle, ylab = ytitle, main = ztitle)
    }
    invisible()
}

