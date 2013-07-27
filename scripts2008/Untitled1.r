Lorenz <- function(activity, base, ...) {
    act.pc <- (100 * activity)/sum(activity)
    base.pc <- (100 * base)/sum(base)
    act.order <- act.pc[rev(order(LQ(activity, base)))]
    base.order <- base.pc[rev(order(LQ(activity, base)))]
    cs.act.order <- cumsum(act.order)
    cs.base.order <- cumsum(base.order)
    plot(cs.act.order, cs.base.order,
         xlim = c(0, 100), ylim = c(0, 100), type="p", ...)
    lines(c(0, cs.act.order), c(0, cs.base.order),
         col=2, type="l")
    lines(c(0, 100), c(0, 100), lty = 2)
    max(cs.act.order - cs.base.order)
}
