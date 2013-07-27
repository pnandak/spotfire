# ----------------------------

library("ggplot2")
library("plyr")
options(digits = 3)
options(prompt = "R> ")
source("ozone-map.R")
brightblue <- rgb(102, 204, 255, max = 255)

# ----------------------------

x <- array(1:24, 2:4)
# Function to return shape of an object: Dimensions, or length
shape <- function(x) if (is.vector(x)) length(x) else dim(x)
shape(x)

# Look at the shape of the value of aaply as FUN returns
# differently-shaped objects
shape(aaply(x, 2, function(y) 0))
shape(aaply(x, 2, function(y) rep(1,5)))
shape(aaply(x, 2, function(y) matrix(0,nrow=5,ncol=6)))
shape(aaply(x, 1, function(y) matrix(0,nrow=5,ncol=6)))


# ----------------------------

# Base ball case study ============================================
baberuth <- subset(baseball, id == "ruthba01")
baberuth <- transform(baberuth, cyear = year - min(year) + 1)


# ----------------------------

baseball <- ddply(baseball, .(id), transform, 
  cyear = year - min(year) + 1)


# ----------------------------

# Runs per bat for Babe Ruth.
qplot(cyear, rbi / ab, data = baberuth, geom = "line")
baseball <- subset(baseball, ab >= 25)


# ----------------------------

baseball <- subset(baseball, ab >= 25)
xlim <- range(baseball$cyear, na.rm=TRUE)
ylim <- range(baseball$rbi / baseball$ab, na.rm=TRUE)
plotpattern <- function(df) {
  qplot(cyear, rbi / ab, data = df, geom = "line", 
    xlim = xlim, ylim = ylim)
}

pdf("paths.pdf", width = 8, height = 4)
d_ply(baseball, .(reorder(id, rbi / ab)), failwith(NA, plotpattern), 
  .print = TRUE)
dev.off()


# ----------------------------

model <- function(df) {
  lm(rbi / ab ~ cyear, data=df)
}
model(baberuth)
bmodels <- dlply(baseball, .(id), model)


# ----------------------------

rsq <- function(x) summary(x)$r.squared
bcoefs <- ldply(bmodels, function(x) c(coef(x), rsquare = rsq(x)))
names(bcoefs)[2:3] <- c("intercept", "slope")


# ----------------------------

baseballcoef <- merge(baseball, bcoefs, by = "id")
subset(baseballcoef, rsquare > 0.999)$id


# ----------------------------

# Histogram of model R-squared with bin width of 0.05.  Most models fit
# very poorly!
qplot(rsquare, data=bcoefs, geom="histogram", binwidth=0.01)


# ----------------------------

# A scatterplot of model intercept and slope, with one point for each
# model (player).  The size of the points is proportional to the
# R-square of the model. Vertical and horizontal lines emphasise the x
# and y origins.
ggplot(bcoefs, aes(slope, intercept)) + 
  geom_point(aes(size = rsquare), alpha = 0.5) +
  geom_vline(xintercept = 0, size=0.5, colour="grey50") + 
  geom_hline(yintercept = 0, size = 0.5, colour="grey50") + 
  scale_area(to = c(0.5, 3), breaks = c(0, 0.25, 0.5, 0.75, 1))
last_plot() + xlim(-0.01, 0.01) + ylim(-0.1, 0.25)


# ----------------------------

# Star glyphs showing variation in ozone over time at each spatial
# location.
ozstars <- make_stars(ozm, "time", "value")
res <- 1.2
ggplot(ozstars, aes(x = long + res * x, y = lat + res * y)) + map +
 geom_path(aes(group = interaction(long, lat)), fill=NA, colour="grey50")


# ----------------------------

# Star glyphs are time-series (left) plotted in polar coordinates
# (right).  Both time and ozone value have been scaled to lie between 0
# and 1: The smallest value in the entire dataset will be 0 and the
# largest will be 1.  Grey lines indicate these boundaries, as well as
# the boundaries between the six years.  A red point shows the position
# of the first value: it is close to the last value in the glyph.  This
# glyph is the glyph on the top-left of Figure~\ref{fig:ozone-glyphs}.
one <- subset(ozstars, lat == max(lat) & long == min(long))
ggplot(one, aes(time, value)) + 
  geom_hline(yintercept = c(0, 1), colour = "grey70") + 
  geom_vline(xintercept = seq(0, 1, length = 7), colour = "grey70") +
  geom_line() + 
  geom_point(subset = .(time == 0), colour = brightblue, size = 4) +
  xlab(NULL) + ylab(NULL)

theta <- seq(0, 2 * pi, length = 100)
circle <- data.frame(x = sin(theta), y = cos(theta))
year_theta <- seq(0, 2 * pi, length = 7)[-7]
year <- data.frame(x = sin(year_theta), y = cos(year_theta))
ggplot(one, aes(x, y)) + 
  geom_polygon(data = circle, fill = NA, colour = "grey70") +
  geom_segment(aes(xend = 0, yend = 0), data = year, colour = "grey70") +
  geom_path(fill = NA, colour = "black") +
  coord_equal() + 
  annotate("point", 0, 0, colour = "grey70", size = 4) +
  geom_point(subset = .(time == 0), colour = brightblue, size = 3) +
  xlab(NULL) + ylab(NULL)


# ----------------------------

value <- ozone[1, 1, ]
time <- 1:72 / 12
month.abbr <- c("Jan", "Feb", "Mar", "Apr", "May", 
 "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month <- factor(rep(month.abbr, length = 72), levels = month.abbr)
year <- rep(1:6, each = 12)


# ----------------------------

# Two ways of displaying the seasonal changes.  (Left) A single time
# series over all six years and (right) a line for each year.
qplot(time, value, geom="line") 
qplot(month, value, geom="line", group = year)


# ----------------------------

library("MASS")

deseas1 <- rlm(value ~ month - 1)
summary(deseas1)
coef(deseas1)


# ----------------------------

# Deasonalised ozone trends.  (Left) deasonalised trend over six years.
# (Right) Estimates of seasonal effects.  Compare to
# Figure~\ref{fig:ozone-ts}.
qplot(time, resid(deseas1), geom="line") + geom_hline(colour="grey70")
qplot(month, unname(coef(deseas1)), geom="line", group = 1) + 
  geom_hline(colour="grey70")


# ----------------------------

deseasf <- function(value) rlm(value ~ month - 1, maxit = 50)
models <- alply(ozone, 1:2, deseasf)
failed <- laply(models, function(x) !x$converged)


# ----------------------------

coefs <- laply(models, coef)
dimnames(coefs)[[3]] <- month.abbr
names(dimnames(coefs))[3] <- "month"

deseas <- laply(models, resid)
dimnames(deseas)[[3]] <- 1:72
names(dimnames(deseas))[3] <- "time"

dim(coefs)
dim(deseas)


# ----------------------------

coefs_df <- melt(coefs)
head(coefs_df)
coefs_df <- ddply(coefs_df, .(lat, long), transform, 
  avg = mean(value),
  std = value / max(value)
)
levels(coefs_df$month) <- month.abbr
head(coefs_df)

deseas_df <- melt(deseas)
head(deseas_df)
 


# ----------------------------

# Star glyphs showing seasonal variation.  Each star shows the twelve
# estimates of monthly seasonal effects, standardised to have maximum
# one.  This focuses on the overall pattern of changes, rather than the
# absolute values, given by the glyph colour. Note the strong spatial
# correlation: Nearby glyphs have similar shapes.
coef_stars <- make_stars(coefs_df, "month", "std")
res <- 1.2
ggplot(coef_stars, aes(x = long + res * x, y = lat + res * y, fill=avg)) +
 map + geom_polygon(aes(group = interaction(long, lat)), colour="grey50") +
 scale_fill_gradient(low = brightblue, high = "yellow") + 
 opts(aspect.ratio = 1)


# ----------------------------

# Star glyphs showing deasonalised trends. Each star shows six years of
# data, with seasonal trend removed. This plot contains a lot of
# data---over 40,000 observations---and rewards detailed study.
# Looking at a printed version also helps as the resolution of a
# printer (600 dpi) is much higher than that of the screen ($\sim$100
# dpi). Interesting features include the higher variability in the
# North, locations in the mountains of South America with a large
# difference between starting and ending temperatures, and an unusual
# month common to many of the locations in the Pacific.
deseas_stars <- make_stars(deseas_df, "time", "value")
res <- 1.2
ggplot(deseas_stars, aes(x = long + res * x, y = lat + res * y)) +
 map + geom_path(aes(group = interaction(long, lat)), 
                 colour="grey50", fill=NA) +
 opts(aspect.ratio = 1)


# ----------------------------

coef_limits <- range(coefs_df$value)
coef_mid <- mean(coefs_df$value)
monthsurface <- function(mon) {
  df <- subset(coefs_df, month == mon)
  qplot(long, lat, data = df, fill = value, geom="tile") + 
  scale_fill_gradient(limits = coef_limits, 
    low = brightblue, high = "yellow") + 
    map + opts(aspect.ratio = 1)
}


# ----------------------------

# Tile plots of coefficients for January (left) and July (right).
monthsurface("Jan")
monthsurface("Jul")

