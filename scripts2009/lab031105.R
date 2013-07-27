###### lab March 11, 2005: multidimensional contingency tables

# example from Agresti book, section 8.4.2, page 327

# generate the dataset (from the table in Agresti)
Auto <- expand.grid(seat.belt=c("no", "yes"), location=c("urban", "rural"), gender=c("female", "male"), injury=c("yes", "no"))
Freq <- c(996, 759, 973, 757, 812, 380, 1084, 513, 7287, 11587, 3246, 6134, 10381, 10969, 6123, 6693)

auto.acc <- cbind(Auto, Freq)

attach(auto.acc) 

# R doesn't recognize it as a table yet
is.table(auto.acc)
is.data.frame(auto.acc)

# until we create a contingency table from the data frame
auto.tab <-  xtabs(Freq ~ gender + location + seat.belt + injury)

is.table(auto.tab)

# greater than 2D tables are not the easiest to look at, however, because it prints each "sheet" separately

auto.tab

# which is why R has the "flat contingency table" function, ftable()
# these are different options using ftable, to construct the same table (should look identical to table 8.8 in Agresti)
auto.tab2 <- ftable(auto.tab, row.vars=1:3)
auto.tab3 <- ftable(injury ~ ., data=auto.tab)

# turning the contingency table back into a data.frame
auto.df <- as.data.frame(auto.tab)

# different options for running a log-linear model using the loglm() function
# note in the first option i used the contingency table as the data, which allows me to omit the left hand side
loglm.auto <- loglm(~ gender + location + seat.belt + injury, auto.tab)
# however, using the table as the data does not allow one to use the shorthand '.' for including all of the possible covariates
# which is why i coerced the table into a dataframe, above. if using a dataframe as the data, however, you need to specify "Freq" on the left hand side
loglm.auto1 <- loglm(Freq ~ ., auto.df)
# update is a handy function to be aware of, as well as update.function. see help(update)
# it allows us to update our first model - the first '.' tells R to include everything on the left hand side in the new model that was on the LHS in the old model.
# the second '.' tells R to include everything on the right hand side in the new model that was on the RHS in the old model.
# the '^2' tells R to add pairwise interactions among each of the covariates as well. saves on typing!
loglm.auto2 <- update(loglm.auto, .~.^2)
# back to using the dataframe. note the default is to include all lower order terms.
loglm.auto3 <- loglm(Freq ~ .^2, auto.df)
loglm.auto4 <- loglm(Freq ~ .^3, auto.df)

# replicating Agresti's results
G <- gender
I <- injury
L <- location
S <- seat.belt

# note the fitted and param options. the default for the loglm() function is FALSE for both. if you want parameter estimates and fitted values, you need to tell R so.
# also need to specify the dataset. first we'll use the dataframe version.
# see help(loglm), but also check out help(loglm1) for more options.
auto.1 <- loglm(Freq ~ G + I + L + S, auto.df, fitted=T, param=T)
auto.2 <- loglm(Freq ~ G:I + G:L + G:S + I:L + I:S + L:S, auto.df, fitted=T, param=T)
auto.3 <- loglm(Freq ~ G:I:L + G:I:S + G:L:S + I:L:S, auto.df, fitted=T, param=T)
auto.4 <- loglm(Freq ~ G:I:L + G:S + I:S + L:S, auto.df, fitted=T, param=T)
auto.5 <- loglm(Freq ~ G:I:S + G:L + I:L + L:S, auto.df, fitted=T, param=T)
auto.6 <- loglm(Freq ~ G:L:S + G:I + I:L + I:S, auto.df, fitted=T, param=T)
auto.7 <- loglm(Freq ~ I:L:S + G:I + G:L + G:S, auto.df, fitted=T, param=T)

# unfortunately, the mosaicplots are easier to deal with if the data is a contingency table object (created from ftable or table), as opposed to a dataframe. 
# thus, while our shorthand above was nice, let's run them again on the table version of our data. (if you haven't picked it up yet, R is quite picky when it comes
# to dataframes, tables, arrays, etc.)
auto.1 <- loglm( ~ gender + injury + location + seat.belt, auto.tab, fitted=T, param=T)
# using the update function to save on typing again
auto.2 <- update(auto.1, . ~ . + gender:injury + gender:location + gender:seat.belt + injury:location + injury:seat.belt + location:seat.belt, fitted=T, param=T)
auto.3 <- update(auto.2, . ~ . + gender:injury:location + gender:injury:seat.belt + gender:location:seat.belt + injury:location:seat.belt, fitted=T, param=T)
auto.4 <- update(auto.1, . ~ . + gender:injury:location + gender:seat.belt + injury:seat.belt + location:seat.belt, fitted=T, param=T)
auto.5 <- update(auto.1, . ~ . + gender:injury:seat.belt + gender:location + injury:location + location:seat.belt, fitted=T, param=T)
auto.6 <- update(auto.1, . ~ . + gender:location:seat.belt + gender:injury + injury:location + injury:seat.belt, fitted=T, param=T)
auto.7 <- update(auto.1, . ~ . + injury:location:seat.belt + gender:injury + gender:location + gender:seat.belt, fitted=T, param=T)

# log-likelihood values
ll.1 <- -.5 * auto.1$deviance
ll.2 <- -.5 * auto.2$deviance
ll.3 <- -.5 * auto.3$deviance
ll.4 <- -.5 * auto.4$deviance
ll.5 <- -.5 * auto.5$deviance
ll.6 <- -.5 * auto.6$deviance
ll.7 <- -.5 * auto.7$deviance

# calculating BICs: -2*ll + npar*log(nobs)
# see page 111 in Long for discussion as well: BIC can be > 0 or < 0. BIC = 0 for the saturated model.
# if BIC > 0, then the saturated model is preferred. when BIC < 0, the other model is preferred. 
BIC.1 <- -2*ll.1 + 4*log(16)
BIC.2 <- -2*ll.2 + 11*log(16)
BIC.3 <- -2*ll.3 + 15*log(16)
BIC.4 <- -2*ll.4 + 12*log(16)
BIC.5 <- -2*ll.5 + 12*log(16)
BIC.6 <- -2*ll.6 + 12*log(16)
BIC.7 <- -2*ll.7 + 12*log(16)

# after the fact: remember there's an option using AIC that will allow you to calculate the BIC.
# also, if you load the library(nlme), there's a BIC() function.

# the object "lrt" in an loglm opject is the likelihood ratio test, so it gives you the value of Gsquared
# the object "df" is the degrees of freedom
m1 <- cbind(auto.1$lrt, auto.1$df, ll.1, BIC.1)
m2 <- cbind(auto.2$lrt, auto.2$df, ll.2, BIC.2)
m3 <- cbind(auto.3$lrt, auto.3$df, ll.3, BIC.3)
m4 <- cbind(auto.4$lrt, auto.4$df, ll.4, BIC.4)
m5 <- cbind(auto.5$lrt, auto.5$df, ll.5, BIC.5)
m6 <- cbind(auto.6$lrt, auto.6$df, ll.6, BIC.6)
m7 <- cbind(auto.7$lrt, auto.7$df, ll.7, BIC.7)

# table 8.9 in Agresti: comparison of the models
tab8.9 <- rbind(m1, m2, m3, m4, m5, m6, m7)

# which model, based upon the various statistics, do we prefer?

# extracting the fitted values
xfit1 <- auto.1$fitted
xfit2 <- auto.2$fitted
xfit3 <- auto.3$fitted
xfit6 <- auto.6$fitted

# mosaicplots
# see help(mosaicplot) 
mosaicplot(auto.tab, color=TRUE, main = "automobile accidents, data")
# plot of the actual data (same as above, but using formula notation)
# the default is empty boxes. if add the option color=TRUE, R fills the boxes in yellow and red. i find blue and green to be more aesthetically pleasing. 
mosaicplot(~ gender + seat.belt + location + injury, data=auto.tab, color=c("blue", "green"), main = "automobile accidents, data") 
x11()
# plot under independence (G, S, L, I)
# not a great fit
mosaicplot(~ gender + seat.belt + location + injury, data=xfit1, color=c("blue", "green"), main = "auto accidents, under independence")
x11()
# plot with pairwise interactions (G, S, L, I, G:S, G:L, G:I, S:L, S:I, L:I)
mosaicplot(~ gender + seat.belt + location + injury, data=xfit2, color=c("blue", "green"), main = "auto accidents, pairwise interactions")
x11()
# plot with threeway interactions ( . . . , G:I:S, G:I:L, G:L:S, I:L:S)
mosaicplot(~ gender + seat.belt + location + injury, data=xfit3, color=c("blue", "green"), main = "auto accidents, threeway interactions")
x11()
# Agresti's preferred plot (G:L:S, G:I, I:L, I:S)
# the plots don't actually look that different. this matches table 8.8 - the fitted values from models 2 & 6 that Agresti prints are very close.
mosaicplot(~ gender + seat.belt + location + injury, data=xfit6, color=c("blue", "green"), main = "auto accidents, GLS, GI, IL, IS")

# let's estimate using optim
auto.df$gender <- as.factor(auto.df$gender)
auto.df$location <- as.factor(auto.df$location)
auto.df$seat.belt <- as.factor(auto.df$seat.belt)
auto.df$injury <- as.factor(auto.df$injury)

# this is the model under independence
pois.auto.ind <- glm(Freq ~ gender + location + seat.belt + injury, data=auto.df, family = poisson)
# this is model 6 (G:L:S, G:I, I:L, I:S)
# note that when running a GLM, R automatically includes the lower order terms
pois.auto.6 <- glm(Freq ~ gender:location:seat.belt + gender:injury + injury:location + injury:seat.belt, data = auto.df, family = poisson)
# so that this isn't necessary:
pois.auto.6 <- update(pois.auto.ind, .~. + gender:location:seat.belt + gender:injury + injury:location + injury:seat.belt, data = auto.df, family = poisson)

# and compare the fitted values
xfit1
pois.auto.ind$fitted

xfit6
pois.auto.6$fitted

# note the subtle difference in coefficients:
auto.1$param
pois.auto.ind$coef
# the loglinear model estimates coefficients for each factor - male, female; rural, urban; etc.
# the poisson model estimates a single coefficient for each - gender, location, etc. - that is to be interpreted with reference to the base category
