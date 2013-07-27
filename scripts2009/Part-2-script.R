##------------------------------------##
## Script for Part 2: Data in S       ##
##    John Fox                        ##
## An Introduction to R               ##
##    UCLA Feb. 2005                  ##
##------------------------------------##


    # Reading data

        # entering data at the keyboard

x <- c(1,2,3,4) # numeric data
x
names <-c ('John', 'Georges', 'Mary')   # character data
names
v <- c(TRUE, FALSE) # logical data
v


cooperation <- scan()
    49 64 37 52 68 54
    61 79 64 29
    27 58 52 41 30 40 39
    44 34 44

cooperation

condition <- rep(c("public", "anonymous"), c(10,10))
condition
sex <- rep(rep(c("male", "female"), c(5,5)), 2)
sex

Guyer <- data.frame(cooperation, condition, sex)
Guyer

        # reading data from a file into a data frame

Prestige <- read.table(file.choose(), header=TRUE)
Prestige

        # accessing data in a package

library(car)
data(Duncan)
Duncan

    # The search path

search()
prestige

Duncan[,"prestige"]

attach(Duncan)
prestige
search()

attach(Prestige)
search()
prestige    # prestige in Prestige shadows prestige in Duncan


Duncan[,"prestige"]

detach(Prestige)
search()
prestige

detach(Duncan)

    # missing data

data(Freedman)
attach(Freedman)
Freedman[1:10,]

density
median(density)
median(density, na.rm=TRUE)

plot(density, crime)
identify(density, crime, rownames(Freedman))

log(c(1, 10, NA, 100), base=10)

plot(log(density, base=10), crime)

lm(crime ~ log(density, base=10))

abline(lm(crime ~ log(density, base=10)), lty=2)

good <- complete.cases(density, crime)
good
lines(lowess(log(density[good], base=10), crime[good]))

options("na.action")

detach(Freedman)
Freedman.good <- na.omit(Freedman)
Freedman.good[1:10,]
dim(Freedman.good)
dim(Freedman)

    # numeric variables, character variables, and factors

objects()
remove(good, Freedman.good)

condition
is.character(condition)

condition <- as.factor(condition)
condition

remove(cooperation, condition, sex)
attach(Guyer)

condition
is.character(condition)
is.factor(condition)

summary(Guyer)


    # Matrices, arrays, and lists

A <- matrix(1:12, 3, 4)
A

B <- matrix(c('a','b','c'), 4, 3, byrow=TRUE)
B

dim(A)
dim(B)

v <- sample(10,10)
v
dim(v)

array.3 <- array(1:24, c(4,3,2))
array.3
dim(array.3)

list.1 <- list(mat.1=A, mat.2=B, vec=v)
list.1

    # Indexing  (time permitting)
    
        # vectors

v
v[2]        # one element
v[c(4,2,6)] # several elements
v[c(4,2,4)] # elements may be repeated

v[-c(2,4,6,8,10)]   # omitting elements

names(v) <- letters[1:10]
v
names(v)
v[c('f','i','g')]   # indexing by names

v < 6
v[v < 6]    # logical indexing

vv <- v
vv

vv[c(1,3,5)] <- c(1,2,3)    # replacing elements
vv

vv[c('b','d','f','h','j')] <- 0
vv

remove(vv)

        # matrices

A
A[2,3]
A[c(1,2), 2]
A[c(1,2), c(2,3)]
A[c(1,2),]

A[c(1,2), 2, drop=FALSE]    # retain column dimension

A[, -c(1,3)]    # delete columns 1 and 3
A[-1, -2]       # delete row 1 and column 2

rownames(A) <- c('one', 'two', 'three')
colnames(A) <- c('w','x','y', 'z')
A

A[c('one','two'), c('x','y')]
A[c(TRUE, FALSE, TRUE),]

AA <- A
AA

AA[1,] <- 0
AA

remove(AA)

        # lists

list.1

list.1[c(2,3)]

list.1[2]   # a one-element list
class(list.1[2])

list.1[[2]] # a list element
class(list.1[[2]])

list.1["mat.1"]
list.1[["mat.1"]]

list.1$mat.1

list.1$mat.1 <- matrix(1, 2, 2)     # replacing a list element
list.1$title <- 'an arbitrary list' # adding an element
list.1$mat.2<-NULL                  # removing an element
list.1

        # data frames

attach(Guyer)
Guyer

Guyer[,1]

Guyer[,'cooperation']

Guyer[c(1,2),]

Guyer[c('1','2'), 'cooperation']

Guyer[-(6:20),]

sex=='female' & condition=='public'
Guyer[sex=='female' & condition=='public',]

Guyer$cooperation
Guyer[['cooperation']]
Guyer['cooperation']  # a one-column data frame
