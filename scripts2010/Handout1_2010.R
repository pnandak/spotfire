####################
## Intro to R     ##
## Handout 1      ##
## Dave Armstrong ##
## ICPSR 2010     ##
####################


x
## Section 2: Assigning Output to Objects

2+2

X <- 4+4
X

## Section 3: Vectors and Matrices

Y <- c(1,2,3,4)
Y

Y <- Y + 3
Y

mat1 <- matrix(c(1,2,3,4), ncol=2)
mat1

mat1 <- matrix(c(1,3,2,4), ncol=2, byrow=T)
mat1

mat1[1,1]

mat1[1,1:2]

mat1[1, ]

vec1 <- c(1,2)
vec2 <- c(3,4)
cbind(vec1, vec2)

vec3 <- c(1,3)
vec4 <- c(2,4)
rbind(vec3, vec4)

mat3 <- rbind(vec3, vec4)
rownames(mat3)
colnames(mat3)

rownames(mat3) <- c("row1", "row2")
colnames(mat3) <- c("col1", "col2")
mat3

## Section 3.1: Matrix Math

mat1 * 3 

mat2 <- matrix(c(5,6,7,8), ncol=2)
mat2 
mat1
mat1 %*% mat2

mat1 
t(mat1)

solve(mat1 %*% t(mat1))


## Section 4: Reading in Your Own Data

library(foreign)
setwd(choose.dir())
library(tcltk)
setwd(tk_choose.dir())

spss.dat <- read.spss("r_example.sav", 
	to.data.frame=T, use.value.labels=T)
spss.dat

stata.dat <- read.dta("r_example.dta", convert.factors=T)

attributes(stata.dat)

nes <- read.spss("anes2004.POR", to.data.frame=T, use.value.labels=T)
attributes(nes)

search.var.labels <- function(dat, str){
	if("var.labels" %in% names(attributes(dat))){
		vlat <- "var.labels"
	}
	if("variable.labels" %in% names(attributes(dat))){
		vlat <- "variable.labels"
	}
	ind <- grep(str, attr(dat, vlat), ignore.case=T)
	vldf <- data.frame(ind=ind, label = attr(dat, vlat)[ind])
	rownames(vldf) <- names(dat)[ind]
	vldf
}

search.var.labels(nes, "age")


## Section 5: Accessing Data

stata.dat$x2
stata.dat[["x2"]]
stata.dat[,2]
## Section 5.1: Attaching 

x2
attach(stata.dat)
x2
detach(stata.dat)


## Section 5.2: Recoding and Adding New Variables

x4 <- c(0,0,0,0,0,1,1,1,1,1)
x4


stata.dat$x4 <- x4
stata.dat$x4 <- c(0,0,0,0,0,1,1,1,1,1)

stata.dat

stata.dat$log_x1 <- log(stata.dat$x1)
stata.dat

recode(stata.dat$x1, "c(1,2)=1; c(3,4,5,6)=2")

stata.dat$recoded_x1 <- recode(stata.dat$x1, "c(1,2)=1; c(3,4,5,6)=2")

x <- c(1,1.1,1.5, 2, 3, 3.2, 5)
recode(x, "1:2 = 1; 2:4 = 2; c(5)=3")
recode(stata.dat$log_x1, "0=0; 0:1.5=1; 1.5:hi = 2")
cbind(stata.dat$log_x1, recode(stata.dat$log_x1, "0=0; 0:1.5=1; 1.5:hi = 2"))

## Section 5.3: Missing Data

stata2.dat <- read.dta("r_example_miss.dta", convert.factors=T)
stata2.dat

as.factor(stata2.dat$x3)

stata2.dat$x3fac <- factor(stata2.dat$x3, levels=c("no", "yes"))
stata2.dat
stata2.dat$x3[which(stata2.dat$x3 == ".")] <- NA
stata2.dat
