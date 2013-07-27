
# These examples were done in R 1.3.1 on an i686 running debian linux. I've
# checked most of the syntax. Everything should work under most
# versions of S-PLUS although the output is slightly different.
#
# Let's work with 2-way tables first
#
# The first thing we need to do is enter our data. Suppose we find the
# following contingency table relating hair color and eye color for a
# number of men:
#        Eye
# Hair    Brown Blue Hazel Green
#   Black    32   11    10     3
#   Brown    38   50    25    15
#   Red      10   10     7     7
#   Blond     3   30     5     8
# 
# to enter these data into a data frame we could do the following:

> hair <- factor(rep(c("Black", "Brown", "Red", "Blond"), c(4,4,4,4)))

> eye  <- factor(rep(c("Brown", "Blue", "Hazel", "Green"), 4))

> freq <- c(32,11,10,3,  38,50,25,15,  10,10,7,7,  3,30,5,8)

> haireye.data <- data.frame(hair, eye, freq)

#
# Let's take a look at this data frame:
#

> haireye.data
    hair   eye freq
1  Black Brown   32
2  Black  Blue   11
3  Black Hazel   10
4  Black Green    3
5  Brown Brown   38
6  Brown  Blue   50
7  Brown Hazel   25
8  Brown Green   15
9    Red Brown   10
10   Red  Blue   10
11   Red Hazel    7
12   Red Green    7
13 Blond Brown    3
14 Blond  Blue   30
15 Blond Hazel    5
16 Blond Green    8

#
# We may also want to put these data into a table. To do this we do
# the following:

> haireye.table <- table(hair, eye)
 
> haireye.table[cbind(hair,eye)] <- freq

#
# Let's look at the table:
#

> haireye.table
       eye
hair    Blue Brown Green Hazel
  Black   11    32     3    10
  Blond   30     3     8     5
  Brown   50    38    15    25
  Red     10    10     7     7
attr(,"class")
[1] "table"

#
# Note that the factor levels have been rearranged alphabetically but
# the information is the same
#
# OK, let's start fitting some models. First we'll use the glm()
# function to fit our models. Here we'll make use of the data as 
# organized in the haireye.data dataframe
#
# First we'll switch to sum-to-zero contrasts
#

> options(contrasts=c("contr.sum", "contr.poly"))

#
# now let's fit a simple independence model
#

> glm.out1 <- glm(freq~hair+eye, family=poisson, data=haireye.data)
> summary(glm.out1)

Call:
glm(formula = freq ~ hair + eye, family = poisson, data = haireye.data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.6724  -0.9527  -0.1018   0.5635   3.0744  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.57730    0.07489  34.416  < 2e-16 ***
hair1       -0.03274    0.11717  -0.279  0.77989    
hair2       -0.22945    0.12515  -1.833  0.06675 .  
hair3        0.79393    0.09331   8.509  < 2e-16 ***
eye1         0.51997    0.09770   5.322 1.03e-07 ***
eye2         0.32369    0.10304   3.141  0.00168 ** 
eye3        -0.59865    0.14052  -4.260 2.04e-05 ***
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 163.492  on 15  degrees of freedom
Residual deviance:  44.315  on  9  degrees of freedom
AIC: 127.46

Number of Fisher Scoring iterations: 4


# 
# Looking at the residual deviance of 44.315 on 9 degrees of freedom
# suggests that we can reject this model in favor of the saturated
# model at any conventional significance level. To get the p-value we
# type
#

> 1-pchisq(44.315,9)
[1] 1.234755e-06

# 
# OK, so let's fit the saturated model:
#


> glm.out2 <- glm(freq~hair*eye, family=poisson, data=haireye.data)
> summary(glm.out2)

Call:
glm(formula = freq ~ hair * eye, family = poisson, data = haireye.data)

Deviance Residuals: 
 [1]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.464190   0.085819  28.714  < 2e-16 ***
hair1       -0.147983   0.157220  -0.941 0.346578    
hair2       -0.417018   0.170213  -2.450 0.014286 *  
hair3        0.904944   0.110207   8.211  < 2e-16 ***
eye1         0.539235   0.122463   4.403 1.07e-05 ***
eye2         0.161940   0.150664   1.075 0.282448    
eye3        -0.506187   0.168015  -3.013 0.002589 ** 
hair1:eye1  -0.457547   0.234685  -1.950 0.051221 .  
hair2:eye1   0.814790   0.211986   3.844 0.000121 ***
hair3:eye1   0.003654   0.157410   0.023 0.981479    
hair1:eye2   0.987589   0.218782   4.514 6.36e-06 ***
hair2:eye2  -1.110500   0.357206  -3.109 0.001878 ** 
hair3:eye2   0.106513   0.184552   0.577 0.563843    
hair1:eye3  -0.711408   0.358987  -1.982 0.047512 *  
hair2:eye3   0.538456   0.284753   1.891 0.058630 .  
hair3:eye3  -0.154897   0.222881  -0.695 0.487071    
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 1.6349e+02  on 15  degrees of freedom
Residual deviance: 1.1742e-13  on  0  degrees of freedom
AIC: 101.15

Number of Fisher Scoring iterations: 2

#
# We can fit the same models using the loglin() function and the
# tabular form of the data. 
#
# First the independence model:
#



> loglin.out1 <- loglin(haireye.table, margin=list(1,2), param=TRUE)
2 iterations: deviation 0 
> loglin.out1
$lrt
[1] 44.31537

$pearson
[1] 42.16325

$df
[1] 9

$margin
$margin[[1]]
[1] "hair"

$margin[[2]]
[1] "eye"


$param
$param$"(Intercept)"
[1] 2.577301

$param$hair
      Black       Blond       Brown         Red 
-0.03274428 -0.22945457  0.79393429 -0.53173544 

$param$eye
      Blue      Brown      Green      Hazel 
 0.5199664  0.3236865 -0.5986465 -0.2450065 


#
# Now the saturated model
#


> loglin.out2 <- loglin(haireye.table, margin=list(c(1,2)), param=TRUE)
2 iterations: deviation 0 
> loglin.out2
$lrt
[1] 0

$pearson
[1] 0

$df
[1] 0

$margin
$margin[[1]]
[1] "hair" "eye" 


$param
$param$"(Intercept)"
[1] 2.46419

$param$hair
     Black      Blond      Brown        Red 
-0.1479831 -0.4170179  0.9049436 -0.3399426 

$param$eye
      Blue      Brown      Green      Hazel 
 0.5392350  0.1619397 -0.5061867 -0.1949880 

$param$hair.eye
       eye
hair            Blue       Brown      Green       Hazel
  Black -0.457546845  0.98758911 -0.7114082  0.18136592
  Blond  0.814790122 -1.11049964  0.5384559 -0.24274640
  Brown  0.003654229  0.10651271 -0.1548969  0.04472999
  Red   -0.360897506  0.01639782  0.3278492  0.01665049


#
# Note the estimates are the same regardless of whether we use the
# glm() function or the loglin() function to fit the models. 
#


# OK, now suppose we have hair and eyecolor data for women as well as
# men. This gives us a 3-way table. Suppose this 3-way table is the
# following:
# , , Sex = Male
# 
#        Eye
# Hair    Brown Blue Hazel Green
#   Black    32   11    10     3
#   Brown    38   50    25    15
#   Red      10   10     7     7
#   Blond     3   30     5     8
# 
# , , Sex = Female
#
#        Eye
# Hair    Brown Blue Hazel Green
#   Black    36    9     5     2
#   Brown    81   34    29    14
#   Red      16    7     7     7
#   Blond     4   64     5     8
# 
#
# To enter these data into a dataframe we could do the following:
#

> hair <- factor(rep(rep(c("Black", "Brown", "Red", "Blond"), c(4,4,4,4)),2))

> eye <- factor(rep(c("Brown", "Blue", "Hazel", "Green"), 8))

> sex <- factor(rep(c("Male", "Female"), c(16,16)))

> freq <- c(32,11,10,3,  38,50,25,15, 10,10,7,7,  3,30,5,8,
36,9,5,2,  81,34,29,14,  16,7,7,7,  4,64,5,8) 

> haireye2.data <- data.frame(hair, eye, sex, freq)
> haireye2.data
   hair   eye    sex  freq
1  Black Brown   Male    32
2  Black  Blue   Male    11
3  Black Hazel   Male    10
4  Black Green   Male     3
5  Brown Brown   Male    38
6  Brown  Blue   Male    50
7  Brown Hazel   Male    25
8  Brown Green   Male    15
9    Red Brown   Male    10
10   Red  Blue   Male    10
11   Red Hazel   Male     7
12   Red Green   Male     7
13 Blond Brown   Male     3
14 Blond  Blue   Male    30
15 Blond Hazel   Male     5
16 Blond Green   Male     8
17 Black Brown Female    36
18 Black  Blue Female     9
19 Black Hazel Female     5
20 Black Green Female     2
21 Brown Brown Female    81
22 Brown  Blue Female    34
23 Brown Hazel Female    29
24 Brown Green Female    14
25   Red Brown Female    16
26   Red  Blue Female     7
27   Red Hazel Female     7
28   Red Green Female     7
29 Blond Brown Female     4
30 Blond  Blue Female    64
31 Blond Hazel Female     5
32 Blond Green Female     8

#
# OK, now lets put these data into table form
#

> haireye2.table <- table(hair, eye, sex)

> haireye2.table[cbind(hair,eye,sex)] <- freq

> haireye2.table
, , sex = Female

       eye
hair    Blue Brown Green Hazel
  Black    9    36     2     5
  Blond   64     4     8     5
  Brown   34    81    14    29
  Red      7    16     7     7

, , sex = Male

       eye
hair    Blue Brown Green Hazel
  Black   11    32     3    10
  Blond   30     3     8     5
  Brown   50    38    15    25
  Red     10    10     7     7

attr(,"class")
[1] "table"


#
# OK, let's fit the independence model using the glm() function
#


> glm.out3 <- glm(freq~hair+eye+sex, family=poisson, data=haireye2.data)
> summary(glm.out3)

Call:
glm(formula = freq ~ hair + eye + sex, family = poisson, data = haireye2.data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-5.4110  -1.5996   0.2315   0.9084   6.3734  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.64265    0.05217  50.657  < 2e-16 ***
hair1       -0.17912    0.08244  -2.173  0.02980 *  
hair2       -0.01705    0.07803  -0.219  0.82699    
hair3        0.79474    0.06257  12.701  < 2e-16 ***
eye1         0.50670    0.06742   7.516 5.67e-14 ***
eye2         0.52969    0.06702   7.904 2.70e-15 ***
eye3        -0.70505    0.10014  -7.040 1.92e-12 ***
sex1         0.10853    0.04132   2.626  0.00863 ** 
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 489.59  on 31  degrees of freedom
Residual deviance: 175.79  on 24  degrees of freedom
AIC: 330.54

Number of Fisher Scoring iterations: 4

# 
# Looking at the residual deviance it is easy to see that independence
# does not hold here. 
#
# Let's fit the saturated model and then do an analysis of deviance 
# to find the best submodel
# 

> glm.out4 <- glm(freq~hair*eye*sex, family=poisson, data=haireye2.data)
> anova(glm.out4, test="Chisq")
Analysis of Deviance Table

Model: poisson, link: log

Response: freq

Terms added sequentially (first to last)


             Df Deviance Resid. Df Resid. Dev P(>|Chi|)
NULL                            31     489.59          
hair          3   165.59        28     324.00 1.138e-35
eye           3   141.27        25     182.73 2.010e-30
sex           1     6.93        24     175.79      0.01
hair:eye      9   146.44        15      29.35 4.806e-27
hair:sex      3     6.27        12      23.08      0.10
eye:sex       3    14.90         9       8.19 1.908e-03
hair:eye:sex  9     8.19         0  1.059e-12      0.52

# If we set our significance level at 0.10, this suggests that the 
# model with all 2-way interactions included but no 3-way interactions 
# is the best model. This means that no pair of variables are 
# conditionally independent given the third but that the association 
# between any two variables is identical at each level of the third 
# variable.
# 
# Given that we probably don't expect an interaction between sex and either 
# hair or eye color and the fact that the hair:sex interaction is only 
# significant at the 0.10 level we might want to look at a model with
# no interactions between sex and the other variables. 

> glm.out5 <- glm(freq~sex+hair*eye, family=poisson, data=haireye2.data)

> anova(glm.out5, test="Chisq")
Analysis of Deviance Table

Model: poisson, link: log

Response: freq

Terms added sequentially (first to last)


         Df Deviance Resid. Df Resid. Dev P(>|Chi|)
NULL                        31     489.59          
sex       1     6.93        30     482.66      0.01
hair      3   165.59        27     317.07 1.138e-35
eye       3   141.27        24     175.79 2.010e-30
hair:eye  9   146.44        15      29.35 4.806e-27

# all of these terms should be in the model but with a residual
# deviance of 29.35 on 15 degrees of freedom we can reject this 
# model in favor of the saturated model. The p-value is:

> 1 - pchisq(29.35, 15)
[1] 0.01449365

# including the eye:sex interaction but not the hair:sex interaction
# we get

> glm.out6 <- glm(freq~sex+eye:sex+hair*eye, family=poisson, 
                  data=haireye2.data)

> anova(glm.out6, test="Chisq")
Analysis of Deviance Table

Model: poisson, link: log

Response: freq

Terms added sequentially (first to last)


         Df Deviance Resid. Df Resid. Dev P(>|Chi|)
NULL                        31     489.59          
sex       1     6.93        30     482.66      0.01
eye       3   141.27        27     341.39 2.010e-30
hair      3   165.59        24     175.79 1.138e-35
sex:eye   3     7.32        21     168.48      0.06
eye:hair  9   146.44        12      22.03 4.806e-27

# here we see that the sex:eye interaction is borderline significant
# with a p-value of 0.06. With a residual deviance of 22.03 
# on 12 degrees of freedom we can reject this model in favor of the 
# saturated model at significance levels above 0.037. 

> 1 - pchisq(22.03, 12)
[1] 0.03718496

# Given some of the problems associated with using classical
# hypothesis test with contingency tables it might make sense to
# compare the models using BIC or AIC. Using BIC we get the following

> AIC(glm.out4, k=log(sum(freq)))
[1] 343.0238
> AIC(glm.out5, k=log(sum(freq)))
[1] 276.621
> AIC(glm.out6, k=log(sum(freq)))
[1] 288.4533

# Since smaller BIC values are better as calculated by R's AIC
# function, the best model appears to be model 5, the model with 
# the eye:hair interaction but without sex:hair and sex:eye
# interactions which makes subtantive sense as well. 







