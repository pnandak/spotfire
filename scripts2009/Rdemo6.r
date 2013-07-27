########################################
##R Programming Session 6
#########################################

##lengths of strings

length("")

vv<- c("abcd","def","k")
vv0 <- vv[vv=="g"]

##length versus nchar
length((vv0))

length(vv)

length(vv[1])

nchar(vv[1])

nchar(vv)

##special characters

a<- 'The aunt\'s pen'
b<- "The aunt's pen"
a
b
a==b

vspecial <-"A \t tab"
print(vspecial)
cat(vspecial)
nchar(vspecial)

##paste
paste(c("X", "Y"), 1:4, sep="")
paste(c("X", "Y"), 1:4, sep="", collapse=" + ")

##substr, substring

substr("abcdef", 2, 4)

substring("abcdef", 1:6, 1:6)

substr(rep("abcdef", 4), 1:4, 4:5)

rep("abcdef", 4)
cbind(1:4, 4:5)

x <- c("asfef", "yuiop[", "b", "stuff.blah.yech")
substr(x, 2, 5) ## note results where index is not present

substring(x, 2, 4:6) ## uses recycling

substring(x, 2) <- c("..", "+++")
##note default final argument to substring is 1000000
##only portion of replacement string is replaced.

[1] "a..ef"  "y+++p["    "b"    "s+++f.blah.yech"

##example of strsplit
x<-c(as="asfef",qu="qwerty","yuiop[","b",
    "stuff.blah.yech")
strsplit(x,"e")  # split x on the letter e

##example of match
match(1:10,c(1,3,5,9))

1:10 %in% c(1,3,5,9)
pmatch("", "")                # NA
pmatch("m",c("mean","median","mode")) # NA
pmatch("med",c("mean","median","mode"))# 2
charmatch("", "")                     #1
charmatch("m",c("mean","median","mode"))#0
charmatch("med",c("mean","median","mode"))#2

##duplicates
pmatch(c("","ab","ab"),c("abc","ab"),dup=FALSE)
pmatch(c("", "ab", "ab"), c("abc", "ab"), dup=TRUE)
##Compare with \\
charmatch(c("", "ab", "ab"), c("abc", "ab"))

## print versus cat
x<- c("abc","dsef\textbackslash n","g")
print(x)
cat(x)

## more cexamples of cat, and other functions
d <- date()
cat("Today's date is:", substring(d, 1, 10),
    substring(d, 21, 24), "\n")
 cat(1, 2, 3, 4, 5, 6, fill = 8, labels = letters)
##format command
format(pi,width=12,nsmall=6)
format(c(pi,pi*pi,pi^3))
##modifying strings
x <- "My string 123"
chartr("yis", "abC", x)
chartr("r-tM", "A-Cw", x)
tolower(x)
toupper(x)
##regular expressions
x <- "My pattern string 32144"
##to demonstrate the matches
gregexpr("n.",x) #. matches any character
gregexpr("[1-3]",x) #[1-3] matches any 1, 2  or 3
gregexpr("n\\>",x) # only consider matches at end of a word
gregexpr("t{2}",x)  #only matches occurrences of 2 t's
##to demonstrate the behaviour of the different functions
grep("pa",c(x,tolower(x))) ## match in each element
grep("pa",c(x,toupper(x))) ## match in first element
grep("pa",c(x,toupper(x)),value=TRUE) # return values of element that matches

regexpr("pa",c(x,toupper(x))) #match in first
regexpr("n",c(x,toupper(x))) #match in
gregexpr("n",c(x,toupper(x)))
##replacement in strings
 x <- "My pattern string 12344"

sub("PA","pa",c(x,toupper(x)))

sub("n.","ZZ",c(x,toupper(x))) ## only changes first match

gsub("n.","XX",c(x,toupper(x))) ## changes both occurrences

###botanical database

###data
mycomments<- c("Salpichroa sp?, Cotopaxi NP, Ecuador",
             "Monnina sp, Cotopaxi NP, Ecuador",
             "Baccharis latifolia, Cotopaxi NP, Ecuador",
               "Calceolaria sp, Cotopaxi NP, Ecuador")
Genus1<- data.frame(genus=c(" ","Baccharis","Calceolaria"),
                     family=c("Not_Known","Asteraceae","Calceolariaceae"),
                    stringsAsFactors=FALSE)

###extract the botanical name
botname<- ifelse(regexpr(",",mycomments)>-1,
                 substring(mycomments,1,regexpr(",",mycomments)-1),
                 mycomments)
###or
mybotnamelist <- strsplit(mycomments,",")
mybotnamelist[c(1,2)]
sapply(mybotnamelist[c(1,2)],"[")
botname<- sapply(mybotnamelist,"[")[1,]
botname
###extract the genus (first part of name)
genus<- substring(botname,1,regexpr(" ",botname)-1)
genus
###create a data.frame of genera
genusdf <-  data.frame(genus=unique(genus),stringsAsFactors=FALSE)
###merge with the previous version, Genus (has family field too)
Genusall<- merge(Genus1,genusdf,all=TRUE)
Genusall
###outside R, fill in the families for the new genera, then read in again
### new data frame Genus
Genus2<- data.frame(genus=c(" ","Baccharis","Calceolaria","Monnina",
                    "Salpichroa"),
                     family=c("Not_Known","Asteraceae","Calceolariaceae",
                     "Polygalaceae", "Solanaceae"),
                   stringsAsFactors=FALSE )
###find the family for the new photos
Family <- Genus2[,2][match(genus,Genus2[,1],nomatch=1)]
cbind(genus,Family)

