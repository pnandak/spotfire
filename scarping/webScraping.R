# CMU MSP 36602 Spring 2013
# Web scraping

#### About html: http://www.stat.cmu.edu/~hseltman/simple.html ####

txt = readLines("http://www.stat.cmu.edu/~hseltman/simple.html")


#### Howard's Reading List ####

# Load a web page
nf = readLines("http://www.stat.cmu.edu/~hseltman/NF.html")

# Look for "links"
length(grep("<a href=", nf))
length(grep("<a href=", nf, ignore.case=TRUE))
hasHref = grep("<a[[:space:]]+href[[:space:]]*=", nf, ignore.case=TRUE)
length(hasHref)
hasFullHref = grep(paste("<[[:blank:]]*a[[:blank:]]+href[[:blank:]]*=.*?",
                  "<[[:blank:]]*[/]a[[:blank:]]*[>]", sep=""),
            nf, ignore.case=TRUE)
length(hasFullHref)
sd = setdiff(hasHref, hasFullHref)
nf[sort(c(sd[1:5], 1+sd[1:5]))]

# Get all links
hasHrefR = regexpr("<a[[:space:]]+href[[:space:]]*=.*?>", nf, ignore.case=TRUE)
urls = regmatches(nf, hasHrefR)
length(urls)
urls[1:5]

# Discard internal links
urls = urls[grep("\"#", urls, invert=TRUE)]
urls = sub(paste("<[[:blank:]]*a[[:blank:]]+href[[:blank:]]*=",
                 "[[:blank:]]*[(\"|\')]?([^\'\">]+)[(\"|\')]?[[:blank:]]*>", sep=""),
           "\\1", urls, ignore.case=TRUE)
urls[1:5]

# Report dead links
for (u in sample(urls,10)) {
  rslt = suppressWarnings(try(readLines(u, n=1, warn=FALSE), silent=TRUE))
  if (is(rslt, "try-error")) {
    cat("Dead link:", u, "\n")
  } else {
    cat("Good link:", u, "\n")
  }
  flush.console()
}



############ Chinese Astronauts ############

astronauts = function(Nationality) {
  ca = try(readLines(paste("http://en.wikipedia.org/wiki/List_of_",Nationality,"_astronauts", sep="")))
  if (is(ca, "try-error")) stop("no such wiki page")
  tabHrefR = regexpr("<td><a[[:space:]]+href[[:space:]]*=.*?>", ca, ignore.case=TRUE)
  urls = regmatches(ca, tabHrefR)
  preUrls = grep("<td><a[[:space:]]+href[[:space:]]*=.*?>", ca, ignore.case=TRUE)-1
  ast = urls[ca[preUrls]=="<tr>"]
  ast = grep("title=", ast, value=TRUE)
  ast = regmatches(ast, regexpr("/wiki/[[:alpha:]_].*?\"", ast))
  ast = sub("/wiki/(.*)\"", "\\1", ast)
  ast = sub("_(astronaut)", "", ast, fixed=TRUE)
  ast = sub("_", " ", ast)
  return(ast)
}
astronauts("Chinese")
astronauts("female")

# Better version
# 'tcol' is the desired table column
astronauts = function(Nationality, tcol=1) {
  ca = try(readLines(paste("http://en.wikipedia.org/wiki/List_of_",Nationality,"_astronauts", sep="")))
  if (is(ca, "try-error")) stop("no such wiki page")
  tr = grep("<tr.*?>", ca)
  tre = grep("</tr>", ca)
  tn = length(tr) # number of tables
  if (length(tre)!=tn) stop("malformed tables")
  allNames = NULL
  for (tab in 1:tn) {

    tdas = grep("<td><a[[:space:]]+href[[:space:]]*=.*?>", ca[tr[tab]:tre[tab]], ignore.case=TRUE)
    if (length(tdas)<tcol) next
    one = ca[tr[tab] - 1 + tdas[tcol]]
    if (length(grep("title=", one, fixed=TRUE)) == 0) next
    nam = regmatches(one, regexpr(">[[:blank:]]*([^<]+?)[[:blank:]]*</a>",  one))
    nam = sub(">(.+)<.*", "\\1", nam)
    if (nchar(nam)>0) allNames = c(allNames, nam)    
  }
  return(allNames)
}
astronauts("Chinese")
astronauts("female")
astronauts("female", tcol=2)
astronauts("Apollo")


######################################################################
#########################  Get and Post Methods ######################
######################################################################

##### NIST Chemistry Webbook: Search by Name ######
# http://webbook.nist.gov/chemistry/name-ser.html

# Key elements of the html:
#
# <input type="text" name="Name" id="Name" />
# <input type="radio" name="Units" value="CAL" id="CALunits" />
# <form action="../cgi/cbook.cgi" method="get">

# Enter "glucose" and click "Search"
# Resulting URL is: http://webbook.nist.gov/cgi/cbook.cgi?Name=glucose&Units=SI

txt = readLines("http://webbook.nist.gov/cgi/cbook.cgi?Name=glucose&Units=SI")
MWL = grep("Molecular weight", txt, value=TRUE)
# href=\"http://www.nist.gov/nist-exit-script.cfm?url=
# http://goldbook.iupac.org/R05271.html\">Molecular weight</a>:</strong> 180.1559</li>
#
MWP = regmatches(MWL, regexpr("</strong>[[:blank:]]*[0-9.]+[[:blank:]]*</li>$", MWL))
MW = regmatches(MWP, regexpr("[0-9.]+", MWP))
cat("Molecular weight is ", MW, ".\n", sep="")


# generic version of molecular weight function
mwNist = function(name) {
  url = paste("http://webbook.nist.gov/cgi/cbook.cgi?Name=",
              name, "&Units=SI", sep="")
  txt = try(readLines(url, warn=FALSE), silent=TRUE)
  if (is(txt, "try-error"))
    stop(name, " is not found at NIST")

  MWL = grep("Molecular weight", txt, value=TRUE)
  MWP = regmatches(MWL, regexpr("</strong>[[:blank:]]*[0-9.]+[[:blank:]]*</li>$", MWL))
  MW = regmatches(MWP, regexpr("[0-9.]+", MWP))
  return(MW)
}

mwNist("glucose")
mwNist("hydrogen_sulfide")
mwNist("benzene")


######################################################

###### CMU directory #######
# http://directory.andrew.cmu.edu/

# Key elements of html
#
# <form action="/search/basic/results" method="post">
# <input name="authenticity_token" type="hidden" value="GwHLioxn7+Vntnc4IHKXaU+0H3x9euQXdItEgrgEbyU=" />
# <input id="search_generic_search_terms" name="search[generic_search_terms]" />
# <input id="search_submit" name="commit" type="submit" value="Search" />

# Enter "Seltman" and click "Search"
# Resulting URL is http://directory.andrew.cmu.edu/search/basic/results

# Using package RCurl
if (!require(RCurl)) { # for getURL()
  install.packages("RCurl")
  library(RCurl)
}

AT = unlist(strsplit(getURL("http://directory.andrew.cmu.edu/"), "\\n"))
AT = grep("authenticity", AT, value=TRUE)
AT = regmatches(AT, regexpr("value=.*?>", AT))
AT = substring(AT, 8, nchar(AT)-4)
rslt = postForm("http://directory.andrew.cmu.edu/search/basic/results",
               authenticity_token=AT,
               "search[generic_search_terms]"="Seltman", commit="Search")
rslt = unlist(strsplit(rslt, "\\n"))
phone = regmatches(rslt, regexpr("[(]\\d{3}[)][[:blank:]]*\\d{3}-\\d{4}", rslt))
cat("Phone: ", phone, "\n")

# As a function
callCMU = function(name) {
  AT = try(getURL("http://directory.andrew.cmu.edu/"), silent=TRUE)
  if (is(AT, "try-error")) stop("Can't read directory @ CMU")
  AT = unlist(strsplit(AT, "\\n"))
  AT = grep("authenticity", AT, value=TRUE)
  if (length(AT)!=1) stop("Can't read authenticity code")
  AT = regmatches(AT, regexpr("value=.*?>", AT))
  if (length(AT)!=1 || nchar(AT)<13) stop("Can't decypher authenticity code")
  AT = substring(AT, 8, nchar(AT)-4)
  rslt = try(postForm("http://directory.andrew.cmu.edu/search/basic/results",
                      authenticity_token=AT,
                      "search[generic_search_terms]"=name, commit="Search"),
             silent=TRUE)
  if (is(rslt, "try-error")) stop("Can't find ", name, " in directory @ CMU")
  rslt = unlist(strsplit(rslt, "\\n"))
  if (length(grep("people matched", rslt)>0))
    stop("Non unique; try 'Firstname Lastname'")
  phone = regmatches(rslt, regexpr("[(]\\d{3}[)][[:blank:]]*\\d{3}-\\d{4}", rslt))
  if (length(phone)==0) stop("Can't find phone number(s)")
  return(phone)
}

callCMU("Seltman")
callCMU("Volichenko")
callCMU("Greenhouse")
callCMU("Joel Greenhouse")






######################################################################
############# Fancier versions for more complex analyses: XML ########
######################################################################

if (!require(XML)) { # for htmlTreeParse, et al.
  install.packages("XML")
  library(XML)
}

# Get page and "parse" it as a tree
nf = getURL("http://www.stat.cmu.edu/~hseltman/NF.html")
nfp = htmlTreeParse(nf)

# The tree's root has 2 nodes: head and body (usually)
r = xmlRoot(nfp)
xmlSize(r) # 2
sapply(xmlChildren(r), xmlName)
#   head   body 
# "head" "body" 

# We only need to look at the body
bod = r[[2]]
bodSizes = sapply(xmlChildren(bod), xmlSize)
bodSizes
#center      a   text      a   text      a   text      p     ul      p     hr      p     ul      p 
#     1      1      0      1      0      1      0      2      3      0      0      1    276      0 
#    hr      p     ul      p     hr      p      p 
#     0      1     27      0      0      2      1 


# The "unordered lists" on this page are in the "ul" elements
ul = which(names(bodSizes)=="ul")
ul
#  9 13 17 


# Examine ul elements
xmlSize(bod[[ul[1]]]) # 3
bod[[ul[1]]]

bod[[ul[2]]]

######## New approach (not ideal)
nfl = XML::xmlToList(nfp[[1]])
sapply(nfl, length)

sapply(nfl$head, length)

sapply(nfl$body, length)

ul = which(names(nfl$body)=="ul")
nfl$body[[ul[1]]]
nfl$body[[ul[2]]]

sapply(nfl$body[[ul[2]]],length)

class(nfl$body[[ul[2]]])
length(nfl$body[[ul[2]]])
names(nfl$body[[ul[2]]])

class(nfl$body[[ul[2]]][[2]])
length(nfl$body[[ul[2]]][[2]])
names(nfl$body[[ul[2]]][[2]])
nfl$body[[ul[2]]][[2]]
unlist(nfl$body[[ul[2]]][[2]])

# List N books
N=5
for (i in 1:N) {
  tmp = unlist(nfl$body[[ul[2]]][[i]])
  if (!is.null(names(tmp))) {
    tmp = tmp[grep("href",names(tmp),invert=TRUE)]
  }
  cat(paste(tmp, collapse=" "), "\n")
}


# Better: handle punctuation, add list name as an attribute
# 'group' is the list number (1-3 for NF.html)
# 'range' is the range of book numbers in the group
listBooks = function(bodyList, group, range) {
  if (!is.list(bodyList)) stop("need a list")
  ul = which(names(bodyList)=="ul")
  if (length(ul)==0)
    stop("no 'ul' elements in ", deparse(substitute(bodyList)))
  if (group<1 || group>length(ul))
    stop("only ", length(ul), " 'ul' elements are present")
  txt = bodyList[[ul[group]]]
  if (length(txt)==0) stop("empty 'ul' element")
  if (min(range)<1 || max(range)>length(txt))
    stop("'ul' element ", group, " has only ", length(txt), " elements")
  allText = NULL
  for (i in range) {
    one = unlist(txt[[i]])
    if (!is.null(names(one)))
      one = one[grep("href", names(one), invert=TRUE)]
    noStartPunc = grep("^[[:punct:]]", one, invert=TRUE)
    noStartPunc = setdiff(noStartPunc, 1)
    one[noStartPunc] = paste("", one[noStartPunc])
    one = paste(one, collapse="")
    one = gsub("\\n ", "", one)
    allText = c(allText, one)
    if (ul[group]>1 && names(bodyList[ul[group]-1])=="p")
      attr(allText,"listName") = unlist(bodyList[[ul[group]-1]][1])[1]
  }
  return(allText)
}

listBooks(nfl$body, 2, 2:6)

listBooks(nfl$body, 1, 2:6)
listBooks(nfl$body, 1, 1:2)

listBooks(nfl$body, 3, 12:16)
