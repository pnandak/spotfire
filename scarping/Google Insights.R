
#########################################
## GLOBAL REQUIREMENTS AND DEFINITIONS ##
#########################################
require(RCurl)
require(XML)

loginURL        <- "https://accounts.google.com/ServiceLogin"
authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
insightsURL     <- "http://www.google.com/insights/search/overviewReport"



##########################
## FUNCTION DEFINITIONS ##
##########################

## get a Curl handle with cookies
setupCurl <- function() {
  require(RCurl)
  
  ch <- getCurlHandle()

  curlSetOpt(curl = ch,
             ssl.verifypeer = FALSE,
             useragent      = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13",
             timeout        = 60,
             followlocation = TRUE,
             cookiejar      = "./cookies",
             cookiefile     = "./cookies")
  return(ch)
}

## do Google Account login
doGoogleLogin <- function(username, password, ch) {
  require(RCurl)
  require(XML)
  
  loginPage <- getURL(loginURL, curl = ch)
  
  ## grab all the form field values using XPath
  loginPageTree <- htmlParse(loginPage, asText = TRUE)
  inputNodes <- getNodeSet(doc = loginPageTree, path = "//form[@id = 'gaia_loginform']//input")
  inputNames <- lapply(inputNodes, function(x) { xmlGetAttr(x, "name") })
  inputValues <- lapply(inputNodes, function(x) { xmlGetAttr(x, "value") })
  names(inputValues) <- inputNames
  
  inputValues[["Email"]] <- username
  inputValues[["Passwd"]] <- password
  authenticatePage <- postForm(authenticateURL, .params = inputValues, curl = ch)
}

## a function to get Google Insights results CSV
getResults <- function(query, ch, geo = NULL) {
  require(RCurl)

  params <- list(q = query, cmpt = "q", content = 1, export = 1)

  if(!is.null(geo)) {
    params[["geo"]] <- geo
  }

  resultsText <- getForm(insightsURL, .params = params, curl = ch)

  if(isTRUE(unname(attr(resultsText, "Content-Type"))[1] == "text/csv")) {
    ## got CSV file
    
    ## create temporary connection from results
    tt <- textConnection(resultsText)
    
    resultsCSV <- read.csv(tt, header = FALSE)
    
    ## close connection
    close(tt)
    
    return(resultsCSV)

  } else {
    ## something went wrong
    ## probably need to log in again?
    ## error handling goes here
  }
  return(NULL)
}


####################
## USAGE EXAMPLES ##
####################

### run this part once:
ch <- setupCurl()

doGoogleLogin("your-username-goes-here@gmail.com", "your-password-goes-here", ch)

### run getResults(...) as many times as you want
resultsCSV1 <- getResults("Sarah Palin", ch)
resultsCSV2 <- getResults("Mitt Romney", ch)

### or perhaps in a loop:
queries <- c("dog", "cat", "hat", "house", "mouse")
for(q in queries) {
  results <- getResults(q, ch)
  output.filename <- sprintf("%s.csv", q)
  write.csv(results, file = output.filename)
}