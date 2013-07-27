# Copyright 2010 Google Inc. All Rights Reserved.
# Author: Mike Pearmain.
# Author: Nick Mihailovski.
# Author: Nicolas Remy.

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This script allows to use the Google Analytics (GA) API to read data
# directly from GA into R as a data.frame.
# As of now this script allows you to:
#   - Create a new export object.
#   - Authenticate with your Google Account.
#   - Return an account profile for an authorised Google Account.
#   - Create a new API query
#   - Use the query to return a data.frame populated with metrics.
# It requires the RCurl and XML packages.
# These packages can be downloaded from http://www.omegahat.org/R
# using the following command,
# 'install.packages('RCurl', repos = "http://www.omegahat.org/R")'
# 'install.packages('XML', repos = "http://www.omegahat.org/R")'
#
# If errors occur when downloading 'Rcurl' or XML' packages ensure the libcurl
# and libxml libraries are up to date on the Linux machine.
# 'sudo apt install libxml2-dev'
# 'sudo apt install libcurl4-gnutls-dev'
#
# A QueryBuilder.R script is also available as set of helper functions for
# constructing GA uri queries.

library(RCurl)
library(XML)

# R functions to load data,

RGoogleAnalytics <- function() {
  # Creates a skeleton shell for accessing the Google Analytics API.
  #
  # Returns:
  #   Returns a list of methods, for accessing the Google Analytics API.
  #   GetXmlDataFeed(),
  #   GetRDataFromXML(),
  #   SetCredentials(),
  #   GetProfileXML(),
  #   GetProfileData(),
  #   GetReportData(),
  #   For more information please look at the help pages for each function.
  #
  # Examples:
  #   ga <- RGoogleAnalytics()
  #   ga$SetCredentials("INSERT_USER_NAME", "INSERT_PASSWORD")
  #   # Get the list of different profiles, to help build the query.
  #   prof <- ga$GetProfileData()
  #   # Build the query.
  #   query.builder <- QueryBuilder()
  #   query.builder$Init(start.date = "2010-05-01",
  #                      end.date   = "2010-08-20",
  #                      dimensions = "ga:date",
  #                      metrics    = "ga:visits",
  #                      sort       = "ga:date",
  #                      table.id   = "ga:30661272")
  #   ga.data <- ga$GetReportData(query.builder)
  #   # Look at the data returned.
  #   head(ga.data$data)

  # Private members.
  # We set the authorization token to ensure the user can access the profile
  # information, and retrieve data.
  # We explicitly avoiding storing the user name and password for security
  # reasons. The Auth token is valid for 14 days.
  auth.token <- NULL

  SetCredentials <- function(user, pass) {
    # Fetches the GA Auth Token for a user with a password.
    # Fetches GA authentication token, which can then be used to fetch data,
    # Technically works by submitting the information to the API and storing the
    # account information in gtoken, we then return the 'Auth' entry token.
    # As mentioned above we don't keep the user name or passwords for security
    # reasons.
    #
    # Args:
    #   user: A valid user name for a GA account.
    #   pass: A valid password for the GA account.
    # Returns:
    #   Returns the authentication token of the users account.

    # Error handling.
    # Ensure that both user name and password have been entered.
    if (is.null(user) || is.null(pass)) {
      stop("Please supply a user name and password")
    }

    auth.resp <- postForm("https://www.google.com/accounts/ClientLogin",
                          Email = user,
                          Passwd = pass,
                          accountType = "GOOGLE",
                          source = "r-google-analytics",
                          service = "analytics")
    gtoken <- unlist(strsplit(auth.resp, "\n"))
    parsed.gtoken <- unlist(strsplit(gtoken[3], "Auth="))
    if (length(parsed.gtoken) >= 2) {
      auth.token <<- unlist(strsplit(gtoken[3], "Auth="))[[2]]
    } else {
      stop("Authentication failed.")
    }

    return(invisible())
  }

  CheckAuthToken <- function() {
    # A Check to see if there is a valid authorization token.
    # We test for the presence of the authorization token at various points in
    # the code. The SetCredentials function sets the global auth.token on
    # assignment, so this checks the presence of the variable.
    #
    # Returns:
    #   A stop call if the auth.token has not been retrieved.
    if (is.null(auth.token))
      stop("Please enter the user name and password in SetCredentials")
  }

  GetAnyXMLAttribute <- function(vNode, attr.name) {
    # Function to return the value attribute of the nodes of the parsed XML.
    #
    # Args:
    #   vNode: The XML node to be inspected.
    #   attr.name: The attribute to be returned.
    # Returns:
    #   The value contained with the XML node.
    if (xmlName(vNode) == "metric")
      return(as.numeric(xmlGetAttr(vNode, attr.name)))
    else
      return(xmlGetAttr(vNode, attr.name))
  }

  GetProfileXML <- function() {
    # Returns a profile XML for an account
    # For each authenticated user for a GA, they may have multiple accounts,
    # We retrieve a raw XML of all the accounts an authenticated user has access
    # to.
    #
    # Returns:
    #   The raw XML string from the GA API.
    CheckAuthToken()
    google.auth <- paste("GoogleLogin auth=", auth.token)
    profile.feed.list <-
      getURL("https://www.google.com/analytics/feeds/accounts/default",
             .encoding = 'UTF-8',
             httpheader = c("Authorization" = google.auth,
               "GData-Version" = 2))
    return(profile.feed.list)
  }

  GetProfileData <- function(profile = GetProfileXML()) {
    # A function to convert the profile XML into an R data.frame.
    # We have the raw XML for a user profile from the GetProfileXML() function,
    # we clean this information and place into a human readable table in R
    # for consumption.  We also allow the user to pass in a string XML if
    # required for offline inspection.
    #
    # Args:
    #   profile: The XML string to be processed.
    # Returns:
    #   A list of the processed XML string comprising of the profile
    #   as a data.frame and the number of accounts the profile has access to.

    # Get XML data into an R object.
    profile.feed.list <- profile
    feed.xml <- xmlTreeParse(profile.feed.list,
                             asText = TRUE,
                             useInternalNode = TRUE)

    # get namespaces of XML doc.
    feed.name.space <- sapply(xmlNamespaceDefinitions(feed.xml),
                              function(ns) ns$uri)
    names(feed.name.space)[1] <- "ns"

    # Return the total number of results available.
    total.results <-
      data.frame(unlist(xpathApply(feed.xml,
                                   path = "//openSearch:totalResults",
                                   xmlValue,
                                   namespaces = feed.name.space)))
    names(total.results) <- "total.results"

    # Return the Table ID's from the account feed.
    table.id.list <-
      data.frame(unlist(xpathApply(feed.xml,
                                   path = "//ns:entry/dxp:tableId",
                                   xmlValue,
                                   namespaces = feed.name.space)))

    ns.path <- "//ns:entry/dxp:property[@name = 'ga:accountName']"
    # Return the account name from the account feed.
    table.account.list <-
      data.frame(unlist(xpathApply(feed.xml,
                                   path = ns.path,
                                   GetAnyXMLAttribute,
                                   "value",
                                   namespaces = feed.name.space)))

    # Return the profile name from the account feed.
    table.profile.list <-
      data.frame(unlist(xpathApply(feed.xml,
                                   path = "//ns:entry/ns:title",
                                   xmlValue,
                                   namespaces = feed.name.space)))

    # Join the data.frames and rename the variables.
    profile <- cbind(table.account.list,
                     table.profile.list,
                     table.id.list)

    names(profile) <- c("AccountName", "ProfileName", "TableId")

    # We return the profiles and the results.
    return(list(profile = profile,
                total.results = total.results))
  }

  GetXMLDataFeed <- function(query) {
    # Returns the data XML for a requested uri.
    # This function uses the RCurl packages to pass the built uri
    # query and authenticate the  Google Account being used.
    #
    # Args:
    #   query: This is the uri query.
    # Returns:
    #  An XML string of the data returned from GA of the specified query.
    CheckAuthToken()
    # Get GA data Feed from API.
    data.feed.uri <- query
    google.auth <- paste("GoogleLogin auth=", auth.token)
    feed.data <- getURL(data.feed.uri,
                        .encoding = 'UTF-8',
                        httpheader = c("Authorization" = google.auth,
                          "GData-Version" = 2))
    return(feed.data)
  }

  GetRDataFromXML <- function(xml.string) {
    # A function to convert the XML data into an R data.frame.
    # This function takes an XML string (GA schema) and processes the
    # XML to return an R data.frame.
    #
    # Args:
    #   xml.string: An XML string from matching the GA API schema.
    # Returns:
    #  A list of information extracted from the XML;
    #  data: A data.frame of metric and/or dimension attributes and values, this
    #        will include C.I's if over 500,000 entries.
    #  aggr.totals: GA might match millions of rows of data, but the API will
    #               only return a max of 10k rows at a time. Along with every
    #               response, the API will return the aggregates of each metric
    #               that was matched (not returned).
    #  total.results: The total number of rows of data that GA matched.

    # Error checking.
    # Ensure the input is of character format.
    if (!is.character(xml.string)) {
      stop("xml.string must be a character string")
    }

    feed.data = xml.string
    # get XML data into an R object.
    feed.xml <- xmlTreeParse(feed.data,
                             asText = TRUE,
                             useInternalNode = TRUE)

    # get namespaces of XML doc.
    feed.name.space <- sapply(xmlNamespaceDefinitions(feed.xml),
                              function(ns) ns$uri)
    names(feed.name.space)[1] <- "ns"

    # get the titles of the dimensions and metrics.
    titles <- unlist(xpathApply(feed.xml,
                                "//ns:entry[1]/*[@name]",
                                xmlGetAttr, "name",
                                namespaces = feed.name.space))

    # return the values of the titles as a data frame.
    df.value <- data.frame(sapply(titles, function(name) {
      unlist(xpathApply(feed.xml,
                        paste("//ns:entry/dxp:*[@name='", name, "']",
                              sep = ""),
                        GetAnyXMLAttribute, "value",
                        namespaces = feed.name.space))
    }))

    # get the names of the confidence interval on the metrics.
    ci.check <- unlist(xpathApply(feed.xml,
                                  "//ns:entry[1]/*[@confidenceInterval]",
                                  xmlGetAttr, "name",
                                  namespaces = feed.name.space))

    # return the C.I of the metrics as a data frame.
    df.ci <- data.frame(sapply(ci.check, function(name) {
      unlist(xpathApply(feed.xml,
                        paste("//ns:entry/dxp:*[@name='", name, "']",
                              sep = ""),
                        GetAnyXMLAttribute, "confidenceInterval",
                        namespaces = feed.name.space))
    }))
    # Rename the variables to reflect C.I status.
    # If the sum of all values is 0, remove
    names(df.ci) <- sub("$", ".C.I", names(df.ci))
    # join the C.I with the data frames if sum of col != 0
    df <- cbind(df.value, df.ci[colSums(df.ci) != 0])

    # get the names of the Aggregates on the metrics
    aggr.check <- unlist(xpathApply(feed.xml,
                                    "//dxp:aggregates/dxp:metric",
                                    xmlGetAttr, "name",
                                    namespaces = feed.name.space))

    # return the values of the aggr as a data frame.
    df.aggr <-
      data.frame("aggregate.totals" = sapply(aggr.check, function(name) {
        unlist(xpathApply(feed.xml,
                          paste("//dxp:aggregates/dxp:*[@name='", name, "']",
                                sep = ""),
                          GetAnyXMLAttribute,
                          "value",
                          namespaces = feed.name.space))
      }))
    # return the total results.
    # The total_results element of the feed, gives you the total numbers of
    # entries (rows) of data that GA matched.
    total.results <- unlist(xpathApply(feed.xml,
                                       path = "//openSearch:totalResults",
                                       xmlValue,
                                       namespaces = feed.name.space))
    names(total.results) <- "total.results"
    total.results <- as.numeric(total.results)
    return(list(data = df,
                aggr.totals = df.aggr,
                total.results = total.results))

  }

  GetReportData <- function(query.builder,
                            start.index = 1,
                            max.rows = 1000) {
    # Returns the data specified by the query, auto-paginating and combining
    # rows if needed. This also validates the query to ensure the minimum
    # required parameters are set.
    #
    # Args:
    #   query.builder: An instance of the QueryBuilder() function.
    #   start.index: The starting point for where GA retrieves data.
    #   max.rows: The total number of results to return and join together in
    #             the data. This is set to 10,000 at the start so the user can
    #             see the total number, and then choose.  We have an upper
    #             limit of 1,000,000 rows in place.
    # Returns:
    #   A R data.frame of all the rows of data available up to the first
    #   1,000,000.

    query.builder$validate()

    # Ensure the starting index is set per the user request
    # We can only return 10,000 rows in a single query
    max.default.rows.per.query <- 10000
    query.builder$max.results(min(max.default.rows.per.query, max.rows))
    query.uri <- query.builder$to.uri()

    ga.feed <- GetXMLDataFeed(query.uri)
    df <- GetRDataFromXML(ga.feed)

    # find the total calls needed, and max of 1,000,000 rows by forcing max
    # pagination == 100
    if(max.rows < max.default.rows.per.query + 1) {
      return(df)
    } else {
      pagination <- min(100, ceiling(df$total.results /
                                     max.default.rows.per.query))
      # We have the first 10,000 rows we add the rest of the data.
      for (i in seq_along(2:pagination)) {
        start.index <- (i * max.default.rows.per.query) + 1
        query.builder$start.index(start.index)
        query.uri <- query.builder$to.uri()
        ga.feed <- GetXMLDataFeed(query.uri)
        ga.data <- GetRDataFromXML(ga.feed)
        df$data <- rbind(df$data, ga.data$data)
        rm(ga.data)
      }
      return(df)
    }
  }


  ##############################################################################
  return(list(GetXMLDataFeed  = GetXMLDataFeed,
              GetRDataFromXML = GetRDataFromXML,
              SetCredentials  = SetCredentials,
              GetProfileXML   = GetProfileXML,
              GetProfileData  = GetProfileData,
              GetReportData   = GetReportData))
}