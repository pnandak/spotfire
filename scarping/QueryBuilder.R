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

# This is the QueryBuilder() function to be used with the RGoogleAnalytics()
# functions in order to process a request from the GA API.
# The RGoogleAnalytics() functions are not dependent on this, but we use
# this as a helper for constructing the correct URI's to retrieve data.

QueryBuilder <- function() {
  # The main builder class for constructing URI requests.
  # This function lists all the elements and parameters that make up a data
  # feed request. In general, you provide the table ID corresponding to the
  # profile you want to retrieve data from, choose the combination of
  # dimensions and metrics, and provide a date range along with other
  # parameters in a query string.
  #
  # More detailed information on each parameter can be found on the below:
  # http://code.google.com/apis/analytics/docs/
  #       gdata/gdataReferenceDataFeed.html#dataRequest
  #
  # We use a builder method to construct the final query for data checking and
  # exceptions, rather than raw strings.
  # This also enables us to scale the parameters map in later versions.
  #
  # All the parameter values that are accepted for a profile can be found
  # by looking at the metrics available in the GetProfileData() function.
  #
  # Returns:
  #   builder: The builder method function to process the parameters.
  #
  # Example:
  # # An example of using this construction pattern is shown below.
  #
  # # Set the relevant variable information, if the variable is optional, you
  # # do not need to specify it in the construction or simply place it as NULL.
  #
  # # query <- QueryBuilder()
  # # query$Init(start.date = "2010-05-01",
  # #            end.date   = "2010-08-20",
  # #            dimensions = "ga:date",
  # #            metrics    = "ga:visits",
  # #            sort       = "ga:date",
  # #            table.id   = "ga:30661272")
  # # ga.data <- ga$GetRDataFromQuery(query)

  # Constants.
  kMaxDimensions <- 7
  kMaxMetrics <- 10
  kMaxTableIds <- 1

  # Query parameters.
  start.date  <- NULL
  end.date    <- NULL
  dimensions  <- NULL
  metrics     <- NULL
  segment     <- NULL
  sort        <- NULL
  filters     <- NULL
  max.results <- NULL
  start.index <- NULL
  table.id    <- NULL

  StartDate <- function(start.date.param = NA) {
    # Sets the start date.
    # Optional.
    # All Analytics feed requests must specify a beginning and ending date
    # range. If you do not indicate start- and end-date values for the
    # request, the server returns a request error.
    # Date values are in the form YYYY-MM-DD.
    # The earliest valid start-date is 2005-01-01. There is no upper limit
    # restriction for a start-date. However, setting a start-date that is
    # too  far in the future will most likely return empty results.
    #
    #  Args:
    #    start.date.param: Optional. A start date of the form "YYYY-MM-DD"
    #                      as a string. If NULL is used, the start.date
    #                      parameter will be unset. If no parameter is
    #                      specified, the current start.date value is
    #                      returned.
    #
    #  Returns:
    #    The start.date value if start.date.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(start.date.param)) {
      start.date <<- NULL
      return(invisible())
    }

    # Returns the current dimension value if no parameter is used.
    if (is.na(start.date.param)) {
      return(start.date)
    }

    # Error handling.
    # Check the form of the start.date.param.
    if (is.na(as.Date(start.date.param, "%Y-%m-%d"))) {
      stop("A start date must be specified of the form YYYY-MM-DD")
    }

    start.date <<- start.date.param
    return(invisible())
  }

  EndDate <- function(end.date.param = NA) {
    # Sets the end date.
    # Optional.
    # All Analytics feed requests must specify a beginning and ending date
    # range. If you do not indicate start- and end-date values for the
    # request, the server returns a request error.
    # Date values are in the form YYYY-MM-DD.
    # The earliest valid start-date is 2005-01-01. There is no upper limit
    # restriction for a start-date. However, setting a start-date that is
    # too far in the future will most likely return empty results.
    #
    #  Args:
    #    end.date.param: An end date of the form 'YYYY-MM-DD'
    #                    as a string. If NULL is used, the end.date.param
    #                    parameter will be unset. If no parameter is specified,
    #                    the current end.date value is returned.
    #
    #  Returns:
    #    The end.date value if end.date.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(end.date.param)) {
      end.date <<- NULL
      return(invisible())
    }

    # Returns the current dimension value if no parameter is used.
    if (is.na(end.date.param)) {
      return(end.date)
    }

    # Error handling.
    # Check the form of the end.date.param.
    if (is.na(as.Date(end.date.param, "%Y-%m-%d"))) {
      stop("An end date must be specified of the form YYYY-MM-DD")
    }

    end.date <<- end.date.param
    return(invisible())
  }

  Dimensions <- function(dimensions.param = NA) {
    # Sets the dimensions.
    # Optional.
    # The dimensions parameter defines the primary data keys for your
    # Analytics report, such as ga:browser or ga:city. Use dimensions to
    # segment your web property metrics. For example, while you can ask for
    # the total number of pageviews to your site, it might be more
    # interesting to ask for the number of pageviews segmented by browser.
    # In this case, you'll see the number of pageviews from Firefox,
    # Internet Explorer, Chrome, and so forth.
    #
    # When using dimensions in a feed request, be aware of the following
    # constraints:
    #   You can supply a maximum of 7 dimensions for any query.
    #   You can not send a query comprised only of dimensions:
    #     You must combine any requested dimension with at least one metric.
    #     Any given dimension can be used with other dimensions or metrics,
    #       but only where Valid Combinations apply for that dimension.
    #
    # More information on valid combinations can be found here:
    # http://code.google.com/apis/analytics/docs/gdata/
    #        gdataReferenceDimensionsMetrics.html#validCombinations
    #
    # NOTE: This method does not check for invalid dimensions or combinations.
    #
    #  Args:
    #    dimensions.param: A vector of up to 7 dimensions, either as
    #                      a single string or a vector or strings, E.g.
    #                      "ga:source,ga:medium" or c("ga:source", "ga:medium")
    #                      If NULL is used, the dimensions parameter will be
    #                      unset. If no parameter is specified, the current
    #                      dimension value is returned.
    #
    #  Returns:
    #    The dimensions value if dimensions.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(dimensions.param)) {
      dimensions <<- NULL
      return(invisible())
    }

    # Returns the current dimension value if no parameter is used.
    if (is.na(dimensions.param[1])) {
      return(dimensions)
    }

    # Error handling.
    # Validate the dimensions input is a vector.
    if (!is.vector(dimensions.param)) {
      stop(paste("dimensions must be a vector of string variables"))
    }

    # Error handling.
    # Validate the length of the vector is no greater than the max number
    # of allowed dimensions.
    if (length(dimensions.param) > kMaxDimensions) {
      stop(paste("Google Analytics can only handle up to", kMaxDimensions,
        "dimensions parameters"))
    }

    # Error handling.
    # Validate the vector is a character type.
    # This will not stop a vector like, c("2", "this")
    if (!is.character(dimensions.param)) {
      stop(paste("dimensions must be character, please refer to the",
                 "Google Analytics API documentation for more information"))
    }

    dimensions <<- paste(dimensions.param, collapse = ",")
    return(invisible())
  }

  Metrics <- function(metrics.param = NA) {
    # Sets the metrics of interest (clicks, pageviews, etc)
    # Optional.
    # The aggregated statistics for user activity in a profile, such as
    # clicks or pageviews. When queried by alone, metrics provide aggregate
    # values for the requested date range, such as overall pageviews or
    # total bounces. However, when requested with dimensions, values are
    # segmented by the dimension. For example, ga:pageviews requested with
    # ga:country returns the total pageviews per country rather than the
    # total pageviews for the entire profile. When requesting metrics, keep
    # in mind:
    #
    # Any request must supply at least one metric because a request cannot
    # consist only of dimensions.
    # You can supply a maximum of 10 metrics for any query.
    # Most combinations of metrics from multiple categories can be used
    # together, provided no dimensions are specified.
    # The exception to the above is the ga:visitors metric, which can only
    # be used in combination with a subset of metrics.
    # Any given metric can be used in combination with other dimensions or
    # metrics, but only where Valid Combinations apply for that metric.
    # Metric values are always reported as an aggregate because the Data
    # Export API does not provide calculated metrics. For a list of common
    # calculations based on aggregate metrics.
    #
    # NOTE: We do check for valid metrics.
    #
    # Args:
    #   metrics.param: A vector of up to 10 dimensions, either as
    #                  a single string or a vector or strings. E.g.
    #                  "ga:visits" or c("ga:visits", "ga:bounces")
    #                  If NULL is used, the metrics parameter will be
    #                  unset. If no parameter is specified, the current
    #                  metrics value is returned.
    # Returns:
    #   The metrics value if metrics.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(metrics.param)) {
      metrics <<- NULL
      return(invisible())
    }

    # Returns the current metrics value if no parameter is used.
    if (is.na(metrics.param[1])) {
      return(metrics)
    }

    # Error handling.
    # Check the metrics input is that of a vector
    if (!is.vector(metrics.param)) {
      stop("metrics must be a vector of string variables")
    }

    # Error handling.
    # Check the length of the vector is no greater than the max number of
    # metrics.
    if (length(metrics.param) > kMaxMetrics) {
      stop(paste("Google Analytics can only handle up to", kMaxMetrics,
        "metrics parameters"))
    }

    # Error handling.
    # Check the vector is a character type.
    # this will not stop a vector like, c("2", "this")
    if (!is.character(metrics.param)) {
      stop(paste("metrics must be character string, please refer to the",
                 "Google Analytics API documentation for more information"))
    }

    # Combine and store the parameters.
    metrics <<- paste(metrics.param, collapse = ",")
    return(invisible())
  }

  Segment <- function(segment.param = NA) {
    # Sets the segments, see dxp:segment in the Account Feed Response section
    # in the GA literature online.
    # http://code.google.com/apis/analytics/docs/gdata/gdataDeveloperGuide.html
    # Optional.
    # For general information on advanced segments, see Advanced
    # Segmentation in the Help Center. You can request an advanced segment
    # in the data feed in two ways:
    #
    # (1) The numeric ID of a default or custom advanced segment.
    #     The account feed returns all default advanced segments and their
    #     IDs, as well as any custom segments defined for the account.
    #     For more information on segment and their IDs, see dxp:segment in
    #     the Account Feed Response section.
    # (2) The dynamic parameter in the query.
    #     Use this method to segment your data request by one or more
    #     dimensions and/or metrics. You can also use regular expressions
    #     for segments just as you would for the filters parameter.
    #     Dynamic segments use the same Expressions and Operators used for
    #     the filters parameter. When using OR boolean logic or AND boolean
    #     logic, dynamic segment expressions follow the same rules as for
    #     the filters parameter, except that you may use OR boolean logic
    #     with both dimensions or metrics.
    # Dimensions/metrics combinations in the advanced segment expression
    # have fewer restrictions. Except where noted in the table, you can use
    # any dimension or metric in combination with another in your filter.
    #
    # The segment parameter is once again difficult to write checks for,
    # as this is a handler we rely on the GA API to report errors with the
    # request.
    #
    # Example:
    # gaid::10
    # dynamic::ga:medium==referral
    #
    # Args:
    #   segment: An advanced segment definition to slice and dice your
    #            Analytics data. If NULL is used, the segment parameter will be
    #            unset. If no parameter is specified, the current segment value
    #            is returned.
    #
    # Returns:
    #   The segment value if segment.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(segment.param)) {
      segment <<- NULL
      return(invisible())
    }

    # Returns the current segment value if no parameter is used.
    if (is.na(segment.param[1])) {
      return(segment)
    }

    segment <<- segment.param
    return(invisible())
  }

  Sort <- function(sort.param = NA) {
    # Sets the sorting criteria.
    # Optional.
    # Indicates the sorting order and direction for the returned data.
    # For example, the following parameter would first sort by ga:browser
    # and then by ga:pageviews in ascending order.
    #
    # If you do not indicate a sorting order in your query, the data is
    # sorted by dimension from left to right in the order listed.
    # When using the sort parameter, keep in mind the following:
    # Sort only by dimensions or metrics value that you have used in the
    # dimensions or metrics parameter. If your request sorts on a field that
    # is not indicated in either the dimensions or metrics parameter, you
    # will receive a request error.
    #
    # Google Analytics treats dimensions as strings, so all dimensions are
    # sorted in ascending alphabetical order in an en-US locale.
    # Google Analytics treats all metrics as numbers, so all metrics are
    # sorted in ascending numeric order.
    #
    # The sort direction can be changed from ascending to descending by
    # using a minus sign (-) prefix on the requested field.
    #
    # Note: We do not check that the sort parameters are also defined in
    # the dimensions or metrics parameters.
    #
    # Args:
    #   sort: The sorting order for the data to be returned.
    #         e.g. "ga:visits" or c("ga:visits", "-ga:browser")
    #         If NULL is used, the sort parameter will be
    #         unset. If no parameter is specified, the current sort value
    #         is returned.
    #
    # Returns:
    #  The sort value if sort.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(sort.param)) {
      sort <<- NULL
      return(invisible())
    }

    # Returns the current sort value if no parameter is used.
    if (is.na(sort.param[1])) {
      return(sort)
    }

    # Error handling.
    # Check the sort input is that of a vector
    if (!is.vector(sort.param)) {
      stop("sort must be a vector of string variables")
    }

    # Error handling.
    # Check the vector is a character type.
    # this will not stop a vector like, c("2", "this")
    if (!is.character(sort.param)) {
      stop(paste("sort must be character string, please refer to the",
                 "Google Analytics API documentation for more information"))
    }

    # Combine the elements.
    sort <<- paste(sort.param, collapse = ",")
    return(invisible())
  }

  Filters <- function(filters.param = NA) {
    # Sets the filters used.
    # Optional.
    # The filters query string parameter restricts the data returned from
    # your request to the Analytics servers. When you use the filters
    # parameter, you supply a dimension or metric you want to filter,
    # followed by the filter expression. For example, the following feed
    # query requests ga:pageviews and ga:browser from profile 12134, where
    # the ga:browser dimension starts with the string Firefox:
    #
    # Args:
    #   filters: The filter string for the GA request.
    #            e.g. "ga:medium==referral".
    #            If NULL is used, the filters parameter will be unset.
    #            If no parameter is specified, the current filters value
    #            is returned.
    #
    # Returns:
    #   The filters value if filters.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(filters.param)) {
      filters <<- NULL
      return(invisible())
    }

    # Returns the current sort value if no parameter is used.
    if (is.na(filters.param[1])) {
      return(filters)
    }

    filters <<- filters.param
    return(invisible())
  }

  MaxResults <- function(max.results.param = NA) {
    # Sets the maximum number of results to return.
    # Optional.
    # Maximum number of entries to include in this feed. You can use this in
    # combination with start-index to retrieve a subset of elements, or use
    # it alone to restrict the number of returned elements, starting with
    # the first.
    #
    # If you do not use the max-results parameter in your query, your feed
    # returns the default maximum of 1000 entries.
    #
    # The Analytics Data Export API returns a maximum of 10,000 entries per
    # request, no matter how many you ask for. It can also return fewer
    # entries than requested, if there aren't as many dimension segments as
    # you expect. For instance, there are fewer than 300 possible values for
    # ga:country, so when segmenting only by country, you can't get more
    # than 300 entries, even if you set max-results to a higher value.
    #
    # Args:
    #   max.results: Maximum number of entries to include in the data feed.
    #                If not specified we return the default of 1000.
    #
    # Returns:
    #   The max.results value if max.results.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(max.results.param)) {
      max.results <<- NULL
      return(invisible())
    }

    # Returns the current sort value if no parameter is used.
    if (is.na(max.results.param[1])) {
      return(max.results)
    }

    # Error handling.
    # Ensure that max.results is a numeric.
    if (!is.numeric(max.results.param)) {
      stop("max.results must be a number")
    }

    # Error handling.
    check.vector.length <- length(max.results.param)
    if (check.vector.length > 1) {
      stop("Max Results must be a single numeric value")
    }

    max.results <<- max.results.param
    return(invisible())
  }

  StartIndex <- function(start.index.param = NA) {
    # Sets the starting index from where to return results from.
    # Optional.
    # If not supplied, the starting index is 1. (Feed indexes are 1-based.
    # That is, the first entry is entry 1, not entry 0.) Use this parameter
    # as a pagination mechanism along with the max-results parameter for
    # situations when totalResults exceeds 10,000 and you want to retrieve
    # entries indexed at 10,001 and beyond.
    #
    # Args:
    #   start.index.param: The starting point of pagination for results to be
    #                      returned. If NULL is used, the start.index parameter
    #                      will be unset. If no parameter is specified, the
    #                      current start.index value is returned.
    #
    # Returns:
    #   The start.index value if start.index.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(start.index.param)) {
      start.index <<- NULL
      return(invisible())
    }

    # Returns the current sort value if no parameter is used.
    if (is.na(start.index.param[1])) {
      return(start.index)
    }

    # Error handling.
    # Ensure that start.index.param is a numeric.
    if (!is.numeric(start.index.param)) {
      stop("start.index must be a number")
    }

    # Error handling.
    check.vector.length <- length(start.index.param)
    if (check.vector.length > 1) {
      stop("Start index must be a single numeric value")
    }

    start.index <<- start.index.param
    return(invisible())
  }

  TableID <- function(table.id.param = NA) {
    # Sets the table id for a user.
    # Optional.
    # The unique table ID used to retrieve the Analytics Report data. This
    # ID is provided by the <ga:table.id> element for each entry in the
    # account feed. We run a series of checks that the form of the data is
    # being correctly entered.
    #
    # NOTE: This function does not test the table.id is valid from the account
    #       profile.
    #
    # Args:
    #   table.id.param: This value is the table ID of the profile,
    #                   e.g "ga:1234".
    #                   If NULL is used, the table.id parameter will
    #                   be unset. If no parameter is specified, the
    #                   current table.id value is returned.
    #
    # Returns:
    #   The table.id value if table.id.param is not set.

    # Un-set the parameter if the value NULL is used.
    if (is.null(table.id.param)) {
      table.id <<- NULL
      return(invisible())
    }

    # Returns the current sort value if no parameter is used.
    if (is.na(table.id.param[1])) {
      return(table.id)
    }

    # Error Handling.
    # A table.id must be character.
    if (!is.character(table.id.param)) {
      stop("A table.id  must be of the form 'ga:####'")
    }

    # Error handling.
    # Check the input is that of type vector.
    if (!is.vector(table.id.param)) {
      stop(paste("table.id must be a vector (length ", kMaxTableIds,
        ") string variable"))
    }

    if (length(table.id.param) != kMaxTableIds) {
      stop(paste("Only", kMaxTableIds, "table.id can be used at a time."))
    }

    table.id <<- table.id.param
    return(invisible())
  }

  Validate <- function() {
    # Returns whether the Query has all the required parameters set. These are
    # the start.date, end.date, metrics, and table.id parameters.
    #
    # Returns:
    #   TRUE if the query has all the required parameters. Otherwise stops the
    #   program execution.

    missing.params <- c()

    if (is.null(start.date)) {
      missing.params <- append(missing.params, "start.date")
    }
    if (is.null(end.date)) {
      missing.params <- append(missing.params, "end.date")
    }
    if (is.null(metrics)) {
      missing.params <- append(missing.params, "metrics")
    }
    if (is.null(table.id)) {
      missing.params <- append(missing.params, "table.id")
    }

    if (length(missing.params) == 0) {
      return(TRUE)
    }
    missing.string <- paste(missing.params, collapse = ", ")
    stop(paste("All GA queries must have", missing.string, "parameters.",
               sep = " "))
  }

  ToUri <- function() {
    # Returns the URI constructed from the parameter settings.
    #
    # Returns:
    #   A full URI that can be processed, by the
    #   RGoogleAnalytics$GetReportData() function.

    query <- c("start.date"  = start.date,
               "end.date"    = end.date,
               "dimensions"  = dimensions,
               "metrics"     = metrics,
               "segment"     = segment,
               "sort"        = sort,
               "filters"     = filters,
               "max.results" = max.results,
               "start.index" = start.index,
               "table.id"    = table.id)

    uri <- "https://www.google.com/analytics/feeds/data?"
    for (name in names(query)) {
      uri.name <- switch(name,
                         start.date  = "start-date",
                         end.date    = "end-date",
                         dimensions  = "dimensions",
                         metrics     = "metrics",
                         segment     = "segment",
                         sort        = "sort",
                         filters     = "filters",
                         max.results = "max-results",
                         start.index = "start-index",
                         table.id    = "ids")


      if (!is.null(uri.name)) {
        uri <- paste(uri, uri.name, "=", query[[name]], "&", sep = "")
      }
    }
    # remove the last '&' that joins the query parameters together.
    uri <- sub("&$", "", uri)
    return(uri)
  }

  ClearData <- function() {
    # A function to reset all the data values to NULL, for a new query.
    # The ClearData() function allows a user to reset the query parameters,
    # (start.date, metrics, etc) back to NULL.
    #
    # Returns:
    #  Resets all the query parameters to NULL.
    start.date  <<- NULL
    end.date    <<- NULL
    dimensions  <<- NULL
    metrics     <<- NULL
    segment     <<- NULL
    sort        <<- NULL
    filters     <<- NULL
    max.results <<- NULL
    start.index <<- NULL
    table.id    <<- NULL
    return(invisible())
  }

  Init <- function(start.date  = NULL,
                   end.date    = NULL,
                   dimensions  = NULL,
                   metrics     = NULL,
                   segment     = NULL,
                   sort        = NULL,
                   filters     = NULL,
                   max.results = NULL,
                   start.index = NULL,
                   table.id    = NULL) {
    # A function setting initial values of a GA URI query.
    #
    # Args:
    #  start.date: See QueryBuilder()
    #  end.date: See QueryBuilder()
    #  dimensions: See QueryBuilder()
    #  metrics: See QueryBuilder()
    #  segment: See QueryBuilder()
    #  sort: See QueryBuilder()
    #  filters: See QueryBuilder()
    #  max.results: See QueryBuilder()
    #  start.index: See QueryBuilder()
    #  table.id: See QueryBuilder()
    #
    # Returns:
    #   Sets the initial query parameters.

    StartDate(start.date)
    EndDate(end.date)
    Dimensions(dimensions)
    Metrics(metrics)
    Segment(segment)
    Sort(sort)
    Filters(filters)
    MaxResults(max.results)
    StartIndex(start.index)
    TableID(table.id)
    return(invisible())
  }

  return(list("start.date"  = StartDate,
              "end.date"    = EndDate,
              "dimensions"  = Dimensions,
              "metrics"     = Metrics,
              "segment"     = Segment,
              "sort"        = Sort,
              "filters"     = Filters,
              "max.results" = MaxResults,
              "start.index" = StartIndex,
              "table.id"    = TableID,
              "to.uri"      = ToUri,
              "clear.data"  = ClearData,
              "validate"    = Validate,
              "Init"        = Init))
}