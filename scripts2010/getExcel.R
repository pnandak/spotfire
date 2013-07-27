######################################################
# A functions for reading data from MS-Excel into R. #
######################################################

# An alternative is dtfFromExcel() in
#   http://www.stat.cmu.edu/~hseltman/files/dtfFromExcel.R.
# This version uses "Open Data Base Connectivity" which
#   involves a simple package, RODBC.  In includes
#   an easy to use function to query Excel workbooks for
#   sheet names and range names.
# In comparison to dtfFromExcel(), getExcel() does not require
#   knowing which sheet contains a particular named range.
#   It does not require Excel to be present on your computer.
#   It does not have the capacity to skip lines at the top
#   of a sheet or get data based on just cell coordinates.

###################### getExcel() ################################
# Location: http://www.stat.cmu.edu/~hseltman/files/getExcel.R
# This is an easy-to-use function to get data from MS-Excel
#   into an R data.frame.
# The R package "RODBC" is required.
#
# "xls.file" is the name of a file (absolute or relative to the
#   current dirctory, or for MS-Windows, NULL for interactive
#   file selection).
# "range" is a workseet name or a named range.
# "dropRows" and/or "dropCols" can be turned on to drop rows
#   and/or columns that are all NA.
# The underlying functions assume that the data have a header
#   row and no header column (rownames).  Changing the default
#   values of "hasHeaderRow" and/or "hasHeaderColumn" attempts
#   to get around this when the data are formatted
#   otherwise.
#
# Returns a data.frame with additional attributes for the
#   expanded "filename" and the "range".
#
getExcel <- function(xls.file, range="Sheet1",
                     dropRows=FALSE, dropCols=FALSE,
                     hasHeaderRow=TRUE, hasHeaderCol=FALSE) {
  if (suppressWarnings(require(RODBC))==FALSE)
    stop("The R package RODBC is required.")

  if (missing(xls.file) && .Platform$OS.type!="windows")
    stop("xls.file can only be missing in MS-Windows")
  if (!is.character(range) || length(range)!=1)
    stop("Bad range")
  if (!is.logical(dropRows) || length(dropRows)!=1)
    stop("Bad dropRows")
  if (!is.logical(dropCols) || length(dropCols)!=1)
    stop("Bad dropCols")
  if (!is.logical(hasHeaderRow) || length(hasHeaderRow)!=1)
    stop("Bad hasHeaderRow")
  if (!is.logical(hasHeaderCol) || length(hasHeaderCol)!=1)
    stop("Bad hasHeaderCol")
  
  # Open Excel file and save filename
  ch <- odbcConnectExcel2007(xls.file)
  if (ch==-1) stop("Failed to connect")
  on.exit(odbcClose(ch))
  filename <- attributes(ch)$connection.string
  chr <- as.numeric(regexpr(";",filename))
  if (chr!=-1) filename <- substring(filename, 5, chr-1)

  mydata <- sqlFetch(ch, range)
  if (!is.data.frame(mydata)) stop("Error on fetch of ", range, ": ", mydata)

  # Convert first column of data to row names,  if requested
  if (hasHeaderCol) {
    rowNames <- as.character(mydata[,1])
    rowNames[is.na(rowNames)] <- ""
    bad <- rowNames=="" | duplicated(rowNames)
    if (any(bad)) rowNames[bad] <- paste(rowNames[bad], "r.", which(bad), sep="")
    rownames(mydata) <- rowNames
    mydata <- mydata[,-1]
  }

  # Drop all NA columns/rows, if requested
  if (dropCols) {
    good <- sapply(mydata, function(x) any(!is.na(x)))
    if (any(!good)) mydata <- mydata[,good]
  }
  if (dropRows) {
    good <- apply(mydata, 1, function(x) any(!is.na(unlist(x))))
    if (any(!good)) mydata <- mydata[good,]
  }

  # Save source info in data.frame attributes
  attr(mydata, "filename") <- filename
  attr(mydata, "range") <- range
  
  # Convert column names to first row of data, if requested and possible
  if (!hasHeaderRow) {
    row1 <- names(mydata)
    c.names <- paste("c.", seq(along=row1), sep="")
    # Can't rbind data.frames with factors with different levels, so
    # temporarily convert factors to strings.
    factors <- sapply(mydata,is.factor)
    if (any(factors)) {
      for (i in which(factors))
        mydata[,i] <- levels(mydata[,i])[mydata[,i]]
    }
    expr <- paste(paste(c.names, "=",
                        "as.", sapply(mydata,class),
                        "(row1[", seq(along=row1), "])",
                        sep=""),
                  collapse=", ")
    expr <- paste("data.frame(", expr, ")")
    oldWarn <- as.numeric(options("warn"))
    options(warn=2)
    tmp = try(eval(parse(text=expr)), silent=TRUE)
    options(warn=oldWarn)
    if ("try-error" %in% class(tmp)) {
      warning("Can't honor hasHeaderRow=FALSE")
    } else {
      colnames(mydata) <- c.names
      mydata <- rbind(tmp, mydata)
    }
    for (i in which(factors)) mydata[,i] <- factor(mydata[,i])
  }
  return(mydata)
}

# Examples using http://www.stat.cmu.edu/~hseltman/files/test.xls
# I couldn't figure out how to connect directly, so with this
# code, you need to download test.xls to your working directory.
if (exists("flyingPigs")) {
  jnk = getExcel("test.xls", range="mydata")  # note: sheet name not used/needed
  print(jnk)
  print(attributes(jnk))
  # See above for improved flexibility with dtfFromExcel()
  jnk = getExcel("test.xls", range="Sheet2",
                 hasHeaderCol=T,
                 dropRows=T, dropCols=T)
  print(jnk)
  print(attributes(jnk)$range)
}  
  

#################### getExcelStucture() ######################
# Helper function to get sheet and range names
#
# Requires package RODBC.
# "xls.file" is the name of a file (absolute or relative to the
#   current dirctory, or  for MS-Windows, NULL for interactive
#   file selection).
#
# Returns a dataframe of all sheets and named ranges.
# Columns are "name" and "isSheet" (logical).
#
getExcelStructure <- function(xls.file) {
  if (suppressWarnings(require(RODBC))==FALSE)
    stop("The R package RODBC is required.")

  if (missing(xls.file) && .Platform$OS.type!="windows")
    stop("xls.file can only be missing in MS-Windows")
  
  # Open Excel file
  ch <- odbcConnectExcel2007(xls.file)
  if (ch==-1) stop("Failed to connect")
  on.exit(odbcClose(ch))

  # Get sheet and range names
  tmp <- sqlTables(ch)$TABLE_NAME
  len <- nchar(tmp)
  last <- substring(tmp,len)
  isSheet <- last=="$"
  dtf <- data.frame(name=substring(tmp,1,len-isSheet),
                    isSheet=isSheet)
  return(dtf)
}

# Example using http://www.stat.cmu.edu/~hseltman/files/test.xls
if (exists("flyingPigs")) {
  print(getExcelStructure("test.xls"))
}
  
