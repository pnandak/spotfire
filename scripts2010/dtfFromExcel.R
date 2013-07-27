##############$#######################################
# A functions for reading data from MS-Excel into R. #
######################################################

# An alternate function, getExcel(), is at
#   http://www.stat.cmu.edu/~hseltman/files/getExcel.R.
#   In comparison to getExcel(), dtfFromExcel() uses
#   the Component Object Model data access method which
#   requires the R package, rcom.  This package does not
#   have a MacOS binary and has some complexities with
#   install/uninstall involving the Windows registry.
#   Use of rcom with Excel only works if Excel is loaded
#   on your computer.
# Compared to getExcel(), dtfFromExcel() is more
#   flexible because it allows skipping of lines at
#   the top of a worksheet or named range.  It also
#   allows reading a range specified by cell
#   coordinates.  Sheet names can be specified by name
#   or number, while getExcel() requires a name.  A
#   limitation of dtfFromExcel() is that it can only
#   read a named range if you know what spreadsheet it
#   is on.


##################### dtfFromExcel() #################################
# Location: http://www.stat.cmu.edu/~hseltman/files/dtfFromExcel.R
# Function to read Excel data into R using package rcom.
# This function silently starts your copy of Excel
#   on your Windows machine opens an existing workbood
#   and flexibly retrieves data from it.
# To load package rcom, goto:
#   http://cran.r-project.org/web/packages/rcom/index.html
#   or, possibly use:
#   http://cran.r-project.org/web/packages/RExcelInstaller/index.html
#   which loads some other things you might not want.
#
# This function takes the name of an Excel workbook, "wb", which
#   is assumed to be in the current directory or in "dir".
# Within that workbook, one worksheet, "sheet", is specified
#   by sheet number or name.
# Within that sheet, "range" or the "range"/"range2" pair
#   specify which data to read.  The entire sheet is read if
#   "range" is NULL.  Alternatively, "range" can be either a
#   named range (from Formula/DefineName in Excel), or it can
#   be the the upper left coordinate of a range (e.g. "B2")
#   if the lower right coordinate is specified in "range2".
# The first "skip" rows of the range or entire sheet are ignored.
# If "hasHeaderRow" is TRUE, the first row in the range is used
#   to name the columns of the data.frame.  If "hasHeaderCol"
#   is TRUE, the first column of the range is used to name the
#   rows of the data.frame.
# If any rownames are duplicates, all non-missing rownames are
#   suffixed with "r#.", where "#" is the row number.
# If hasHeaderRow=TRUE and/or hasHeaderCol=TRUE, then columns
#   and/or rows with missing names are dropped if "dropCols"
#   and/or "dropRow" is TRUE, otherwise they are named r.# or
#   c.#.  If not using header row/column, "dropRows"/"dropCols"
#   causes rows/columns that are all NA to be dropped.
# If nameSpaceReplace is of type character and of length 1,
#   row and column names with spaces have them replaced by
#   the character string (possibly "").  A value of NULL leaves
#   the spaces in the names.
#
# Returns a data.frame with additional attributes for the
#   expanded "filename" and the "range".
# (If the data are ill-formed the resulting data columns will
# be mostly factors, and there may be lots of NAs.)
#
# (Note using on.exit() doesn't seem to clear Excel from the
#   Windows task manager.  Manually calling gc() after the
#   function completes or just quitting R does remove it.)
# 
dtfFromExcel <- function(wb, sheet=1, range=NULL, range2=NULL,
                        hasHeaderRow=TRUE, hasHeaderCol=FALSE,
                        dropRows=FALSE, dropCols=FALSE,
                        skip=0, nameSpaceReplace=".",
                        dir=NULL) {
  if (suppressWarnings(require(rcom))==FALSE)
    stop("The R package rcom is required.")
  if (!is.character(wb) || length(wb)!=1) stop("Bad wb")
  if (length(sheet)!=1 ||
      class(sheet)%in%c("character","numeric","integer")==FALSE)
    stop("Bad sheet")
  if (is.null(range2)) {
    if (!is.null(range) && (length(range)!=1 || class(range)!="character"))
      stop("Bad range")
  } else {
    if (is.null(range) || length(range)!=1 || class(range)!="character" ||
        length(range2)!=1 || class(range2)!="character")
      stop("Bad range/range2 combination")
  }
  if (length(hasHeaderRow)!=1 || !is.logical(hasHeaderRow))
    stop("Bad hasHeaderRow")
  if (length(hasHeaderCol)!=1 || !is.logical(hasHeaderCol))
    stop("Bad hasHeaderCol")
  if (length(dropCols)!=1 || !is.logical(dropCols))
    stop("Bad dropCols")
  if (length(dropRows)!=1 || !is.logical(dropRows))
    stop("Bad dropRows")
  if (length(skip)!=1 ||
      class(skip)%in%c("numeric","integer")==FALSE ||
      skip<0)
    stop("Bad skip")
  if (!is.null(nameSpaceReplace) &&
      (length(nameSpaceReplace)!=1 || class(nameSpaceReplace)!="character"))
    stop("Bad nameSpaceReplace")
  if (!is.null(dir) && (length(dir)!=1 || class(dir)!="character"))
    stop("Bad dir")

  ## prefix "wb" with directory name
  if (is.null(dir)) dir=getwd()
  if (substring(dir, nchar(dir)) %in% c("\\","/") == FALSE)
    dir <- paste(dir, "\\", sep="")
  wb <- paste(dir, wb, sep="")

  ## Get existing or new handle for Excel ##
  hExcel <- comGetObject('Excel.Application')
  if (is.null(hExcel)) {
    hExcel <- comCreateObject('Excel.Application')
    if (is.null(hExcel))
      stop("Cannot start Excel.")
  }
  on.exit(comInvoke(hExcel, "Quit"))

  ## Get a handle to the worksheet ##
  hBooks <- comGetProperty(hExcel, "Workbooks")
  if (is.null(hBooks))
    stop("Cannot get workbooks property")
  hBook <- comInvoke(hBooks, "Open", wb)
  if (is.null(hBook))
    stop("Cannot open ", wb)
  hSheet <- comGetProperty(hBook,"Worksheets",sheet)
  if (is.null(hSheet))
    stop("Cannot open worksheet ", sheet)

  ## Get data from the whole worksheet or a named range. ##
  # The resulting object, "tmp", is of class "rcomdata"
  # which is a column-major-order list of all elements,
  # and it has a dim() attribute describing the original
  # range dimensions.
  if (is.null(range)) {
    rangeName <- "Entire sheet"
    tmp <- comGetProperty(comGetProperty(hSheet,"UsedRange"),"Value")
  } else {
    if (is.null(range2)) {
      rangeName <- paste("range", range)
      hRange <- comGetProperty(hSheet, "Range", range)
    } else {
      rangeName <- paste("range ", range, ":", range2, sep="")
      hRange <- comGetProperty(hSheet, "Range", range, range2)
    }
    if (is.null(hRange))
      stop("Cannot read ", rangeName)
    tmp <- comGetProperty(hRange, "Value")
  }
  if (is.null(tmp))
    stop("Failed while attempting to read ", rangeName)

  ## Special case for a single cell
  if (class(tmp)!="rcomdata") {
    tmp <- list(tmp)
    class(tmp) <- "rcomdata"
    dim(tmp) <- c(1,1)
  }
  
  ## Drop complete rows at the top of the range or sheet ##
  if (skip>0) {
    NR <- nrow(tmp)
    if (NR<=skip) return(NULL)
    NC <- ncol(tmp)
    tmp[row(matrix(0,NR,NC)) %in% 1:skip] <- NULL
    dim(tmp) <- c(NR-skip, NC)
    rm(NR, NC)
  }
  
  ## Get row and column headers ##
  NRtmp <- nrow(tmp) # number of rows in raw data ("tmp")
  NCtmp <- ncol(tmp) # number of columns in raw data ("tmp")
  if (hasHeaderRow) {
    if (NRtmp==1) return(NULL)
    colNames <- as.character(unlist(tmp[seq(start=1, by=NRtmp, len=NCtmp)]))
  } else {
    colNames <- paste("c.", seq(ifelse(hasHeaderCol,0,1), len=NCtmp), sep="")
  }
  if (hasHeaderCol) colNames <- colNames[-1]
  if (!hasHeaderRow && !is.null(nameSpaceReplace))
    colNames <- gsub(" ",nameSpaceReplace,colNames)
  isBadCol <- is.na(colNames) | colNames==""
  if (any(isBadCol)) colNames[isBadCol] <- paste("c.", which(isBadCol), sep="")
  if (hasHeaderCol) {
    if (NCtmp==1) return(NULL)
    NC <- NCtmp - 1   # number of columns in the final data.frame
    rowNames <- as.character(unlist(tmp[1:NRtmp]))
  } else {
    NC <- NCtmp
    rowNames <- paste("r.", seq(ifelse(hasHeaderRow,0,1), len=NRtmp), sep="")
  }
  if (hasHeaderRow) rowNames <- rowNames[-1]
  if (!hasHeaderCol && !is.null(nameSpaceReplace))
    rowNames <- gsub(" ",nameSpaceReplace,rowNames)
  isBadRow <- is.na(rowNames) | rowNames==""
  if (any(isBadRow)) rowNames[isBadRow] <- paste("r.",
                                             which(isBadRow) - as.numeric(hasHeaderRow),
                                             sep="")

  ## Construct data.frame() expression so as to allow the original column ##
  ## data type (if all identical) to persist in the data.frame.           ##
  # (Fake column names needed here in case true column names have embeded spaces.)
  descr <- paste("c.", 1:NC, "=unlist(tmp[",
                 seq(from=ifelse(hasHeaderRow,2,1)+ifelse(hasHeaderCol,NRtmp,0),
                     by=NRtmp, len=NC), ":",
                 seq(from=ifelse(hasHeaderCol,2*NRtmp,NRtmp),
                     by=nrow(tmp), len=NC), "])",
                 sep="", collapse=", ")
  tmp=unclass(tmp)
  tmp[sapply(tmp,is.null)]=NA
  mydata <- try(eval(parse(text=paste("data.frame(",descr,")",collapse=""))))
  if ("try-error" %in% class(mydata)) stop("Cannot coerce data to data.frame")
  names(mydata) <- colNames  # (may have spaces if nameSpaceReplace is NULL)
  if (any(duplicated(rowNames)))
    rowNames[!isBadRow] = paste(rowNames[!isBadRow], "r.",
                              which(!isBadRow) - as.numeric(hasHeaderRow),
                              sep="")
  rownames(mydata) <- rowNames

  ## Drop bad rows/cols if desired, and return the data.frame.
  if (!hasHeaderRow && dropCols)
    isBadCol = sapply(mydata, function(x) all(is.na(x)))
  mydata <- mydata[, !(isBadCol&dropCols), drop=FALSE]
  if (!hasHeaderCol && dropRows)
    isBadRow = apply(mydata, 1, function(x) all(is.na(unlist(x))))
  mydata <- mydata[!(isBadRow&dropRows), , drop=FALSE]
  attr(mydata, "filename") = wb
  attr(mydata, "range") = paste(sheet,
                                ifelse(is.null(range), "",
                                       paste(" / ",range)),
                                ifelse(is.null(range2), "",
                                       paste(" : ",range2)),
                                sep="")
  invisible(mydata)
}

# Examples using http://www.stat.cmu.edu/~hseltman/files/test.xls
if (exists("flyingPigs")) {
  jnk = dtfFromExcel("test.xls", sheet="Sheet1", range="mydata",
                     hasHeaderRow=T, hasHeaderCol=T,
                     dir="http://www.stat.cmu.edu/~hseltman/files/")
  print(jnk)
  print(attributes(jnk))
  jnk = dtfFromExcel("test.xls", sheet="Sheet2", skip=2,
                     hasHeaderRow=T, hasHeaderCol=T,
                     dropRows=T, dropCols=T,
                     dir="http://www.stat.cmu.edu/~hseltman/files/")
  print(jnk)
  print(attributes(jnk)$range)
}  
