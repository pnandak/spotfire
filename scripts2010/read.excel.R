# read.excel() reads a data.frame from an Excel worksheet.
# Howard Seltman 4/21/2009

#######################################################################
# Get a data.frame from a sheet in an Excel workbook.
# Based on code from Gabor Grothendiek at:
# http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg67087.html.
# Improvements: allow sheet name or number, handle errors better.
# The file is read from the current directory unless "dir" is non-null.
#
# "file" is the workbook name, and "sheet" is a sheet name or number.
# To read a matrix with row names and column names, typically the 
# option "row.names=1" (which is sent by read.excel() to read.table())
# is used.
# This function returns a data.frame, which can be coerced to a
# matrix if needed.
#
# Requires package gdata to do the actual worksheet reading
# using read.xls().
# Requires package rcom (load once with RExcelInstaller at
# http://cran.r-project.org/web/packages/RExcelInstaller/index.html)
# to query the sheet names in the workbook.  
# Note: If you don't want to load rcom, read.xls() can be used
# directly with only minimal loss of functionality.
# The read.xls() function does requires Perl 
# (http://www.perl.com/download.csp).
#
read.excel <- function(file, sheet, dir=NULL, ...) {
  if (!is.character(file) || length(file)!=1)
    stop("file must be a single character string")
  if (length(sheet)!=1 || (!is.character(sheet) && !is.numeric(sheet)))
    stop("sheet must be a number or single character string")
  if (!is.null(dir) && (!is.character(dir) || length(dir)!=1))
    stop("dir must be null or a single character string")

  if (is.null(dir)) {
    fname = paste(getwd(),"\\",file,sep="")
  } else {
    if (!substring(dir,nchar(dir)) %in% c("\\","/"))
      dir = paste(dir, "\\", sep="")
    fname = paste(dir,file,sep="")
  }

  if (is.numeric(sheet)) {
    read.xls(fname, sheet, ...)
  } else {
    require(rcom)
    require(gdata)
    oxl <- comCreateObject('Excel.Application')
    on.exit(comInvoke(oxl, "Quit"))
    comSetProperty(oxl, "Visible", TRUE)  # this line optional
    owb <- comGetProperty(oxl, "Workbooks")
    ob <- comInvoke(owb, "Open", fname)
    if (is.null(ob)) stop("Can't open ", fname)
    osheets <- comGetProperty(ob, "Worksheets")
    n <- comGetProperty(osheets, "Count")
    ithSheetName <- function(i)
      comGetProperty(comGetProperty(osheets, "Item", i), "Name")
    sheetNames <- sapply(1:n, ithSheetName)
    m <- match(sheet, sheetNames)
    if (is.na(m)) stop(sheet, " is not in ", file)
    read.xls(fname, m, ...)
  }
}

# Example: 
# tmp = read.excel("ConfusionMatrix.xls", "InitialEnglish", row.names=1)
