# This function uses R to get all or several datasets in some particular
# "foreign" data format (e.g., SPSS, SAS, etc.), and then writes them
# back out as text files (or returns a data.frame).

# The default action is to read all SPSS files in the working directory and
# write comma-separated-values files with column names and a ".dat" suffix.

# Note that read.xport (for SAS export libraries) works a little differently.
#   Each file may contain several datasets, so several output files will be
#   written when "suffix" is defined (see below).  If "suffix" is NULL, a
#   list of data.frames is returned.

# Input parameters:
# "filetype" defaults to "spss".  Change to another listed type for other input data types.
# "pattern" defaults to all files of the appropriate extension (matching the "filetype").
#    Change this to limit the files to be converted (see the help for list.files).
# Change "ignore.case" to TRUE if some extensions are capitalized on a Windows system.
# "suffix" defaults to a "dat" output suffix.  If suffix is NULL and only one file
#   matches the pattern, a data.frame is returned and no file is written.
# Change write.table() parameters (sep, row.names, col.names, na, and/or quote) if needed.
# Change longName to TRUE to get longer forms of variables names (works, at least, in 
#   SPSS, where labels instead of names become the variable names.  For this option,
#   "spaceTo" is the character that replaces spaces in long variable names.
# Change "silent" to true to suppress printed output.
# Change "dropFinalNA" to FALSE to keep final lines of data that are all NA.
# See http://cran.r-project.org/web/packages/foreign/foreign.pdf for optional parameters
#   for each data type.

# This function returns NULL or the last read file as a data.frame (if "suffix" is NULL).

convertData = function(filetype=c("spss","arff","dbf","stata","minitab",
                                  "octave","systat","SASexport"),
                       pattern=NULL, ignore.case=FALSE,
                       suffix="dat", dropFinalNAs=TRUE,
                       longNames=FALSE, spaceTo=".",
                       sep=", ", row.names=FALSE, col.names=TRUE, 
                       na="NA", quote=FALSE, silent=FALSE, ...) {
  filetype = match.arg(filetype)
  if (!is.null(pattern) && (!is.character(pattern) || length(pattern)!=1)) stop("Bad pattern")
  if (!is.null(suffix)) {
    if (!is.character(suffix) || length(suffix)!=1) stop("Bad suffix")
    if (nchar(suffix)>1 && substring(suffix,1,1)==".") suffix = substring(suffix,2)
  }
  if (!is.logical(longNames) || length(longNames)!=1) stop("Bad longNames")
  if (!is.character(spaceTo) || length(spaceTo)!=1) stop("Bad spaceTo")
  if (!is.character(na) || length(na)!=1) stop("Bad na")
  if (!is.character(sep) || length(sep)!=1) stop("Bad sep")
  if (!is.logical(row.names) || length(row.names)!=1) stop("Bad row.names")
  if (!is.logical(col.names) || length(col.names)!=1) stop("Bad col.names")
  if (!is.logical(quote) || length(quote)!=1) stop("Bad quote")
  if (!is.logical(dropFinalNAs) || length(dropFinalNAs)!=1) stop("Bad dropFinalNAs")

  # Get required package for SPSS file reading
  if (require(foreign)==FALSE) stop("Use Packages/InstallPackages to get 'foreign'.")

  # Function to replace the extension on a filename, e.g. a.b.sav to a.b.dat.
  replaceExtension = function(input, newExt) {
    period = rev(which(unlist(strsplit(input,""))=="."))[1]
    if (!is.na(period)) input=substring(input,1,period-1)
    return(paste(input,".",newExt,sep=""))
  }

  # Function to drop annoying terminal data lines which are all NA.
  if (dropFinalNAs) {
    dropNAs = function(dat) {
      if (!is.data.frame(dat)) dat=data.frame(dat)
      allna = apply(dat, 1, function(x) all(is.na(unlist(x))))
      if (all(allna)) {
	  warning("No non-missing data.")
        return(NULL)
      }
	len = length(allna)
      lastKeep = len + 1 - min(which(rev(allna)==FALSE))[1]
      if (lastKeep<len) dat=dat[1:lastKeep,]
      return(dat)
    }
  }

  # Adjust for chosen file type
  filetypes = eval(formals()$filetype)
  patterns = c(".sav$", ".arff$", ".dbf$", ".dta$", ".mtp$",
               ".txt$", ".syd$", ".xpt$")
  functions = c(read.spss, read.arff, read.dbf, read.dta, read.mtp, 
                read.octave, read.systat, read.xport)
  ftIndex = match(filetype,filetypes)
  if (is.null(pattern)) pattern =  patterns[ftIndex]
  fun = functions[[ftIndex]]

  # Prepare filenames
  if (!silent) cat("Looking for ", pattern, " files in ", getwd(), ".\n", sep="")
  files = list.files(pattern=pattern, ignore.case=ignore.case)
  if (length(files)==0) stop("No ", pattern, " files were found.")

  # Read and write files
  for (file in files) {
    data = try(fun(file, ...), silent=TRUE)
    if ("try-error" %in% class(data)) {  # Error in reading file
	if (!silent) cat("For file", file, ":", as.character(data))
	data = NULL
    } else {  # Successful file read
      if (filetype!="SASexport") {
        if (longNames) {
          if ("variable.labels"%in%names(attributes(data))) {
            longLab = gsub(" ", spaceTo, attributes(data)$variable.labels, fixed=TRUE)
          } else {
            longNames=FALSE
          }
        }
        data = data.frame(data)
        if (longNames) names(data) = longLab
      }
      if (dropFinalNAs) {
	  if (filetype=="SASexport") {
          for (i in 1:length(data)) data[[i]]=dropNAs(data[[i]])
        } else {
          data=dropNAs(data)
        }
	}
	if (is.null(suffix)) {  # output returned directly
	  if (!silent) {
          if (filetype=="SASexport") {
            cat(file, "contains data.frames:", paste(names(data), sep=", "), ".\n", sep="")
          } else {
            cat(file, " has been converted to a data.frame.\n", sep="")
          }
        }
      } else {  # output to a file
        if (filetype!="SASexport") {
          outname = replaceExtension(file, suffix)
          write.table(data, file=outname, row.names=row.names,
                      col.names=col.names, sep=sep, na=na, quote=quote)
          if (!silent) cat("Created ", outname, " from ", file, ".\n", sep="")
        } else {
          tmpnames = names(data)
          for (index in 1:length(data)) {
            outname = paste(tmpnames[index], ".", suffix, sep="")
            write.table(data[[index]], file=outname, row.names=row.names,
                        col.names=col.names, sep=sep, na=na, quote=quote)
            if (!silent) cat("Created ", outname, " from ", file, ".\n", sep="")
          }
        }
        data = NULL
      }
    }
  } # end for each file
  invisible(data)
}

# Testing the function:
if (exists("testingConvertData")) {
  convertData("arff")
  convertData("arff", suffix=".txt")
  convertData()
  cat("Abrasion.sav names are:",names(convertData("spss", "Abrasion.sav", silent=TRUE, suffix=NULL)),"\n")
  cat("Abrasion2.dbf:\n")
  print(convertData("dbf", pattern="Abrasion2.dbf", suffix=NULL, silent=TRUE))
  cat("Dim of bc.dta is", dim(convertData("stata", "bc.dta", suffix=NULL)), "\n")
  convertData("minitab") # ?? working ??
  convertData("systat", ignore.case=TRUE)
  convertData("SASexport", quote=TRUE)
  convertData("spss", "distance.sav", longNames=T)
}

