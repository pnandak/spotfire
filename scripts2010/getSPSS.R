# Get data from SPSS and write back out as text files.

# The default action is to read all SPSS files in the working directory and
# write comma-separated-values files with column names and a ".dat" suffix.

# (Note: Test data came from http://www.stat.lsa.umich.edu/~bkg/stat350/spssdata/index.html)

# Input parameters:
# Change "pattern" (according to the help for list.files) to limit the files converted.
# Change "suffix" for a different output suffix.
# Change write.table() parameters (sep, row.names, col.names, na, quote) if needed.
# See help for read.spss for optional parameters.

# This function returns NULL.

getSPSS = function(pattern=".sav$", suffix="dat", 
                   sep=", ", row.names=FALSE, col.names=TRUE, 
                   na="NA", quote=FALSE, ...) {
  if (!is.character(pattern) && length(pattern)==1) stop("Bad pattern")
  if (!is.character(suffix) && length(suffix)==1) stop("Bad suffix")
  if (!is.character(na) && length(na)==1) stop("Bad na")
  if (!is.character(sep) && length(sep)==1) stop("Bad sep")
  if (!is.logical(row.names) && length(row.names)==1) stop("Bad row.names")
  if (!is.logical(col.names) && length(col.names)==1) stop("Bad col.names")
  if (!is.logical(quote) && length(quote)==1) stop("Bad quote")

  # Get required package for SPSS file reading
  if (require(foreign)==FALSE) stop("Use Packages/InstallPackages to get 'foreign'.")

  # Prepare filenames
  cat("Looking for files in ", getwd(), ".\n", sep="")
  files = list.files(pattern=pattern)
  if (length(files)==0) stop("No ", pattern, " files were found.")

  # Read and write files
  for (file in files) {
    input = try(read.spss(file, ...))
    if ("try-error" %in% class(input) == FALSE) {
      outname = paste(substring(file, 1, nchar(file)-4),
                      ".", suffix, sep="")
      write.table(input, file=outname, row.names=row.names,
                  col.names=col.names, sep=sep, na=na, quote=quote)
      cat("Created ", outname, ".\n", sep="")
    }
  } # end for each file
  invisible(NULL)
}

# Run the function:
getSPSS()

