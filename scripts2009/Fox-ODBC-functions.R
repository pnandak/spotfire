# These functions demonstrate how to create and use pseudo-dataframes that 
# actually reside in a DBMS

# last modified 3 Feb 05 by J. Fox

require(RODBC)

odbcDataset <- function(channel, table, rownames){
    # create a pseudo-dataframe (odbcDataset object)
    # arguments:
    #   channel: object returned by odbcConnect()
    #   table: name of database table
    #   rownames: TRUE if the first character variable in the data base contains row names;
    #             FALSE if there is no row names variable;
    #             if not specified, the function will look for a suitable variable in the
    #             data base
    if (length(channel) != 1 || !is.integer(channel) || channel < 0) stop("channel must be a non-negative integer")
    if (length(table) != 1 || !is.character(table)) stop("table must be a data-base table name")
    RODBC:::odbcTableExists(channel, table)
    col.info <- sqlColumns(channel, table)
    if (!is.data.frame(col.info))
        stop(paste("database connected on channel", channel, "not supported\ndatabase info:", odbcGetInfo(channel)))
    col.sel <- which(toupper(names(sqlColumns(channel, table))) == "COLUMN_NAME")
    if (1 != length(col.sel)) 
        stop(paste("database connected on channel", channel, "not supported\ndatabase info:", odbcGetInfo(channel)))
    if (missing(rownames)){
        type.sel <- which(toupper(names(sqlColumns(channel, table))) == "TYPE_NAME")
        if (1 != length(type.sel)) 
            stop(paste("database connected on channel", channel, "not supported\ndatabase info:", odbcGetInfo(channel)))
        rownames <- 1 == length(grep("char", col.info[1, type.sel]))
        }
    result <- list(channel=channel, table=table, rownames=rownames, column.selector=col.sel)
    class(result) <- c("odbcDataset", "data.frame")
    result
    }

print.odbcDataset <- function(x, ..., verbose=FALSE){
    # print method for odbcDataset objects
    x <- unclass(x)  # necessary because of $.odbcDataset
    if (verbose) {
        x <- sqlFetch(x$channel, x$table, rownames=x$rownames)
        NextMethod("print")
        }
    else {
        cat(  "channel:           ", x$channel)
        cat("\ntable:             ", x$table)
        cat("\nrow names:         ", x$rownames)
        class(x) <- c("odbcDataset", "data.frame")
        dim <- dim(x)
        cat("\nnumber of rows:    ", dim[1])
        cat("\nnumber of columns: ", dim[2], "\n")
        invisible(x)
        }
    }

summary.odbcDataset <- function(object, ...){
    # summary method for odbcDataset objects
    object <- unclass(object)
    object <- sqlFetch(object[["channel"]], object[["table"]], rownames=object[["rownames"]])
    NextMethod("summary")
    }

sqlFetch <- function (channel, sqtable, ..., colnames = FALSE, rownames = FALSE) 
{ # slightly modified from the RODBC package
    if (channel < 0) 
        stop("invalid channel")
    if (missing(sqtable)) 
        stop("Missing parameter")
    tablename <- as.character(substitute(sqtable))                 # modified
    tablename <- if (exists(tablename) && is.character(sqtable))   # lines
        sqtable else tablename                                     #
    dbname <- RODBC:::odbcTableExists(channel, tablename)
    ans <- sqlQuery(channel, paste("SELECT * FROM", dbname), 
        ...)
    if (is.logical(colnames) && colnames) {
        colnames(ans) <- as.character(as.matrix(ans[1, ]))
        ans <- ans[-1, ]
    }
    if (is.logical(rownames) && rownames) {
        rownames(ans) <- as.character(as.matrix(ans[, 1]))
        ans <- ans[, -1]
    }
    ans
}


"[.odbcDataset" <- function(x, i, j, ...){
    # indexing method for odbcDataset objects: supports SQL fetches
    #   i: row 'subscript' is a quoted SQL statement
    #   j: column subscript, may be column numbers, negative numbers, or names
    same.sign <- function(x) {
        any(x > 0) == all(x >= 0)
        }
    x <- unclass(x) 
    names <- sqlColumns(x$channel, x$table)[[x$column.selector]]
    selection <- if (missing(j)) "*" 
                    else if (is.numeric(j)) {
                        j <- j[j != 0]
                        if (length(j) == 0) return(NULL)
                        if (!same.sign(j)) stop("cannot mix positive and negative subscripts")
                        if (j[1] > 0) names[j + x$rownames]
                            else names[-1 * c(rep(1, x$rownames), (abs(j) + x$rownames))]
                        }                        
                        else j
    selection <- if (selection == "*" || !x$rownames) selection
                    else c(names[1], selection)
    if (selection != "*" && (any(is.na(selection)) || any(!is.element(selection, names)))) stop("bad column index")
    result <- if (missing(i)) sqlQuery(x$channel, paste("select", paste(selection, collapse=","), "from", x$table))
                else {
                    if (!is.character(i) && length(i) != 1) stop("row 'subscript' must be an SQL row selector")
                    sqlQuery(x$channel, paste("select", paste(selection, collapse=","), "from", x$table, "where", i))
                    }
    if (x$rownames) {
        rownames(result) <- as.character(result[,1])
        result <- result[, -1]
        }
    if (length(dim(result)) == 2 && dim(result)[2] == 1) result[,,drop=TRUE] else result
    }
    
"$.odbcDataset" <- function(x, i){
    # indexing method for odbcDataset objects
    x <- unclass(x)
    sqlQuery(x$channel, paste("select", i, "from", x$table))[[1]]
    }

"[[.odbcDataset" <- function(x, i){
    # indexing method for odbcDataset objects
    x <- unclass(x)
    if (is.numeric(i)) i <- sqlColumns(x$channel, x$table)[[x$column.selector]][i + x$rownames] 
    sqlQuery(x$channel, paste("select", i, "from", x$table))[[1]]
    }

as.data.frame.odbcDataset <- function(x, rownames, optional){
    # returns odbcDataset object as a dataframe
    x <- unclass(x)
    sqlFetch(x$channel, x$table, rownames=x$rownames)
    }

as.matrix.odbcDataset <- function(x) as.matrix(as.data.frame(x))
    # returns odbcDataset object as a matrix

as.list.odbcDataset <- function(x, ...) as.list(as.data.frame(x))
    # returns odbcDataset object as a matrix

row.names.odbcDataset <- function(x) {
    # returns row names of odbcDataset object
    x <- unclass(x)
    if (x$rownames) {
        name <- sqlColumns(x$channel, x$table)[[x$column.selector]][1] 
        as.character(sqlQuery(x$channel, paste("select", name, "from", x$table))[[1]])
        }
    else NULL
    }
    
names.odbcDataset <- function(x){
    # returns names of odbcDataset object
    x <- unclass(x)
    names <- sqlColumns(x$channel, x$table)[[x$column.selector]]
    if (x$rownames) names[-1] else names
    }
    
dimnames.odbcDataset <- function(x) list(row.names(x), names(x))
    # returns dimension names of odbcDataset object

dim.odbcDataset <- function (x) {
    # returns dimensions of odbcDataset object
    if (!is.null(rownames(x))) c(length(row.names(x)), length(names(x)))
    else c(length(x[[1]]), length(names(x)))
    }

    
