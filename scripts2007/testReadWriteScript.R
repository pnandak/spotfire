  cat( "### ****************************************************************************\n" )
  cat( "### ***   R read/write testscript for xlsReadWrite                           ***\n" )
  cat( "### ****************************************************************************\n" )
  
  ### This script has to be sourced from testscript.R
  
  
  cat( "################################################################################\n" )
  cat( "Time:", paste( total <- Sys.time() ), "\n" )
  
  
  cat( "\n################################################################################\n" )
  cat( "### TEST: double read/write (compare with original values)\n" )
  
    # choose a filename
  rfile <- "TestReadDataFile.xls"
  wfile <- "TestWriteDataFile.xls"
  
    # original data to check if read is ok
  orig <- cbind( c( 42.345000000000000,2.131852701746450,5.196028573821160,4.052032658276100,5.455442814521230,0.920121101217604,4.320537450438270,9.228986838061070,3.495177339311360,7.162184977985050,0.079735376876580,0.190438423685664,6.947886357716580,8.342849375654040,3.203864000470110,9.075757953267320,4.700369680902120,4.889124225952680,5.519390864371310,4.852458850181800 ),
                 c( 3.395006816620560,3.688808025310860,1.182829713821520,5.286272675203200,2.772115393878420,4.547634389328750,9.723958473331380,4.811160580044460,6.250465426071360,9.579865360317900,6.635986679801000,2.808309283842360,6.869654789485020,6.786933214202360,8.276725566162250,8.232178099655150,8.048072598480710,3.608102286243230,5.226040623965190,3.940313408647470 ),
                 c( 3.800523340130150,3.147041728858900,7.401595346568170,4.757634840096100,7.530930520646660,2.108720283363290,8.516694933737470,2.070764974017880,9.260555562457060,8.930731727676670,6.070516769429020,8.473031283814710,4.987162411190200,5.551250102340950,1.694583582632750,4.814321435453790,3.165188266928200,1.912401883371740,8.509662322375530,3.261048351825740 ) )
  
    # read and check with orig values
  res1 <- read.xls( rfile,, "doubleSheet", "double", from = 2 )
  if (!isTRUE( all.equal( res1, orig, check.attributes = FALSE ) )) stop( "double read/write (data not equal)" )
  if (!ncol( res1 ) == 3 ) stop( "double read/write (data not equal)" )
  if (!nrow( res1 ) == 20 ) stop( "double read/write (data not equal)" )
  if (!all( colnames( res1 ) == c( "Kol1", "Kol2", "Kol3") )  ) stop( "double read/write (data not equal)" )
    # write orig values, then read again and check
  write.xls( orig, wfile, colNames = FALSE )
  res2 <- read.xls( wfile, FALSE )
  if (!all( res2 == orig)) stop( "double read/write (data not equal)" )
  
    # check user colNames (note: "from" has been increased by one in order to point to the first data row)
  res1 <- read.xls( rfile, colNames = c( "MyCol1", "MyCol2", "MyCol3" ), "doubleSheet", "double", from = 3 )
  if (!isTRUE( all.equal( res1, orig, check.attributes = FALSE ) )) stop( "double read/write (data not equal)" )
  if (!all( colnames( res1 ) == c( "MyCol1", "MyCol2", "MyCol3" ) )  ) stop( "double read/write (data not equal)" )
  write.xls( res1, wfile, colNames = TRUE )
  res2 <- read.xls( wfile, TRUE,, "double" )
  if (!isTRUE( all.equal( res1, res2 ))) stop( "double read/write (data not equal)" )
  

cat( "\n################################################################################\n" )
cat( "### TEST: read/write (integer/logical/character/data.frame)\n" )

  # choose a filename
rfile <- "TestReadDataFile.xls"
wfile <- "TestWriteDataFile.xls"

  # if you want, open the excel file (it is possible to read from an opened file)
# have a look at the sheets
# (doubleSheet, integerSheet, logicalSheet, characterSheet, data.frameSheet)

  # integer
res1 <- read.xls( rfile, FALSE, "integerSheet", "integer" )
write.xls( res1, wfile, FALSE )
res2 <- read.xls( wfile, FALSE,, "integer" )
if (!identical( res1, res2 ) ) stop( "read/write test (data not equal)" )

  # logical
res1 <- read.xls( rfile,, 3, "logical", from = 2 )
write.xls( res1, wfile, FALSE )
res2 <- read.xls( wfile, FALSE,, "logical" )
if (!all( res1 == res2 ) ) stop( "read/write test (data not equal)" )

  # character
res1 <- read.xls( rfile,, "characterSheet", "character" )
if (!all( colnames( res1 ) == c( "Bachmann", "Chatwin" ) )) stop( "read/write test (data not equal)" )
write.xls( res1, wfile, TRUE )
res2 <- read.xls( wfile, TRUE,, "character" )
if (!identical( res1, res2 ) ) stop( "read/write test (data not equal)" )

  # data.frame (this is the default)
res1 <- read.xls( rfile, sheet = "data.frameSheet", from = 5 )
if (!identical( rownames(res1)[1], "Courtelary" ) ) stop( "read/write test (data not equal)" )
if (!res1[2,2] == 45.1 ) stop( "read/write test (data not equal)" )
write.xls( res1, wfile, TRUE )
res2 <- read.xls( wfile, TRUE,, "data.frame" )
if (!identical( res1, res2 ) ) stop( "read/write test (data not equal)" )


  cat( "\n################################################################################\n" )
  cat( "### TEST: frame/matrix read (special cases)\n" )
  
    # choose a filename
  rfile <- "TestReadDataFile.xls"
  wfile <- "TestWriteDataFile.xls"
  
    # frame with 2 empty columns
  cat( "(2 warnings occur with the next statement (we did suppress them))\n" )
  suppressWarnings( res1 <- read.xls( rfile, sheet = "frame.NA.1" ) )
  if (!all( is.na( res1[,1] ) )) stop( "frame/matrix read special cases (data not equal)" )
  if (!all( is.na( res1[,2] ) )) stop( "frame/matrix read special cases (data not equal)" )
  if (!res1[2,5] == 45.1 ) stop( "frame/matrix read special cases (data not equal)" )
  
    # frame with 2 empty rows and 2 empty columns
    ### in the open source version the data must follow the header immediately !!!
    ### (in the Pro version the first 10 rows are scanned for data)
  cat( "(2 warnings occur with the next statement (we did suppress them))\n" )
  suppressWarnings( res1 <- read.xls( rfile, sheet = "frame.NA.2" ) )
  if (!all( is.na( res1[,c(1,2)] ) )) stop( "frame/matrix read special cases (data not equal)" )
  
    # frame and character vector with only 1 column
  dat <- c( "Courtelary", "Delemont", "Franches-Mnt", "Moutier", "Neuveville", "Porrentruy" )
  res1 <- read.xls( rfile, FALSE, sheet = "frame.1col", from = 4, stringsAsFactor = FALSE )  # data row
  if (!isTRUE( all.equal( res1[[1]], dat ))) stop( "frame/matrix read special cases (data not equal)" )
  write.xls( res1, wfile, colNames = FALSE )
  res2 <- read.xls( wfile, FALSE, stringsAsFactor = FALSE )
  if (!isTRUE( all.equal( res1, res2 ) )) stop( "double read/write (data not equal)" )
  
  res1 <- read.xls( rfile, TRUE, sheet = "frame.1col", from = 3, stringsAsFactor = FALSE )   # (empty) header row
  if (!isTRUE( all.equal( res1[[1]], dat ))) stop( "frame/matrix read special cases (data not equal)" )
  res1 <- read.xls( rfile, TRUE, sheet = "frame.1col", from = 3, colClasses = "character" )
  if (!isTRUE( all.equal( res1[[1]], dat ))) stop( "frame/matrix read special cases (data not equal)" )
  res1 <- read.xls( rfile, TRUE, sheet = "frame.1col", from = 3, colClasses = "factor" )
  if (!isTRUE( all.equal( res1[[1]], as.factor( dat ) ))) stop( "frame/matrix read special cases (data not equal)" )
  write.xls( res1, wfile, colNames = FALSE )
  res2 <- read.xls( wfile, FALSE, colClasses = "factor" )
  if (!isTRUE( all.equal( res1[[1]], as.factor( res2[[1]] ) ))) stop( "frame/matrix read special cases (data not equal)" )
  
  res1 <- read.xls( rfile, TRUE, sheet = "frame.1col", from = 3, type = "character" )
  if (!isTRUE( all.equal( drop( res1 ), dat ) )) stop( "frame/matrix read special cases (data not equal)" )
  
    # frame with only 1 column and NA-factor
  res1 <- read.xls( rfile, TRUE, sheet = "frame.1col", from = 3, colClasses = "factor" )
  res1[[1]][2] <- NA
  write.xls( res1, wfile, colNames = FALSE )
  res2 <- read.xls( wfile, FALSE, stringsAsFactor = FALSE )
  dat[2] <- ""   # not so well
  if (!isTRUE( all.equal( res2[[1]], dat ) )) stop( "double read/write (data not equal)" )
  

cat( "\n################################################################################\n" )
cat( "### TEST: frame read (colClasses/rowNames/colNames)\n" )

  # check double/integer/character
res1 <- read.xls( rfile, colClasses = c( "double", "integer", "character" ), from = 2 )
if (!isTRUE( all.equal( res1$Kol1, orig[,1] ) )) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$Kol2, as.integer(orig[,2]) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$Kol3, as.character(orig[,3]) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

  # check scalar colClass (character)
res1 <- read.xls( rfile, colClasses = "character", from = 2 )
# don't check first column (the 11th row is different (decimal places))
if (!isTRUE( all.equal( res1$Kol2, as.character(orig[,2]) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$Kol3, as.character(orig[,3]) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

  # check logical/NA/character  (for logical, numbers will be truncated first)
res1 <- read.xls( rfile, colClasses = c( "logical", "NA", "character" ), from = 2 )
if (!isTRUE( all.equal( res1$Kol1, as.logical(as.integer(orig[,1])) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$Kol2, orig[,2] ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$Kol3, as.character(orig[,3]) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

  # check rowname
res1 <- read.xls( rfile, rowNames = TRUE, from = 2 )
if (!isTRUE( all.equal( as.double( rownames( res1 ) ), orig[,1] ) )) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$Kol2, orig[,2] ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$Kol3, orig[,3] ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

  # check custom rowname
myr <- paste( 1:20, "a", sep = "" )
res1 <- read.xls( rfile, rowNames = myr, from = 2 )
if (!all( rownames( res1 ) == myr )) stop( "frame read colClasses (data not equal)" )
if (!isTRUE( all.equal( res1$Kol1, orig[,1] ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$Kol2, orig[,2] ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$Kol3, orig[,3] ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

  # check custom rowname with wrong number of supplied rownames
cat( "(1 error occurs with the next statements (we did suppress it))\n" )
res1fkt <- function() read.xls( rfile, rowNames = paste( 1:19, "a", sep = "" ), from = 2 )
res1 <- try( res1fkt(), silent = TRUE )
if (class( res1 ) != "try-error") stop( "frame read colClasses/rowNames/colNames (data not equal)" )
cat( "SUPPRESSED error message:\n", res1 )

  # check factor and rownames (with colHeader, with/without entry for rowname column)
char <- read.xls( rfile, sheet = "data.frameSheet", from = 5, rowNames = TRUE, colClasses = c( rep( "NA", 6 ), "character" ) )[,7]
res1 <- read.xls( rfile, sheet = "data.frameSheet", from = 5, rowNames = TRUE, colClasses = c( rep( "NA", 6 ), "factor" ) )
if (!isTRUE( all.equal( res1$Testcharacter, as.factor( char ) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

char <- read.xls( rfile, sheet = "data.frameSheet", from = 5, rowNames = TRUE, colClasses = c( rep( "NA", 7 ), "character" ) )[,7]
res1 <- read.xls( rfile, sheet = "data.frameSheet", from = 5, rowNames = TRUE, colClasses = c( rep( "NA", 7 ), "factor" ) )
if (!isTRUE( all.equal( res1$Testcharacter, as.factor( char ) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

  # check factor and rownames (without colHeader, with/without entry for rowname column)
res1 <- read.xls( rfile, FALSE, sheet = "data.frameSheet", from = 6, rowNames = TRUE, colClasses = c( rep( "NA", 6 ), "factor" ) )
if (!isTRUE( all.equal( res1[,7], as.factor( char ) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

res1 <- read.xls( rfile, FALSE, sheet = "data.frameSheet", from = 6, colClasses = c( rep( "NA", 7 ), "factor" ) )
if (!isTRUE( all.equal( res1[,8], as.factor( char ) ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

  # check custom colNames (with rownames)
res1 <- read.xls( rfile, colNames = c( "dummyCol", "MyCol2", "MyCol3" ), from = 3, rowNames = TRUE )
if (!isTRUE( all.equal( as.double( rownames( res1 ) ), orig[,1] ) )) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$MyCol2, orig[,2] ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1$MyCol3, orig[,3] ))) stop( "frame read colClasses/rowNames/colNames (data not equal)" )

cat( "(1 error occurs with the next statements (we did suppress it))\n" )
res1fkt <- function() read.xls( rfile, colNames = c( "MyCol2", "MyCol3" ), from = 3, rowNames = TRUE )
res1 <- try( res1fkt(), silent = TRUE )
if (class( res1 ) != "try-error") stop( "frame read colClasses/rowNames/colNames (data not equal)" )
cat( "SUPPRESSED error message:\n", res1 )


cat( "\n################################################################################\n" )
cat( "### TEST: matrix read (rowNames/colNames)\n" )

  # check rowname
res1 <- read.xls( rfile, rowNames = TRUE, from = 2, type = "double" )
if (!isTRUE( all.equal( as.double( rownames( res1 ) ), orig[,1] ) )) stop( "matrix read rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1[,1], orig[,2], check.attributes = FALSE, check.names = FALSE ))) stop( "matrix read rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1[,2], orig[,3], check.attributes = FALSE, check.names = FALSE  ))) stop( "matrix read rowNames/colNames (data not equal)" )

  # check custom rowname
myr <- paste( 1:20, "a", sep = "" )
res1 <- read.xls( rfile, rowNames = myr, from = 2, type = "double" )
if (!all( rownames( res1 ) == myr )) stop( "frame read colClasses (data not equal)" )
if (!isTRUE( all.equal( res1[,1], orig[,1], check.attributes = FALSE, check.names = FALSE ))) stop( "matrix read rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1[,2], orig[,2], check.attributes = FALSE, check.names = FALSE  ))) stop( "matrix read rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1[,3], orig[,3], check.attributes = FALSE, check.names = FALSE  ))) stop( "matrix read rowNames/colNames (data not equal)" )

  # check custom rowname with wrong number of supplied rownames
cat( "(1 error occurs with the next statements (we did suppress it))\n" )
res1fkt <- function() read.xls( rfile, rowNames = paste( 1:19, "a", sep = "" ), from = 2, type = "double" )
res1 <- try( res1fkt(), silent = TRUE )
if (class( res1 ) != "try-error") stop( "matrix read rowNames/colNames (data not equal)" )
cat( "SUPPRESSED error message:\n", res1 )

  # check custom colNames (with rownames)
res1 <- read.xls( rfile, colNames = c( "dummyCol", "MyCol2", "MyCol3" ), from = 3, rowNames = TRUE, type = "double" )
if (!isTRUE( all.equal( as.double( rownames( res1 ) ), orig[,1] ) )) stop( "matrix read rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1[,colnames( res1 ) == "MyCol2"], orig[,2], check.attributes = FALSE, check.names = FALSE ))) stop( "matrix read rowNames/colNames (data not equal)" )
if (!isTRUE( all.equal( res1[,colnames( res1 ) == "MyCol3"], orig[,3], check.attributes = FALSE, check.names = FALSE ))) stop( "matrix read rowNames/colNames (data not equal)" )

cat( "(1 error occurs with the next statements (we did suppress it))\n" )
res1fkt <- function() read.xls( rfile, colNames = c( "MyCol2", "MyCol3" ), from = 3, rowNames = TRUE, type = "double" )
res1 <- try( res1fkt(), silent = TRUE )
if (class( res1 ) != "try-error") stop( "matrix read rowNames/colNames (data not equal)" )
cat( "SUPPRESSED error message:\n", res1 )


cat( "\n################################################################################\n" )
cat( "### TEST: specialities\n" )

  # value from eric that was wrong earlier
res1 <- read.xls( rfile, sheet = "specialities", type = "double", from = 10 )[1]
if (!res1 == 143.28) stop( "specialities (data not equal)" )

cat( "\n################################################################################\n" )
cat( "### TEST: date/time\n" )

myd <- c( 38429, 38432, 38433, 38434, 38435, 38440, 38441, 38442, 38443 )

  # dateTimeToStr and strToDateTime
s <- dateTimeToStr( myd )
myd2 <- strToDateTime( s )
if (!all( myd == myd2 )) stop( "date/time (data not equal)" )

  # formatted dateTimeToStr
s <- dateTimeToStr( myd, 'mm-dd-yyyy' )
if (!all( s[c(3, 7)] == c( "03-22-2005", "03-30-2005" ) )) stop( "date/time (data not equal)" )
  
  # dateTimeToIsoStr and isoStrToDateTime
s <- dateTimeToIsoStr( myd )
myd2 <- isoStrToDateTime( s )
if (!all( myd == myd2 )) stop( "date/time (data not equal)" )

  # check different iso strings
myd2 <- isoStrToDateTime( c( "20070319", "2007-03-19", "20070319233112", "2007-03-19 23:31:12"  ) )
if (!all( myd2 == c( 39160, 39160, 39160.98, 39160.98  ) )) stop( "date/time (data not equal)" )

  # isodatetime, isodate and isotime
cc <- c( "numeric", "isodate", "numeric", "numeric", "numeric", "isotime", "isodatetime" )
res1 <- read.xls( rfile, sheet = "specialities", colClasses = cc, from = 15 )
if (!all( colnames( res1 ) == c( "DateAsNumber", "DateAsDate", "Hour", "Minute", "Sec", "TimeAsTime", "DateTimeAsDateTime" ) )  ) stop( "date/time (data not equal)" )
if (!dateTimeToStr( res1$DateAsNumber[7], "YYYY-MM-DD" ) == "2005-03-30" ) stop( "date/time (data not equal)" )
if (!res1$DateAsDate[1] == "2005-03-18" ) stop( "date/time (data not equal)" )
if (!res1$TimeAsTime[19] == "11:00:31" ) stop( "date/time (data not equal)" )
if (!res1$DateTimeAsDateTime[14] == "2005-04-08 10:26:57" ) stop( "date/time (data not equal)" )


cat( "\n######################################\n" )
cat( "### CONGRATULATION, it works fine!\n" )
cat( "######################################\n" )
cat( "### Time:", paste( Sys.time() ), "\n" )
cat( "### Duration:", Sys.time() - total, "seconds\n" )
cat( "######################################\n" )
