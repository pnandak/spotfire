cat( "### ****************************************************************************\n" )
cat( "### ***   R testscript for xlsReadWrite                                      ***\n" )
cat( "### ****************************************************************************\n" )
rm( list = ls() )

### This script fills the role of a (primitive) unit testing framework.
### While it is primarily an internal quality tool it also serves as a demo file.

### To run it at your computer, follow these steps:

# - install the xlsReadWrite zip-File (if not done before)
# - unzip the testDataAndScript.zip in a testfolder
# - set the working path (wd, below) to this testfolder (use double backslashes (shell fkt...))
# - go

wd <- "D:\\Projekte\\People\\_chappi\\RPackages\\xlsReadWrite\\Test"

cat( "################################################################################\n" )
cat( "### section not interesting. On my computer the dll and the functions will be\n" )
cat( "### loaded, on your computer 'library( 'xlsReadWrite' )' will be called.\n" )
cat( "\n" )

if (exists( ".wd" )) {      # use .wd variable if called from release-testscript (Rterm)
		wd <- .wd
    	dolowlevel <- FALSE
} else {
	  dolowlevel <- TRUE    # TRUE or FALSE
}
setwd( wd )

if (dolowlevel && all( Sys.info()["user"] == "chappi" )) {
    myDll <- "D:/Projekte/People/_chappi/RPackages/xlsReadWrite/_Dev/xlsReadWrite.dll"
    myRSrc <- "D:/Projekte/People/_chappi/RPackages/xlsReadWrite/_Package/xlsReadWrite/R"
    info <- dyn.load( myDll )
    invisible( apply( cbind( list.files( myRSrc, full.names = TRUE ) ), 1, source ) )
} else {
    library( "xlsReadWrite" )
}


cat( "################################################################################\n" )
cat( "################################################################################\n" )
cat( "### the sub-testscript files will be sourced now...\n" )
cat( "################################################################################\n" )
cat( "################################################################################\n" )
testtotal <- Sys.time()
cat( "\n\n\n" )

source( "testReadWriteScript.R" )
cat( "\n\n\n" )
if (dolowlevel && all( Sys.info()["user"] == "Chappi" )) {
    source( "INTERNALtestLicensingScript.R" )
}

cat( "\n" )
cat( "################################################################################\n" )
cat( "### R testscript for xlsReadWritePro finished                                ***\n" )
cat( "################################################################################\n" )
cat( "### Duration:", Sys.time() - testtotal, "seconds\n" )
cat( "################################################################################\n" )

