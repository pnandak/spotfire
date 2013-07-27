# *********************************************************************
#               hmLib.r
# 
#  This file holds all the r functions needed to produce heatmaps
#  for the MetricsCenter dashboards
#
# **************************************************************************
# hmPrintHTML(chartTitle,metricName,htmlFileName,cssFileName,values,styles)
#     chartTitle is the title for the html window
#     metricName is the name of the metric
#     htmlFileName is the absolute path name of the file to write the HTML
#     cssFileName is the css file with heatmap styles defined
#     values is the matrix of numeric values for the heatmap
#     stules is the matrix of styles for each cell in the heatmap
# **************************************************************************
hmPrintHTML<-function(chartTitle,metricName,htmlFileName,cssFileName,values,styles) { 
cat("<html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
    <title>",metricName,": ",chartTitle,"</title>
    <style type=\"text/css\">@import url(\"",cssFileName,"\");</style>
  </head>
  <body>
    <table class=\"heatmap\">
      <tr>
         <th scope=\"col\"></th>", file=htmlFileName, append=FALSE, sep="")
for (j in (1:ncol(values))) {
  cat("
         <th scope=\"col\">",colnames(values)[j],"</th>",file=htmlFileName, append=TRUE, sep="")
}
cat("
      </tr>", file=htmlFileName, append=TRUE, sep="")
for (i in (1:nrow(values)))  {
   cat("
      <tr>
         <th scope=\"row\">",rownames(values)[i],"</th>", file=htmlFileName, append=TRUE, sep="")
   for (j in (1:ncol(values)))  {
      cat("
         <td class=\"",styles[i,j],"\">",values[i,j],"</td>", file=htmlFileName, append=TRUE, sep="")
    }                             # col loop
    cat("
      </tr>", file=htmlFileName, append=TRUE, sep="")
}                                 # row loop
cat("
</table>
</body>
</html>",file=htmlFileName, append=TRUE, sep="")
}

