# *********************************************************************
#               htmlLib.r
# 
#  This file holds all the r functions needed to produce html
#  for the MetricsCenter dashboards
#
# **************************************************************************
#  Heat Map Function
# **************************************************************************
# hmPrintHTML(chartTitle,metricName,htmlFileName,values,styles)
#     chartTitle is the title for the html window
#     metricName is the name of the metric
#     htmlFileName is the absolute path name of the file to write the HTML
#     values is the matrix of numeric values for the heatmap
#     styles is the matrix of styles for each cell in the heatmap
# **************************************************************************
hmPrintHTML<-function(chartTitle,metricName,htmlFileName,values,styles) { 
cat("<html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
    <title>",metricName,": ",chartTitle,"</title>
    <style>
    body {
        font-family: arial, Helvetica, sans-serif;
        color: black;
    }
    /* Heatmap Styles */
    table.legend td {
        padding: 4px; 
        border-collapse: collapse;
        text-align: center;
    }
    table.heatmap td { 
        padding: 8px; 
        border-collapse: collapse;
        text-align: center;
    }
    th.hm-row-title {
        text-align: right;
        font-weight: bold;
        color: #000000;
    }
    /*  Very high correlation:  dark green .9-1.0  */
    td.vhi {
        color: #FFFFFF;
        background-color: #4AC948;
        font-weight: bold;
        font-size: larger;
    }
    /* Good correlation: Good   .8-.9 */
    td.hi  {
        color: #000000;
        background-color: #33FF33;
        font-weight: bold;
        font-size: larger;
    }
    /* Medium correlation:  yellow  .6-.8  */
    td.med {
        color: #000000;
        background-color: #FFFF00;
    }
    /* Very low correlation:  light yellow .3-.6  */
    td.low {
        color: #000000;
        background-color: #FFFFAA;
    }
    /* No correlation:  black  0 to .3*/
    td.noc {
        border: 1px solid black;
        color: #000000;
        background-color: #FFFFFF;
    }
    /* Significance Styles */
    td.na {
        border: 1px solid black;
        color: #000000;
        background-color: #EDEDED;
    }
    td.none {
        border: 1px solid black;
        color: #000000;
        background-color: #FFFFFF;
    }
    /* 95% Confidence */
    td.conf95 {
        color: #000000;
        background-color: #B4EEB4;
        font-size: larger;
    }
    /* 99% Confidence */
    td.conf99 {
        color: #000000;
        background-color: #33FF33;
        font-weight: bold;
        font-size: larger;
    }
    /* 99.9% Confidence */
    td.conf999 {
        color: #FFFFFF;
        background-color: #4AC948;
        font-weight: bold;
        font-size: larger;
    }
    </style>
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
         <th scope=\"row\" class=\"hm-row-title\">",rownames(values)[i],"</th>", file=htmlFileName, append=TRUE, sep="")
   for (j in (1:ncol(values)))  {
      cat("
         <td class=\"",styles[i,j],"\">",values[i,j],"</td>", file=htmlFileName, append=TRUE, sep="")
    }                             # col loop
    cat("
      </tr>", file=htmlFileName, append=TRUE, sep="")
}                                 # row loop
cat("
</table>
<br>Legend: ",file=htmlFileName,append=TRUE, sep="")

chi.strings<-c("na","none","conf95","conf99","conf999")
cor.strings<-c("noc","low","med","hi","vhi")
ex<-styles[1,1]

# Print either a ChiSquare or Correlation legend depending upon hm.style values
if (length(grep(ex,chi.strings))) {
    # ChiSquare Legend
    cat("
    Confidence Levels
    <br><table class=\"legend\">
      <tbody>
      <tr>
        <td class=\"na\"     >NA</td> 
        <td class=\"none\"   > &lt;95%</td> 
        <td class=\"conf95\" >95%</td> 
        <td class=\"conf99\" >99%</td> 
        <td class=\"conf999\">99.9%</td> 
      </tr>
      </tbody>
    </table>", file=htmlFileName, append=TRUE, sep="")
    }
if (length(grep(ex,cor.strings))) {
    # Correlation Legend
    cat("
    Correlation Levels
    <br><table class=\"legend\">
      <tbody>
      <tr>
        <td class=\"noc\" >&plusmn;0-.3</td>
        <td class=\"low\" >&plusmn;.3-.6</td>
        <td class=\"med\" >&plusmn;.6-.8</td>
        <td class=\"hi\"  >&plusmn;.8-.9</td>
        <td class=\"vhi\" >&plusmn;.9-1</td>
      </tr>
      </tbody>
    </table>", file=htmlFileName, append=TRUE, sep="")
    }
cat("
</body>
</html>",file=htmlFileName, append=TRUE, sep="")
}

# **************************************************************************
#  HTML top10 Table Function
# **************************************************************************
# top10PrintHTML(chartTitle,metricName,htmlFileName,values,rowStyles,evenColor,oddColor)
#     chartTitle is the title for the html window
#     metricName is the name of the metric
#     htmlFileName is the absolute path name of the file to write the HTML
#     values is the matrix of numeric values for the table
#     rowStyles is a vector styles for each row in the table
#     evenColor is the color for the background of even rows
#     oddColor is the color for the background of odd rows
# **************************************************************************
top10PrintHTML<-function(chartTitle,metricName,htmlFileName,values,styles,evenColor,oddColor) { 
cat("<html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
    <title>",metricName,": ",chartTitle,"</title>
    <style>
        body {
            font-family: Helvetica,arial,sans-serif;
            color: black;
        }
        table { 
            border-collapse: collapse;
            margin-bottom:  2em;
        }
        tr {
            text-align: center;
            vertical-align: middle;
        }
        th {
            border: 1px solid black;
            color: #000000;
            font-weight: normal;
            text-align: left;
            padding: 0.1em 0.5em;
        }
        td {
            border: 1px solid black;
            font-weight: normal;
        }
        tr.rowEven { 
            background-color: ", evenColor, ";
            color: #000000;
        }
        tr.rowOdd { 
            background-color: ", oddColor, ";
            color: #000000;
        }
        tr.rowTop {
            background-color: #E8E8E8;
            color: #000000;
        } 
    </style>
  </head>
  <body>
    <table>
      <tr class=\"rowTop\">", file=htmlFileName, append=FALSE, sep="")
for (j in (1:ncol(values))) {
  cat("
         <th scope=\"col\">",values[1,j],"</th>",file=htmlFileName, append=TRUE, sep="")
}
cat("
      </tr>", file=htmlFileName, append=TRUE, sep="")
for (i in (2:nrow(values)))  {
   cat("
      <tr class=\"",rowStyles[i],"\">
         <th scope=\"row\">",values[i,1],"</th>", file=htmlFileName, append=TRUE, sep="")
   for (j in (2:ncol(values)))  {
      cat("
         <td>",values[i,j],"</td>", file=htmlFileName, append=TRUE, sep="")
    }                             # col loop
    cat("
      </tr>", file=htmlFileName, append=TRUE, sep="")
}                                 # row loop
cat("
</table>
</body>
</html>",file=htmlFileName, append=TRUE, sep="")
}
