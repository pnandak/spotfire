# *********************************************************************
#
#               rsaLib.r
# 
#  This file holds all the r functions needed to produce amCharts 
#  files  the MetricsCenter dashboards
#
# **************************************************************************
# General functions that work for all amChart types
# **************************************************************************

# **************************************************************************
# drop.levels(dataframe)
#   This function drops unused levels from all factors in a data.frame.
#   Obtained from:
#   http://wiki.r-project.org/rwiki/doku.php?id=tips:data-manip:drop_unused_levels&s=levels
drop.levels <- function(dat){
  # Drop unused factor levels from all factors in a data.frame
  # Author: Kevin Wright.  Idea by Brian Ripley.
  dat[] <- lapply(dat, function(x) x[,drop=TRUE])
  return(dat)
}

# **************************************************************************

# **************************************************************************
# yahInterval(h,value)
#     h is a histogram object returned by hist()
#     value is the value for which the corresponding bar is to be identified
#     yahInterval returns the subscript of the bar where 1 is the first bar
#
# To test the above function:
# for (i in 1:max(x$breaks)) { cat(i, yahInterval(x,i), "\n",sep="")
# **************************************************************************
yahInterval<-function(h,value)  {
   length(h$breaks) - sum( findInterval(h$breaks,value) )
}

# **************************************************************************
# printEventCSVData(xvalues,yvalues,metricName,fileName)
#     xvalues are the values for the x-axis
#     yvalues are the values to be plotted above the x-axis
# **************************************************************************
printEventCSVData<-function(xvalues,yvalues,metricName,fileName) {
   cat(xvalues[1],yvalues[1,],"\n",
      file=fileName, append=FALSE, sep=";")
   for(i in 2:(length(xvalues)-1)) {
      cat(xvalues[i],yvalues[i,],"\n",
      file=fileName, append=TRUE, sep=";")
   }
}

# **************************************************************************
# amPrintHTML(metricName, dataFileName, settingsFileName, htmlFileName)
#     flashObj is one of {amxy, amline,amstock}
#     metricName is the name of the metric
#     amChartsPath is the path to where amCharts flash objects are stored 
#         relative the base URL of the web site
#     dataFileName is the relative (to base URL) path name of the the file to 
#         use to obtain data to plot
#     settingsFileName is the relative (to base URL) path name of the the file 
#         to use to obtain settings 
#     htmlFileName is the absolute path name of the file to write the HTML
# **************************************************************************
amPrintHTML<-function(flashObj,chartTitle,metricName,amChartsPath,dataFileName,settingsFileName,htmlFileName) { 
   cat("<html>","\n",
        "  <head>", "\n",
        "  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />","\n",
        "  <title>",metricName,": ",chartTitle,"</title>","\n",
        "  </head>","\n",
        "  <body>", "\n",
        "  <script type=\"text/javascript\" src=\"",amChartsPath,flashObj,"/swfobject.js\"></script>","\n",
	"    <div id=\"flashcontent\">","\n",
	"      <strong>You need to upgrade your Flash Player</strong>","\n",
	"    </div>","\n",
	"    <script type=\"text/javascript\">","\n",
	"       // <![CDATA[","\n",
	"         var so = new SWFObject(\"",amChartsPath,flashObj,"/",flashObj,".swf\", \"",metricName,"\", \"100%\", \"100%\", \"8\", \"#FFFFFF\");","\n",
	"         so.addVariable(\"path\", \"\");","\n",
	"         so.addVariable(\"settings_file\", encodeURIComponent(\"", settingsFileName,"\"));","\n",
	"         so.addVariable(\"data_file\",     encodeURIComponent(\"", dataFileName,    "\"));","\n",
	"         so.addVariable(\"preloader_color\", \"#999999\");","\n",
	"         so.write(\"flashcontent\");","\n",
	"       // ]]>","\n",
	"    </script>","\n",
        "  </body>","\n",
        "</html>","\n", file=htmlFileName, append=FALSE, sep=""
     )
}
# **************************************************************************
# Trend Line Functions (amline)
# **************************************************************************
# **************************************************************************
# linePrintSeriesXML(xvalues,fileName)
#     x is the series vector of values for the x axis
#     fileName is the name of the file to write the XML to
# **************************************************************************
linePrintSeriesXML<-function(x,fileName) {
   cat("  <series>","\n", file=fileName, append=TRUE, sep="")
   for (i in 1:(length(x))) {
      cat("    <value xid=\"",i,"\">",x[i],"</value>","\n",
      file=fileName, append=TRUE, sep="")
   }
   cat("  </series>","\n",file=fileName, append=TRUE, sep="")
}
# **************************************************************************
# linePrintGraphXML(gName,gColor,x,y,fileName)
#     gName is the gid of the graph
#     gColor is the color of the graph
#     x is the vector of values for the x axis
#     y is the vector of values for the y axis
#     fileName is the name of the file to write the XML to
#     Note:  line_width is hardcoded to be 2 pixels.
#            selected is hardcoded to false
# **************************************************************************
linePrintGraphXML<-function(gName,gColor,x,y,fileName) {
   cat("      <graph gid=\"",gName,"\" title=\"",gName,"\" color=\"",gColor,
                    "\" line_width=\"2\" selected=\"false\">", "\n", file=fileName, append=TRUE, sep="")
   for (i in 1:(length(x))) {
     cat("        <value xid=\"",i,"\">",y[i],"</value>","\n", file=fileName, append=TRUE, sep="")
   }
   cat("      </graph>\n", file=fileName, append=TRUE, sep="")
}
# **************************************************************************
# linePrintXMLData(xvalues,yvalues,graphNames,graphColors,fileName)
#     xvalues is the vector of values for the x axis
#     yvalues is the matrix of values for the y axis.
#     graphNames is a vector of names for the graphs
#     graphColors is a vector of colors to use for the graphs. Same length as graphNames.
#     fileName is the name of the file to write the XML to
# **************************************************************************
linePrintXMLData<-function(xvalues,yvalues,graphNames,graphColors,fileName) {
   cat("<chart>","\n", file=fileName, append=FALSE, sep="")
   linePrintSeriesXML(xvalues,fileName)
   cat("  <graphs>","\n", file=fileName, append=TRUE, sep="")
   for (i in (1:length(graphNames))) {
      linePrintGraphXML(graphNames[i],graphColors[i],xvalues,yvalues[,i],fileName)
   }
   cat("  </graphs>","\n", file=fileName, append=TRUE, sep="")
   cat("</chart>","\n",file=fileName, append=TRUE, sep="")
}
# **************************************************************************
# linePrintSettings(metricName,yLabel,fileName)
#     metricName is the name of the metric
#     yLabel is the label for the y-axis
#     fileName is the name of the file to write the XML to
#
#     Note:  settings file is the same for all trendline charts with the
#            exception that settings for charts with and without trend
#            lines are different.  Hence, the yah flag.
# **************************************************************************
linePrintSettings<-function(metricName,yLabel,fileName) { 
  cat(
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<settings> 
  <data_type>xml</data_type>  
  <decimals_separator>.</decimals_separator>   
  <thousands_separator>,</thousands_separator> 
  <digits_after_decimal>2</digits_after_decimal>
  <precision>2</precision>
  <redraw>true</redraw>                        
  <plot_area>
    <margins>
      <left>60</left>
      <top>30</top>
      <right>10</right>
      <bottom>60</bottom>
    </margins>
  </plot_area>
  <grid>    
    <x>    
      <approx_count>6</approx_count> 
    </x>
    <y_left>                       
      <approx_count>6</approx_count>  
    </y_left>
  </grid>
  <indicator>
    <zoomable>true</zoomable>  
    <color>#DADADA</color>    
    <selection_color>#00000</selection_color> 
    <x_balloon_text_color>#000000</x_balloon_text_color> 
  </indicator>
  <balloon>                                             
    <text_size>16</text_size>                    
  </balloon>    
  <legend>                                 
    <text_size>12</text_size>
    <x>10</x>
    <spacing>0</spacing>
    <max_columns>6</max_columns>
    <width>100%</width>
    <values>                          
      <enabled>true</enabled>          
      <width>100</width>                 
      <align>left</align>                
      <text><![CDATA[: {value}]]></text>  
    </values>
  </legend>
  <export_as_image>              
    <file></file>               
    <target></target>          
    <x></x>                   
    <y></y>                  
    <color></color>         
    <alpha></alpha>        
    <text_color></text_color> 
    <text_size>14</text_size>    
  </export_as_image>
  <context_menu>                           
     <default_items>
       <zoom>false</zoom>    
       <print>false</print> 
     </default_items>
  </context_menu>  
  <labels>      
    <label lid=\"0\">
      <x></x>        
      <y>60%</y>     
      <rotate>true</rotate>
      <width>100%</width>  
      <text_size>12</text_size>   
      <text>                     
        <![CDATA[<b>",yLabel,"</b>]]>
      </text>        
    </label>    
  </labels>
</settings>", file=fileName, append=FALSE, sep="")
}
# **************************************************************************
# Scatter Plot Functions (xy)
# **************************************************************************
# **************************************************************************
# xyPrintGraphXML(gName,gColor,gBullet,gBalloonText,gAlpha,xvalues,yvalues,fileName)
#     gName = the gid of the graph
#     gColor = the color of the graph
#     gBullet = the bullet shape of the scatter points
#     gBalloonText = the text to go in the mouse-over of each point
#     gAlpha = 0 if no line (just points) and 100 if there is a line
#     xvalues = x values for points to be plotted
#     yvalues = y values for points to be plotted
#     fileName is the name of the file to write the XML to
#
# The function to write out a graph section of an amxy XML data file
#     Note:  This function hard codes "value" (bubble size) to 0 for all points.
#            This function does not do bubble charts (but it could)
# **************************************************************************
xyPrintGraphXML<-function(gName,gColor,gBullet,gBalloonText,gAlpha,xvalues,yvalues,fileName) {
   cat("      <graph gid=\"",gName,"\" title=\"",gName,"\" color=\"",gColor,"\" bullet=\"",gBullet,
                "\" bullet_size=\"2\" balloon_text=\"",gBalloonText,"\" alpha=\"",gAlpha,"\" width=\"2\">", 
                "\n", file=fileName, append=TRUE, sep="")
   for (i in 1:(length(xvalues))) {
     cat("        <point x=\"",
         xvalues[i],"\" y=\"",
         yvalues[i],"\" value=\"0\"></point>","\n", 
         file=fileName, append=TRUE, sep="")
   }
   cat("      </graph>\n", file=fileName, append=TRUE, sep="")
}
# **************************************************************************
# xyPrintXMLData(yah,graphNames,graphColors,graphBullets,xvalues,yvalues,trendValues,gradients,fileName)
#     yah is a flag.  yah=1 means show trend line and no points for 
#                     all graphs except the yah graph.  For the yah
#                     graph, show points and line fitted thru them.
#     graphNames = the root names of the graphs (eg "min" or "inside")
#     graphColors = hex values to be used for graphColors
#     graphBullets = strings to specify the bullet type for the points
#     xvalues = x values for points to be plotted.  This is a vector.
#     yvalues = y values for points to be plotted.  This is a matrix 
#               where cols 1-length(graphNames) are y values.
#     trendValues = (x,y) values for first and last point of trend lines
#        col 1 is x values; cols 2-length(graphNames)+1 are trend y values
#     gradients = a vector of gradients for trendlines described by trendValues.
#     fileName is the name of the file to write the XML to
# **************************************************************************
xyPrintXMLData<-function(yah,graphNames,graphColors,graphBullets,xvalues,yvalues,trendValues,gradients,fileName)  {
   cat("<chart>","\n", file=fileName, append=FALSE, sep="")
   cat("  <graphs>","\n", file=fileName, append=TRUE, sep="")
   for (i in (1:length(graphNames))) {
      # Scatter points are not displayed for non yah data on yah graphs plots(too busy)
      if (yah==0 | graphNames[i]=="yah") {
         gBalloonText<-" {x}, {y} "
         gAlpha<-0
         xyPrintGraphXML(graphNames[i],graphColors[i],graphBullets[i],gBalloonText,gAlpha,xvalues,yvalues[,i],fileName)
      }
      # Trend graph is always displayed
      gName<-paste(graphNames[i],"(Trend)",sep="")
      gBullet<-"round"
      gBalloonText<-paste("Gradient: ",round(gradients[i],3),sep="")
      gAlpha<-100
      xyPrintGraphXML(gName,graphColors[i],gBullet,gBalloonText,gAlpha,trendValues[,1],trendValues[,i+1],fileName)
   }
   cat("  </graphs>","\n", file=fileName, append=TRUE, sep="")
   cat("</chart>","\n",file=fileName, append=TRUE, sep="")
}
# **************************************************************************
# xyPrintSettings(metricName,dateFormats,yLabel,fileName)
#     metricName is the name of the metric
#     dateFormats holds three parameters that define the date formats
#     yLabel is the text label for the y-axis
#     fileName is the name of the file to write the XML to
#
#     Note:  settings file is the same for all scatter charts
# **************************************************************************
xyPrintSettings<-function(metricName,dateFormats,yLabel,fileName) {
  cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<settings>
  <data_type>xml</data_type>
  <font>Arial</font>
  <text_size>14</text_size>
  <text_color>#000000</text_color>
  <decimals_separator>.</decimals_separator>
  <digits_after_decimal>2</digits_after_decimal>
  <precision>2</precision>
  <redraw>true</redraw>
  <plot_area>
    <margins>
      <left>50</left>
      <top>40</top>
      <right>10</right>
      <bottom>10</bottom>
    </margins>
  </plot_area>
  <labels>                
    <label lid=\"yaxis\">
      <x>0</x>               
      <y>70%</y>             
      <rotate>90</rotate>    
      <text_color>#000000</text_color>          
      <text_size>12</text_size>         
      <text>",yLabel,"</text>        
    </label>
  </labels>
  <grid>
    <x>
      <alpha>10</alpha>
      <approx_count>15</approx_count>
    </x>
    <y>
      <alpha>10</alpha>
      <approx_count>10</approx_count>
    </y>
  </grid>
  <values>
    <x>
      <inside>false</inside>
      <skip_first>false</skip_first>
      <skip_last>false</skip_last>
      <type>date</type>
      <color>#000000</color>
      <text_size>14</text_size>
    </x>
    <y>
      <inside>false</inside>
      <skip_last>false</skip_last>
      <color>#000000</color>
      <text_size>14</text_size>
    </y>
  </values>
  <axes>
    <x>
      <width>1</width>
      <color>#000000</color>
    </x>
    <y>
      <width>1</width>
      <color>#000000</color>
    </y>
  </axes>
  <date_formats>
    <date_input>",dateFormats[1],"</date_input>
    <duration_input>",dateFormats[2],"</duration_input>
    <balloon>",dateFormats[3],"</balloon>
  </date_formats>
  <balloon>
    <color>#FFFF99</color>
    <text_color>#000000</text_color>
    <corner_radius></corner_radius>
    <border_width>1</border_width>
    <border_alpha>50</border_alpha>
    <border_color>#FFFF99</border_color>
    <alpha>80</alpha>
  </balloon>
  <bullets>
    <grow_time>0</grow_time>
    <sequenced_grow>false</sequenced_grow>
  </bullets>
  <legend>
    <text_size>14</text_size>
    <enabled>true</enabled>
  </legend>
</settings>", file=fileName, append=FALSE, sep="")
}
#
# *********************************************************************
# Histogram Functions (amcolumn) 
# **************************************************************************
#
# **************************************************************************
# colPrintSettings(metricName,xLabel,yLabel,legendFlag,fileName)
#     metricName is tha name of the metric
#     xLabel is the string to use to label the x-axis
#     yLabel is the string to use to label the y-axis
#     legendFlag = flag to turn on/off legend.  "true" means ON.
#     fileName is the name of the file to write the XML to
# **************************************************************************
colPrintSettings<-function(metricName,xLabel,yLabel,legendFlag,fileName) { 
  cat(
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<settings> 
  <type>column</type>
  <data_type>xml</data_type>
  <font>Arial</font>
  <text_size>14</text_size>
  <decimals_separator>.</decimals_separator>
  <thousands_separator>,</thousands_separator>
  <digits_after_decimal>0</digits_after_decimal>
  <redraw>true</redraw>
  <precision>0</precision>
  <column>
    <width>60</width>
    <spacing>1</spacing>
    <grow_time>0</grow_time>
    <grow_effect>regular</grow_effect>   
    <data_labels>
    <![CDATA[]]>
    </data_labels>
    <balloon_text> 
        <![CDATA[{series}: {value}%]]>
    </balloon_text> 
  </column>
  <plot_area>
    <alpha>100</alpha>
    <margins>                      
      <left>50</left>              
      <top>40</top>                
      <right>10</right>            
      <bottom>60</bottom>          
    </margins>
  </plot_area>
  <grid>            
    <category>    
      <alpha>5</alpha>   
    </category>
    <value>              
      <alpha>5</alpha> 
    </value>
  </grid>
  <values>                             
    <value>                        
      <min>0</min>          
    </value>
  </values>
  <axes>                       
    <category>                  
      <width>1</width>      
    </category>
    <value>                    
      <width>1</width>            
    </value>
  </axes>  
  <legend>
    <enabled>",legendFlag,"</enabled>
    <x>50%</x>
    <y>!35</y>
    <text_color>#000000</text_color>
    <text_size>14</text_size>
  </legend>
  <balloon>                
    <text_size>16</text_size> 
  </balloon>
  <export_as_image>              
    <file></file>               
    <target></target>          
    <x></x>                   
    <y></y>                  
    <color></color>         
    <alpha></alpha>        
    <text_color></text_color>       
    <text_size>14</text_size>        
  </export_as_image>
  <context_menu>                      
     <default_items>
       <zoom>false</zoom>   
       <print>false</print>
     </default_items>
  </context_menu>
  <labels>                
    <label lid=\"yaxis\">
      <x>0</x>               
      <y>70%</y>             
      <rotate>90</rotate>    
      <text_color>#000000</text_color>          
      <text_size>14</text_size>         
      <text>",yLabel,"</text>        
    </label>
    <label lid=\"xaxis\">
      <x>15</x>               
      <y>!35</y>             
      <rotate>0</rotate>    
      <text_color>#000000</text_color>          
      <text_size>14</text_size>         
      <text>",xLabel,"</text>        
    </label>
  </labels> 
</settings> ", file=fileName, append=FALSE, sep="")
}
# **************************************************************************
# rowPrintSettings(metricName,xLabel,yLabel,legendFlag,fileName)
#     metricName is tha name of the metric
#     xLabel is the string to use to label the x-axis
#     yLabel is the string to use to label the y-axis
#     legendFlag = flag to turn on/off legend.  "true" means ON.
#     fileName is the name of the file to write the XML to
# **************************************************************************
rowPrintSettings<-function(metricName,xLabel,yLabel,legendFlag,fileName) {
  cat(
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<settings>
  <type>bar</type>
  <data_type>xml</data_type>
  <font>Arial</font>
  <text_size>14</text_size>
  <decimals_separator>.</decimals_separator>
  <thousands_separator>,</thousands_separator>
  <digits_after_decimal>0</digits_after_decimal>
  <redraw>true</redraw>
  <precision>0</precision>
  <column>
    <width>60</width>
    <spacing>1</spacing>
    <grow_time>0</grow_time>
    <grow_effect>regular</grow_effect>
    <data_labels>
    <![CDATA[]]>
    </data_labels>
    <balloon_text>
        <![CDATA[{series}: {value}%]]>
    </balloon_text>
  </column>
  <plot_area>
    <alpha>100</alpha>
    <margins>
      <left>85</left>
      <top>30</top>
      <right>10</right>
      <bottom>55</bottom>
    </margins>
  </plot_area>
  <grid>
    <category>   
      <alpha>5</alpha>  
    </category>
    <value>
      <alpha>5</alpha>
    </value>
  </grid>
  <values>
    <value>
      <min>0</min>
    </value>
  </values>
  <axes>
    <category>
      <width>1</width>
    </category>
    <value>
      <width>1</width>
    </value>
  </axes>
  <legend>
    <enabled>",legendFlag,"</enabled>
    <x>50%</x>
    <y>!35</y>
    <text_color>#000000</text_color>
    <text_size>14</text_size>
  </legend>
  <balloon>
    <text_size>16</text_size>
  </balloon>
  <export_as_image>
    <file></file>
    <target></target>
    <x></x>
    <y></y>
    <color></color>
    <alpha></alpha>
    <text_color></text_color>
    <text_size>14</text_size>
  </export_as_image>
  <context_menu>
     <default_items>
       <zoom>false</zoom>
       <print>false</print>
     </default_items>
  </context_menu>
  <labels>
    <label lid=\"yaxis\">
      <x>40%</x>
      <y>93%</y>
      <rotate>0</rotate>
      <text_color>#000000</text_color>
      <text_size>12</text_size>
      <text>",yLabel,"</text>
    </label>
    <label lid=\"xaxis\">
      <x>5</x>
      <y>75%</y>
      <rotate>90</rotate>
      <text_color>#000000</text_color>
      <text_size>12</text_size>
      <text>",xLabel,"</text>
    </label>
  </labels>
</settings> ", file=fileName, append=FALSE, sep="")
}


# ***************************************************************************
# colPrintXMLData(metricName, graphNames,graphColors,legendTitle,x,y,fileName)
#     metricName = the name of the metric
#     graphNames = vector of names for bars 
#     graphColors = vector of colors for bars
#     legendTitle = string that will appear in a legend, if enabled
#     x = values (strings) for x axis
#     y = values (numeric) for bar heights
#     fileName is the name of the file to write the XML to
# ****************************************************************************
colPrintXMLData<-function(metricName,graphNames,graphColors,legendTitle,x,y,fileName) { 
   legendColor<-"#999999"
   if (legendTitle == "YouAreHere") { legendColor<-"#6699CC" }
   # Print the Series XML
   N<-length(x)
   cat("<chart>","\n",
        "  <series>", "\n",
        "    <value xid=\"1\">",x[1],"</value>","\n", file=fileName, append=FALSE, sep="")
   for (i in (2:N) ) {
      cat("    <value xid=\"",i,"\">",x[i],"</value>","\n",file=fileName, append=TRUE, sep="")
   }
   #  Print graph XML for y data AS PERCENTS of TOTAL
   Pcts<-round(100*(y/sum(y)),0)
   cat("  </series>","\n", "  <graphs>", "\n",
       "    <graph gid=\"hist\" color=\"",legendColor,"\" balloon_color=\"#000000\" title=\"",legendTitle,"\">","\n",
       "      <value xid=\"1\" color=\"",graphColors[1],"\" title=\"",graphNames[1],"\">",Pcts[1],"</value>","\n", file=fileName, append=TRUE, sep="")
   for (i in (2:N) ) {
      cat("      <value xid=\"",i,"\" color=\"",graphColors[i],"\" title=\"",graphNames[i],"\">",Pcts[i],"</value>","\n",
      file=fileName, append=TRUE, sep="")
   }
   cat("    </graph>","\n", file=fileName, append=TRUE, sep="")
   cat("  </graphs>", "\n",
       "</chart>", file=fileName, append=TRUE, sep="")
}

# *****************************************************************
#                             THE END
# *****************************************************************
