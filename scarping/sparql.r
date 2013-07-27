#   Author:        Willem Robert van Hage
#   E-mail:        W.R.van.Hage@vu.nl
#   WWW:           http://www.few.vu.nl/~wrvhage/R/sparql.r
#   Copyright (C): 2011, VU University Amsterdam
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License
#   as published by the Free Software Foundation; either version 2
#   of the License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU Lesser General Public
#   License along with this library; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#   As a special exception, if you link this library with other files,
#   compiled with a Free Software compiler, to produce an executable, this
#   library does not by itself cause the resulting executable to be covered
#   by the GNU General Public License. This exception does not however
#   invalidate any other reasons why the executable file might be covered by
#   the GNU General Public License.

library(XML)

#
# Example function call plotting Event types per region
# Run like this: test()
#
test <- function() {
	sparql.plot(host="semanticweb.cs.vu.nl",
		path="/poseidon/ns/sparql/",
		ns=c("eez","http://semanticweb.cs.vu.nl/poseidon/ns/eez/",
		     "lop","http://semanticweb.cs.vu.nl/poseidon/ns/instances/"),
		port="",
		query="PLOT ?et AGAINST ?r
		 SELECT * WHERE { 
			?e sem:eventType ?et .
			?e sem:hasPlace ?p .
			?p eez:inPiracyRegion ?r .
		}");
}

# a few example plot calls
sparql.plot <- function(...) {
	t <- sparql.against(...);
	#plot.sums(t,...);
	#plot.stacked(t,...);
	plot.heatmap(t,scale="none");
}

sparql.against <- function(query="",...) {
	fields <- unlist(strsplit(query,"(?i)(PLOT|PUT|AGAINST|SELECT)"));
	df <- sparql.data.frame(query=paste("SELECT",fields[4]),...);
	d1 <- as.vector(sapply(unlist(strsplit(fields[2],",")),
		function(x){trim(gsub("( |\\?)","",x))}))
	d2 <- as.vector(sapply(unlist(strsplit(fields[3],",")),
		function(x){trim(gsub("( |\\?)","",x))}))
	if (length(d1) > 1 || length(d2) > 1) {
		
	} else {
		print(d1[1]);
		print(d2[1]);
		t <- table(df[[d1[1]]],df[[d2[1]]]);
	}
	rm(df,d1,d2);
	t;
}

#
# Read SPARQL results from end-point
#

sparql.data.frame <- function(host="localhost",path="/",ns=NULL,port="",query="") {
	tf <- tempfile(query);
	if(port != "") {
		portstr <- paste(":",port,sep="")
	} else {
		portstr <- ""
	}
	download.file(paste('http://',host,portstr,path,
		'?query=',URLencode(query),sep=""),tf,quiet=TRUE);
	DOM <- xmlParse(tf);
	attrs <- unlist(xpathApply(DOM,
		paste('//s:head/s:variable',sep=""),
		namespaces=c('s'='http://www.w3.org/2005/sparql-results#'),
		quote(xmlGetAttr(x,"name"))));
	df <- data.frame(sapply(
		attrs,
		function(attr) {
			sapply(
				getNodeSet(DOM,
					paste('//s:result/s:binding[@name="',attr,'"]/s:uri/text() ',
						'| //s:result/s:binding[@name="',attr,'"]/s:bnode',
						'| //s:result/s:binding[@name="',attr,'"]/s:literal/text()',
						sep=""),
					namespaces=c('s'='http://www.w3.org/2005/sparql-results#')),
				function(x) {
					qnames(xmlValue(x),ns)
				})
			}));
	names(df) <- attrs;
	rm(DOM);
	df;
}
		
#
# Barplots
#
plot.stacked <- function(T,...) {
    par(mar=c(3,14,1,18),
    	mfrow=c(1,1),
    	xpd=TRUE);
    c <- length(colnames(T))+length(rownames(T));
    max <- max(colSums(T));
	barplot(T,
		horiz=TRUE,
		las=1,
		col=detshuffle.rainbow(c),
		...);
	legend(x=(max+(max/15)),
		y=length(colnames(T)),
		legend=rownames(T),
		fill=detshuffle.rainbow(c));
	rm(c);
}

# plot marginal frequencies
plot.sums <- function(T,...) {
	barplot(sort(rowSums(T),decreasing=FALSE),
 		horiz=TRUE,
 		las=1,...);
	barplot(sort(colSums(T),decreasing=FALSE),
 		horiz=TRUE,
 		las=1,...);
}

#
# Heatmaps
#
plot.heatmap <- function(T,...) {
	if (dim(T)[1] > dim(T)[2]) {
		heatmap(T,...)	
	} else {
		heatmap(t(T),...)			
	}	
}

#
# Aux. functions
#

detshuffle.rainbow <- function(n) {
	rainbow(n)[
		append(seq(1,n)[seq(1,n) %% 3 == 0],
			append(seq(1,n)[seq(1,n) %% 3 == 1],
				seq(1,n)[seq(1,n) %% 3 == 2]))]
}

subst_ns <- function(str0, ns) {
	gsub(ns[2],paste(ns[1],":",sep=""),str0);
}

qnames <- function(str0, ns_list) {
	if(!length(ns_list))
		str0
	else
		subst_ns(qnames(str0,ns_list[-1:-2]),ns_list[1:2])
}

trim <- function(x) {
	sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
}
