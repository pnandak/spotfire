## more infos, tutorials, etc.: http://www.omegahat.org/

## Two basic strategies:
## 1) Through API (typically more user-friendly)
## 2) DIY (more data preparation needed)

##---------------------- Example 1: WDI -------------------------
## Reading economic data from http://data.worldbank.org/
## WDI package makes use of the API and allows to extract various economic numbers

library(WDI)
help(package = "WDI")
WDIsearch()
WDIindicator <- "NY.GNS.ICTR.ZS" #gross savings (% of GDP)

wdi.at <- WDI(country = "AT", indicator = WDIindicator, start = 1998, end = 2011, extra = FALSE)
wdi.at
wdi.de <- WDI(country = "DE", indicator = WDIindicator, start = 1998, end = 2011, extra = FALSE)
wdi.gb <- WDI(country = "GB", indicator = WDIindicator, start = 1998, end = 2011, extra = FALSE)

plot(wdi.at[,4], wdi.at[,3], type = "l", xlim = c(1998,2011), ylim = c(8,28), xlab = "years", ylab = "Gross savings (% of GNI)", main = "Gross Savings")
lines(wdi.de[,4], wdi.de[,3], col = 2)
lines(wdi.gb[,4], wdi.gb[,3], col = 3)
legend("bottomright", legend = c("Austria", "Germany", "Great Britain"), col = 1:3, lty = 1)


##------------------------- Example 2: RLastFM ----------------------------
## Data from online radio LastFM: www.last.fm
library(RLastFM)
ls(2) 

pinkfloyd.records <- album.search("Pink Floyd")$album[1:15]
pinkfloyd.records
pinkfloyd.records1 <- pinkfloyd.records[c(1:2, 4:9)]
pinkfloyd.records1

?album.getInfo
album.getInfo(album = "The Wall", artist = "Pink Floyd")

#determine how often it was played
pinkfloyd.count <- sapply(pinkfloyd.records1, function(tr) {album.getInfo(album = tr, artist = "Pink Floyd")$playcount})
pinkfloyd.count
pinkfloydbar <- barplot(sort(pinkfloyd.count, decreasing = TRUE), cex.names = 0.6, main = "Pink Floyd Records")         
text(pinkfloydbar, sort(pinkfloyd.count + 5e5, decreasing = TRUE), format(sort(pinkfloyd.count, decreasing = TRUE)), xpd = TRUE, cex = 0.6)


##---------------------- Example 3: RGoogleMaps -----------------
## import maps from GoogleMaps: http://maps.google.com
# API: http://code.google.com/apis/maps/documentation/staticmaps/
# API sign-in: http://code.google.com/apis/maps/signup.html

library(RgoogleMaps)
library(rgdal) 

wumap1 <- GetMap(destfile="mypic1.png", zoom = 14, center = c(48.23178, 16.3579))   ## get WU
PlotOnStaticMap(wumap1)

wumap2 <- GetMap(destfile="mypic1.png", zoom = 17, center = c(48.23178, 16.3579), maptype = "hybrid")
PlotOnStaticMap(wumap2)

wumap3 <- GetMap(destfile="mypic1.png", zoom = 17, center = c(48.23178, 16.3579), maptype = "satellite", marker = cbind.data.frame(lat = 48.23280, lon = 16.35921))
PlotOnStaticMap(wumap3)

wulon <- c(16.3568,16.3622,16.3602,16.3562,16.3568)
wulat <- c(48.22892,48.22944,48.23338,48.23281,48.22892)
wumap4 <- GetMap(destfile="mypic1.png", zoom = 16, center = c(48.23178, 16.3579), maptype = "satellite", marker = cbind.data.frame(lat = 48.23280, lon = 16.35921))
PlotOnStaticMap(wumap4, lon = wulon, lat = wulat, col = 'yellow', lty = 2, type = "o")


## ----------------------------- Example 4: Table from Wikipedia ----------------
## scrape Table from Wikipedia: http://en.wikipedia.org/wiki/World_population 

wptable <- readHTMLTable("http://en.wikipedia.org/wiki/World_population")
names(wptable)
histpred.df <- wptable[[9]]
histpred <- histpred.df[,2:5]
rownames(histpred) <- histpred.df[,1]
histpred                              ## problem: "," for values larger than 999
histpred.num <- apply(histpred, 2, function(col) {as.integer(gsub(",","",col))})
histpred.num #Differences that are not give are coerced to NAs
rownames(histpred.num) <- rownames(histpred)
histpred.num

years <- as.integer(rownames(histpred.num))
matplot(years, histpred.num[,c(1, 3)], type = "l", xlab = "Year", ylab = "Population", main = "World Population", lty = 1)
legend("topleft", legend = colnames(histpred.num)[c(1, 3)], cex = 0.7, col = 1:length(rownames(histpred.num)), lty = 1)


## ---------------------- Example 5: Simple Wikipedia Text Extraction (Rambo Movies) ---------------------

library(tm)
library(RCurl)
library(rjson)

pageSource1.R1 <- htmlParse("http://en.wikipedia.org/wiki/First_Blood")
text <- xpathSApply(pageSource1.R1,"//div//p",xmlValue)                  ## transform into character vector 
length(text)
text.R1 <- gsub("[[:space:]]+", " ", paste(text, collapse = "\n"))       ## transform to character vector of length 1

pageSource1.R2 <- htmlParse("http://en.wikipedia.org/wiki/Rambo:_First_Blood_Part_II")
text <- xpathSApply(pageSource1.R2,"//div//p",xmlValue)                  ## transform into character vector 
text.R2 <- gsub("[[:space:]]+", " ", paste(text, collapse = "\n"))       ## transform to character vector of length 1

pageSource1.R3 <- htmlParse("http://en.wikipedia.org/wiki/Rambo_III")
text <- xpathSApply(pageSource1.R3,"//div//p",xmlValue)                  ## transform into character vector 
text.R3 <- gsub("[[:space:]]+", " ", paste(text, collapse = "\n"))       ## transform to character vector of length 1

pageSource1.R4 <- htmlParse("http://en.wikipedia.org/wiki/Rambo_(film)")
text <- xpathSApply(pageSource1.R4,"//div//p",xmlValue)                  ## transform into character vector 
text.R4 <- gsub("[[:space:]]+", " ", paste(text, collapse = "\n"))       ## transform to character vector of length 1


rambo.corp <- Corpus(VectorSource(c(text.R1, text.R2, text.R3, text.R4)))
inspect(rambo.corp)

mystopwords <- c("disc","bluray","dvd","film","box","films","series","movie","version","released")
rambo.corp2 <- tm_map(rambo.corp, removeWords, mystopwords)
rambo.dtm <- DocumentTermMatrix(rambo.corp2, control = list("tolower", removePunctuation = TRUE, removeNumbers = TRUE, stripWhitespace = TRUE, stopwords = TRUE))
inspect(rambo.dtm)

rambo.dtm.full <- as.matrix(rambo.dtm)
ind <- which(colSums(rambo.dtm.full) > 10)
rambo.mat <- rambo.dtm.full[,ind]
rambo.mat
rownames(rambo.mat) <- c("Rambo I", "Rambo II", "Rambo III", "Rambo IV")

#Correspondence Analysis:
library("ca")
ca.rambo <- ca(rambo.mat)
plot(ca.rambo)