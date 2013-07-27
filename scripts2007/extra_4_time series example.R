library(foreign)

#get data from all years for time-series plots
#need average district vote and dem per seats

#get 2006 Data
#use 2004 vars from 2006 data to account for GA switch of 3rd and 8th districts
    #for s-v analyis
house.2006 <- read.dta("2006_house_data.dta")
attach.all(house.2006)
i2006 <- house.2006[,"incumb06"]#incumbency in 2006
unc2006 <- house.2006[,"uncontested06"]#uncontested in 2006
dvote.2006 <- house.2006$dvoteimputed#imputed vote in 2006
adv.2006 <- mean(dvote.2006) #mean avg. district vote (imputed)
winner.2006 <- house.2006$winner#winner of each race (1 for Dems, 0 for GOP
dem.seats.2006 <- mean(winner.2006) #percent of Dem seats (233/435); counting FL-13 for GOP
v2004 <- dlag06imputed #use imputed lagged vote to get 2004 vote for 2006 s-v prediction
i2004 <- incumb04 #incumbency in 04 for s-v prediction


#first get dem seats for 1946--2006; unit of observation here is the election year (i.e. "aggregate")
agg.house.data <- read.dta("House_1946-2006_aggregate.dta")
attach.all(agg.house.data)
unique.year.all <- unique(year)#create variable for each uniqye year
dem.seats <- dem.seats.per#Dem percentage of seats in House stemming from each election
#get total vote for 1946-2004
dem.per.total <- dem.per.total#dems percentage of total vote in each election
#get total vote for 2006
attach.all(house.2006)
dem.total.2006 <- sum(dem.total)/(sum(dem.total)+sum(gop.total))#total vote in 2006
#add on 2006 to total
dem.per.total[31] <- dem.total.2006#add 2006 to total vote vector (since it's not part of main dataset yet

#get average district vote for all years
###########################################
detach()
house.4604 <- read.dta("House_1946_2004_updated.dta") #1946-2004 data -- statecd is unit
attach.all(house.4604)
house.4604 <- house.4604[winner < 9,]#drop handful of 3rd party candidates
                        #not-including Bernie Sanders, who we count as a Democrat
detach()
attach.all(house.4604)
unique.year.4604 <- unique(year)#create year var for 1946-2004 (helps avoid confusion when looping)
adv <- rep(NA, length(unique.year.4604))#create empty vector for imputed avg. district vote, 1946-2004
for (i in 1:length(unique.year.4604)){
    adv[i] <- mean(dvoteimputed[year==unique.year.4604[i]])#get adv for each year, 1946-2004
}
adv[31] <- adv.2006#add in 2006 (since it's not part of main dataset yet)

########################################################################

#USE THIS CODE TO CREATE PDF

#Plot avg. dist. vote versus percent of seats time series for 1946-2000
year.at.vec <- c((seq(1946, 2006, by = 2)))#use to plot tick marks on x-axis
year.label.vec <- c("", "", "1950", "","","", "", "1960", "","","", "", "1970",
    "","","", "", "1980", "","","", "", "1990", "","","", "", "2000","", "", "2006" )#x-axis labels
#pdf("time_series_house.pdf", height = 9, width = 21)
#png("time_series_house.png", height = 500, width = 1500)
par(mfrow=c(1,1), mar = c(7,7.1,2,.5))#set up margins for plot
plot(unique.year.all, adv, axes = F, type = "n", xlab = "", 
    ylab = "", xlim = c(1946, 2010), ylim = c(.43,.68), xaxs="i", yaxs="i",
    main = "")#call empty plot so can add shading first, or else shading would block out lines
#add shading for different periods of party control GOP: 1946-1948, 1952-54, 1994-2006
shade.color = "gray92"
polygon(x=c(1946, 1946, 1948, 1948),
    y=par()$usr[c(3,4,4,3)],
    col= shade.color,   ## desired color
    border=F)         ## no border
polygon(x=c(1952, 1952, 1954, 1954),
    y=par()$usr[c(3,4,4,3)],
    col= shade.color,   ## desired color
    border=F)         ## no border
polygon(x=c(1994, 1994, 2006, 2006),
    y=par()$usr[c(3,4,4,3)],
    col= shade.color,   ## desired color
    border=F)         ## no border
points(unique.year.all, adv, type = "l")#points/line for avg. district vote
points(unique.year.all, dem.seats, type = "l", lty = 2)##points/line for dem percent of seats
axis(1,  at = year.at.vec, labels = year.label.vec, las = 1, cex.axis = 1.8, mgp = c(2,1.5,0))
axis(2, las = 1, at = c(seq(.4, .70, by = .05)), 
    label = c("40", "45%", "50%", "55%", "60%", "65%", "70%"), cex.axis = 1.8,mgp = c(2,1.2,0))
segments(1946, .5, 2006, .5,  col="gray", lwd=.5)#light line for 50%
text(1975, .52, "Average district vote\nfor Democrats", cex = 1.7)#labels for each line
segments(1973, .534, 1972.6, .547)
text(1982, .66, "Democrats' percentage\n of House seats", cex = 1.7)#labels for each line
segments(1978, .66, 1977.6, .65)
#dev.off()


#USE THIS CODE TO CREATE PNG

#Plot avg. dist. vote versus percent of seats time series for 1946-2000
year.at.vec <- c((seq(1946, 2006, by = 2)))#use to plot tick marks on x-axis
year.label.vec <- c("", "", "1950", "","","", "", "1960", "","","", "", "1970",
    "","","", "", "1980", "","","", "", "1990", "","","", "", "2000","", "", "2006" )#x-axis labels
png("time_series_house.png", height = 300, width = 800)
par(mfrow=c(1,1), mar = c(3,3,1,0))#set up margins for plot
plot(unique.year.all, adv, axes = F, type = "n", xlab = "", 
    ylab = "", xlim = c(1946, 2010), ylim = c(.43,.68), xaxs="i", yaxs="i",
    main = "")#call empty plot so can add shading first, or else shading would block out lines
#add shading for different periods of party control GOP: 1946-1948, 1952-54, 1994-2006
shade.color = "gray92"
polygon(x=c(1946, 1946, 1948, 1948),
    y=par()$usr[c(3,4,4,3)],
    col= shade.color,   ## desired color
    border=F)         ## no border
polygon(x=c(1952, 1952, 1954, 1954),
    y=par()$usr[c(3,4,4,3)],
    col= shade.color,   ## desired color
    border=F)         ## no border
polygon(x=c(1994, 1994, 2006, 2006),
    y=par()$usr[c(3,4,4,3)],
    col= shade.color,   ## desired color
    border=F)         ## no border
points(unique.year.all, adv, type = "l")#points/line for avg. district vote
points(unique.year.all, dem.seats, type = "l", lty = 2)##points/line for dem percent of seats
axis(1,  at = year.at.vec, labels = year.label.vec, las = 1, cex.axis = .7, mgp = c(2,.3,0))
axis(2, las = 1, at = c(seq(.4, .70, by = .05)), 
    label = c("40", "45%", "50%", "55%", "60%", "65%", "70%"), cex.axis =.7,mgp = c(2,.5,0))
segments(1946, .5, 2006, .5,  col="gray", lwd=.5)#light line for 50%
text(1975, .517, "Average district vote\nfor Democrats", cex = .8)#labels for each line
segments(1973, .534, 1972.6, .547)
text(1984, .66, "Democrats' percentage\n of House seats", cex = .8)#labels for each line
segments(1978, .66, 1977.6, .65)
dev.off()
