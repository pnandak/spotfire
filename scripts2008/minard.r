# R Program to recreate Minard's plot of Napoleon's march.
# Original program by Hadley Wickham.
# Tweaked by Bob Muenchen, adding labels and placing both on one plot.

setwd("L:/R for SAS & SPSS - Files/Minard")
library(ggplot2)

troops <- read.table("troops.txt", header=T)
head(troops)

cities <- read.table("cities.txt", header=T)
head(cities)

temps <- read.table("temps.txt", header=T)
head(temps)

temps$date <- as.Date(strptime(temps$Date,"%d%b%Y"))
head(temps$date)

library(maps)
borders <- data.frame(map("world", xlim=c(10,50), ylim=c(40, 80), plot=F)[c("x","y")])
xlim <- scale_x_continuous(limits = c(24, 39))

# Create and save plot of march locations and survivors.
cityPlot <- ggplot(cities, aes(x = Longitude, y = Latitude)) + 
  geom_path(  aes(size = Survivors, colour = Direction, group = Group), data=troops) + 
  geom_point() + 
  geom_text(aes(label = City), hjust=0, vjust=1, size=4) +
  scale_size(to = c(1, 10)) + 
  scale_colour_manual(values = c("grey75","grey40")) + xlim
print(cityPlot)
ggsave(file = "March.pdf", width=16, height=4)

# Create and save plot of temperature by longitude.
tempPlot <- qplot(Longitude, Temperature, data=temps, geom="line") + 
  geom_text(aes(label = paste(Day, Month)), vjust=1) + xlim
print(tempPlot)
ggsave(file = "Temperatures.pdf", width=16, height=4)

# Printing both to one plot to match Minard's original.
# First to windows device.
windows(width=16, height=7)
grid.newpage()
pushViewport( viewport(layout=grid.layout(4,100) ) )
print(cityPlot, vp=viewport(
   layout.pos.row=1:3, 
   layout.pos.col=1:100) )
print(tempPlot, vp=viewport(
   layout.pos.row=4, 
   layout.pos.col=1:93) )

# And again to PDF file. Note that ggsave function cannot
# save a multiframe grid, so we need to use the pdf function.
pdf(file="both.pdf", width=16, height=7)
grid.newpage()
pushViewport( viewport(layout=grid.layout(4,100) ) )
print(cityPlot, vp=viewport(
   layout.pos.row=1:3, 
   layout.pos.col=1:100) )
print(tempPlot, vp=viewport(
   layout.pos.row=4, 
   layout.pos.col=1:94) )
dev.off()


