## 3. Of the four types of sources indicated by the type (point, nonpoint, 
## onroad, nonroad) variable, which of these four sources have seen 
## decreases in emissions from 1999–2008 for Baltimore City? Which have seen
## increases in emissions from 1999–2008? Use the ggplot2 plotting system to
## make a plot answer this question.

NEI <- readRDS("summarySCC_PM25.rds")
baltimoretotal <- NEI[ which(NEI$fips=="24510"), ]

baltimorepoint <- baltimoretotal[ which(baltimoretotal$type=="POINT"), ]
baltimorepoint <- data.frame(baltimorepoint$year, baltimorepoint$Emissions)
attach(baltimorepoint)
baltimorepoint <-aggregate(baltimorepoint.Emissions ~ baltimorepoint.year, FUN=mean)
detach(baltimorepoint)


baltimorenonpoint <- baltimoretotal[ which(baltimoretotal$type=="NONPOINT"), ]
baltimorenonpoint <- data.frame(baltimorenonpoint$year, baltimorenonpoint$Emissions)
attach(baltimorenonpoint)
baltimorenonpoint <-aggregate(baltimorenonpoint.Emissions ~ baltimorenonpoint.year, FUN=mean)
detach(baltimorenonpoint)

baltimoreonroad <- baltimoretotal[ which(baltimoretotal$type=="ON-ROAD"), ]
baltimoreonroad <- data.frame(baltimoreonroad$year, baltimoreonroad$Emissions)
attach(baltimoreonroad)
baltimoreonroad <-aggregate(baltimoreonroad.Emissions ~ baltimoreonroad.year, FUN=mean)
detach(baltimoreonroad)

baltimorenonroad <- baltimoretotal[ which(baltimoretotal$type=="NON-ROAD"), ]
baltimorenonroad <- data.frame(baltimorenonroad$year, baltimorenonroad$Emissions)
attach(baltimorenonroad)
baltimorenonroad <-aggregate(baltimorenonroad.Emissions ~ baltimorenonroad.year, FUN=mean)
detach(baltimorenonroad)

pkgLoad <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE, repos='http://star-www.st-andrews.ac.uk/cran/')
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  suppressPackageStartupMessages(library(x,character.only=TRUE))
}

pkgLoad("ggplot2")

library(ggplot2)

p1 <- ggplot(data=baltimorepoint, aes(x=baltimorepoint.year, y=baltimorepoint.Emissions)) + geom_bar(fill="red", stat="identity") + xlab("") + ylab("Point") + ggtitle("Baltimore Point Emssions") + scale_x_continuous(breaks=c(1999,2002,2005,2008))

p2 <- ggplot(data=baltimorenonpoint, aes(x=baltimorenonpoint.year, y=baltimorenonpoint.Emissions)) + geom_bar(fill="red", stat="identity") + xlab("") + ylab("Non-Point") + ggtitle("Baltimore Non-Point Emssions") + scale_x_continuous(breaks=c(1999,2002,2005,2008))

p3 <- ggplot(data=baltimoreonroad, aes(x=baltimoreonroad.year, y=baltimoreonroad.Emissions)) + geom_bar(fill="red", stat="identity") + xlab("") + ylab("On-Road") + ggtitle("Baltimore On-Road Emssions") + scale_x_continuous(breaks=c(1999,2002,2005,2008))

p4 <- ggplot(data=baltimorenonroad, aes(x=baltimorenonroad.year, y=baltimorenonroad.Emissions)) + geom_bar(fill="red", stat="identity") + xlab("") + ylab("Non-Road") + ggtitle("Baltimore Non-Road Emssions") + scale_x_continuous(breaks=c(1999,2002,2005,2008))


png(file="plot3.png", width=640, height=640)
pkgLoad("gridExtra")
library(gridExtra) 
grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()