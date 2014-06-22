NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## For each of the following save a separate R file and plot graphic (PNG).
## For example, plot1.R and plot1.png.

## 1. Have total emissions from PM2.5 decreased in the United States 
## from 1999 to 2008? Using the base plotting system, make a plot showing 
## the total PM2.5 emission from all sources for each of the years 1999, 
## 2002, 2005, and 2008.

NEI <- readRDS("summarySCC_PM25.rds")
emissionstotal <- data.frame(NEI$year, NEI$Emissions)
attach(emissionstotal)
emissionsagg <-aggregate(NEI.Emissions ~ NEI.year, FUN=mean)
png(file="plot1.png", width=640, height=320)
barplot(emissionsagg$NEI.Emissions, 
        main="PM2.5 Emissions by Year", 
        ylab="Total PM2.5 Emissions", cex.lab=0.75, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","orange"))
dev.off()
detach(emissionstotal)

## 2. Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? Use the base plotting 
## system to make a plot answering this question.

NEI <- readRDS("summarySCC_PM25.rds")
baltimoretotal <- NEI[ which(NEI$fips=="24510"), ]
baltimoretotal <- data.frame(baltimoretotal$year, baltimoretotal$Emissions)
attach(baltimoretotal)
baltimoreagg <-aggregate(baltimoretotal.Emissions ~ baltimoretotal.year, FUN=mean)
png(file="plot2.png", width=640, height=320)
barplot(baltimoreagg$baltimoretotal.Emissions, 
        main="Baltimore PM2.5 Emissions by Year", 
        ylab="Total PM2.5 Emissions in Baltimore", cex.lab=0.7, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","orange"))
dev.off()
detach(baltimoretotal)

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

## 4. Across the United States, how have emissions from coal combustion-
## related sources changed from 1999–2008?

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCCcoal <- SCC[grep("Coal", SCC$EI.Sector), ]

NEIcoal <- NEI[NEI$SCC %in% SCCcoal$SCC, ]

attach(NEIcoal)
NEIcoalagg <-aggregate(Emissions ~ year, FUN=mean)
png(file="plot4.png", width=640, height=320)
barplot(NEIcoalagg$Emissions, 
        main="Coal Emissions by Year", 
        ylab="Total Coal Emissions", cex.lab=0.75, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","red"))
dev.off()
detach(NEIcoal)

## 5. How have emissions from motor vehicle sources changed from 1999–2008 
## in Baltimore City?

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCCvehicle <- SCC[grep("road", ignore.case=TRUE, SCC$Data.Category), ]

NEIvehicle <- NEI[NEI$SCC %in% SCCvehicle$SCC, ]

baltimorevehicle <- NEIvehicle[ which(NEIvehicle$fips=="24510"), ]
attach(baltimorevehicle)
baltimorevehagg <-aggregate(Emissions ~ year, FUN=mean)
png(file="plot5.png", width=640, height=320)
barplot(baltimorevehagg$Emissions, 
        main="Baltimore Motor Vehicle Emissions by Year", 
        ylab="Total Vehicle Emissions in Baltimore", cex.lab=0.7, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","darkorange"))
dev.off()
detach(baltimorevehicle)

## 6. Compare emissions from motor vehicle sources in Baltimore City with 
## emissions from motor vehicle sources in Los Angeles County, California 
## (fips == "06037"). Which city has seen greater changes over time in motor
## vehicle emissions?

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCCvehicle <- SCC[grep("road", ignore.case=TRUE, SCC$Data.Category), ]

NEIvehicle <- NEI[NEI$SCC %in% SCCvehicle$SCC, ]

baltimorevehicle <- NEIvehicle[ which(NEIvehicle$fips=="24510"), ]
attach(baltimorevehicle)
baltimorevehagg <-aggregate(Emissions ~ year, FUN=mean)
detach(baltimorevehicle)

LAvehicle <- NEIvehicle[ which(NEIvehicle$fips=="06037"), ]
attach(LAvehicle)
LAvehagg <-aggregate(Emissions ~ year, FUN=mean)
detach(LAvehicle)

png(file="plot6.png", width=640, height=320)
par(mfrow=c(1,2))
barplot(baltimorevehagg$Emissions, 
        main="Baltimore Vehicle Emissions by Year", 
        ylab="Total Vehicle Emissions in Baltimore", cex.lab=0.85, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","darkorange"))

barplot(LAvehagg$Emissions, 
        main="Los Angeles Vehicle Emissions by Year", 
        ylab="Total Vehicle Emissions in LA", cex.lab=0.85, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","darkred"))
dev.off()

