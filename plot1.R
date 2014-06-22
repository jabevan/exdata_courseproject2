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
        ylab="Average PM2.5 Emissions", cex.lab=0.85, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","orange"))
dev.off()
detach(emissionstotal)