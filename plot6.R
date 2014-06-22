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
        ylab="Average Vehicle Emissions in Baltimore", cex.lab=0.85, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","darkorange"))

barplot(LAvehagg$Emissions, 
        main="Los Angeles Vehicle Emissions by Year", 
        ylab="Average Vehicle Emissions in LA", cex.lab=0.85, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","darkred"))
dev.off()