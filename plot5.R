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
        ylab="Average Vehicle Emissions in Baltimore", cex.lab=0.85, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","darkorange"))
dev.off()
detach(baltimorevehicle)