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
        ylab="Average Coal Emissions", cex.lab=0.85, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","red"))
dev.off()
detach(NEIcoal)
