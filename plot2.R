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
        ylab="Average PM2.5 Emissions in Baltimore", cex.lab=0.85, 
        names.arg=c("1999", "2002", "2005", "2008"), 
        col=c("darkred","red","red","orange"))
dev.off()
detach(baltimoretotal)