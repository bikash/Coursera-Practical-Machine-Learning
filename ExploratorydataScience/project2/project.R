
setwd("/Users/bikash/repos/Coursera-Practical-Machine-Learning/ExploratorydataScience/project2")
# Set seed
set.seed(123)


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
#fips      SCC Pollutant Emissions  type year
str(SCC) #11717 obs. of  15 variables:


#10-year period 1999–2008
data <- subset(NEI, subset=(year >= "1999" & year <= "2008"))
str(data) #6497651 obs. of  6 variables:

########################################################################################################################
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system,
#make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
########################################################################################################################
aggregatedTotalByYear <- aggregate(Emissions ~ year, NEI, sum)

barplot(height=aggregatedTotalByYear$Emissions, names.arg=aggregatedTotalByYear$year, xlab="years", 
        ylab=expression('Total PM2.5 emission'),main=expression('Total PM2.5 emissions at various years'))

########################################################################################################################
#Have total emissions from PM2.5 decreased in the 
#Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
########################################################################################################################
data <- subset(NEI, subset=(fips == "24510"))
str(data) #2096 obs. of  6 variables:
agg.TotalByYear <- aggregate(Emissions ~ year, data, sum)
barplot(height=agg.TotalByYear$Emissions, names.arg=agg.TotalByYear$year, xlab="years", 
        ylab=expression('Total PM2.5 emission'),main=expression('Total PM2.5 emissions in Baltimore City, Maryland'))

########################################################################################################################
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
########################################################################################################################
library(ggplot2)
data <- subset(NEI, subset=(fips == "24510"))
agg.TotalByYear <- aggregate(Emissions ~ year + type, data, sum)

ggplot(data=agg.TotalByYear, aes(x=year, y=Emissions, group = type, colour = Type)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white") +
  xlab("year") +
  ylab(expression("Total PM2.5 Emissions")) +
  ggtitle('Total Emissions in Baltimore City, Maryland')


########################################################################################################################
#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
########################################################################################################################
## merge
NEI.SCC <- merge(NEI, SCC, by="SCC")
# fetch all NEIxSCC records with Short.Name (SCC) Coal
coalMatches  <- grepl("coal", NEISCC$Short.Name, ignore.case=TRUE)
subset.NEISCC <- NEI.SCC[coalMatches, ]

## emission by aggregate
agg.TotalByYear <- aggregate(Emissions ~ year, subset.NEISCC, sum)

ggplot(data=agg.TotalByYear, aes(x=factor(year), y=Emissions, fill=year)) + 
  geom_bar(colour="black", fill="#DD8888", stat="identity") + 
  guides(fill=FALSE) +
  xlab("Year") + ylab("Total PM2.5 Emissions") +
  ggtitle("Total Emissions from coal")


########################################################################################################################
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
########################################################################################################################


subset.NEI <- NEI[NEI$fips=="24510" & NEI$type=="ON-ROAD",  ]
## emission by aggregate
agg.TotalByYear <- aggregate(Emissions ~ year, subset.NEI, sum)

ggplot(data=agg.TotalByYear, aes(x=factor(year), y=Emissions, fill=year)) + 
  geom_bar(colour="black", fill="#DD8888", stat="identity") + 
  guides(fill=FALSE) +
  xlab("Year") + ylab("Total PM2.5 Emissions") +
  ggtitle("Total Emissions from  motor vehicle sources in Baltimore City")


########################################################################################################################
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in 
#Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
########################################################################################################################
subset.NEI.los <- NEI[NEI$fips=="06037" & NEI$type=="ON-ROAD",  ]
subset.NEI.Baltimore <- NEI[NEI$fips=="24510" & NEI$type=="ON-ROAD",  ]

## emission by aggregate
agg.TotalByYear.los <- aggregate(Emissions ~ year, subset.NEI.los, sum)
agg.TotalByYear.Baltimore <- aggregate(Emissions ~ year, subset.NEI.Baltimore, sum)

agg.TotalByYear.los$city<-"Los Angeles, CA"
agg.TotalByYear.Baltimore$city<-"Baltimore, MD"
# Combine the two subsets into one data frame
data.comb <- rbind(agg.TotalByYear.los, agg.TotalByYear.Baltimore) 

ggplot(data=data.comb, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("year") +
  ylab(expression("Total PM2.5 Emissions")) +
  ggtitle('Compare Emissions in two Cities')




