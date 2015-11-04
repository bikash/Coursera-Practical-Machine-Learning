
## point to current project folder
setwd("/Users/bikash/repos/Coursera-Practical-Machine-Learning/ExploratorydataScience/project1")

## data load
########################################################################################################################
##q1: The dataset has 2,075,259 rows and 9 columns. First calculate a rough estimate of 
#how much memory the dataset will require in memory before reading into R. Make sure your computer has enough memory (most modern computers should be fine).
########################################################################################################################
data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", stringsAsFactors=FALSE, na="?")

########################################################################################################################
#We will only be using data from the dates 2007-02-01 and 2007-02-02. 
#One alternative is to read the data from just those dates rather than reading in the entire dataset and subsetting to those dates.
########################################################################################################################
data$Date <- as.Date(data$Date, format="%d/%m/%Y")

## subset of the data from dates 2007-02-01 and 2007-02-02.
data_sub <- subset(data, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
str(data_sub) ##2880 obs. of  9 variables:

## convert date into datetime format
data_sub$Datetime <- as.POSIXct(paste(as.Date(data_sub$Date), data_sub$Time))

##plot 3
png("plot3.png", width = 480, height = 480)
plot(data_sub$Sub_metering_1~data_sub$Datetime, type="l", main="", ylab="Energy sub metering", xlab="")
lines(data_sub$Sub_metering_2~data_sub$Datetime, type="l", col="red")
lines(data_sub$Sub_metering_3~data_sub$Datetime, type="l", col="blue")
legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()