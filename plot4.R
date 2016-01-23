library(lubridate)
library(sqldf)
library(data.table)

#download data, if needed, and plot the returned data frame
x <- loaddata()
plot4(x)


loaddata <- function()
{
    
    ##  Assumes we will just be using the working directory
    ##
    #Download the file/package if it doesn't exist
    zipFileName <- "power.zip"
    fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

    if (!file.exists(zipFileName))
    {
        #download the file
        print(paste("Downloading File: ", zipFileName, " from ", fileUrl))
        download.file(fileUrl, zipFileName)
        print(paste( "Unzipping File from Archive: ", zipFileName))
        unzip(zipFileName)
    }
    
    
    filename <- "household_power_consumption.txt"

    #fread should be faster that the read.csv method
    alldata <- data.table(fread(file.path(getwd(), filename , fsep = .Platform$file.sep), sep = ";", na.strings = "?"))
    
    
    #Dates in question are D/M/YYYY -- no zero-padding for feb 1 and feb 2 of 2007
    #Use SQLDF to make it easier to filter
    
    mydata <- sqldf( "select * from alldata where Date IN ('1/2/2007','2/2/2007') ")
    mydata$Date <- dmy(mydata$Date)
    mydata$Time <- strptime( paste(mydata$Date, mydata$Time), "%Y-%m-%d %H:%M:%S")
    #return the filtered Data
    mydata
    
}

plot4 <- function(x)
{
    #organize a 2 x 2 layout
    par(mfrow=c(2,2), mar = c(4,4,2,1), oma=c(0,0,2,0))
    png(filename="plot4.png", width=480, height=480) 
    
    #Plot 1
    hist( x$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
    
    #Plot 2
    plot(x$Time, x$Global_active_power , type = "l", ylab="Global Active Power (kilowatts)", xlab = "")
    
    #Plot 3
    plot(x$Time, x$Sub_metering_1, type = "l", xlab="", ylab="Energy Sub Metering", col = "black")
    lines(x$Time, x$Sub_metering_2, col = "red")
    lines(x$Time, x$Sub_metering_3, col = "blue")
    legend("topright", col = c("black", "red", "blue") , legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd=par("lwd") )
    
    #Plot 4 - global reactive power over time
    plot(x$Time, x$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
    dev.off()
    print("Saved File: plot4.png to local working Directory.")
}



