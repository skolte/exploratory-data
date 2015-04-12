# Author - Sandeep Kolte
# Date - 04/11/2015
# Plot 4
#----------------------------------------------
# This function uses data from the UC Irvine Machine Learning Repository. (http://archive.ics.uci.edu/ml/)
# This dataset contains measurements of electric power consumption in one household with a 
# one-minute sampling rate over a period of almost 4 years. 
# This function examines how household energy usage varies over a 2-day period in February, 2007 and
# creates 4 plots in the same figure. 
# First line plot with the metering observations such that the three metering observations are 
Plot4 <- function()
{
        library(data.table)
        library(lubridate)
        fn <- 'household_power_consumption.txt'
        DF.row1 <- read.table(fn, header = TRUE, sep = ";", nrow = 1, fill = TRUE)
        
        # The slicing and analyzing the entire dataset, using a few R statements with 'dplyr', revealed that the data specific to two dates Feburay 1, 2007 (starting at midnight) and February 2, 2007
        # (ending at midnight) is available in just about a few thousand rows (2880) compared to 2.07 million rows of data. 
        # In order to speed up the data retrieval and processing, the following read table call skips the first 66k rows that correspond to dates prior to 02/01/2007 00:00:00, hence
        # those can be ignored. Similarly, records corresponding to date and time after 02/02/2007 23:59:59 can be ignored.
        myData <- read.table(fn, col.names = names(DF.row1), header = TRUE, sep = ";", skip=66636, nrow=2880, fill = TRUE)
        
        # Convert data frame to data table.
        myTable <- data.table(myData)
        
        # Use lubridate to combine date and time columns so that it becomes easier to plot the date and time.
        myTable$Date <- dmy_hms(paste(myTable$Date,myTable$Time))
        
        # Write PNG.
        png(file="Plot4.png",width=480, height=480)
        
        # Set canvas using 'mfrow' such that 4 plots can fit on it.
        par(mfrow=c(2,2))
        
        # Plot 1 - Global Active Power over two days.
        plot(myTable$Date, myTable$Global_active_power, type = "l", ylab = "Global Active Power", xlab = "")
        
        # Plot2 - Voltage over two days.
        plot(myTable$Date, myTable$Voltage, type = "l", ylab = "Voltage", xlab = "datetime", ylim= c(min(myTable$Voltage),max(myTable$Voltage)))
        
        # Plot3 - Energy sub metering over two days with each sub metering colored with a different color, hiding the box around the legend.
        # This line just gets the plot ready without the data.
        plot(myTable$Date, myTable$Sub_metering_3, type = "n", ylab = "Energy sub metering", xlab = "", ylim=c(0,max(myTable$Sub_metering_1)))
        # Plotting the points for each individual sub metering.
        points(myTable$Date, myTable$Sub_metering_1, type = "l", col = "black", ylim=c(0,max(myTable$Sub_metering_1)))
        points(myTable$Date, myTable$Sub_metering_2, type = "l", col = "red", ylim=c(0,max(myTable$Sub_metering_1)))
        points(myTable$Date, myTable$Sub_metering_3, type = "l", col = "blue", ylim=c(0,max(myTable$Sub_metering_1)))
        legend("topright", lty=1, cex=0.75, bty='n', col=c("black", "red", "blue"), legend=(c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")))

        # Plot4 - Global reactive power over two days.
        plot(myTable$Date, myTable$Global_reactive_power, type = "l", ylab = "Global_reactive_power", xlab = "datetime", ylim= c(0.0,0.5), yaxt="n")
        # Adjust the Y axis labels on Plot 4.
        axis(2, cex.axis=0.8)
        
        dev.off()
}