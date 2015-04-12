# Author - Sandeep Kolte
# Date - 04/11/2015
# Plot 3
#----------------------------------------------
# This function uses data from the UC Irvine Machine Learning Repository. (http://archive.ics.uci.edu/ml/)
# This dataset contains measurements of electric power consumption in one household with a 
# one-minute sampling rate over a period of almost 4 years. 
# This function examines how household energy usage varies over a 2-day period in February, 2007 and
# creates a line plot with the metering observations such that the three metering observations are 
# plotted in different colors on the same plot. 
Plot3 <- function()
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
        
        # Write to PNG since the file may not show the legend correctly.
        png(file="Plot3.png",width=480, height=480)
        
        # Create the plot with type 'n' i.e. create a plot with all properties set but without data.
        plot(myTable$Date, myTable$Sub_metering_3, type = "n", ylab = "Energy sub metering", xlab = "", ylim=c(0,max(myTable$Sub_metering_1)))
        
        # Set margins
        par(mar=c(5,4,3,2))
        
        # Plot the points for the three sub metering types using a line plot, specifying colors and max limits on Y axis.
        points(myTable$Date, myTable$Sub_metering_1, type = "l", col = "black", ylim=c(0,max(myTable$Sub_metering_1)))
        points(myTable$Date, myTable$Sub_metering_2, type = "l", col = "red", ylim=c(0,max(myTable$Sub_metering_1)))
        points(myTable$Date, myTable$Sub_metering_3, type = "l", col = "blue", ylim=c(0,max(myTable$Sub_metering_1)))
        
        # Specify legend on the plot.
        legend("topright", cex=1.3, pt.cex = 0.7, lty=1, col=c("black", "red", "blue"), legend=(c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")))
        dev.off()

}