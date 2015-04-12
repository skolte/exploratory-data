# Author - Sandeep Kolte
# Date - 04/11/2015
# Plot 1
#----------------------------------------------
# This function uses data from the UC Irvine Machine Learning Repository. (http://archive.ics.uci.edu/ml/)
# This dataset contains measurements of electric power consumption in one household with a 
# one-minute sampling rate over a period of almost 4 years. 
# This function examines how household energy usage varies over a 2-day period in February, 2007 and
# creates a simple histogram that shows frequency of global active power.
Plot1 <- function()
{
        library(data.table)
        
        fn <- 'household_power_consumption.txt'
        DF.row1 <- read.table(fn, header = TRUE, sep = ";", nrow = 1, fill = TRUE)

        # The slicing and analyzing the entire dataset, using a few R statements with 'dplyr', revealed that the data specific to two dates Feburay 1, 2007 (starting at midnight) and February 2, 2007
        # (ending at midnight) is available in just about a few thousand rows (2880) compared to 2.07 million rows of data. 
        # In order to speed up the data retrieval and processing, the following read table call skips the first 66k rows that correspond to dates prior to 02/01/2007 00:00:00, hence
        # those can be ignored. Similarly, records corresponding to date and time after 02/02/2007 23:59:59 can be ignored.
        myData <- read.table(fn, col.names = names(DF.row1), header = TRUE, sep = ";", skip=66636, nrow=2880, fill = TRUE)
        
        # Convert the data frame to data table.
        myTable <- data.table(myData)
        
        # Write PNG.
        png(file="Plot1.png",width=480, height=480)
        
        # Create a histogram matching Title, color and x axis label set as seen in the plot shown on the assignment page.
        hist(myTable$Global_active_power, main = "Global Active Power", col = "red", xlab = "Global Active Power (kilowatts)")
        
        # Set the margins.
        par(mar=c(4,4,2,2))
        dev.off()
}