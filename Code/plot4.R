#The createTable function check if the dataset is loaded in memory if not load it.
createTable <- function(){
    if (exists('tbl_input') == FALSE) {
        
        if (!file.exists('household_power_consumption.txt')){
            stop('File household_power_consumption.txt is not in working directory')
        }
        
        #read the text file specifying separator, na strings and that file contains header
        tbl_input <- read.table('household_power_consumption.txt',sep=";",na.strings = '?',header = T)
        
        #Convert the Date and Time subset to character and concatenate the values in the Data
        tbl_input$Date <- as.character(tbl_input$Date)
        tbl_input$Time <- as.character(tbl_input$Time)
        tbl_input$Date <- paste(tbl_input$Date,tbl_input$Time)
        
        #Convert the Date character column to POSIX data
        tbl_input$Date <- strptime(tbl_input$Date,format='%d/%m/%Y %H:%M:%S')
        #tbl_input$Date <- as.Date(tbl_input$Date,format='%d/%m/%Y')
        #tbl_input$Time <- strptime(tbl_input$Time,'%H:%M:%S')
        
        # Delete the Time column
        tbl_input$Time <- NULL 
        
        #Subset the data set so it contains the value between 2007-02-01 and 2007-02-02
        tbl_input <- subset(tbl_input,Date >= '2007-02-01' & Date < '2007-02-03')
        #tbl_input <- tbl_input[tbl_input$Date >= '2007-02-01' & tbl_input$Date <= '2007-02-02',]
    }
    #return(tbl_input)
    tbl_input <<- tbl_input
}

#Plot the fourth diagram on the screen and write it in a PNG file
R4 <- function(){
    tbl_input <- createTable() #Call the createTable function which will return the dataframe
    
    #Create the histogram in the screen device
    plot.new() #reset the screen device
    par(mfrow=c(2,2))
    
    #Print R2 to the top left (First row, First col)
    plot(tbl_input$Date,tbl_input$Global_active_power,type='l',xlab='',ylab='Global Active Power (Kilowatts)')#plot the graph into the screen
    
    #Print new plot to the top right (First row, 2nd col)
    plot(tbl_input$Date,tbl_input$Voltage,type='l',xlab='datetime',ylab='Voltage')
    
    #Print R3 to the bottow left (2nd Row, 1st col)
    plot(tbl_input$Date,tbl_input$Sub_metering_1,type='l',xlab='',ylab='Energy sub metering')
    points(tbl_input$Date,tbl_input$Sub_metering_2,type='l',col='red',xlab='',ylab='Energy sub metering')
    points(tbl_input$Date,tbl_input$Sub_metering_3,type='l',col='blue',xlab='',ylab='Energy sub metering')
    legend('topright',
           legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),
           col=c('black','red','blue'),lty=1)
    
    #Print new plot to the bottom right (2nd row, 2nd col)
    plot(tbl_input$Date,tbl_input$Global_reactive_power,type='l',xlab='datetime',
         ylab='Global_reactive_power')#plot the graph into the screen   
    
    #Draw diagrams in the file
    png('../plots/plot4.png')
    plot.new() #reset the screen device
    par(mfrow=c(2,2))
    
    #Print R2 to the top left (First row, First col)
    plot(tbl_input$Date,tbl_input$Global_active_power,type='l',xlab='',ylab='Global Active Power (Kilowatts)')#plot the graph into the screen
    
    #Print new plot to the top right (First row, 2nd col)
    plot(tbl_input$Date,tbl_input$Voltage,type='l',xlab='datetime',ylab='Voltage')
    
    #Print R3 to the bottow left (2nd Row, 1st col)
    plot(tbl_input$Date,tbl_input$Sub_metering_1,type='l',xlab='',ylab='Energy sub metering')
    points(tbl_input$Date,tbl_input$Sub_metering_2,type='l',col='red',xlab='',ylab='Energy sub metering')
    points(tbl_input$Date,tbl_input$Sub_metering_3,type='l',col='blue',xlab='',ylab='Energy sub metering')
    legend('topright',
           legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),
           col=c('black','red','blue'),lty=1)
    
    #Print new plot to the bottom right (2nd row, 2nd col)
    plot(tbl_input$Date,tbl_input$Global_reactive_power,type='l',xlab='datetime',
         ylab='Global_reactive_power')#plot the graph into the screen   
    dev.off()
    return(0)
    
}