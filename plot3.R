plot3 <- function() {
  #read and convert data
  data_raw <- read.table("household_power_consumption.txt",
                         header =T,sep = ";",colClasses = "character",
                         skip = 66636,nrows = 2880,
                         col.names = c("Date","Time","Global_active_power",
                                       "Global_reactive_power","Voltage",
                                       "Global_intensity","Sub_metering_1",
                                       "Sub_metering_2","Sub_metering_3"))
  
  data <- data.frame(Date_Time = strptime(paste(data_raw[,1],data_raw[,2]),
                                          "%d/%m/%Y %H:%M:%S"))
  data[,2:8] <- data_raw[,3:9]
  
  #process plotting
  data[,6] <- as.numeric(data[,6])
  data[,7] <- as.numeric(data[,7])
  data[,8] <- as.numeric(data[,8])
  png("plot3.png",width = 480,height = 480)
  ran <- range(data[,6:8]) #get the y label range
  with(data,{
    plot(Date_Time,Sub_metering_1,type = "l", 
         xlab = "", ylab = "", ylim = ran, col = "black")
    par(new=T)
    plot(Date_Time,Sub_metering_2,type = "l", axes = FALSE,
         xlab = "", ylab = "", ylim = ran, col = "red")
    par(new=T)
    plot(Date_Time,Sub_metering_3,type = "l", axes = FALSE,
         xlab = "", ylab = "", ylim = ran, col = "blue")
  })
  title(ylab = "Energy sub metering")
  legend("topright",col = c("black","red","blue"),
         legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty = 1)
  
  dev.off()
  
}