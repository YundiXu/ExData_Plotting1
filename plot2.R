plot2 <- function() {
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
  data[,2] <- as.numeric(data[,2])
  png("plot2.png",width = 480,height = 480)
  with(data,plot(Date_Time,Global_active_power,type = "l", 
                 xlab = "", ylab = "Global Active Power (kilowatts)"))
  dev.off()
  
}