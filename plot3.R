#Function to plot line graph of datetime vs 3 different kinds of Energy Sub_metering in the household power consumption data

readdata <- function()
{
  #Download file if file isn't downloaded to working director yet, this includes unzipping file 
  if(!file.exists("./household_power_consumption.txt"))
  {
    temp <- tempfile()
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp, mode="wb")
    unzip(temp, "household_power_consumption.txt")
    unlink(temp)
  }
  
  
  powerConsumptionData <- read.table("./household_power_consumption.txt", sep=";",na.strings = "?", header=T) #Read data to table where you
  powerConsumptionData$DateTime = strptime(paste(powerConsumptionData$Date,powerConsumptionData$Time,sep = " "), "%d/%m/%Y %H:%M:%S") #Create column made up of date and time
  
  #Subset your data leaving only the columns you require(date and time replaced by datetime) and datetime range : "2007-02-01" <= x < "2007-02-03"
  finalPowerConsumptionData <- powerConsumptionData[powerConsumptionData$DateTime >= "2007-02-01" & powerConsumptionData$DateTime<"2007-02-03",c("DateTime","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")]
  
  #Return required data for plotting
  return(finalPowerConsumptionData)
}

plot3 <- function()
{
  #read data using the function above
  powerConsumption <- readdata()
  
  par(mfrow = c(1, 1))# retain mfrow to 1:1 ratio for one graph in frame if this was distorted previously to get required image
  
  #Graph first view of the final graph
  plot(data$DateTime, data$Sub_metering_1, type = "l",col="black",xlab = "",ylab = "Energy sub metering")
  lines(data$DateTime, data$Sub_metering_2, col="red") #Append graph with 2nd Submetering readings giving these a red colour
  lines(data$DateTime, data$Sub_metering_3, col="blue") #Append graph with 3nd Submetering readings giving these a blue colour
  
  #Create legend on the top right and other argument are for positioning and line representations of the data colour coding
  legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black","red","blue"), lty = c(1, 1, 1), cex = 1.5)
  
  #copy screen device output to png with 480*480pixels
  dev.copy(png,filename="plot3.png", width = 480, height = 480);
  dev.off ();
}