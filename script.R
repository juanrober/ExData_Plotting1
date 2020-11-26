mtext("Ozone and Weather in New York City", outer = TRUE)
plot(airquality$Temp, airquality$Ozone, main = "Ozone and Temperature")
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
plot(airquality$Wind, airquality$Ozone, type = "n")

setwd("C:/Users/52331/Documents/proyecto")


#Calculate a rough estimate of how much memory the dataset will require in memory before reading into R.

Size_mb  <- (9*2075259*8)/(10^6)

# Load the database.

base_1 <- read.table("household_power_consumption.txt",
                     sep = ";",
                     header = TRUE,
                     na.strings = "?")
head(base_1)

# Subset the records that are between 2007-2-1 and 2007-2-2

base_1$Date <- as.Date(base_1$Date, "%d/%m/%Y")

base_2 <- subset(base_1, Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

base_2 <- base_2[complete.cases(base_2), ]  

# The first plot

hist(base_2$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")  
  
dev.copy(png,"plot1.png", width=480, height=480)

dev.off()

# The second plot

base_2$dateTime <- paste(base_2$Date, base_2$Time)

base_2$dateTime <- as.POSIXct(base_2$dateTime)

plot(base_2$Global_active_power~base_2$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

dev.copy(png,"plot2.png", width=480, height=480)

dev.off()

# The third plot

with(base_2, {plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1),
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       cex=0.7, y.intersp = .25, x.intersp = .9) ##

dev.copy(png, file="plot3.png", height=480, width=480)

dev.off()

# The least plot

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))

with(base_2, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})

dev.copy(png, file="plot4.png", height=480, width=480)

dev.off()
