## HW 10

zip_file <- "individual+household+electric+power+consumption1.zip"

data_file <- unzip(zip_file, list = TRUE)$Name[1]


col_names <- names(read.table('household_power_consumption.txt',
                              sep = ';', header = TRUE, nrows = 1))
power <- read.table('household_power_consumption.txt',
                    sep = ';',
                    header = FALSE,
                    skip = 66637,
                    nrows = 2880,
                    col.names = col_names,
                    na.strings = '?',
                    stringsAsFactors = FALSE)

nrow(power)
length(power$DateTime)
length(power$Global_active_power)
class(power$DateTime)
names(power)

head(power)
tail(power)

power$DateTime <- as.POSIXct(strptime(paste(power$Date, power$Time),
                                      format = '%d/%m/%Y %H:%M:%S'))
# Histogram of Global Active Power

plot_a <- function() {
  hist(power$Global_active_power,
       breaks = 12, col = 'red',
       main = 'Global Active Power',
       xlab = 'Global Active Power (kilowatts)',
       ylab = 'Frequency')
}

# Global Active Power Over Time

plot_b <- function() {
  plot(power$DateTime, power$Global_active_power,
       type = 'l',
       main = 'Global Active Power Over Time',
       xlab = '', ylab = 'Global Active Power (kilowatts)')
}

# Energy Sub-Metering Over Time

plot_c <- function() {
  plot(power$DateTime, power$Sub_metering_1,
       type = 'l', col = 'black',
       ylim = range(power[, c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")],
                    na.rm = TRUE),
       main = "Energy Sub-Metering Over Time",
       xlab = "",
       ylab = "Energy Sub-Metering")
  lines(power$DateTime, power$Sub_metering_2, col = 'red')
  lines(power$DateTime, power$Sub_metering_3, col = 'blue')
  legend("topright",
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         col = c('black', 'red', 'blue'),
         lty = 1, cex = 0.7, bty = 'n')
}

# Voltage Over Time

plot_d <- function() {
  plot(power$DateTime, power$Voltage,
       type = 'l',
       main = 'Voltage Over Time',
       xlab = 'Date and Time', ylab = 'Voltage (volts)')
}

jpeg('plot_a_histogram.jpeg', width = 480, height = 480); plot_a(); dev.off()
jpeg('plot_b_global_power.jpeg', width = 480, height = 480); plot_b(); dev.off()
jpeg('plot_c_sub_metering.jpeg', width = 480, height = 480); plot_c(); dev.off()
jpeg('plot_d_voltage.jpeg', width = 480, height = 480); plot_d(); dev.off()

jpeg('plot_combined.jpeg', width = 960, height = 960)
par(mfrow = c(2,2))
plot_a()
plot_b()
plot_c()
plot_d()
dev.off()






