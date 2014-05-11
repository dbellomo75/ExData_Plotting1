plot2 <- function() {
	
## Expected size of the data file: 2075260*9*8/(2^20)~=142Mb << myRAMsize

## Read the data into a data frame
allData <- read.table("household_power_consumption.txt",sep=";", nrows=100000, header=TRUE)

## Get the data for two days: "2007-02-01" and "2007-02-02"
allDates<-as.Date(allData$Date, "%d/%m/%Y")								# factor -> Date
myData<-allData[(allDates >= "2007-02-01")&(allDates <="2007-02-02"),]	# subsetting of the data
# strptime() and as.Date()

x <- paste(myData$Date, myData$Time)
y<-strptime(x, "%d/%m/%Y %H:%M:%S")

## Get the active power
xc<-as.character(myData$Global_active_power) 	# factor -> character
xc[xc=="?"]<-NA 							 	# replace "?" with NA
xn<-as.numeric(xc)							 	# character -> numeric
bad<-is.na(xn) 								 	# find NA
power<-xn[!bad] 							 	# remove NA

# Plot 
t<-y[!bad]
par(bg ="white")
plot(t,power,type="l",xlab="",ylab = "Global Active Power (kilowatt)")

## Create png file
dev.copy(png, file = "plot2.png", width=480, height=480) ## Copy my plot to a PNG file
dev.off() # close the dedive
}

# 2075260