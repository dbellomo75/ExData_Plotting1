plot3 <- function() {
	
## Expected size of the data file: 2075260*9*8/(2^20)~=142Mb << myRAMsize

## Read the data into a data frame
allData <- read.table("household_power_consumption.txt",sep=";", nrows=100000, header=TRUE)

## Get the data for two days: "2007-02-01" and "2007-02-02"
allDates<-as.Date(allData$Date, "%d/%m/%Y")								# factor -> Date
myData<-allData[(allDates >= "2007-02-01")&(allDates <="2007-02-02"),]	# subsetting of the data

x <- paste(myData$Date, myData$Time)
y<-strptime(x, "%d/%m/%Y %H:%M:%S")

## Get Energy sub metering
s1c<-as.character(myData$Sub_metering_1) 	# factor -> character
s1c[s1c=="?"]<-NA 							 	    # replace "?" with NA
s1n<-as.numeric(s1c)							 	# character -> numeric
bad1<-is.na(s1n) 								 	# find NA
sbm1<-s1n[!bad1] 							 	    # remove NA
t1<-y[!bad1]# Plot 

s2c<-as.character(myData$Sub_metering_2) 	# factor -> character
s2c[s2c=="?"]<-NA 							 	    # replace "?" with NA
s2n<-as.numeric(s2c)							 	# character -> numeric
bad2<-is.na(s2n) 								 	# find NA
sbm2<-s2n[!bad2] 							 	    # remove NA
t2<-y[!bad2]# Plot 


s3c<-as.character(myData$Sub_metering_3) 	# factor -> character
s3c[s3c=="?"]<-NA 							 	    # replace "?" with NA
s3n<-as.numeric(s3c)							 	# character -> numeric
bad3<-is.na(s3n) 								 	# find NA
sbm3<-s3n[!bad3] 							 	    # remove NA
t3<-y[!bad3]# Plot 


par(bg ="white")
plot(t1,sbm1,type="n",xlab="",ylab = "Energy sub metering")
lines(t1,sbm1,type="l")
lines(t2,sbm2,type="l",col="red")
lines(t3,sbm3,type="l",col="blue")
legend("topright",pch = "-", col = c("gray","blue", "red"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))

## Create png file
dev.copy(png, file = "plot3.png", width=480, height=480) ## Copy my plot to a PNG file
dev.off() # close the dedive
}

# 2075260