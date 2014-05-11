plot4 <- function() {
	
## Expected size of the data file: 2075260*9*8/(2^20)~=142Mb << myRAMsize

## Read the data into a data frame
allData <- read.table("household_power_consumption.txt",sep=";", nrows=100000, header=TRUE)

## Get the data for two days: "2007-02-01" and "2007-02-02"
allDates<-as.Date(allData$Date, "%d/%m/%Y")								# factor -> Date
myData<-allData[(allDates >= "2007-02-01")&(allDates <="2007-02-02"),]	# subsetting of the data

x <- paste(myData$Date, myData$Time)
y<-strptime(x, "%d/%m/%Y %H:%M:%S")

## Get Global_active_power
GAPc<-as.character(myData$Global_active_power) 	# factor -> character
GAPc[GAPc=="?"]<-NA 							 	   # replace "?" with NA
GAPn<-as.numeric(GAPc)							 	# character -> numeric
badGAP<-is.na(GAPn) 								 	# find NA
GAP<-GAPn[!badGAP] 							 	 # remove NA
tGAP<-y[!badGAP]

## Get Voltage
Vc<-as.character(myData$Voltage) 	# factor -> character
Vc[Vc=="?"]<-NA 							 	   # replace "?" with NA
Vn<-as.numeric(Vc)							 	# character -> numeric
badV<-is.na(Vn) 								 	# find NA
V<-Vn[!badV] 							 	 # remove NA
tV<-y[!badV]


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

## Get Global_reactive_power
GRPc<-as.character(myData$Global_reactive_power) 	# factor -> character
GRPc[GRPc=="?"]<-NA 							 	   # replace "?" with NA
GRPn<-as.numeric(GRPc)							 	# character -> numeric
badGRP<-is.na(GRPn) 								 	# find NA
GRP<-GRPn[!badGRP] 							 	 # remove NA
tGRP<-y[!badGRP]


## Plot
par(bg ="white")
par(mfrow = c(2, 2))
plot(tGAP,GAP,type="l",xlab="",ylab = "Global Active Power")
plot(tV,V,type="l",xlab="datetime",ylab = "Voltage")
plot(t1,sbm1,type="n",xlab="",ylab = "Energy sub metering")
lines(t1,sbm1,type="l")
lines(t2,sbm2,type="l",col="red")
lines(t3,sbm3,type="l",col="blue")
legend("topright",pch = "-", col = c("gray","blue", "red"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),cex=0.6, bty='n', title.adj=0.15)
plot(tGRP,GRP,type="l",xlab="datetime",ylab = "Global Reactive Power")


#plot(t1,sbm1,type="n",xlab="",ylab = "Energy sub metering")
#lines(t1,sbm1,type="l")
#lines(t2,sbm2,type="l",col="red")
#lines(t3,sbm3,type="l",col="blue")


## Create png file
dev.copy(png, file = "plot4.png", width=480, height=480) ## Copy my plot to a PNG file
dev.off() # close the dedive
}

# 2075260