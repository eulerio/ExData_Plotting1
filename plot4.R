plot4 <- function(){
	#download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile="./temp.zip")

	#unzip("temp.zip")
	data <- read.table("household_power_consumption.txt", header=TRUE,sep=";")
	
	data$Date<-as.Date(data$Date,format="%d/%m/%Y")
	
	start_date <- as.Date("2007-02-01")
	end_date <- as.Date("2007-02-02")
	
	data_sub <- subset(data,(Date>=start_date & Date<=end_date))

	GAP <- as.numeric(as.character(data_sub$Global_active_power))
	GAR <- as.numeric(as.character(data_sub$Global_reactive_power))
	time <- as.character(data_sub$Time)
	datee <- as.character(data_sub$Date)
	posixtime <- paste(datee,time,sep=" ")
	posixtime <- strptime(posixtime,format="%Y-%m-%d %H:%M:%S")
	
	sm1 <- as.numeric(as.character(data_sub$Sub_metering_1))
	sm2 <- as.numeric(as.character(data_sub$Sub_metering_2))
	sm3 <- as.numeric(as.character(data_sub$Sub_metering_3))
	volt <- as.numeric(as.character(data_sub$Voltage))
	
	cc <- complete.cases(sm1,sm2,sm3)
	
	sm1<-sm1[cc]
	sm2<-sm2[cc]
	sm3<-sm3[cc]
	posixtime<-posixtime[cc]
	
	png(file="plot4.png",width=480,height=480,units="px")
	par(mar=c(4,4,2,2))
	par(mfrow=c(2,2))
	
	
	plot(posixtime,GAP,xlab="",ylab="Global Active Power (kilowatts)",type="n")
	lines(posixtime,GAP)
	
	plot(posixtime,volt,xlab="datetime",ylab="Voltage",type="n")
	lines(posixtime,volt)
	
	plot(posixtime,sm1,xlab="",ylab="Energy sub metering",type="n")
	lines(posixtime,sm1)
	lines(posixtime,sm2,col="red")	
	lines(posixtime,sm3,col="blue")
   legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),col=c("black","red","blue"))
	
	plot(posixtime,GAR,xlab="datetime",ylab="Global_reactive_power",type="n")
	lines(posixtime,GAR)
	
	
		
	dev.off()
	
}

