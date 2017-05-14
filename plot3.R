plot3 <- function(){
	#download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile="./temp.zip")

	unzip("temp.zip")
	data <- read.table("household_power_consumption.txt", header=TRUE,sep=";")
	
	data$Date<-as.Date(data$Date,format="%d/%m/%Y")
	
	start_date <- as.Date("2007-02-01")
	end_date <- as.Date("2007-02-02")
	
	data_sub <- subset(data,(Date>=start_date & Date<=end_date))

	GAP <- as.numeric(as.character(data_sub$Global_active_power))
	time <- as.character(data_sub$Time)
	datee <- as.character(data_sub$Date)
	posixtime <- paste(datee,time,sep=" ")
	posixtime <- strptime(posixtime,format="%Y-%m-%d %H:%M:%S")
	
	sm1 <- as.numeric(as.character(data_sub$Sub_metering_1))
	sm2 <- as.numeric(as.character(data_sub$Sub_metering_2))
	sm3 <- as.numeric(as.character(data_sub$Sub_metering_3))
	
	cc <- complete.cases(sm1,sm2,sm3)
	
	sm1<-sm1[cc]
	sm2<-sm2[cc]
	sm3<-sm3[cc]
	posixtime<-posixtime[cc]
	
	
	
	png(file="plot3.png",width=480,height=480,units="px")
	par(mar=c(4,4,2,2))
	
	plot(posixtime,sm1,xlab="",ylab="Energy sub metering",type="n")
	lines(posixtime,sm1)
	#plot(posixtime,sm2,xlab="",ylab="Global Active Power (kilowatts)",type="n")
	lines(posixtime,sm2,col="red")	
	#plot(posixtime,sm3,xlab="",ylab="Global Active Power (kilowatts)",type="n")
	lines(posixtime,sm3,col="blue")
	legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),col=c("black","red","blue"))
		
	dev.off()
	
}

