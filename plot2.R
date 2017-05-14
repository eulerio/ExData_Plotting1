plot2 <- function(){
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
	
	posixtime <- posixtime[!is.na(GAP)]
	GAP <- GAP[!is.na(GAP)]
	#time <- time[!is.na(GAP)]
	#datee <- datee[!is.na(GAP)]
	
	
	
	png(file="plot2.png",width=480,height=480,units="px")
	par(mar=c(4,4,2,2))
	
	plot(posixtime,GAP,xlab="",ylab="Global Active Power (kilowatts)",type="n")
	lines(posixtime,GAP)
	dev.off()
	
}

