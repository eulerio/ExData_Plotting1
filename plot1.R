plot1 <- function(){
	download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile="./temp.zip")

	unzip("temp.zip")
	data <- read.table("household_power_consumption.txt", header=TRUE,sep=";")
	
	data$Date<-as.Date(data$Date,format="%d/%m/%Y")
	
	start_date <- as.Date("2007-02-01")
	end_date <- as.Date("2007-02-02")
	
	data_sub <- subset(data,(Date>=start_date & Date<=end_date))
	
	#data_char <- as.character(data_sub)
	
	#for(i in names(data_char)){
	#	gsub("?","NA",data_char[[paste(i)]])
	#}
	
	GAP <- as.numeric(as.character(data_sub$Global_active_power))
	
	GAP <- GAP[!is.na(GAP)]
	
	GAP
	
	png(file="plot1.png",width=480,height=480,units="px")
	par(mar=c(4,4,2,2))
	
	hist(GAP,col="red",xlab="Global Active Power (kilowatts)",main="Global Active Power")
	dev.off()
	
}

