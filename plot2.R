#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.

#download the file is does not exists
downloadData <- function (fileName) {
  setInternet2(use=T) #required to support SSL in the url
  urlSource=paste0("https://raw.githubusercontent.com/juancarlosgarcia/ExData_CourseProject2/master/",fileName)
  if (!file.exists(fileName)) {
    download.file(urlSource,destfile=fileName,method="auto")
  }
}

#load the RDS file and returns in a data.table format
loadData <- function(fileName) {
  library(data.table)
  downloadData(fileName)
  data<-readRDS(fileName)
  data<-as.data.table(data)
  data
}

#aggregate the data per year
getData <- function() {
  pm25Data <- loadData("summarySCC_PM25.rds")
  pm25Data <- pm25Data[pm25Data$fips == "24510",]
  pm25Data <- pm25Data[,sum(Emissions),by=year]
  setnames(pm25Data,"V1","Emissions")
  
  pm25Data
}

createPlot2 <- function() {
  pm25Data<-getData()
  
  #Open the PNG device
  png("plot2.png",height=480,width=480)
  
  #Build the plot
  plot(pm25Data$year,pm25Data$Emissions,pch=20,col="steelblue"
       ,xlab="Year"
       ,ylab="Total PM2.5 emitted, in tons"
       ,main="PM2.5 Total Emission/Year in Baltimore City, Maryland")
  lines(pm25Data$year,pm25Data$Emissions,col="blue")
  
  #Close the PNG device
  dev.off()
}

createPlot2()


