#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission 
#from all sources for each of the years 1999, 2002, 2005, and 2008

#download the file is does not exists
downloadData <- function (fileName) {
  library(RCurl)
  setInternet2(use=TRUE) #required to support SSL in the url
  urlSource=paste0("https://raw.githubusercontent.com/juancarlosgarcia/ExData_CourseProject2/master/",fileName)
  if (!file.exists(fileName)) {
    download.file(urlSource,destfile=fileName)
  }
}

#load the RDS file and returns in a data.table format
loadData <- function(fileName) {
  library(data.table)
  if (!file.exists(fileName)) {
    fileZip="exdata-data-NEI_data.zip"
    downloadData(fileZip)
    unzip(fileZip)
  }
  
  data<-readRDS(fileName)
  data<-as.data.table(data)
  data
}

#aggregate the data per year
getData <- function() {
  pm25Data <- loadData("summarySCC_PM25.rds")
  pm25Data <- pm25Data[,sum(Emissions),by=year]
  setnames(pm25Data,"V1","Emissions")
  pm25Data
}

createPlot1 <- function() {
  pm25Data<-getData()
  
  #Open the PNG device
  png("plot1.png",height=480,width=480)
  
  #Build the plot
  plot(pm25Data$year,pm25Data$Emissions,pch=20,col="steelblue"
       ,xlab="Year"
       ,ylab="Total PM2.5 emitted, in tons"
       ,main="PM2.5 Total Emission/Year")
  lines(pm25Data$year,pm25Data$Emissions,col="blue")
  
  #Close the PNG device
  dev.off()
}

createPlot1()
