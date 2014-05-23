#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

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
  sccData <- loadData("Source_Classification_Code.rds")
  sccData <- sccData[grepl("Motorcycles|Motor Vehicles", sccData$Short.Name,  ignore.case = TRUE),]$SCC
  
  pm25Data <- loadData("summarySCC_PM25.rds")
  pm25Data <- pm25Data[pm25Data$SCC %in% sccData & pm25Data$fips == "24510"]
  pm25Data <- pm25Data[,sum(Emissions),by=c("type","year")]
  setnames(pm25Data,"V1","Emissions")
  
  pm25Data
}

createPlot5 <- function() {
  pm25Data<-getData()
  
  #Open the PNG device
  png("plot5.png",height=480,width=520)
  
  #Build the plot
  plot(pm25Data$year,pm25Data$Emissions,pch=20,col="steelblue"
       ,xlab="Year"
       ,ylab="Total PM2.5 emitted, in tons"
       ,main="PM2.5 Total Emission/Year of Motor in Baltimore City, Maryland")
  lines(pm25Data$year,pm25Data$Emissions,col="blue")
  
  #Close the PNG device
  dev.off()
}

createPlot5()