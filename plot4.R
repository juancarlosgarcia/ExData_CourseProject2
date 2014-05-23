#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

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
  sccData <- loadData("Source_Classification_Code.rds")
  sccData <- sccData[grepl("coal", sccData$Short.Name,  ignore.case = TRUE) & grepl("comb", sccData$Short.Name,  ignore.case = TRUE),]$SCC
  
  pm25Data <- loadData("summarySCC_PM25.rds")
  pm25Data <- pm25Data[pm25Data$SCC %in% sccData]
  pm25Data <- pm25Data[,sum(Emissions),by=c("type","year")]
  setnames(pm25Data,"V1","Emissions")
  
  pm25Data
}

createPlot4 <- function() {
  pm25Data<-getData()
  
  #Open the PNG device
  png("plot4.png",height=480,width=520)
  
  #Build the plot
  plot(pm25Data$year,pm25Data$Emissions,pch=20,col="steelblue"
       ,xlab="Year"
       ,ylab="Total PM2.5 emitted, in tons"
       ,main="PM2.5 Total Emission/Year from coal combustion-related sources")
  lines(pm25Data$year,pm25Data$Emissions,col="blue")
  
  #Close the PNG device
  dev.off()
}

createPlot4()