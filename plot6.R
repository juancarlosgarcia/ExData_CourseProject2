# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

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
  sccData <- sccData[grepl("Motorcycles|Motor Vehicles", sccData$Short.Name,  ignore.case = TRUE),]$SCC
  cities <- c("24510","06037")
  pm25Data <- loadData("summarySCC_PM25.rds")
  pm25Data <- pm25Data[pm25Data$SCC %in% sccData & pm25Data$fips %in% cities]
  pm25Data <- pm25Data[,sum(Emissions),by=c("fips","year")]
  pm25Data <- pm25Data[,city := {ifelse(fips == cities[2], "Los Angeles County, California", "Baltimore City, Maryland")}]
  
  setnames(pm25Data,"V1","Emissions")
  
  pm25Data
}

createPlot6 <- function() {
  pm25Data<-getData()
  
  #Build the plot
  p <- ggplot(pm25Data, aes(x=year, y=Emissions)) 
  p <- p + geom_point(aes(color=city)) 
  p <- p + facet_grid(city ~ .,scale="free")
  p <- p + geom_smooth(method="lm",aes(color=city))
  p <- p + labs(title="PM2.5 Total Emission by Type - Year \n Baltimore City, Maryland vs Los Angeles County, California",x="Year",y="Total PM2.5 emitted, in tons")
  p <- p + theme(legend.position="none")
  p <- p + theme(text=element_text(size=4), plot.title=element_text(size=6,face="bold"))
  p <- p + theme(axis.title=element_text(size=5,face="bold"))
  p <- p + theme(legend.title=element_text(size=5,face="bold")) 
  
  
  ggsave(file="plot6.png",plot=p, width=3,height=2.8)
}

createPlot6()