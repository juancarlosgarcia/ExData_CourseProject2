#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

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
  pm25Data <- pm25Data[pm25Data$fips == "24510"]
  pm25Data <- pm25Data[,sum(Emissions),by=c("type","year")]
  setnames(pm25Data,"V1","Emissions")
  #sccData <- loadData("Source_Classification_Code.rds")
  pm25Data
}

createPlot4 <- function() {
  library(ggplot2)
  pm25Data<-getData()
  
  #Build the plot
  p <- ggplot(pm25Data, aes(x=year, y=Emissions)) 
  p <- p + geom_point(aes(color=type)) 
  p <- p + facet_grid(. ~ type)
  p <- p + geom_smooth(method="lm",aes(color=type))
  p <- p + labs(title="PM2.5 Total Emission by Type - Year in Baltimore City, Maryland",x="Year",y="Total PM2.5 emitted, in tons")
  p <- p + theme(legend.position="none")
  p <- p + theme(text=element_text(size=4), plot.title=element_text(size=6,face="bold"))
  p <- p + theme(axis.title=element_text(size=5,face="bold"))
  p <- p + theme(legend.title=element_text(size=5,face="bold")) 
  
  
  ggsave(file="plot4.png",plot=p, width=4,height=2)
}

createPlot4()