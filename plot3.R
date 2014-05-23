#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system 
#to make a plot answer this question.


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
  pm25Data <- pm25Data[pm25Data$fips == "24510"]
  pm25Data <- pm25Data[,sum(Emissions),by=c("type","year")]
  setnames(pm25Data,"V1","Emissions")
  
  pm25Data
}

createPlot3 <- function() {
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
 
  
  ggsave(file="plot3.png",plot=p, width=4,height=2)
}

createPlot3()
