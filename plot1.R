downloadData <- function (fileName) {
  setInternet2(use=T) #required to support SSL in the url
  urlSource=paste0("https://raw.githubusercontent.com/juancarlosgarcia/ExData_CourseProject2/master/",fileName)
  if (!file.exists(fileName)) {
    download.file(urlSource,destfile=fileName,method="auto")
  }
}


loadData <- function(fileName) {
  downloadData(fileName)
  data<-readRDS(fileName)
  data
}


pm25Data <- loadData("summarySCC_PM25.rds")
sccData <- loadData("Source_Classification_Code.rds")

str(pm25Data)
str(sccData)