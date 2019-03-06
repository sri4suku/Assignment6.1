library(readxl)
library(stringr)
library(dplyr)
library(plyr)

mainFunc<-function(){
  setwd("C://Users//DELL//Desktop//Assignments//Session6")
  titanicDF <- read_excel("titanic3.xls")
  tDf<-data.frame(cbind(sapply(titanicDF$name,function(x)  getTitle(x),simplify = T)))
  colnames(tDf)<-"TitlNm"
  ttlCnt<-count(tDf, "TitlNm")
  mCnt <- max(ttlCnt[,2])+1
  plot(ttlCnt,type="p",main="Family Title and Count Representation", ylab="No. of Family members", 
          xlab="Family Title", ylim=c(0,mCnt))
  View(ttlCnt)
}

getTitle <- function (x){
  if (str_detect(x,",") == T ) {
    cPtr <- str_locate(x,",")
    titleNm<-substr(x,1, cPtr-1)
    return (titleNm)
  }

}

mainFunc()