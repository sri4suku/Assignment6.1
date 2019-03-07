
library(dplyr)
library(Amelia)
library(mice)
library(ggplot2)
library(lattice)
library(readxl)
library(VIM)


setwd("C://Users//DELL//Desktop//Assignments//Session6")
titanicDF <- read_excel("titanic3.xls")
titanic<-select(titanicDF,name,age)

#before 
aggr_plot <- aggr(titanic, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#after
imp <- mice(titanic, method = "norm.predict", m = 1)
data_imp <- complete(imp)
aggr_plot <- aggr(data_imp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

