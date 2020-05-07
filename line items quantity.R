getwd()
setwd("C:/Users/guan0337f/Desktop/PA_costings")
NHGPFY18_old<-read.csv("NHGPFY18 for PA.csv")
NUPFY18_old<-read.csv("NUPFY18 for PA.csv")
library(dplyr)
library(tidyverse)
library(reshape2)

#Subsetting
AMK<-subset(NHGPFY18_old, NHGPFY18_old$Polyclinic.Code=="AMK")
AMK<-AMK[,c("Polyclinic.Code","Service.Quantity..All.")]

WDL<-subset(NHGPFY18_old, NHGPFY18_old$Polyclinic.Code=="WDL")
WDL<-WDL[,c("Polyclinic.Code","Service.Quantity..All.")]

BBK<-subset(NUPFY18_old, NUPFY18_old$Polyclinic.Code=="BBK")
BBK<-BBK[,c("Polyclinic.Code","Service.Quantity..All.")]

CLM<-subset(NUPFY18_old, NUPFY18_old$Polyclinic.Code=="CLM")
CLM<-CLM[,c("Polyclinic.Code","Service.Quantity..All.")]

#create dataframe
table<- data.frame("Polyclinic"= c("AMK", "WDL", "BBK", "CLM"), "1", "2", "3", "4", "5", "6 to 10", "10 and above")
colnames(table)<-c("Polyclinic","1", "2", "3", "4", "5", "6 to 10", "10 and above")

##############################################################################################################
#Inputting data

AMK_freq <- table(AMK$Service.Quantity..All.)
AMK_freq<-data.frame(AMK_freq, stringsAsFactors=FALSE)
colnames(AMK_freq)<-c("Value", "Frequency")
AMK_freq<-AMK_freq[-1,]

AMK_6_to_10 <- AMK_freq[6:10,]
AMK_10_and_above <- AMK_freq[10:436,]

AMK1<-sum(AMK_6_to_10$Frequency)
AMK2<-sum(AMK_10_and_above$Frequency)

##################################################################################################

WDL_freq <- table(WDL$Service.Quantity..All.)
WDL_freq<-data.frame(WDL_freq, stringsAsFactors=FALSE)
colnames(WDL_freq)<-c("Value", "Frequency")
WDL_freq<-WDL_freq[-1,]

WDL_6_to_10 <- WDL_freq[6:10,]
WDL_10_and_above <- WDL_freq[10:438,]

WDL1<-sum(WDL_6_to_10$Frequency)
WDL2<-sum(WDL_10_and_above$Frequency)


###########################################################################################
BBK_freq <- table(BBK$Service.Quantity..All.)
BBK_freq <- data.frame(BBK_freq, stringsAsFactors=FALSE)
colnames(BBK_freq) <- c("Value", "Frequency")


BBK_6_to_10 <- BBK_freq[6:10,]
BBK_10_and_above <- BBK_freq[10:365,]

BBK1<-sum(BBK_6_to_10$Frequency)
BBK2<-sum(BBK_10_and_above$Frequency)

#########################################################################################

CLM_freq <- table(CLM$Service.Quantity..All.)
CLM_freq <- data.frame(CLM_freq, stringsAsFactors=FALSE)
colnames(CLM_freq) <- c("Value", "Frequency")


CLM_6_to_10 <- CLM_freq[6:10,]
CLM_10_and_above <- CLM_freq[10:319,]

CLM1<-sum(CLM_6_to_10$Frequency)
CLM2<-sum(CLM_10_and_above$Frequency)


View(CLM2)
