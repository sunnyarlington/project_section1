#Load libraries
library(tidyverse)
install.packages("haven")
library(readxl)
library(foreign)
library(haven)
#set file names

#download files and process it
#proc_csv <- function(inZip,varList){  #inZip is the ZIP Archive, varList is the variable specifications from readr
outFile <- data.frame()
final <- data.frame()

#state codes
setwd("H:/Income_comparison")
Data<- read_excel("Book1.xlsx")
codes <- sort(unique(Data$geo))

for (i in codes) {
  temp <- tempfile()    
  download.file(paste("https://www2.census.gov/programs-surveys/acs/data/pums/2019/1-Year/csv_h",i,".zip", sep = ""),temp,mode="wb")
  filList <- unzip(temp, list = TRUE) # Create list of files
  for(j in 1:nrow(filList)) { # Loop through the list of files
    if(grepl("csv",filList[j,1])) {  #If a file is a csv file, unzip it and read the data
      dd <- read.csv(unz(temp, filList[j,1]))
      dd <-dd[,c("RT","SERIALNO", "DIVISION", "PUMA", "REGION", "ST", "HINCP","WGTP")]
      as.character(dd$ST)
      dd$ST<-ifelse(nchar(dd$ST)==1, paste("0", dd$ST, sep = ""), dd$ST)
      final<-rbind(final,dd)
    }
  }
}
print(final)  
table(final$HINCP)
table(final$ST)
today <- Sys.Date()
today <- format(today,format = "%b%d%Y")
as.numeric(final$HINCP)


library(dplyr)

weighted.median <- function(x, w) {
  w <- w[order(x)]
  x <- x[order(x)]
  
  prob <- cumsum(w)/sum(w)
  ps <- which(abs(prob - .5) == min(abs(prob - .5)))
  return(x[ps])
}

if (final$hincp=0) {
  df_summary3 <-
    final %>%
    group_by(final$ST) %>%
    summarise(weighted_income = weighted.mean(HINCP, WGTP))
}
df_summary2


test1<-(final$ST="01")
install.packages("doBy")
library(doBy)
summaryBy(HINCP ~ ST, data = final,
          FUN = list(mean, max, min, median, sd))

fin_data<-na.omit(final, cols="final$HINCP")
head(fin_data, n=10)
head(final, n=10)

data.table::fwrite(fin_data, paste("ACS_2019",today,".csv",sep=""),col.names=T,row.names = F,sep = ",")

#write.foreign(final, paste("ACS_2019",today,".txt",sep=""), paste("ACS_2019",today,".sas",sep=""),   package="SAS")
write_sas(final, paste("ACS_2019",today,".sas7bdat",sep=""))