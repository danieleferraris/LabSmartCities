setwd("C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\Dati_Aggregati")
# setwd("C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\out")
library(plyr)
library(data.table)
library(zoo)

Meteodata <- fread("totalWeatherCR.csv")
Meteodata$DateTime <- format(as.POSIXct(Meteodata$DateTime), "%Y-%m-%d %H")

table(Meteodata$Desc)

## HOURLY AVARAGES FOR TYPE OF METEO MEASURE.
# The dataset needs this variables: Desc, DateTime, Value. v is the name of desc
myaverage <- function(data, d){
  averages <- ddply(data[data$Desc==d,], c("Desc","DateTime"), function(x) mean(x$Value))
  averages$Desc <- NULL
  colnames(averages) <- c("DateTime",d)
  return (averages)
}
# Apply the function for all the desc levels and create a new dataset
All <- data.frame(DateTime=sort(unique(Meteodata$DateTime)))
values  <- unique(Meteodata$Desc)
for ( v in values) All=merge(All,myaverage(Meteodata, v),all.x=TRUE, by="DateTime") 
rm(v,values)
##


## FIX THE NA VALUES WITH POLINOMIAL INTERPOLATION
summary(All)
All[,-1] <- na.spline(All[,-1])
##


## WRITE CSV FILE
outDir <- "C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\Dati_Aggregati\\"
outfile <- "aggrWeatherCR.csv"
outPath <- paste(outDir,outfile,sep='')
write.csv(All,file=outPath)
