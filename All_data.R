setwd("C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\Dati_Aggregati")
library(data.table)
library(dplyr)
library(corrplot)


## READ THE DATASETS
airdata <- fread("aggrAirSensorsCR.csv")[,-1]
meteodata <- fread("aggrWeatherCR.csv")[,-1]
trafficdata <- fread("aggrTrafficCR.csv")
##


## COMBINE ALL THE DATA
alldata <- meteodata %>% 
  right_join(trafficdata,by=c("DateTime"="DATE")) %>%
  inner_join(airdata,by="DateTime")

alldata[,2:9] <- na.approx(alldata[,2:9],rule=2) # ho inserito nel posto degli NA i valori
# della riga successiva
#


## CORRPLOT
corrplot(cor(alldata[,-1]))
#


## WRITE A NEW CSV FILE
outDir <- "C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\Dati_Aggregati\\"
outfile <- "All.csv"
outPath <- paste(outDir,outfile,sep='')
write.csv(alldata,file=outPath,sep=",",col.names = T,row.names = F)
#
