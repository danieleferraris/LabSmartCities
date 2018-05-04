library(dplyr)
library(data.table)
setwd("C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\Dati_Aggregati")
data <- fread("totalTrafficCR.csv")
# data <- totalTraffic[1:10000,]
data <- data[,-c(2,3,9,10,11,12)] # remove the not important variables

# TRASFORM THE TIMESTAMP IN HOURLY DATA:
data$Timestamp <- format(as.POSIXct(data$Timestamp), "%Y-%m-%d %H")
# aggiungo una variabile di 1 che mi servirà per sommare i valori aggregati 
data$I <- rep(1,NROW(data)) %>% as.integer()

data$Length <- cut(data$Length, breaks=c(0, 2000, 4000, 6000, 20000), labels = c('(0-2]','(2-4]','(4-6]','>6'),right = T)
# some NA length (because was negative). Adjuste with proportional method
z <- sample(x = c('(0-2]','(2-4]','(4-6]','>6'), size = sum(is.na(data$Length)), replace = T, prob = table(data$Length)/(NROW(data)-sum(is.na(data$Length))) )
data$Length[which(is.na(data$Length))] <- z
#

hCar <- aggregate(I ~ Timestamp, data=data, sum) ## NUMBER OF PASSAGES
hEuro <- dcast(data,Timestamp ~ EURO ,sum, value.var="I") ## NUMBER OF PASSAGES BY EURO
VType <- dcast(data,Timestamp ~ VType ,sum,value.var="I") ## NUMBER OF PASSAGES BY TYPE OF VEHICLE (VType)
FType <- dcast(data,Timestamp ~ FType,sum,value.var="I")  ## NUMBER OF PASSAGES BY FUELD USED (FType)
DPF   <- dcast(data,Timestamp ~ DPF,sum,value.var="I") ## NUMBER OF PASSAGES BY FILTER (DPF)
Length   <- dcast(data,Timestamp ~ Length,sum,value.var="I") ## NUMBER OF PASSAGES BY FILTER (DPF)

# PUT ALL THE RESULTS IN ONE TABLE AND REASSIGN THE COLNAMES
AggregateVehicle <- cbind(hEuro[,-8],VType[,-c(1,2)],FType[,-c(1,4)],DPF[,-c(1,3)],Length[,-c(1,5)],hCar[,-1])
colnames(AggregateVehicle) <- c("DATE",
                                "E0","E1","E2","E3","E4","E5",
                                "BUS","FREIGHT","PEOPLE",
                                "PETROL","DIESEL","GAS","HYBRID",
                                "FILTER",
                                "L.(0-2]","L.(2-4]", "L.(4-6]",
                                "TOT_PASSAGES")
# I REMOVE THE FOLLOWING COLUMNS: EURO6,OTHER_VEHICLES,ELETRIC_CAR,NO_FILTER,LENGTH>6 because
# i considered also the tot_passeges 

## WRITE A CSV FILES
outDir <- "C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\Dati_Aggregati\\"
outfile <- "aggrTrafficCR.csv"
outPath <- paste(outDir,outfile,sep='')
write.csv(AggregateVehicle,file=outPath,sep=",",col.names = T,row.names = F)
