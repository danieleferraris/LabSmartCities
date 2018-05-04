setwd("C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\Dati_Aggregati")
library(data.table)
library(dplyr)
library(corrplot)


## READ THE DATASETS
All <- fread("All.csv")

# you have to compute the PM10Avg (last 24 hours ), 
# the NO2Max (hourly) and the
# O3Max (in the last 8 hours)
 
## PM10Avg
#
PM10Avg <- numeric(NROW(All)-23)
for (i in 24:NROW(All)) {
  PM10Avg[i-23] <- mean(All$PM10[i-23:i])
}

# cbind(All$DateTime[-c(1:24)],as.double(PM10Avg))

#PM.10
# PM <- All[,c("DateTime","PM10")]
# PM$DateTime <- format(as.POSIXct(All$DateTime), "%Y-%m-%d")
# PM <- PM %>% group_by(DateTime) %>% summarise(PM10=mean(PM10))
# #





#

## NO2Max
NO2 <- numeric(NROW(All)-23)
for (i in 24:NROW(All)) {
  NO2[i-23] <- max(All$`Total Nitrogen`[i-23:i])
}
#

## O3
O3 <- numeric(NROW(All)-8)
for (i in 9:NROW(All)) {
  O3[i-8] <- max(All$Ozone[i-8:i])
}

# tolgo le prime osservazioni perchè gli altri indici partono 
# dal giorno successivo
O3 <- O3[-c(1:15)]

# INDICI
I_pm10 <- All$PM10/50*100 
I_NO2  <- NO2/200*100
I_O3   <- O3/120*100

IQA <-( I_pm10 + max(I_NO2,I_O3) ) / (120) 

plot(IQA)
