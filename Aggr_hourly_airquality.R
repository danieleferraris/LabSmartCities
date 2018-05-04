setwd("C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\Dati_Aggregati")
# setwd("C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\out")
library(plyr)
library(data.table)
library(zoo)

Airdata <- fread("totalAirSensorsCR.csv")
Airdata$DateTime <- format(as.POSIXct(Airdata$DateTime), "%Y-%m-%d %H")
table(Airdata$Desc)


## HOURLY AVARAGES FOR TYPE OF POLLUTANT.
# The dataset needs this variables: Desc, DateTime, Value. v is the name of desc
myaverage <- function(data, d){
  averages <- ddply(data[data$Desc==d,], c("Desc","DateTime"), function(x) mean(x$Value))
  averages$Desc <- NULL
  colnames(averages) <- c("DateTime",d)
  return (averages)
}

# Apply the function for all the desc levels and create a new dataset
All <- data.frame(DateTime=sort(unique(Airdata$DateTime)))
values  <- unique(Airdata$Desc)
for ( v in values) All=merge(All,myaverage(Airdata, v),all.x=TRUE, by="DateTime") 
rm(v,values)
##


## FIX THE NA 
# 
head(All)
# FOR THE FIRST VARIABLES I APPLICATED THE POLINOMIAL INTERPOLATION
All2 <- All
All2[,c(2:6,8)] <- na.spline(All[,c(2:6,8)])
colnames(All2)[7] <- 'PM10'
All2$PM10 <- na.locf(All2$PM10)
All2$PM2.5 <- na.locf(All$PM2.5)


## FIX THE NA FOR BLACKCARBON (& AMMONIACA) I USED THE LINEAR MODEL PREDICTIONS
##
plot(All[,-1]) # vedo che c'è correlazione lineare tra variabili
data <- All2[complete.cases(All2),] # select only the rows without NAs
data <- data[,-1] # remove the time variable
intrain <- sample(x= 1:NROW(data), size=as.integer(NROW(data)/100*70))
train <- data[intrain,]
test <- data[-intrain,]
# fit & predict the model
lm_fit <- lm(BlackCarbon~.-Ammonia,data=train)
pred <- predict(lm_fit,newdata = test)
MSE  <- sqrt(mean((test$BlackCarbon-pred)^2))

newdata <- All2[which(is.na(All2$BlackCarbon)),]
y_pred <- predict(lm_fit,newdata)
y_pred <- round(y_pred,2)
# replace the NA values
All2$BlackCarbon[which(is.na(All2$BlackCarbon))] <- y_pred
##

## the same for AMMONIACA
data <- All2[complete.cases(All2),] # select only the rows without NAs
data <- data[,-1] # remove the time variable
intrain <- sample(x= 1:NROW(data), size=as.integer(NROW(data)/100*70))
train <- data[intrain,]
test <- data[-intrain,]
# fit & predict the model
lm_fit <- lm(Ammonia~.-BlackCarbon,data=train)
pred <- predict(lm_fit,newdata = test)
MSE  <- sqrt(mean((test$Ammonia-pred)^2))

newdata <- All2[which(is.na(All2$Ammonia)),]
y_pred <- predict(lm_fit,newdata)
y_pred <- round(y_pred,2)
# replace the NA values
All2$Ammonia[which(is.na(All2$Ammonia))] <- y_pred
##


## WRITE CSV FILE
outDir <- "C:\\Users\\daniele\\Documents\\Università\\MADAS\\2. Lab Smart Cities\\DatiAirQuality\\Dati_Aggregati\\"
outfile <- "aggrAirSensorsCR.csv"
outPath <- paste(outDir,outfile,sep='')
write.csv(All2,file=outPath)
