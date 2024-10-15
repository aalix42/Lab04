##install.packages(c("lubridate", "ggplot2", "dplyr"))
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project//activity03/activity04/campus_weather.csv", 
                    na.strings = "#N/A")

weather$dateF <- mdy_hm(weather$Date)
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

#create a function to repeatedly check time intervals, -length is cutting off the first
## and then the last data point 
interval <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval


#set up time intervals in a vector of dates 
#anything in curly brackets is going to be the function 
timeInterval <- function(x){
  x[-length(x)] %--% x[-1]
}

timeInterval

#for loop 
for(i in 1:6){
  print(paste("example", i))
  
}

seqEx <- c(1, 4, 6)
for (i in seqEx){
  print(paste("example", i))
}

#create empty vector with character type data 
chEx <- character()
for(i in 1:6){
  chEx[i] <- paste("example", i)
}

numEx <- numeric()
for(i in 1:6){
  numEx[i] <- 6*i
}

ggplot(data=weather,
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()

#in class prompt 1 
#parsing out dates
weather$month <- month(weather$dateF)
weather$year <- year(weather$dateF)

Jan22 <- weather %>% 
  filter(month == 1 & year == 2022)

mean(Jan22$AirTemp[1:8])

rollAveTemp <- numeric()
for(i in 8: nrow(Jan22)){
  rollAveTemp[i] <- mean(Jan22$AirTemp[(i-7):i])
}


Jan22$rollAveTemp <- rollAveTemp
ggplot(Jan22, aes(x = AirTemp, y = rollAveTemp)) + 
  geom_point() + 
  labs(x = "Air Temperature (C)", y = "Rolling Average Temperature (C)")

#in class prompt 2 
MayAndJune <- weather %>% 
  filter(month <= 6 & month >= 5 & year == 2021)

# creating a bounds for outliers 
Q1 <- quantile(MayAndJune$SolRad, .25)
Q3 <- quantile(MayAndJune$SolRad, .75)
max(MayAndJune$SolRad)
IQR <- Q3 - Q1 

UpperOutlier <- Q3 + 1.5*IQR

MayAndJune$SolRadFlags <- ifelse(MayAndJune$SolRad >= UpperOutlier, 
                                 1, 
                                 0)
table(MayAndJune$SolRadFlags)

##Homework Questions: 
#homework q1
#create a flag for the criteria 
weather$FlagTwo <- ifelse(weather$AirTemp <= 0,
                          NA,
                          weather$Precip)

weather$FlagTwo <- ifelse(weather$XLevel >= 2 & weather$YLevel >= 2,
                          NA, 
                          weather$Precip)

#homework q2. Create a flag if the voltage goes below 8.5 volts. 



#question5
#creating a new data frame that contains the daily minimum temperature, and the daily precip value 
DailyPrecip <- weather %>% 
  filter(year == 2021) %>%
  group_by(doy) %>% 
  summarize(Precipitation = sum(Precip, na.rm = TRUE), Temperature = min(AirTemp))

#filtering it to be in March and April only 
MarchAprilPrecip <- DailyPrecip %>%
  filter(doy >= 60 & doy <= 120)

###Creating the for loop with an ifelse statement 
PrecipTempQC <- numeric(nrow(MarchAprilPrecip))

for(i in 2: nrow(MarchAprilPrecip)){
PrecipTempQC[i] <- ifelse(MarchAprilPrecip$Temperature[i] <= 1.6667 |
                            MarchAprilPrecip$Temperature[i-1] <= 1.667,
                            NA, MarchAprilPrecip$Precipitation[i])

}

PrecipTempQC


##assigning the values to the variable and counting the number of none NA variables 
MarchAprilPrecip$PrecipTempQC<- PrecipTempQC
length(na.omit(MarchAprilPrecip$PrecipTempQC))

