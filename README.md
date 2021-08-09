# DSSApracticum_hurricaneSandy
Sandy Hook Barrier Island Erosion after Superstorm Sandy: buoy data analyzed from two locations via graphs of key meteorological parameters and PCA analysis!

---
title: "DSSA Practicum"
author: "Ayshe Kilic"
og date before had to switch: "July 9 2021"
date: "August 5 2021"
modified: "August 9 2021"
---

Purpose: analyze buoy data from two locations during Hurricane Sandy via graphs of key meteorological parameters and PCA analysis!


```{r}
library(ggplot2) #basic visuals
library(tidyverse) #glimpse()
library(lubridate) #switching date column to appropriate class type
#install.packages("weathermetrics")
library(weathermetrics) #C to F
#install.packages("measurements")
library(measurements) #m to ft
```
buoy station 44025 from 2016-2020:  https://www.ndbc.noaa.gov/station_history.php?station=44065
Measurement description: https://www.ndbc.noaa.gov/measdes.shtml

PRES -- sea level pressure (hPa)
ATMP -- air temperature (Celsius)
WTMP -- sea surface temperature (Celsius)
DEWP -- dew point temperature taken at the same height as the air temperature measurement (Celsius)...The dew point is the temperature to which air must be cooled to become saturated with water vapor. this is the maximum amount of saturation just before it condenses->The higher the dew point rises, the greater the amount of moisture in the air.
TIDE -- water level above or below mean lower low water (MLLW) (Feet)...The average of the lower low water height of each tidal day observed over the National Tidal Datum Epoch. [all same]
WVHT -- significant wave height (Meters)...average of the highest one-third of all of the wave heights during the 20-minute sampling period

```{r}
### READING IN BUOY STATION DATA AND MAKING THEM USEFUL:

#Problem1: txt file smushed all of data points into one variable/column
#Solution: fixed with sep = "" which considers ANY length of white space characters, while sep = " " only looks for one white space

#Station 44065: New York Harbor::
nyHarbor <- read.csv("NYbuoy_2012.txt", sep = "" , header = TRUE)
#Station SDHN4: Sandy Hook, NJ::
sandyHook <- read.csv("SHbuoy_2012.txt", sep = "" , header = TRUE)




### REMOVE FIRST ROW CONTAINING UNIT MEASUREMENTS:         !!only run this once because each re-run will remove the new row of values in 1st place!!

#Problem2: first row of unit measurements are in chr class and creating issues in R functions. I don't need them anyway
#Solution: remove the first row of data from all data frames
nyHarbor <- nyHarbor[-c(1), ]
sandyHook <- sandyHook[-c(1), ]
```


```{r}
### COMBINE AND REFORMAT DATES:

#Problem3: Date-time are in separate columns, but I want them together and formated to a date class
#Solution: use mutate() to concatenate together into a new column called "Date"
nyHarbor  <- nyHarbor  %>% mutate(Date=ymd_h(paste(X.YY,MM,DD,hh)))
sandyHook <- sandyHook %>% mutate(Date=ymd_h(paste(X.YY,MM,DD,hh)))


### SELECTING DATA I WANT TO USE:

#Problem4: I do not want to use every attribute included in the dataset
#Solution: use subset() to remove columns I don't want and override the existing dataframe
nyHarbor <- subset(nyHarbor, select = -c(X.YY:mm, VIS,TIDE) )
sandyHook <- subset(sandyHook, select = -c(X.YY:mm, VIS,TIDE) )
```


```{r}
### CHANGE ALL NUMERIC COLUMNS FROM CHR TO DBL CLASS:

#Problem5: All numeric variables are listed as chr class which does not allow me to use data in functions
#Solution: use sapply() to change those columns to dbl
nyHarbor[, c(1:11)]  <- sapply(nyHarbor [, c(1:11)], as.numeric)
sandyHook[, c(1:11)] <- sapply(sandyHook[, c(1:11)], as.numeric)




### CONVERT CELSIUS TO FAHRENHEIT:      !!Only run this once or it will keep converting the numbers making them larger in value!!

#Problem6: ATMP and WTMP is recorded in C, but since I am presenting this in America, I want my audience to quickly understand the visuals and metrics. 
#Solution: install "weathermetrics" library and convert the units to nearest whole number
nyHarbor$ATMP <- celsius.to.fahrenheit(nyHarbor$ATMP, round = 0) 
nyHarbor$WTMP <- celsius.to.fahrenheit(nyHarbor$WTMP, round = 0)

sandyHook$ATMP <- celsius.to.fahrenheit(sandyHook$ATMP, round = 0) 
sandyHook$WTMP <- celsius.to.fahrenheit(sandyHook$WTMP, round = 0)


### CONVERT M/S TO MPH:

#Problem7: WSPD and GST is recorded in m/s, but my work is presented in America. I want my audience to understand the visuals and metrics quickly.
#Solution: "weathermetrics" library converts units to nearest whole number
nyHarbor$WSPD <- convert_wind_speed(nyHarbor$WSPD, old_metric = "mps", new_metric = "mph", round = 0)
nyHarbor$GST  <- convert_wind_speed(nyHarbor$GST, old_metric = "mps", new_metric = "mph", round = 0)

sandyHook$WSPD <- convert_wind_speed(sandyHook$WSPD, old_metric = "mps", new_metric = "mph", round = 0)
sandyHook$GST  <- convert_wind_speed(sandyHook$GST, old_metric = "mps", new_metric = "mph", round = 0)


### CONVERT METERS TO FEET

#Problem8: WVHT is recorded in m, but since I am presenting info in America, I want my audience to quickly understand the visuals and metrics.
#Solution: (not many people know that 1m=3.3ft) install "measurements" library and convert units
  #..only applicable to NY station because NJ station does not record WVHT
nyHarbor$WVHT <- conv_unit(nyHarbor$WVHT, "m", "ft")
```
practice::
conv_unit(2.54, "cm", "inch")  # Result = 1 in
conv_unit(3, "m", "ft")        # Result = 9.84252 ft

checking top 3 after converting whole nyHarbor dataframe::
conv_unit(0.91, "m", "ft") # = 2.985 ft
conv_unit(0.99, "m", "ft") # = 3.248 ft
conv_unit(1.05, "m", "ft") # = 3.444 ft

```{r}
### REPLACE MISSING VALUES WITH NA

#Problem9: NOAA uploaded missing values as a series of 9s which is confusing to work with
#Solution: replace all series of 9s with NA using na_if() and override dataframes
sandyHook <- sandyHook %>% mutate(WDIR=na_if(WDIR,999),WSPD=na_if(WSPD,99) , GST=na_if(GST,99),DPD=na_if(DPD,99),APD=na_if(APD,99), MWD=na_if(MWD,999),  ATMP=na_if(ATMP,999), WTMP=na_if(WTMP,999), WVHT=na_if(WVHT,99), PRES=na_if(PRES,9999), DEWP=na_if(DEWP,999) )

nyHarbor <- nyHarbor %>% mutate(WDIR=na_if(WDIR,999),WSPD=na_if(WSPD,99) , GST=na_if(GST,99),DPD=na_if(DPD,99),APD=na_if(APD,99), MWD=na_if(MWD,999),  ATMP=na_if(ATMP,999), WTMP=na_if(WTMP,999), WVHT=na_if(WVHT,99), PRES=na_if(PRES,9999), DEWP=na_if(DEWP,999) )

#glimpse(sandyHook)
#glimpse(nyHarbor)
```


FILTERED DATES NEEDED FOR SIMPLE ANALYSIS:

nyNORM <- filter(nyHarbor, Date >= as.Date("2012-10-22") & Date <= as.Date("2012-10-24"))
nySANDY <- filter(nyHarbor, Date >= as.Date("2012-10-29") & Date <= as.Date("2012-10-31"))

summary(nyNORM)
summary(nySANDY)


nySANDY %>% group_by(Date) %>% summarise(aveWaveHeight=mean(WVHT, na.rm=T)) %>%
  ggplot(aes(Date, aveWaveHeight)) +
  geom_line() +
  labs(title="Wave Height") +
  geom_smooth(method='lm')




```{r}
##---------WAVE HEIGHTS BEFORE AND DURING HURRICANE @ NY:

### REMOVING NAs FOR NY STATION:

#Problem9: about 84 total observations have really high wave heights that is obviously due to machine recording error.
#Solution: use na.omit(), but only for these WVHT graphs because I need to keep the 84 observations for other graphs below where readings were taken as normal
noNAsNY <- na.omit(nyHarbor)

filter(noNAsNY, Date >= as.Date("2012-10-21") & Date <= as.Date("2012-10-26")) %>% #grabbing 5 days worth of data. filter code reads like this: take all data from beginning of 10-21 to end of 10-25
ggplot(aes(Date, WVHT)) +
  geom_line(size=1, color="dodgerblue2") +
  theme_bw() +
  labs(title="Wave Height a Week BEFORE Sandy", subtitle = "NY Harbor Station 44065", y="wave height (ft)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.9: Normal wave heights observed exactly a week prior to Sandy. Heights fluctuated between 1ft and 4.5ft due to natural \n atmospheric and oceanic processes.") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) )  
ggsave(file="wavegraph1.png", width=10, height=6, dpi=300)

filter(noNAsNY, Date >= as.Date("2012-10-28") & Date <= as.Date("2012-11-02")) %>% 
  ggplot(aes(Date, WVHT)) +
  geom_line(size=1, color="dodgerblue2") +
  theme_bw() +
  labs(title="Wave Height DURING Sandy", subtitle = "NY Harbor Station 44065", y="wave height (ft)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.10: Maximum wave height was over 30ft tall when Sandy made landfall on October 30th, then quickly fell back to average \n height the following day. Comparatively this is more than 6x taller than the average male at 5.6ft. \n Note: 84 NA observations are not included.") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) )  +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4))
ggsave(file="wavegraph2.png", width=10, height=6, dpi=300)

#wave height (ft)

#filter(nyHarbor, Date >= as.Date("2012-11-04") & Date <= as.Date("2012-11-08")) %>% ggplot(aes(Date, WVHT)) + geom_line() + labs(title="Wave Height Week AFTER Sandy", y="wave height (m)")
```



filter(nyHarbor, Date >= as.Date("2012-10-14") & Date <= as.Date("2012-10-18")) %>%
ggplot(aes(Date, WVHT)) +
  geom_line() +
  labs(title="Wave Height 2 Weeks Before Sandy", y="wave height (m)", x="", caption = "normal ") +
  theme_classic()
  
  
## Not applicable to Sandy Buoy since that one does not record waveheight data!
  
```{r}
##---------WIND SPEED BEFORE AND DURING HURRICANE @ NY and NJ:

# @ NY::
filter(nyHarbor, Date >= as.Date("2012-10-21") & Date <= as.Date("2012-10-26")) %>%
ggplot(aes(Date, WSPD)) +
  geom_line(size=1, color="orchid4") +
  labs(title="Sustained Wind Speed a Week BEFORE Sandy", subtitle = "NY Harbor Station 44065", y="wind speed (mph)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.1: Sustained wind speeds in open ocean water exactly a week prior to Sandy. Speeds naturally fluctated between roughly \n 2mph and 25 mph due to complex atmospheric dynamics.") +
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),  plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5) )
ggsave(file="graph1.png", width=10, height=6, dpi=300)

filter(nyHarbor, Date >= as.Date("2012-10-28") & Date <= as.Date("2012-11-02")) %>% 
  ggplot(aes(Date, WSPD)) +
  geom_line(size=1, color="orchid4") +
  theme_bw() +
  labs(title="Sustained Wind Speed DURING Sandy", subtitle = "NY Harbor Station 44065", y="wind speed (mph)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.5: Sustained wind speeds in open water gradually increased the day before Sandy made landfall and doubled average \n readings during the height of the storm on October 30th. Maximum wind speed was about 55mph, then quickly dropped the \n following day.") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),  plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) ) 
ggsave(file="graph5.png", width=10, height=6, dpi=300)


# @ NJ::
filter(sandyHook, Date >= as.Date("2012-10-21") & Date <= as.Date("2012-10-26")) %>%
ggplot(aes(Date, WSPD)) +
  geom_line(size=0.65, color="orchid4") +
  labs(title="Sustained Wind Speed a Week BEFORE Sandy", subtitle = "Sandy Hook, NJ Station SDHN4", y="wind speed (mph)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.2: Sustained wind speeds in inland water exactly a week prior to Sandy. Speeds naturally fluctuated between about 2mph \n and 22 mph due to complex atmospheric dynamics interacting with the topography of the land. \n Note: Sandy Hook's buoy station takes readings every 5 minutes compared to NY Harbor's every hour. This explains the \n jagged look of the graphs.") +
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),  plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) ) 
ggsave(file="graph2.png", width=10, height=6, dpi=300)

filter(sandyHook, Date >= as.Date("2012-10-28") & Date <= as.Date("2012-11-02")) %>% 
  ggplot(aes(Date, WSPD)) +
  geom_line(size=0.65, color="orchid4") +
  theme_bw() +
  labs(title="Sustained Wind Speed DURING Sandy", subtitle = "Sandy Hook, NJ Station SDHN4", y="wind speed (mph)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.6: Sustained wind speeds in inland water steadily increased from the evening of October 29th and ceased at 11pm later \n that night. It can be assumed that Hurricane Sandy broke the recording instrument.") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),  plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) )
ggsave(file="graph6.png", width=10, height=6, dpi=300)
```
## Sandy hook recordings taken every 5 minutes whereas NY buoy station taken every hour!


```{r}
##---------GUST SPEED BEFORE AND DURING HURRICANE @ NY and NJ:

# @ NY::
filter(nyHarbor, Date >= as.Date("2012-10-21") & Date <= as.Date("2012-10-26")) %>%
ggplot(aes(Date, GST)) +
  geom_line(size=1, color="springgreen4") +
  labs(title="Gust Speed a Week BEFORE Sandy", subtitle = "NY Harbor Station 44065", y="gust speed (mph)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.3: Wind gust speeds in open water exactly a week prior to Sandy. Gust speeds are highly variable, but in this timeframe \n fairly normal speeds were recorded between 2mph and 35mph.") +
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) )  +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
ggsave(file="graph3.png", width=10, height=6, dpi=300)

filter(nyHarbor, Date >= as.Date("2012-10-28") & Date <= as.Date("2012-11-02")) %>% 
  ggplot(aes(Date, GST)) +
  geom_line(size=1, color="springgreen4") +
  theme_bw() +
  labs(title="Gust Speed DURING Sandy", subtitle = "NY Harbor Station 44065", y="gust speed (mph)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.7: Wind gusts steadily increased the day before Sandy made landfall. During the height of the storm on October 30th, the \n values doubled the average readings recorded from the week before. Maximum gust speed was 70mph, then quickly dropped \n the following day. ") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) )  +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
ggsave(file="graph7.png", width=10, height=6, dpi=300)


# @ NJ::
filter(sandyHook, Date >= as.Date("2012-10-21") & Date <= as.Date("2012-10-26")) %>%
ggplot(aes(Date, GST)) +
  geom_line(size=0.65, color="springgreen4") +
  labs(title="Gust Speed a Week BEFORE Sandy", subtitle = "Sandy Hook, NJ Station SDHN4", y="gust speed (mph)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.4: Wind gust speeds in inland water exactly a week prior to Sandy. Speeds naturally fluctuated between 2mph and \n 27mph. It just about matches the sustained wind speeds from the same timeframe.") +
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) ) 
ggsave(file="graph4.png", width=10, height=6, dpi=300)

filter(sandyHook, Date >= as.Date("2012-10-28") & Date <= as.Date("2012-11-02")) %>% 
  ggplot(aes(Date, GST)) +
  geom_line(size=0.65, color="springgreen4") +
  theme_bw() +
  labs(title="Gust Speed DURING Sandy", subtitle = "Sandy Hook, NJ Station SDHN4", y="gust speed (mph)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.8: Wind gusts steadily increased from late morning of October 29th and ceased at 11pm later that evening. It can be \n assumed that Hurricane Sandy broke the recording instrument.") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) ) 
ggsave(file="graph8.png", width=10, height=6, dpi=300)

```



```{r}
# sea level PRES @ NY::

#filter(nyHarbor, Date >= as.Date("2012-10-14") & Date <= as.Date("2012-10-18")) %>%
#  ggplot(aes(Date, PRES)) + geom_line() + labs(title = "2 weeks before just to test comparison")
filter(nyHarbor, Date >= as.Date("2012-10-21") & Date <= as.Date("2012-10-26")) %>%
ggplot(aes(Date, PRES)) +
  geom_line(size=1, color="tomato3") +
  labs(title="Sea Level Pressure a Week BEFORE Sandy", subtitle = "NY Harbor Station 44065", y="pressure (hPa)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.12: Normal sea level pressure observed exactly a week prior to Sandy. Although the graph looks as if a trend exists, it \n is simply due to the small time frame used. Atmospheric pressure fluctuates often due to complex atmospheric and oceanic \n processes.") +
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) ) 
ggsave(file="pressuregraph1.png", width=10, height=6, dpi=300)


filter(nyHarbor, Date >= as.Date("2012-10-28") & Date <= as.Date("2012-11-02")) %>% 
  ggplot(aes(Date, PRES)) +
  geom_line(size=1, color="tomato3") +
  theme_bw() +
  labs(title="Sea Level Pressure DURING Sandy", subtitle = "NY Harbor Station 44065", y="pressure (hPa)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.13: Sea level pressure readings were relatively normal until there was a sharp decline on October 30th. A sudden drop \n in pressure always indicates the presence of a storm-aka Superstorm Sandy.") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) ) 
ggsave(file="pressuregraph2.png", width=10, height=6, dpi=300)

```


```{r}
###------Extra exploration------###
# sea surface WTMP @ both?


# @ NY::
filter(nyHarbor, Date >= as.Date("2012-10-21") & Date <= as.Date("2012-10-26")) %>%
ggplot(aes(Date, WTMP)) +
  geom_line() +
  labs(title="Sea Surface Temperature Week BEFORE Sandy", subtitle = "NY Harbor Station 44065", y="temperature (F)", x="") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() ) 

filter(nyHarbor, Date >= as.Date("2012-10-28") & Date <= as.Date("2012-11-02")) %>% 
  ggplot(aes(Date, WTMP)) +
  geom_line() +
  theme_bw() +
  labs(title="Sea Surface Temperature DURING Sandy", subtitle = "NY Harbor Station 44065", y="temperature (F)", x="") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() ) 



# @ NJ::
filter(sandyHook, Date >= as.Date("2012-10-21") & Date <= as.Date("2012-10-26")) %>%
ggplot(aes(Date, WTMP)) +
  geom_line() +
  labs(title="Sea Surface Temperature Week BEFORE Sandy", subtitle = "Sandy Hook, NJ Station SDHN4", y="temperature (F)", x="") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() ) 

filter(sandyHook, Date >= as.Date("2012-10-28") & Date <= as.Date("2012-11-02")) %>% 
  ggplot(aes(Date, WTMP)) +
  geom_line() +
  theme_bw() +
  labs(title="Sea Surface Temperature DURING Sandy", subtitle = "Sandy Hook, NJ Station SDHN4", y="temperature (F)", x="") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() ) 
```


  
ggsave(file="wavegraph2.png", width=10, height=6, dpi=300)

```{r}
noNAsNY %>%
ggplot(aes(Date, WVHT)) +
  geom_line( color="dodgerblue2") +
  theme_bw() +
  labs(title="Recorded Wave Heights in 2012", subtitle = "NY Harbor Station 44065", y="wave height (ft)", x="", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.11: Wave heights for the entire year of 2012 showed the majority of wave formations were between \n 1ft and 10ft. There are a small handful of waves exceeding 10ft. Notice the +30ft wave later in the year \n indicating when Superstorm Sandy passed through.")  +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=16), plot.caption = element_text(hjust = 0) )
ggsave(file="waveheights2012.png", width=10, height=6, dpi=300)

```


===================================================================================================================================
===================================================================================================================================

```{r}
### PCA on NY HARBOR STATION 44065:

library(tidyverse)  #to use gather(if it's here) and a few other functions I barely touch
library(tidyr)      #technically contains gather(), but somehow was able to use that function without this library. Calling it in just case.
library(devtools)   #to install_github below
#install_github("kassambara/factoextra")
library(factoextra)
```

```{r}
#read in data
ny2012_ALL <- read.csv("NYbuoy_2012.txt", sep = "" , header = TRUE)
#get rid of first row
ny2012_ALL <- ny2012_ALL[-c(1), ]
#removing data I will not use
ny2012_ALL <- subset(ny2012_ALL, select = -c(X.YY:mm, VIS,TIDE) )

#converting columns to numeric
ny2012_ALL[, c(1:11)] <- sapply(ny2012_ALL[, c(1:11)], as.numeric)
#converting C to F:
#Air Temps
ny2012_ALL$ATMP <- celsius.to.fahrenheit(ny2012_ALL$ATMP, round = 0) 
#Water Temps
ny2012_ALL$WTMP <- celsius.to.fahrenheit(ny2012_ALL$WTMP, round = 0)

#changing 9s to NAs
ny2012_ALL <- ny2012_ALL %>% mutate(WDIR=na_if(WDIR,999),WSPD=na_if(WSPD,99) , GST=na_if(GST,99),DPD=na_if(DPD,99),APD=na_if(APD,99), MWD=na_if(MWD,999),  ATMP=na_if(ATMP,999), WTMP=na_if(WTMP,999), WVHT=na_if(WVHT,99), PRES=na_if(PRES,9999), DEWP=na_if(DEWP,999) )
  #summary(ny2012_ALL)

#set NAs to O to use in PCA:
ny2012_ALL[is.na(ny2012_ALL)] = 0
  #summary(ny2012_ALL)
```


```{r}
### PCA ALGORITHM IN ACTION:
pca1 <- princomp(ny2012_ALL, cor = TRUE, scores = TRUE)
pca1
```

```{r}
summary(pca1) 
pca1$loadings
```


```{r}
### ANALYZING AND INTERPRETING PRINCIPAL COMPONENTS OF SOCCER DATA ###

## Biplot:    is a pca score plot + loading plot all in one. Bottom axis: PC1 score, Left axis: PC2 score, Top axis: loadings on PC1, Right axis: loadings on PC2. In other words, the left and bottom axes are of the PCA plot — use them to read PCA scores of the samples (dots). The top and right axes belong to the loading plot — use them to read how strongly each characteristic (vector) influence the principal components.
      #Another way to think of it: a biplot plots the component's loadings and scores against the datapoints/samples
set.seed(18)
fviz_pca_biplot(pca1, repel=T)
```

```{r}
biplot(pca1) #the ugly, but simple way of graphing the above
```


```{r}
## Screeplot: (aka Elbow-curve) plots the eigenvalues found after running pca. Shows the relative importance of principal components. The variance of Comp.1 is largest (usually is), but the rest of the components are significant up until the elbow in the curve --> here it would be Comp.5/Comp.6. I'm sticking with Comp.6
    # Eigenvalues are a special set of scalars associated with a linear system of equations (i.e., a matrix equation) that are sometimes also known as characteristic roots, characteristic values, proper values, or latent roots. **Simply put, an eigenvector is a direction, such as "vertical" or "45 degrees", while an eigenvalue is a number telling you how much variance there is in the data in that direction. The eigenvector with the highest eigenvalue is, therefore, the first principal component.**


#Basic screeplot to get the job done---this plots component variance
screeplot(pca1, main = "Percent Variance of Principal Components") #this is the same as doing 'plot(pca)' 
  #screeplot(pca1, type="l", main = "Percent Variance of Principal Components")
```

```{r}
## Using facet_grid to plot the weights of top components and comparing them with each other.
  #Comp.1 mostly has the same sign (the usual) as they are share a common factor (mostly likely because these relate to player stats)


weightsofcomp <- data.frame( pca1$loadings[,1:4] ) #choosing the loadings that I want to graph from pca and converting(or ensuring) that it's a df
weightsofcomp$var_names <- row.names(weightsofcomp) #adding a column of the data variable names(aka the dimensions) so I can use them as the x-axis of the plot. We were able to see the variable names before, but could only SEE them not USE them.
  # row.name() = A data frame has (by definition) a vector of row names which has length the number of rows in the data frame, and contains neither missing nor duplicated values. Where a row names sequence has been added by the software to meet this requirement, they are regarded as ‘automatic’.
  #R documentation: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/row.names
loadings <- gather(weightsofcomp, "Comp", "Weight", -var_names) #gathering all columns for components and weights into their own corresponding columns, but not var_names since it only occupies 1 column. We are also creating names for the two column using ""...now this df has half as many columns as before, but 6 times as many rows --> this is what we graph with now
            # gather() = gather columns into key-value pairs. R-documentation: https://www.rdocumentation.org/packages/tidyr/versions/1.1.3/topics/gather

##Plots weights of the components##
ggplot(loadings, aes(x=var_names, y=Weight) ) +
  geom_bar(stat='identity', fill=NA, color="black") +
  facet_grid(Comp ~ ., scales='free_y') +
  #theme_dark() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )  +
  labs(x=""  , y="Component Loadings") + 
  theme(strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid")) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() ) #rids of the vertical lines in grid
```



```{r}
##---PCA with NY HARBOR SANDY DAYS:

#read in data
ny2012_SANDY <- read.csv("NYbuoy_2012.txt", sep = "" , header = TRUE)
#get rid of first row
ny2012_SANDY <- ny2012_SANDY[-c(1), ]
#merge and format date-time column
ny2012_SANDY  <- ny2012_SANDY  %>% mutate(Date=ymd_h(paste(X.YY,MM,DD,hh)))
#removing data I will not use
#filter to keep dates that I want (taking the 3 days instead of 5 because that was the height of the storm)
ny2012_SANDY <- filter(ny2012_SANDY, Date >= as.Date("2012-10-29") & Date <= as.Date("2012-11-02")) #will capture all of Oct 31, not Nov 1
ny2012_SANDY <- subset(ny2012_SANDY, select = -c(X.YY:mm, VIS,TIDE, Date) )


#converting columns to numeric
ny2012_SANDY[, c(1:11)] <- sapply(ny2012_SANDY[, c(1:11)], as.numeric)
#converting C to F:
#Air Temps
ny2012_SANDY$ATMP <- celsius.to.fahrenheit(ny2012_SANDY$ATMP, round = 0) 
#Water Temps
ny2012_SANDY$WTMP <- celsius.to.fahrenheit(ny2012_SANDY$WTMP, round = 0)

#changing 9s to NAs
ny2012_SANDY <- ny2012_SANDY %>% mutate(WDIR=na_if(WDIR,999),WSPD=na_if(WSPD,99) , GST=na_if(GST,99),DPD=na_if(DPD,99),APD=na_if(APD,99), MWD=na_if(MWD,999),  ATMP=na_if(ATMP,999), WTMP=na_if(WTMP,999), WVHT=na_if(WVHT,99), PRES=na_if(PRES,9999), DEWP=na_if(DEWP,999) )
  #summary(ny2012_SANDY)


#set NAs to O to use in PCA:
ny2012_SANDY[is.na(ny2012_SANDY)] = 0
  #summary(ny2012_SANDY)


```


```{r}
pca2 <- princomp(ny2012_SANDY, cor = TRUE, scores = TRUE)
pca2
```


```{r}
summary(pca2) 
pca2$loadings
```


```{r}
### ANALYZING AND INTERPRETING PRINCIPAL COMPONENTS...fig.15

## Biplot:
set.seed(8)
fviz_pca_biplot(pca2, repel=T, geom="point", col.var = "sienna3", ggtheme = theme_minimal(), title="PCA Biplot ", subtitle="NY Harbor Station 44065 - During Sandy", xlab="Comp.1 (67.7%)", ylab="Comp.2 (11.3%)")
#ggsave(file="pca1.png", width=10, height=6, dpi=300)


#, ggtheme = theme_gray(), col.var = "purple"
#title = "PCA - GB", xlab="PC1 (39%)", ylab="PC2 (22%)"
```
help sources: https://www.rdocumentation.org/packages/factoextra/versions/1.0.7/topics/fviz_pca
        http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining

http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

```{r}
biplot(pca2) #the ugly, but simple way of graphing the above
```


```{r}
#Basic screeplot (aka elbow-curve) to get the job done---this plots component variance...fig.14
screeplot(pca2, main = "Percent Variance of Principal Components")   #this is the same as doing 'plot(pca)' 
ggsave(file="pcatwo.png", width=10, height=6, dpi=300)
  #screeplot(pca2, type="l", main = "Percent Variance of Principal Components") 

```

```{r}
## Using facet_grid to plot the weights of top components and comparing them with each other...fig.16
  #Comp.1 mostly has the same sign (the usual) as they are share a common factor (mostly likely because these relate to player stats)
weightsofcomp <- data.frame( pca2$loadings[,1:3] ) 
weightsofcomp$var_names <- row.names(weightsofcomp) 
loadings <- gather(weightsofcomp, "Comp", "Weight", -var_names)


##Plots weights of the components##
ggplot(loadings, aes(x=var_names, y=Weight) ) +
  geom_bar(stat='identity', fill=NA, color="sienna3", size=0.6) +
  facet_grid(Comp ~ ., scales='free_y') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )  +
  labs(title = "Loadings of Top Principal Components", subtitle = "NY Harbor Station 44065", x="", y="loadings", caption = "Data Source: NOAA National Data Buoy Center © 2021 \n \n Fig.16: Weights (aka loadings) of the top three principle components. It is clear that the first component is sea \n level/atmospheric pressure (PRES), the second is wave direction from dominant wave period (MWD), and the third is \n dominant wave period (DPD). They account for 67.7%, 11.3%, and 9.5% of the data’s variance respectively.") +
  theme(strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid")) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.caption = element_text(hjust = 0) ) #rids of the vertical lines in grid
ggsave(file="pca3.png", width=10, height=6, dpi=300)

```
PCA help: https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff
          https://stackoverflow.com/questions/14716965/principal-component-analysis-label-of-component






