#remotely sensed variables only
#libraries#####
library(lubridate)
library(rerddap)
library(rerddapXtracto)
library(ggplot2)
library(viridis)
library(dplyr)
library(mgcv)
library(lunar)
#prep combo11 for erdapp####
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
str(combo)
comboo<-combo%>%
  tidyr::unite("dend",c(Date,Time.end),sep=" ",remove = F)%>%tidyr::unite("send",c(Date,Time.start),sep=" ", remove=F)
combooo<-comboo%>% #times in local
  mutate(EndDateTime=lubridate::mdy_hms(dend, tz="HST"), .keep="unused")%>%
  mutate(StartDateTime=lubridate::mdy_hms(send,tz="HST"), .keep="unused")%>%
  mutate("moon_date"=suncalc::getMoonIllumination(date = StartDateTime,keep = "phase"))%>%
  mutate("phase"=moon_date$phase)%>%
  mutate("cat_moon"=lunar::lunar.phase(x = StartDateTime, shift=10, name=T))
WHIP_Tows<-combo
WHIP_Tows$Year<-year(WHIP_Tows$Date)
WHIP_Tows$Month<-month(WHIP_Tows$Date)

Years<-unique(WHIP_Tows$Year)
Months<-unique(WHIP_Tows$Month)

Years<-Years[!is.na(Years)]
Months<-Months[!is.na(Months)]

WHIP_Tows<-WHIP_Tows[!is.na(WHIP_Tows$LONG_DD_start),]
WHIP_Tows<-WHIP_Tows[!is.na(WHIP_Tows$Date),]

#chla####
ERDDAP_Node<-"https://comet.nefsc.noaa.gov/erddap/griddap/"
dataset="occci_V6_8day_4km" #Ocean Color, ESA CCI Ocean Colour Products (v6.0), Global 0.0417°, 8-Day, 1997-present 
swchlInfo <- rerddap::info(dataset, url=ERDDAP_Node)
chla_Match <- rxtracto(swchlInfo, parameter = "chlor_a",
                      xcoord = WHIP_Tows$LONG_DD_start, ycoord = WHIP_Tows$LAT_DD_start, tcoord = WHIP_Tows$Date, 
                      xlen = .2, ylen = .2, progress_bar = TRUE)

#old erdapp urls#####
##swchlInfo <- as.info("noaacrwsstDaily", url="https://coastwatch.noaa.gov/erddap/")
#"https://coastwatch.noaa.gov/erddap/" ##"https://pae-paha.pacioos.hawaii.edu/erddap/griddap" #