#salinity data from Jessie WITH rain data
#dataset continues to give 404 not found error >.<
#libraries#####
library(lubridate)
library(rerddap)
library(rerddapXtracto)
library(ggplot2)
library(viridis)
library(dplyr)
library(mgcv)
library(lunar)
library(tidyverse)
##BF data read in###############
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
str(combo)
comboo<-combo%>%
  unite("dend",c(Date,Time.end),sep=" ",remove = F)%>%
  unite("send",c(Date,Time.start),sep=" ", remove=F)
combooo<-comboo%>% #times in local
  mutate(EndDateTime=lubridate::as_datetime(dend, tz="HST"), .keep="unused")%>%
  mutate(StartDateTime=lubridate::as_datetime(send,tz="HST"), .keep="unused")%>%
  mutate("moon_date"=suncalc::getMoonIllumination(date = StartDateTime,keep = "phase"))%>%
  mutate("phase"=moon_date$phase)%>%
  mutate("cat_moon"=lunar::lunar.phase(x = StartDateTime, shift=10, name=T))
mini_combo<-combooo%>%dplyr::select(c(Site,EndDateTime, StartDateTime,LAT_DD_start,LONG_DD_start, phase, cat_moon))
mini2<-mini_combo%>%
  mutate(dur=lubridate::interval(start=StartDateTime, end=EndDateTime))%>%
  mutate(duration=as.numeric(dur, "minutes"))%>%
  filter(duration>0 & duration<60)
#jessie's salinity code#######
data=("C:/Users/Andrea.Schmidt/Desktop/for offline/cmems_mod_glo_phy_my_0.083deg_P1D-m_1727396375892.nc")
sal <-brick(data, varname = "so") # double check this is the actual variable name
NAvalue(sal) <- -9999 # for whatever reason, the "no value" grid cells were stored as NA rather than a numeric in the glorys dataset, so you have to reclassify these.
mini<-mini2%>%
  filter(is.na(LONG_DD_start)==F)%>%
  filter(is.na(LAT_DD_start)==F)

mini$GLORYS_sal <- NA

for(i in 1:nlayers(sal)) {
  
  # i = 291
  
  year=as.numeric(substr(names(sal[[i]]),2,5))
  month=as.numeric(substr(names(sal[[i]]),7,8))
  day=as.numeric(substr(names(sal[[i]]),10,11))
  ymd = as.Date(as.POSIXct(ymd(paste(year,month,day)), tz = "HST"), tz = "HST")
  
  idx <- which(as.Date(mini$StartDateTime) == ymd)
  
  if (length(idx)>0){
    
    pts <- SpatialPoints(mini[idx,c('LONG_DD_start', 'LAT_DD_start')], crs(sal))
    mini$GLORYS_sal[idx] <- raster::extract(sal[[i]], pts)
    
  }
  else if (length(idx)==0) {}
  
  print(paste("Completed",i,"of",nlayers(sal),"layers"))
  
}

saveRDS(mini, file = "mini_sal_GLORYS.rds")

#rain data#########
#map salinity against map/geography to see if signal is true vs artefact
#compare salinity to rain values, erdapp  wrf_hi, specify rain data (goes back to 2010 only)
#https://pae-paha.pacioos.hawaii.edu/erddap/griddap/wrf_hi.graph?rain%5B(2024-10-07T23:00:00Z)%5D%5B(20.495):(20.605)%5D%5B(-158.17):(-158.05)%5D&.draw=surface&.vars=longitude%7Clatitude%7Crain&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff

WHIP_Tows<-combo
WHIP_Tows$Year<-year(WHIP_Tows$Date)
WHIP_Tows$Month<-month(WHIP_Tows$Date)

Years<-unique(WHIP_Tows$Year)
Months<-unique(WHIP_Tows$Month)

Years<-Years[!is.na(Years)]
Months<-Months[!is.na(Months)]

WHIP_Tows<-WHIP_Tows[!is.na(WHIP_Tows$LONG_DD_start),]
WHIP_Tows<-WHIP_Tows[!is.na(WHIP_Tows$Date),]

#junk <- rerddapXtracto::GET("https://upwell.pfeg.noaa.gov/erddap/griddap/wrf_hi.nc?rain%5B(2010-05-115):1:(2018-12-08T00:00:00Z)%5D%5B(19.29692):1:(20.11283)%5D%5B(-155.8704):1:(-156.0757)%5D",write_disk("rain.nc", overwrite=TRUE))
#rain <-brick(junk, varname = "so") 

ERDDAP_Node<-"https://apdrc.soest.hawaii.edu/erddap/griddap/" #"https://upwell.pfeg.noaa.gov/erddap/griddap/" 
dataset<-"hawaii_soest_082f_8f5f_78a3"
swchlInfo <- rerddap::info(dataset, url=ERDDAP_Node)
rain_Match <- rxtracto(swchlInfo, parameter = "precip", 
                      xcoord = WHIP_Tows$LONG_DD_start, ycoord = WHIP_Tows$LAT_DD_start, tcoord = WHIP_Tows$Date, 
                      xlen = .2, ylen = .2, progress_bar = TRUE)


WHIP_Tows$rain_Sat<-rain_Match$`mean precip`#`mean analysed_rain`


ggplot()+geom_point(data=WHIP_Tows, aes(x=LONG_DD_start, y=LAT_DD_start, colour=rain_Sat ))+scale_colour_viridis()


#read_in_ID_Data
mas<-read.csv("C:/github/billfish_2024/MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240925 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240926.csv")
mas2<-mas%>% #still need to fix this as some values in unk. ist. are lost, some extra values in xiph
  mutate(dre_length=as.numeric(dre_length))%>%
  mutate(unknown.sizes=ifelse(is.na(dre_length)==T, 1,NA))%>% #takes care of dre_length v.s. unknown length only
  mutate(unknown.sizes=ifelse(is.na(rowSums(mas[,38:137]))==T|rowSums(mas[,38:137])==0,1,NA))
mas_long<-pivot_longer(mas2,X01mm:X100m, names_to="paper_length", values_to="length_occurence",values_drop_na = TRUE)
mas_clean<-mas_long%>%
  mutate("paper_length_num"=gsub("mm","",paper_length))%>%
  mutate("paper_length_num"=gsub("X","",paper_length_num))%>%
  mutate("paper_length_num"=gsub("m","",paper_length_num))%>%
  mutate("paper_length_num"=as.numeric(paper_length_num))%>%
  mutate("dre_length"=as.numeric(dre_length))
SWD_Larvae<-filter(mas_clean, taxa=="Xiphias gladius")
SDW_Larvae<-SWD_Larvae%>%filter(paper_length_num<11)

SWD_Larvae_by_Station<-SWD_Larvae %>%
  group_by(cruise, Site)%>%
  summarize(SWD_Count=sum(count))


WHIP_SWDs<-merge(WHIP_Tows, SWD_Larvae_by_Station, by.x=c("Cruise","Site"), by.y=c("cruise","Site"), all.x=TRUE)
WHIP_SWDs$SWD_PA<-WHIP_SWDs$SWD_Count
WHIP_SWDs$SWD_PA[is.na(WHIP_SWDs$SWD_PA)]<-0
WHIP_SWDs$SWD_PA[WHIP_SWDs$SWD_PA>0]<-1

WHIP_SWDs$Year_Fac<-as.factor(as.character(WHIP_SWDs$Year))

WHIP_SWDs$DOY<-yday(WHIP_SWDs$Date)
#WHIP_SWDs$Moon_Phase<-lunar.phase(WHIP_SWDs$DateTime)
WHIP_SWDs$Moon_Phase<-suncalc::getMoonIllumination(date = WHIP_SWDs$DateTime,keep = "phase")
SWD_rain<-gam(SWD_PA~Year_Fac+Gear+offset(log10(vol.m3))+s(rain_Sat, k=4)+s(WHIP_SWDs$Moon_Phase$phase, bs="cc",k=6), data=WHIP_SWDs, family=binomial)#+s(DOY, bs="cc", k=5)
summary(SWD_rain)
plot(SWD_rain, ylim=c(-2,2))

boxplot(WHIP_SWDs$SWD_Count~WHIP_SWDs$Month) #count values wrong
plot(WHIP_SWDs$rain_Sat, WHIP_SWDs$SWD_PA)
plot(WHIP_SWDs$rain_Sat, WHIP_SWDs$SWD_Count)

SWD_Count_rain<-gam(SWD_Count~Year_Fac+Gear+offset(log10(vol.m3))+s(rain_Sat, k=4), data=WHIP_SWDs, family=tw)#+s(DOY, bs="cc", k=5)
summary(SWD_Count_rain)
plot(SWD_Count_rain)
