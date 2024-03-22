#instructions######
#start here: TransectMetadata_Whitneyetal2020_SuppTableS6.csv# (slicks)
#addd in slick variables compile.csv
#filter to temp.1m, sal.1m, fluo.1m, chla and chltot (from bottle water samples)
#once all together
#merge WHIP_TowMetadata_20220923 with slicks: SE1704, SE1606, MP1812 
#pay attention to column names, they are different across sheets
#libraries####
library(tidyverse)
library(lubridate)
library(readxl)
library(suncalc)
library(measurements)
library(stringr)

#data read in####
slick<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/TransectMetadata_Whitneyetal2020_SuppTableS6.csv")
slick_var<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/SlickVariables_Compile_2016-2017_100tran_20181003.csv")
whip<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/WHIP_TowMetadata_20220923 - WHIP_TowMetadata.csv")

#join slick+slick var####
str(slick)
slick<-slick %>%
  mutate("Date"=mdy(Date)) %>%
  mutate("DateTime"=mdy_hm(DateTime)) %>%
  mutate("Time"= parse_time(str_pad(as.character(Time), 4, pad = "0"), "%H:%M"))
ggplot(slick, aes(x=Transect, y=tow.length, color=sample))+geom_point() #multiple samples/transect
goop<-group_by(slick, Transect, sample)

str(slick_var)
slick_var <-slick_var %>%
  rename("Transect"=tran) %>%
  rename("Site"=site)
#type (var) v.s. Habitat (main)? worth combining?

#if you want to merge the full dataset, skip this next step with select()
slick_var_short <-slick_var %>%
  select(Transect, sample, temp.1m, sal.1m, fluo.1m, chla, chltot)
slick_whole_short<-full_join(slick, slick_var_short)

#join slick and whip#####
str(slick_whole_short)
str(whip)
whip_ik<- whip %>%
  filter(str_detect(Tow.type, "6' IK|paravane"))
#make whip names consistent with slick names
whip_ik <-whip_ik %>%
  mutate("Date"=mdy(Date)) %>%
  mutate("tow.length"=(Length_km*1000)) %>%
  mutate("Habitat"=slick) %>%
  mutate("Latitude"=LAT_DD_start) %>%
  mutate("Longitude"=LONG_DD_start) %>%
  mutate("dist.shore"=Dist2Shore_km) %>%
  mutate("temp.1m"=as.numeric(Surface.Temp)) %>%
  mutate("sal.1m"=as.numeric(Surface.Salinity))%>%
  mutate("Time"= parse_time(str_pad(as.character(Time.start), 4, pad = "0"), "%H%M"))%>%
  mutate("Time.end"= parse_time(str_pad(as.character(Time.end), 4, pad = "0"), "%H%M"))%>%
  mutate("sample"=as.numeric(Station))%>%
  mutate("Gear"=Tow.type) %>%
  mutate("Site"=as.character(Sample))

whip_ik_short<-whip_ik%>%
  select(c(sample,Cruise,Year,Date,Time,Site,Latitude,Longitude,Habitat,Gear,vol.m3,tow.length,depth,dist.shore,temp.1m,sal.1m))
str(whip_ik_short)
slick_whole_short <- slick_whole_short %>%
  mutate("depth"=as.character(depth))

whip_slick_short<-left_join(slick_whole_short, whip_ik_short)
######
str(whip_slick_short)