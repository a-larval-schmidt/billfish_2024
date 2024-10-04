#SSH data, altered from J. Scua Script
#read in the billfish data
library(lubridate)
library(rerddap)
library(rerddapXtracto)
library(ggplot2)
library(viridis)
library(dplyr)
library(mgcv)
library(lunar)
library(tidyverse) #✖ dplyr::filter()  masks stats::filter(), ✖ dplyr::select()  masks raster::select()

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
WHIP_Tows<-combo
WHIP_Tows$Year<-year(WHIP_Tows$Date)
WHIP_Tows$Month<-month(WHIP_Tows$Date)

Years<-unique(WHIP_Tows$Year)
Months<-unique(WHIP_Tows$Month)

Years<-Years[!is.na(Years)]
Months<-Months[!is.na(Months)]

WHIP_Tows<-WHIP_Tows[!is.na(WHIP_Tows$LONG_DD_start),]
WHIP_Tows<-WHIP_Tows[!is.na(WHIP_Tows$Date),]

ERDDAP_Node<-"https://coastwatch.noaa.gov/erddap/" ##"https://pae-paha.pacioos.hawaii.edu/erddap/griddap" #
dataset="noaacwBLENDEDsshDaily" # <-original one from JS script
swchlInfo <- rerddap::info(dataset, url=ERDDAP_Node)
WHIP_Tows<-filter(WHIP_Tows, Year>2016 )
SSH_Match <- rxtracto(swchlInfo, parameter = "sla",#'analysed_sst', 
                      xcoord = WHIP_Tows$LONG_DD_start, ycoord = WHIP_Tows$LAT_DD_start, tcoord = WHIP_Tows$Date, 
                      xlen = .2, ylen = .2, progress_bar = TRUE)


WHIP_Tows$SSH_Sat<-SSH_Match$`mean sla`#`mean analysed_SSH`


ggplot()+geom_point(data=WHIP_Tows, aes(x=LONG_DD_start, y=LAT_DD_start, colour=SSH_Sat))+scale_colour_viridis(option="G")


#read_in_ID_Data
#SWD_Larvae<-read.csv("C:/Swrd_Spawning_Movement/Larval_SWD_WHIP.csv", header=TRUE)
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
SWD_Ssh<-gam(SWD_PA~Year_Fac+Gear+offset(log10(vol.m3))+s(SSH_Sat, k=4)+s(WHIP_SWDs$Moon_Phase$phase, bs="cc",k=6), data=WHIP_SWDs, family=binomial)#+s(DOY, bs="cc", k=5)
summary(SWD_Ssh)
plot(SWD_Ssh, ylim=c(-2,2))

boxplot(WHIP_SWDs$SWD_Count~WHIP_SWDs$Month) #count values wrong
plot(WHIP_SWDs$SSH_Sat, WHIP_SWDs$SWD_PA)
plot(WHIP_SWDs$SSH_Sat, WHIP_SWDs$SWD_Count)

SWD_Count_Ssh<-gam(SWD_Count~Year_Fac+Gear+offset(log10(vol.m3))+s(SSH_Sat, k=4), data=WHIP_SWDs, family=tw)#+s(DOY, bs="cc", k=5)
summary(SWD_Count_Ssh)
plot(SWD_Count_Ssh)
