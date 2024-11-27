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
#ERDDAP_Node<-"https://apdrc.soest.hawaii.edu/erddap/griddap/" #"https://upwell.pfeg.noaa.gov/erddap/griddap/" 
#dataset<-"hawaii_soest_082f_8f5f_78a3"
#swchlInfo <- rerddap::info(dataset, url=ERDDAP_Node)
#rain_Match <- rxtracto(swchlInfo, parameter = "precip", xcoord = WHIP_Tows$LONG_DD_start, ycoord = WHIP_Tows$LAT_DD_start, tcoord = WHIP_Tows$Date, xlen = .2, ylen = .2, progress_bar = TRUE)
#rain<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_precip_data.csv")
#2010-2013
data=("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/wrf_hi_e0b7_f9b3_e4bd_U1732662984409.nc") # from : https://upwell.pfeg.noaa.gov/erddap/griddap/wrf_hi.html
#other option for precipitation but could not downlaod:https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalPentadP05.html 
rain <-brick(data, varname = "rain") 
df<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/GLORYS_sal_matched_to_Site.csv")
combo<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_short16.csv")
mini<-combo%>%dplyr::select(c(LAT_DD_start,LONG_DD_start,Site))%>%
  filter(is.na(LONG_DD_start)==F)%>%
  filter(is.na(LAT_DD_start)==F)
#TAKES A LOOOOONNNNGGGG TIIIMMMEEEEE
mini$rain<- NA
for(i in 1:nlayers(rain)) {
  year=as.numeric(substr(names(rain[[i]]),2,5))
  month=as.numeric(substr(names(rain[[i]]),7,8))
  day=as.numeric(substr(names(rain[[i]]),10,11))
  ymd = as.Date(as.POSIXct(ymd(paste(year,month,day)), tz = "UTC"), tz = "UTC")
  idx <- which(mini$Date == ymd)
  if (length(idx)>0){
    
    pts <- SpatialPoints(mini[idx,c('LONG_DD_start', 'LAT_DD_start')], crs(rain))
    mini$rain[idx] <- raster::extract(rain[[i]], pts)
    
  }
  else if (length(idx)==0) {}
  
  print(paste("Completed",i,"of",nlayers(rain),"layers"))
  
}
minic<-mini%>%dplyr::select(c(Site, rain))%>%distinct(Site,.keep_all = T)
nrow(minic)
partial_rain<-full_join(combo, minic)
ggplot(partial_rain, aes(x=rain, y=sal.1m))+geom_point()
#2016-2018 rain
data=("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/wrf_hi_3eba_de0f_0ea8_U1732663267590.nc") 
mini$rain<- NA
for(i in 1:nlayers(rain)) {
  year=as.numeric(substr(names(rain[[i]]),2,5))
  month=as.numeric(substr(names(rain[[i]]),7,8))
  day=as.numeric(substr(names(rain[[i]]),10,11))
  ymd = as.Date(as.POSIXct(ymd(paste(year,month,day)), tz = "UTC"), tz = "UTC")
  idx <- which(mini$Date == ymd)
  if (length(idx)>0){
    
    pts <- SpatialPoints(mini[idx,c('LONG_DD_start', 'LAT_DD_start')], crs(rain))
    mini$rain[idx] <- raster::extract(rain[[i]], pts)
    
  }
  else if (length(idx)==0) {}
  
  print(paste("Completed",i,"of",nlayers(rain),"layers"))
  
}
minic<-mini%>%dplyr::select(c(Site, rain))%>%distinct(Site,.keep_all = T)
nrow(minic)
duh<-full_join(combo, minic)
ggplot(duh, aes(x=sal.1m, y=rain))+geom_point()

write.csv(duh, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_withRain.csv")

#old##########
rain<-rain%>%
  mutate("LONG_DD_start"=as.numeric(longitude)+360)%>%
  mutate("LAT_DD_start"=as.numeric(latitude))%>%
  mutate("datetime"=ymd_hms(time))
LonSamp=WHIP_Tows$LONG_DD_start
LatSamp=WHIP_Tows$LAT_DD_start
LonTemp=rain$LONG_DD_start
LatTemp=rain$LAT_DD_start
Temperature=rain$precip
#making location matrix for CTD collections
Temperature_Locations<-cbind(LonTemp, LatTemp)
#now identify the distance between each sample and all CTD locations
library(raster)
Nearest_Temp_Val<-NULL
len<-nrow(rain)
for (i in 1:length(LonSamp)){
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  Dist_Vals<-which(Dist_Vals>0)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<10000){#select the distance you want as your cut-off 
    Nearest_Temp_Val[i]<-Temperature[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=10000){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}
Nearest_Rain_Val<-Nearest_Temp_Val
WHIP_Tows<-WHIP_Tows%>%left_join(WHIP_Tows,rain, by=(c("LONG_DD_start","LAT_DD_start", "LAT_DD_end","LONG_DD_end","LAT_DD_mid","LONG_DD_mid")),relationship="many-to-many")
#WHIP_Tows$rain_Sat<-rain_Match$`mean precip`#`mean analysed_rain`
ggplot()+geom_point(data=WHIP_Tows, aes(x=LONG_DD_start, y=LAT_DD_start, colour=precip))+scale_colour_viridis(option="G")


#maps#####
mini_rds<-readRDS("mini_sal_GLORYS.rds")
library(suncalc)
library(measurements)
library(stringr)
library(geosphere)
library(sp) #already in raster
library(raster)# error
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
library(sf)
library(scatterpie)

#map sampling sites###
world<-ne_countries(scale="medium", returnclass = "sf")
#oahu_raster <- raster(file.path("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/all_hi_bathy.tiff"))
oahu_raster <- raster(file.path("C:/Users/Andrea.Schmidt/Desktop/for offline/billfish_hi_bathy.tiff")) #ETPO_2022(Bedrock) from https://www.ncei.noaa.gov/maps/bathymetry
oahu_df <- fortify(as.bathy(oahu_raster))
str(oahu_df)

#kona zoom
kona_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y#, fill = z
                                  )) +#labs(fill = "Depth (m)")+
  #scale_fill_gradient(high = "lightskyblue1", low = "lightskyblue2",limits=c(-6000,0))+new_scale_fill()+
  theme(panel.background=element_blank(),axis.title.x = "", axis.title.y = "")+#, panel.grid.major.x=element_line())+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-154.8,-157), ylim=c(18.8, 20.3))#coord_sf(xlim=c(-154.8,-158), ylim=c(18.8, 20.3))
kona_map+geom_point(data=combo, aes(y=LAT_DD_start, x=LONG_DD_start, color=Year))+scale_color_viridis_c(option="F")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(),legend.title=element_text(size=20),legend.text=element_text(size=17),
        legend.direction = "vertical", legend.box = "vertical", axis.text = element_text(size=20))+
  scale_x_continuous(breaks=seq((-158), (-154.8), 0.5))+
  scale_y_continuous(breaks = seq(18.8, 20.3,0.2))

kona_map+geom_point(data=rain, aes(y=LAT_DD_start, x=LONG_DD_start, color=precip))+#facet_wrap(~year(datetime))+scale_color_viridis_c(option="H")
kona_map+geom_point(data=mini_rds,aes(y=LAT_DD_start, x=LONG_DD_start, color=GLORYS_sal))+facet_wrap(~year(StartDateTime))+scale_color_viridis_c(option="H")

ggplot()+geom_point(data=rain, aes(y=LAT_DD_start, x=LONG_DD_start, color=precip))
ggplot()+geom_point(data=mini_rds, aes(y=LAT_DD_start, x=LONG_DD_start, color=GLORYS_sal))#+facet_grid(larv$taxa)
 