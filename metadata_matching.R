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
library(geosphere)

#data read in####
slick<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/TransectMetadata_Whitneyetal2020_SuppTableS6.csv")
slick_var<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/SlickVariables_Compile_2016-2017_100tran_20181003.csv")
whip<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/WHIP_TowMetadata_20220923 - WHIP_TowMetadata.csv")
length(unique(slick$sample))
length(unique(whip$Sample))
length(unique(whip$Station))
#join slick+slick var####
#prep slick
str(slick_var)

slick<-slick %>%
  mutate("Date"=mdy(Date)) %>%
  mutate("DateTime"=mdy_hm(DateTime)) %>%
  mutate("Time.start"= parse_time(str_pad(as.character(Time), 4, pad = "0"), "%H:%M"), .keep = "all")%>%
  rename("lat_start"=Latitude) %>%
  rename("long_start"=Longitude) %>%
  mutate("tow.depth.category"=ifelse(depth>-1,"surface","subsurface"),.keep="all")%>%
  mutate("Ship"=ifelse(Platform=="Ship (Sette)","Sette","Small Boat"))%>%
  mutate("day.night"="day")%>%
  unite("Station",c(Cruise,Transect,sample,Habitat),sep = "_", remove=F)%>%
  mutate("tow.duration"=8)%>%#from nature pub. Surface slick and ambient water neuston tows were conducted for ~8 min at a speed of ~4 km h−1.
  mutate("tow.speed.kts"=(4/1.852))%>%#from nature pub, converted to knots
  mutate("mesh"=ifelse(Platform== "Small Boat","335um","505um"))%>%
  mutate("gear"=ifelse(Platform== "Small Boat","1m straight-conical ring-net","6'IK"))%>%
  mutate("Sample"=sample, .keep="all")%>%
  unite("transect_sample_habitat", c(Cruise,Transect,sample,Habitat), sep="_", remove=F)

missing_list<-c("Time.end","LAT_DD_end","LONG_DD_end","LAT_DD_mid","LONG_DD_mid","Dist2Shore_km","distance","coord","end.lon","end.lat") 
slick[,missing_list] <- NA

#prep slick var##
str(slick_var)
slick_var <-slick_var %>%
  rename("Transect"=tran) %>%
  rename("Site"=site)

slick_var_short <-slick_var %>%dplyr::select(Transect, sample, temp.1m, sal.1m, fluo.1m, chla, chltot)#if you want to merge the full dataset, skip this next step with select()

#merge slick and slick_var##
slick_whole_short<-full_join(slick, slick_var_short, by = join_by(Transect, sample)) #join by transect number (not sample)
str(slick_whole_short)

#slick_whole_short<-slick_whole_short %>%mutate("Dist2Shore_km"=as.numeric(distance), .keep="unused")

#prep whip#####
str(whip)
length(unique(whip$Sample))
whip_ik<- whip %>%
  filter(str_detect(Tow.type, "6' IK|paravane"))
length(unique(whip_ik$Sample))
#make whip names consistent with slick names
whip_ik <-whip_ik %>%
  mutate("Habitat"=ifelse(slick== "Inside","Slick","Ambient"),.keep="unused")
  
whip_ik2 <-whip_ik %>%
  filter(depth !="midwater")%>%
  mutate("tow.depth.category"=depth, .keep="all")%>%
  mutate("Date"=mdy(Date)) %>%
  mutate("tow.length"=(Length_km*1000),.keep = "unused") %>%
  mutate("lat_start"=LAT_DD_start,.keep = "unused" ) %>%
  mutate("long_start"=LONG_DD_start,.keep = "unused") %>%
  mutate("dist.shore"=Dist2Shore_km*1000) %>%
  mutate("temp.1m"=coalesce(sst.mean.tsg, as.numeric(Surface.Temp)),.keep = "unused") %>%
  mutate("sal.1m"=coalesce(sss.mean.tsg, as.numeric(Surface.Salinity)),.keep = "unused") %>%
  mutate("Time"= parse_time(str_pad(as.character(Time.start), 4, pad = "0"), "%H%M"))%>%
  mutate("Time.end"= parse_time(str_pad(as.character(Time.end), 4, pad = "0"), "%H%M"))%>%
  mutate("sample"=as.numeric(Sample),.keep="all")%>% #from v2 to v3 sample was as.numeric(station) is now as.numeric(Sample)
  mutate("Station"=as.numeric(Station),.keep="all")%>%
  unite("transect_sample_habitat",c(Cruise,UID_tran,sample,Habitat),sep = "_", remove=F)%>%
  mutate("Gear"=gear) %>% 
  mutate("tow.speed.kts"=coalesce(tow.sog.kt,tow.sog.mean.scs.kt,speed.kts),.keep = "unused") %>%
  mutate("Site"=as.character(Sample)) %>%
  mutate("calc.speed.knts"=((tow.length/(tow.duration*60))*1.944))%>% #use dist.gps, duration (coverted to seconds)= m/s *1.944 to get speed in knts
  unite("DateTime.old",c(Date, Time),sep=" ",remove = F)

length(unique(whip_ik2$Sample))

whip_ik<-whip_ik2%>%
  mutate("tow.speed.kts"=coalesce(calc.speed.knts,tow.speed.kts),.keep = "unused") %>%
  mutate("DateTime"=ymd_hms(DateTime.old), .keep="unused")
  
#QC compare calculated to written speed values

#what parts of WHIP dataset do we really want? would it be easier to exclude the vales that we do not want?
whip_ik_short<-whip_ik%>%
  dplyr::select(!(c(direction, slick.og, slick.paper, slick.cromwell, slick.chck, 
             Processed.count, pair.ID, paravane, Tow.type,#(pull out notes elements then drop??)
             Fish.removed,Notes, #(keep for unabridged version, drop for public one)
             TL, LJFL,EFL,weight,eggs,flag,OBJECTID,Notes.joey,FLAG,Month,Day,
             Total.Fish.Larvae, Fish.eggs,Fish.eggs.large,remove.flag, UID_tran,
             paper.notes, Tow.type.1, Gear.paper))) #leave all iterations of lat, long in
#marmap dist2iso plus cam's map code for dist2shore#####
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
world<-ne_countries(scale="medium", returnclass = "sf")
oahu_raster <- raster(file.path("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/all_hi_bathy.tiff"))
oahu_bath<-as.bathy(oahu_raster)
oahu_df <- fortify(oahu_bath)
str(oahu_df) #x=long, y=lat
woah<-dist2isobath(oahu_bath, as.numeric(na.omit(whip_ik_short$long_start)), as.numeric(na.omit(whip_ik_short$lat_start)), isobath = -5)
woah<-woah %>%
  rename("long_start"=start.lon)%>%
  rename("lat_start"=start.lat) %>%
  unite("coord", c(long_start,lat_start), sep=",", remove=T)
whip_ik_short_dist <- whip_ik_short %>%
  unite("coord", c(long_start,lat_start), sep=",", remove=F)

whip_ik_short_dist<-full_join(woah,whip_ik_short_dist, by=join_by(coord),relationship = "many-to-many")
#need lat long from Jon for transects 53-59


whip_ik_short<-whip_ik_short_dist%>%
  mutate("chla"=NA) %>%
  mutate("chltot"=NA)%>%
  mutate("Platform"="ship")%>%
  mutate("mechanism"=NA) %>%
  mutate("Transect"=NA)%>%
  mutate("fluo.1m"=NA)#%>%
  #mutate("Dist2Shore_km"=coalesce(Dist2Shore_km,(distance/1000)))
str(whip_ik_short)

#DRE START HERE: join slick and whip#######
diff_list<-setdiff(colnames(whip_ik_short),colnames(slick_whole_short)) #in whip not in slick
diff_list
diff_list2<-setdiff(colnames(slick_whole_short),colnames(whip_ik_short)) #in slick not in whip
diff_list2
#this was bad because it set the existing column names to NA: whip_ik_short[,diff_list2] <- NA
#this was bad because it set the existing column names to NA: slick_whole_short[,diff_list] <- NA

combo_whip_slick_short<-rbind(slick_whole_short, whip_ik_short)
combo_whip_slick_short<-combo_whip_slick_short%>%
  mutate("is_slick"= replace(Habitat, "Inside" == "Slick", "Outside"== "Ambient")) #make Habitat values consistent (inside/ouside vs ambient/slick)

#continue to coalesce duplicates across new df#####
combo_whip_slick_short<-combo_whip_slick_short%>%
  dplyr::select(!c("distance","coord","end.lon","end.lat", "mechanism", "Dist2Shore_km", "gear", "Sample","is_slick"))
  
str(combo_whip_slick_short)

#write.csv(combo_whip_slick_short,"C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short3.csv",row.names = F)
#see about coalesceing these with whip once merged
#check for 1704 tsg data#####
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short2.csv")
#combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short3.csv")

summary(combo)
glimpse(combo)
#insight::export_table(summary(combo),format="html")

#map sampling sites
world<-ne_countries(scale="medium", returnclass = "sf")
#oahu_raster <- raster(file.path("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/all_hi_bathy.tiff"))
oahu_raster <- raster(file.path("C:/Users/Andrea.Schmidt/Desktop/for offline/billfish_hi_bathy.tiff")) #ETPO_2022(Bedrock) from https://www.ncei.noaa.gov/maps/bathymetry
oahu_df <- fortify(as.bathy(oahu_raster))
str(oahu_df)
oahu_map <- ggplot(data=world) +
  #coord_sf(crs = oahu_raster@crs,xlim=c(-161.5,-154.5),ylim=c(18.85,22.29)) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  #scale_fill_gradientn(colours = c(grey(50/100), grey(80/100)),values = rescale(c(min(oahu_df$z, na.rm = TRUE), 0,  0.0001, max(oahu_df$z, na.rm = TRUE))), na.value = "transparent",guide = "none")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,0))+new_scale_fill()+
  theme(panel.background=element_blank(), panel.grid.major.x=element_line())+
  theme_void()+geom_sf()+coord_sf(xlim=c(-152.5,-161.5), ylim=c(18.5, 22.5))#+
#theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+
#scale_x_continuous(breaks=seq(-155, -161, 1))+
#scale_y_continuous(breaks = seq(18.5,22.3,1))
oahu_map+geom_point(data=combo, aes(y=lat_start, x=long_start, color=Year))+scale_color_viridis_c(option="F")


#kona zoom
kona_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,0))+new_scale_fill()+
  theme(panel.background=element_blank(), panel.grid.major.x=element_line())+
  theme_void()+geom_sf()+coord_sf(xlim=c(-154.8,-158), ylim=c(18.8, 20.3))
kona_map+geom_point(data=combo, aes(y=lat_start, x=long_start, color=Year))+scale_color_viridis_c(option="F")

kona_map+geom_point(data=combo, aes(y=lat_start, x=long_start))
