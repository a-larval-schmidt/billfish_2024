#Xiphias sizes/counts/env
library(tidyverse)
library(lubridate)
bf<-read.csv("C:/github/billfish_2024/MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20241305 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240816.csv")
str(bf)
#doublechecks
summary(as.factor(bf$taxa)) #checks out with what is in the data sheet
bf %>%
  group_by(taxa)%>%
  summarize(sum(as.numeric(count)),na.rm=TRUE) #this is correct
#move lengths into long form
pivotcolumns<-ls(dplyr::select(bf, contains("mm"))) # this does NOT include lengths that are unknown
bff<- bf %>% 
  pivot_longer(all_of(pivotcolumns), names_to="lengthy",values_to="freq") %>%
  mutate(frequency=as.numeric(freq))%>%
  mutate_at(vars(frequency), ~replace(., is.na(.), 0))
bff %>%
  group_by(taxa)%>%
  summarize(sum(frequency),na.rm=TRUE) #this is seems correct, fewer counts than length measurements are available for
xg<-bf %>%
  filter(taxa=="Xiphias gladius")
xg$X100m<-NULL #taking out 100mm length individuals because they get confused with 10 since I'm using substr. If a clearner method to separate out length values is found 100mm should be added back in
#xgg<-xg %>%transform(useless = substr(lengthy, 1, 1), length_hist = substr(lengthy, 2, 3), l_unit=substr(4,6,stop=6))%>%mutate("length"=as.numeric(length_hist))%>%unique(xgg$vial)
minix<-xg%>%
  group_by(sample)%>%
  summarize(sum(count),na.rm=TRUE)
  
#plots###########
ggplot(data=xgg, aes(x=length, y=freq))+geom_point()
ggplot(data=xgg, aes(x=Year, y=count.sum))+geom_point()
#maps###########
library(readxl)
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
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
xg_env<-left_join(minix, combo, relationship="many-to-many",join_by(sample==Site))
#map sampling sites###
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
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-153,-161), ylim=c(18.7, 22.2))+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=17),
        legend.direction = "vertical", legend.box = "vertical", axis.text = element_text(size=20))+
  scale_x_continuous(breaks=seq((-161), (-153), 1))+
  scale_y_continuous(breaks = seq(18.7, 22.2,0.5))
oahu_map+geom_point(data=combo, aes(y=LAT_DD_start, x=LONG_DD_start, color=Year))+
  scale_color_viridis_c(option="F")+theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())

#kona zoom
kona_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,0))+new_scale_fill()+
  theme(panel.background=element_blank(),axis.title.x = "", axis.title.y = "")+#, panel.grid.major.x=element_line())+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-154.8,-157), ylim=c(18.8, 20.3))#coord_sf(xlim=c(-154.8,-158), ylim=c(18.8, 20.3))
kona_map+geom_point(data=combo, aes(y=LAT_DD_start, x=LONG_DD_start, color=Year))+scale_color_viridis_c(option="F")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(),legend.title=element_text(size=20),legend.text=element_text(size=17),
        legend.direction = "vertical", legend.box = "vertical", axis.text = element_text(size=20))+
  scale_x_continuous(breaks=seq((-158), (-154.8), 0.5))+
  scale_y_continuous(breaks = seq(18.8, 20.3,0.2))

kona_map+geom_point(data=xg_env, aes(y=LAT_DD_start, x=LONG_DD_start, color=temp.1m,size=(sum(count))))+
  facet_wrap(~Year)+
  #scale_color_viridis_c(option="F")
  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())

kona_map+geom_point(data=xg_env, aes(y=LAT_DD_start, x=LONG_DD_start, color=sal.1m,size=(sum(count))))+facet_wrap(~Year)+
  scale_fill_gradient(high = "purple4", low = "purple1")+new_scale_fill()
  #scale_color_viridis_c(option="plasma")

