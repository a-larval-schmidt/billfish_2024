#plotting
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
#libraries####
library(tidyverse)
library(lubridate)
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
#map sampling sites######
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



#mini<-combo_whip_slick_short%>%filter(lat_start>(19.4))%>%filter(lat_start<(19.7))%>%filter(long_start>(-155.95))%>%filter(long_start<(-155.7))
#kona_map+geom_label(data=mini, aes(y=lat_start, x=long_start, label=blah), alpha=0.2)

#species distributions#########
bf<-read.csv("C:/github/billfish_2024/MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20241305 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240202.csv")
str(bf)
#site=cruise-station
bf_env<-left_join(bf, combo, relationship="many-to-many",join_by(sample==Site))
bf_Genus_Level<- bf %>%
  dplyr::group_by(genus,sample,.drop=F) %>%
  dplyr::summarise(Total = sum(as.numeric(count), na.rm=T))
bf_env2<-left_join(bf_Genus_Level, combo, relationship="many-to-many",join_by(sample==Site))
#jet.colors <- colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

kona_map+geom_point(data=bf_env2, aes(y=LAT_DD_start, x=LONG_DD_start, color=sal.1m,size=Total))+facet_wrap(~genus)+
  #scale_color_viridis_c(option="F")+
  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")+theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())
genera<-unique(bf_env2$genus)

#scattermap#############
#https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html
#mini_df in proper format
bf_taxa<- bf %>%
  dplyr::group_by(taxa,sample,.drop=F) %>%
  summarise(n = n())%>%
  mutate(freq = n / sum(n), na.rm=T)
bf_taxa2<-filter(bf_taxa, taxa!="")
taxab<-unique(bf_taxa2$taxa)
bf_taxa3<-bf_taxa2%>%mutate(taxa_nsp=gsub(" ","",taxa))
bf_taxa_long<-bf_taxa3%>%
  pivot_wider(names_from=taxa_nsp, values_from = freq)
bf_env3<-left_join(bf_taxa_long, combo, relationship="many-to-many",join_by(sample==Site))

kona_map+geom_scatterpie(data=bf_env3,aes(y=LAT_DD_start, x=LONG_DD_start),
                         cols=c("Kajikia audax","Makaira nigricans","Tetrapturus angustirostris","Unk.Istiophoridae","Xiphias gladius"),color=NA)#+
  #coord_equal()+facet_wrap(~Year)
#bf_env4<-bf_env3[!with(bf_env3,is.na(`Kajikia audax`)& is.na(`Makaira nigricans`)& is.na(`Tetrapturus angustirostris`)&is.na(`Unk.Istiophoridae`)& is.na(`Xiphias gladius`))]
bf_env4<-bf_env3%>%filter(drop_na(across(bf_env3[2:7,])))
#bf_env4<-bf_env3%>%drop_na(`Kajikia audax`)
#bf_env4<-bf_env3%>%drop_na(`Makaira nigricans`)
bf_env4<-bf_env3%>%drop_na(`Tetrapturus angustirostris`)
bf_env4<-bf_env4%>%drop_na(`Unk.Istiophoridae`)
bf_env4<-bf_env4%>%drop_na(`Xiphias gladius`)

ggplot()+geom_scatterpie(data=bf_env3,aes(y=LAT_DD_start, x=LONG_DD_start),
                         cols = c(Kajikiaaudax,Makairanigricans,Unk.Istiophoridae,Xiphiasgladius,Tetrapturusangustirostris))+
  coord_equal()
#code from justin###############
for(i in 1: length(genera)){ 
  Genus_count<-bf_Genus_Level[bf_Genus_Level$genus==genera[i],]
  png(paste0(genera[i],"_bf_year_Plots.png"), height=5, width=7, units="in", res=300)#temp.1m
  p<-kona_map+geom_point(data=bf_env2, aes(y=LAT_DD_start, x=LONG_DD_start,size=Total,color=sal.1m))+facet_wrap(~Year)+theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())+  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")
  
  #p<-ggplot() +geom_point( data=Genus_count,aes(x=LONG_DD_start, y=LAT_DD_start, size = Total, color = Year))+ geom_sf(data = world)+coord_sf(xlim = c(-180, -152), ylim = c(18,32))+scale_color_viridis()+ylab("Latitude")+xlab("Longitude")+ggtitle(genera[i])
  print(p)
  dev.off()}
#statistics#######
#woah<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/dist2isobath_whip.csv")
ggplot(bf_env2, aes(y=Total, x=temp.1m, color=genus))+geom_point()
ggplot(bf_env2, aes(y=Total, x=sal.1m, color=genus))+geom_point()
ggplot(bf_env2, aes(y=Total, x=chla, color=genus))+geom_point()
ggplot(bf_env2, aes(y=Total, x=fluo.1m, color=genus))+geom_point()
ggplot(bf_env2, aes(y=Total, x=chltot, color=genus))+geom_point()

model1<-mgcv::gam(data=larvah, uku_present1_absent0~sampling_style+cruise_number+
                    s(this_sal, k=4)+
                    s(log(Dist2Shore_m),k=4)+
                    offset(log10(volume)),family=binomial) #for month could add bs="cc" creates cyclic spline to connect dots at the end; might need
mod1_param_no<-5
summary(model) 
anova(model) #sampling style highly sig for model 1, sal and distance to shore are absoltely not. same for model 2; 3 overfit of no relation?
qqnorm(resid(model)) #model1,2, kind of okay
gam.check(model)
hist(resid(model))
#model 1,2 suggestive of normal, need more binss?
plot(resid(model)~fitted(model))#model1 nice
plot(cooks.distance(model),type="h") 
#main concern is spatial autocorrelation of residuals
#simply plot on a map
#residual autocorrelation, update this with bf specifica things###################
oahu_map <- ggplot(data=world) +
  #coord_sf(crs = oahu_raster@crs,xlim=c(-161.5,-154.5),ylim=c(18.85,22.29)) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  #scale_fill_gradientn(colours = c(grey(50/100), grey(80/100)),values = rescale(c(min(oahu_df$z, na.rm = TRUE), 0,  0.0001, max(oahu_df$z, na.rm = TRUE))), na.value = "transparent",guide = "none")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,0))+new_scale_fill()+
  #theme(panel.background=element_blank(), panel.grid.major.x=element_line())+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-154.9,-161.2), ylim=c(19, 22.2))

df2<-tibble(resid(model7))
larvahh<-filter(larvah,is.na(larvah$volume)==F)
larvahh<-filter(larvahh,is.na(larvahh$this_temp)==F)
larvahhh<-larvahh %>% mutate(model2_resids=add_column(df2))
larvahhh$latitude_start_dd <- unlist(larvahhh$latitude_start_dd)
larvahhh$longitude_start_dd <- unlist(larvahhh$longitude_start_dd)
larvahhh$model2_resids<-unlist(larvahhh$model2_resids)
model.assesment.colors <- colorRampPalette(c("blue", "white", "red"))
c<-model.assesment.colors(10)
oahu_map+geom_point(data=larvahhh, mapping=aes(y=latitude_start_dd,x=longitude_start_dd,
                                               color=model2_resids, size=3))+
  scale_color_gradientn(colours = c)+labs(color = "Model residuals")+
  theme(legend.direction = 'horizontal', legend.position = "bottom")

