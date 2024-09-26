#Xiphias sizes/counts/env
library(tidyverse)
library(lubridate)
bf<-read.csv("C:/github/billfish_2024/MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240925 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240816.csv")
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
xg<-bff %>%
  filter(taxa=="Xiphias gladius")
xg$X100m<-NULL #taking out 100mm length individuals because they get confused with 10 since I'm using substr. If a clearner method to separate out length values is found 100mm should be added back in
xgg<-xg %>%transform(useless = substr(lengthy, 1, 1), length_hist = substr(lengthy, 2, 3), l_unit=substr(4,6,stop=6))%>%mutate("length"=as.numeric(length_hist))#%>%unique(xgg$vial)
xg%>%
  group_by(cruise)%>%
  summarize(sum(count),na.rm=TRUE) #why is this SO incorrect??
#partial environmental data#####
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")

xg_env<-left_join(xg, combo, relationship="many-to-many",join_by(Site))
#write.csv(xg_env,"C:/Users/Andrea.Schmidt/Desktop/for offline/xg_env.csv")
#merege TSG env data#####
df<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/merged_tsgs_redo.csv")
d<-df %>%
  mutate(datetime=as_datetime(datetime),.keep="unused")%>%
  mutate(date=ymd(Day),.keep="unused")%>%
  mutate(time=hms(str_replace_all(time,"[:alpha:]","")))%>%
  mutate(local_datetime=with_tz(datetime, "HST")) #view time as HST to match combo data
str(d) #times in UTC
#QC##
#append9703 and 9804
o9804<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/CTD_TC_9804 CTD Station Log.csv")
o9804[,1]<-NULL
colnames(o9804) <- c("DATE", "TIME_HST", "TSG.TEMP", "TSG.SALINITY", "CTD_Temp", "CTD_Salinity")#,"uncertain")#,"Temp2","Salinity","uncertain")
str(o9804)
e<-o9804%>%
  unite("datetime",DATE:TIME_HST, sep=" ", remove=F)%>%
  mutate(datetime=mdy_hm(datetime,tz="HST"))%>%#,format="%m/%d/%y %H:%M"))
  mutate(local_datetime=datetime, .keep="all")%>%
  mutate(Day_Time_Julian=decimal_date(datetime))%>%
  mutate(date=date(datetime))%>%
  mutate(Year=year(datetime))%>%
  mutate(time=hm(TIME_HST))%>%
  mutate(TempC=as.numeric(TSG.TEMP))%>%
  mutate(Salinity=as.numeric(TSG.SALINITY))%>%
  mutate(X=NA)
str(e)
f<-select(e, c(colnames(d)))
str(f)
d2<-rbind(d,f)
o9703<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/CTD_TC 9703 CTD Station Log Comparitive temp-sal.csv")
str(o9703)
g<-o9703%>%
  unite("datetime",DATE:TIME..LOCAL., sep=" ", remove=F)%>%
  mutate(datetime=mdy_hm(datetime,tz="HST"))%>%#,format="%m/%d/%y %H:%M"))
  mutate(local_datetime=datetime, .keep="all")%>%
  mutate(Day_Time_Julian=decimal_date(datetime))%>%
  mutate(date=date(datetime))%>%
  mutate(Year=year(datetime))%>%
  mutate(time=hm(TIME..LOCAL.))%>%
  mutate(TempC=as.numeric(TSG.TEMP))%>%
  mutate(Salinity=as.numeric(TSG.SALINITY))%>%
  mutate(X=CAST.NO.)
h<-select(g, c(colnames(d2)))
d3<-rbind(d2,h)

#add station number infomation for each cruise by matching start/end times of tows in combo to each cruise datasheet
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/Product1_LarvalBillfish_TowMetadata.csv")
#combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
str(combo)
comboo<-combo%>%
  unite("dend",c(Date,Time.end),sep=" ",remove = F)%>%
  unite("send",c(Date,Time.start),sep=" ", remove=F)
combooo<-comboo%>% #times in local
  mutate(EndDateTime=lubridate::as_datetime(dend, tz="HST"), .keep="unused")%>%
  mutate(StartDateTime=lubridate::as_datetime(send,tz="HST"), .keep="unused")
#with_tz() changes the time zone in which an instant is displayed. The clock time displayed for the instant changes, but the moment of time described remains the same.
#add column to d to start
mini<-select(combooo, c(Site,EndDateTime, StartDateTime))
str(mini)
mini2<-mini%>%
  mutate(dur=lubridate::interval(start=StartDateTime, end=EndDateTime))%>%
  mutate(duration=as.numeric(dur, "minutes"))
mini3<-filter(mini2, (duration>0 & duration<60))
ggplot(mini3, aes(x=StartDateTime, y=duration))+geom_point()
#take mean in d3 over tow time durations
d4<-filter(d3, Salinity<40)
d5<-filter(d4,30<Salinity)
#make a column in d3 to say T or F if value falls between combo's start/end times
mend<-mini$EndDateTime
mart<-mini$StartDateTime
d6<-d5%>%
  mutate(found = map_chr(
    .x = local_datetime,
    .f = ~ if_else(
      condition = any(.x > mini$StartDateTime & .x < mini$EndDateTime),
      true = "YES",
      false = NA_character_
    )
  ))
df7<-filter(d6, found=="YES")
length(df7$found)
#find where local_datetime==StartDateTime, average Sal and temp over number minutes in "duration"
#find mean salinity by station
#more than 1 value per time step, need mean before pulling these together

what<-left_join(d7,mini2,join_by(local_datetime<=EndDateTime, local_datetime>=StartDateTime)) #join-by closest value
str(what)
ggplot(what, aes(x=datetime,y=TempC))+geom_point()
#test
length(unique(what$Site))
length(unique(what$Year))
length(unique(combooo$Site))# many stations did NOT have a match based on DateTime format(datetimes,format='%Y%m%d %H:%M')
length(unique(combooo$Year)) #474 stations in 6 years?
check<-combooo%>%
  group_by(Year)%>%
  summarise(stations_per_year = n_distinct(Site))
ggplot(check, aes(x=Year,y=stations_per_year))+geom_point()

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

kona_map+geom_point(data=xg_env, aes(y=LAT_DD_start, x=LONG_DD_start, color=temp.1m))+#,size=(sum(count))))+
  facet_wrap(as.month(date))+
  #scale_color_viridis_c(option="F")
  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())

kona_map+geom_point(data=xg_env, aes(y=LAT_DD_start, x=LONG_DD_start, color=sal.1m,size=(sum(count))))+facet_wrap(~Year)+
  scale_fill_gradient(high = "purple4", low = "purple1")+new_scale_fill()
  #scale_color_viridis_c(option="plasma")
#####ncei????################
library(ncdf4)
library(httr)
library(tidyverse)
nc <- nc_open('C:/Users/Andrea.Schmidt/Documents/billfish_not_github/WTEE_20170406v10001.nc')
v1 <- nc$var[[13]]
dates<- as.POSIXlt(v1$dim[[1]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step
