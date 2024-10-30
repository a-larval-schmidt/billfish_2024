
#TO DO: analysis section, look for correlations####
##notes from meeting with justin##
#sept/nov no fish... july is where we start seeing uncertainty in this
#peak is at 25
#what processes the modelled surface salinity is really representing? To do this, you will want to decompose the salinity trends through time and see if there are broad patterns or anomalies that stand out (e.g. is there any periodicity or oscillations in salinity off Kona at seasonal, inter-annual, or decadal time scales)
##when does 27N reaches 24C and how does it affect the fishing fleet moving down
#adults like cooler water, need to balance their need for cool with larval need of warm, so not hot
#year as a factorto elimitate "timeseries effect"
#day of year and sst are correalted obiouvsly so don't inlude that
#lat/long does not have model importance for sst
#assuming linear relationship with things set as offsets 
#take log 10 of volume filtered
#early part of year (April) sees  
#lack of fish above 27deg, negative linear relationship
#mix of temp and seasonality but ly JS thinks its potential real
#TO DO!!!! lunar data with small (<10mm larvae)
#check cruise reports for sorting methodology

#libraries#########
library(tidyverse)
library(lubridate)
library(suncalc)
library(lunar)
library(raster)
library(ncdf4)
library(httr)
library(sp)


##data read in and sanity check#######
ex<-read.csv("C:/github/billfish_2024/billfish genetics - tissue extractions (2).csv")
mas<-read.csv("C:/github/billfish_2024/M1ASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240925 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240202.csv")
mas %>% # check that data display as they should/ are consistent with the MASTER google sheet, this is correct
  group_by(taxa)%>%
  summarize(sum(as.numeric(count)),na.rm=TRUE) 
summary(as.factor(mas$taxa)) #checks out with what is in the data sheet
exmas<-left_join(mas, ex, by="specimen_identification",relationship = "many-to-many")
exmas2<-exmas%>%
  mutate("spp_id"=gsub("Mn","Makaira nigricans",spp.id. ))%>%
  mutate("spp_id"=gsub("Ka","Kajikia audax",spp_id))%>%
  mutate("spp_id"=gsub("Xg","Xiphias gladius",spp_id))%>%
  mutate("spp_id"=gsub("Ta","Tetapterus angustrirostris",spp_id))%>%
  mutate("taxa2"=gsub("#N/A" ,"Unk.Istiophoridae",taxa))%>%
  mutate("taxa3"=dplyr::coalesce(taxa2,spp_id))
length(unique(mas$specimen_identification))
#number of unique extracted larvae
length(unique(ex$specimen_identification))
#number of unique in combined dataframe, should be equal to master sheet.... ex is a subset of entries in master sheet
length(unique(exmas2$specimen_identification))
#number of specimens per unique vial
exmas3<-exmas2[which(exmas2$specimen_identification %in% unique(exmas2$specimen_identification)),]
length(unique(exmas3$specimen_identification))
##metadatasheet as combo then subset to mini####
#add station number infomation for each cruise by matching start/end times of tows in combo to each cruise datasheet
#combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/Product1_LarvalBillfish_TowMetadata.csv")
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
str(combo)
comboo<-combo%>%
  unite("dend",c(Date,Time.end),sep=" ",remove = F)%>%
  unite("send",c(Date,Time.start),sep=" ", remove=F)
combooo<-comboo%>% #times in local
  mutate(EndDateTime=lubridate::as_datetime(dend, tz="HST"), .keep="unused")%>%
  mutate(StartDateTime=lubridate::as_datetime(send,tz="HST"), .keep="unused")
#with_tz() changes the time zone in which an instant is displayed. The clock time displayed for the instant changes, but the moment of time described remains the same.
#add column to d to start
mini<-combooo%>%dplyr::select(c(Site,EndDateTime, StartDateTime,LAT_DD_start,LONG_DD_start))
#merge TSG env data#####
tsg1<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/merged_tsgs_redo.csv")
tsg2<-tsg1 %>%
  mutate(datetime=as_datetime(datetime),.keep="unused")%>%
  mutate(date=ymd(Day),.keep="unused")%>%
  mutate(time=hms(str_replace_all(time,"[:alpha:]","")))%>%
  mutate(local_datetime=with_tz(datetime, "HST")) #view time as HST to match combo data
str(tsg2) #times in UTC; this includes 1999 to 2006
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
sub_9804<-select(e, c(colnames(tsg2)))
str(sub_9804)
tsg3<-rbind(tsg2,sub_9804)
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
sub_9703<-select(g, c(colnames(tsg3)))
tsg4<-rbind(tsg3,sub_9703)
str(tsg4)
#append 1704
sbe45<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/se1704_AllInclusiveContinuous.csv")
str(sbe45)
sbe46<-sbe45%>%
  unite("datetime",Date: Time, sep=" ", remove=F)%>%
  mutate(datetime=mdy_hms(datetime,tz="HST"))%>%#,format="%m/%d/%y %H:%M"))
  mutate(local_datetime=datetime, .keep="all")%>%
  mutate(date=date(datetime))%>%
  mutate(Year=year(datetime))%>%
  mutate(time=hm(Time))%>%
  mutate(TempC=as.numeric(SBE.45.Remote.Temperature))%>%
  mutate(Salinity=as.numeric(SBE.45.Salinity))%>%
  mutate(lat=as.numeric(Furuno.GP90_Latitude))%>%
  mutate(Day_Time_Julian=decimal_date(datetime))%>%
  mutate(lon=as.numeric(Furuno.GP90_Longitude ))
sbe47<-select(sbe46, c(colnames(tsg3)))
tsg5<-rbind(tsg4,sbe47)
tsg5<-filter(tsg5, Salinity<40)
tsg5<-filter(tsg5,30<Salinity)
rawva<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/combined_raw_files_2009_2011.csv")
rawva<-rawva%>%
  mutate(datetime=ymd_hms(datetime,tz="HST"))%>%#,format="%m/%d/%y %H:%M"))
  mutate(local_datetime=datetime, .keep="all")%>%
  mutate(date=date(datetime))%>%
  mutate(Year=year(datetime))%>%
  mutate(time=hm(datetime))%>%
  mutate(Day_Time_Julian=decimal_date(datetime))
tsg6<-rbind(tsg5,rawva)

#####add in 2011 data####
el<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/sept14_surface_1106.csv")
el<-el %>% rename("cruise"=V26, "date"= V25, "day_night"=V24, "lon_dd"=V22, 
                  "lat_dd"=V21,"mean_sal"=V6,"mean_temp"=V4,"dbar"=V1)
el2<-el%>%
  mutate(datetime=mdy_hm(V2,tz="HST"))%>%#,format="%m/%d/%y %H:%M"))
  mutate(local_datetime=datetime, .keep="all")%>%
  mutate(date=date(datetime))%>%
  mutate(Year=year(datetime))%>%
  mutate(time=hms(datetime))%>%
  mutate(TempC=as.numeric(mean_temp))%>%
  mutate(Salinity=as.numeric(mean_sal))%>%
  mutate(lat=as.numeric(lat_dd))%>%
  mutate(Day_Time_Julian=decimal_date(datetime))%>%
  mutate(lon=as.numeric(lon_dd))
str(el2)
el2<-select(el2, c(colnames(tsg3)))
tsg7<-rbind(tsg6,el2)
#####2016 env data####
sixteen<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/SE-16-06_MOA_Snapped_Compiled copy.csv")
str(sixteen)
meep<-sixteen%>%
  mutate(X=1)%>%
  mutate(date=lubridate::mdy(Date, tz="UTC"), .keep="unused")%>%
  mutate(time=lubridate::hms(Time), .keep="unused")%>%
  mutate(Button=as_factor(Button), .keep="unused")%>%
  mutate(TempC=as.numeric(SBE.45.Remote.Temperature))%>%
  mutate(Salinity=as.numeric(SBE.45.Salinity))%>%
  mutate(datetime=str_c(date,time,sep=" "))%>%
  mutate(local_datetime=mdy(datetime, tz="HST"), .keep="all")%>%
  mutate(Year=year(date))%>%
  mutate(Day_Time_Julian=decimal_date(date))%>%
  mutate(lon_degree=as.numeric(str_sub(Furuno.GP90_Longitude,1,3)),.keep="all")%>%
  mutate(lon_minute=as.numeric(str_sub(Furuno.GP90_Longitude,4,5)),.keep="all")%>%
  mutate(lon_seconds=as.numeric(str_sub(Furuno.GP90_Longitude,7,10),.keep="all"))%>%
  mutate(og_lon=as.numeric(str_replace(Furuno.GP90_Longitude,"[:alpha:]","")))%>%
  mutate(lon_dd=(-1*lon_degree)+(((lon_minute+lon_seconds)/3600)))%>%
  mutate(lat_degree=as.numeric(str_sub(Furuno.GP90_Latitude,1,2)),.keep="all")%>%
  mutate(lat_minute=as.numeric(str_sub(Furuno.GP90_Latitude,3,4)),.keep="all")%>%
  mutate(lat_seconds=as.numeric(str_sub(Furuno.GP90_Latitude,6,9),.keep="all"))%>%
  mutate(og_lat=as.numeric(str_replace(Furuno.GP90_Latitude,"[:alpha:]","")))%>%
  mutate(lat_dd=lat_degree+(lat_minute+lat_seconds)/3600)%>%
  mutate(LAT_DD_START=og_lat/100)%>%
  mutate(LON_DD_START=(-1*(og_lon/100))+0.1) #addition of 0.1 is arbitrary
meep2<-select(meep, c(colnames(tsg3)))
tsg8<-rbind(tsg7,meep2)


#time join TSG and mini, site join this to specimen data#######
mini_time_join1<-left_join(tsg8,mini,
                           join_by(local_datetime<=EndDateTime, local_datetime>=StartDateTime)) #join-by closest value
full_site_join<-left_join(combooo,mini_time_join1,
                          join_by(Year, Site, LAT_DD_start,LONG_DD_start,EndDateTime, StartDateTime),
                          relationship="many-to-many")
specimen_site_join<-left_join(exmas2,full_site_join,join_by(Year, Site))
full<-specimen_site_join%>%
  mutate(DateTime=mdy_hm(DateTime))%>%
  mutate(Time=hms(Time))%>%
  mutate(Time.end=hms(Time.end))%>%
  mutate("moon_date"=suncalc::getMoonIllumination(date = StartDateTime,keep = "phase"))%>%
  mutate("phase"=moon_date$phase)%>%
  mutate("cat_moon"=lunar::lunar.phase(x = StartDateTime, shift=10, name=T))%>%
  mutate(dur=lubridate::interval(start=StartDateTime, end=EndDateTime))%>%
  mutate(duration=as.numeric(dur, "minutes"))%>%#filter(duration>0 & duration<60)%>%
  mutate("temp_fix"=ifelse(is.na(temp.1m)==F,temp.1m,(ifelse(is.na(temp.1m)==T,TempC,NA))))%>%
  mutate("sal_fix"=ifelse(is.na(sal.1m)==F,sal.1m,(ifelse(is.na(sal.1m)==T,Salinity,NA))))%>%
  mutate("density"=count/vol.m3)

ggplot(full, aes(x=year(DateTime), y=sal_fix,color=Year))+geom_point()
ggplot(full, aes(x=Habitat, y=density, color=temp_fix))+facet_grid(~taxa3)+geom_point()


#prep modeled salinity data####
data=("C:/Users/Andrea.Schmidt/Desktop/for offline/cmems_mod_glo_phy_my_0.083deg_P1D-m_1727396375892.nc")
#data = ("C:/Users/jessica.perelman/Downloads/salinity.nc")
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
salty<-combooo%>%dplyr::select(c(LONG_DD_start,LAT_DD_start,sal.1m))
mini3<-left_join(mini, salty)
plot(mini$sal.1m, mini$GLORYS_sal)
plt(tsg7$Salinity,mini$GLORYS_sal)
cor(mini$sal.1m~mini$GLORYS_sal)
mini<-mini%>%
  pivot_longer(cols=c(sal.1m, GLORYS_sal), values_to = "salinity", names_to = "source")
ggplot(mini, aes(x=StartDateTime, y=salinity,color=source))+geom_point()

meansal<-cbind(as.vector(res),sal_dates)
colnames(meansal)<-c("modelled_salinity","date")
xg_env<-xg_env%>%mutate(date=date(StartDateTime))
wsal<-left_join(xg_env, meansal, by=date)
str(mini3)
ggplot(mini, aes(x=Site,y=TempC))+geom_point()+facet_wrap(year(mini$StartDateTime))
#alltsg+geom_point(data=combooo, aes(x=month(StartDateTime), y=temp.1m,color=temp.1m))
#test/sanity  check
length(unique(what$Site))
length(unique(mini$Site))
length(unique(d3$Site))
length(unique(what$Year))
length(unique(combooo$Site))# many stations did NOT have a match based on DateTime format(datetimes,format='%Y%m%d %H:%M')
length(unique(combooo$Year)) #474 stations in 6 years?

#salinity values from three sources combined########
salty<-combooo%>%dplyr::select(c(LONG_DD_start,LAT_DD_start,sal.1m))
mini<-left_join(mini, salty)
mini_co<-mini%>%
  mutate(sal_comb=coalesce(Salinity,GLORYS_sal))
ggplot(mini, aes(x=month(StartDateTime), y=GLORYS_sal,color=year(StartDateTime)))+geom_point()#+facet_grid(~year(StartDateTime))
larv_env<-left_join(combooo, mas_clean, relationship="many-to-many",join_by(Site))
ggplot(larv_env, aes(x=month(StartDateTime), y=GLORYS_sal,color=taxa))+geom_point()+facet_grid(~year(StartDateTime))

##lat/long join TSG and specimen data (mini) #######
#making sample locations
LonSamp=mini$LAT_DD_start
LatSamp=mini$LONG_DD_start
LonTemp=mini$LONG_DD_start #v1$dim[[1]]$vals #gives vector of longitude
LatTemp=mini$LONG_DD_start#v1$dim[[2]]$vals #gives vector of latitude
Salinity=mini$GLORYS_sal#res

#making location matrix for CTD collections
#Salinity_Locations<-cbind(LonTemp, LatTemp)

#now identify the distance between each sample and all CTD locations
library(raster)
Nearest_Sal_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Salinity[i]=mean(glorysal[,,mean(1:7823)],na.rm=TRUE)
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Salinity_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<1000000){#select the distance you want as your cut-off 
    Nearest_Sal_Val[i]<-Salinity[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=1000000){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}

LonSamp=mini$LAT_DD_start
LatSamp=mini$LONG_DD_start
LonTemp=el$lat_dd #v1$dim[[1]]$vals #gives vector of longitude
LatTemp=el$lon_dd#v1$dim[[2]]$vals #gives vector of latitude
Salinity=el$mean_sal#res
Salinity_Locations<-cbind(LonTemp, LatTemp)
#now identify the distance between each sample and all CTD locations
library(raster)
Nearest_Sal_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Salinity_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<10000){#select the distance you want as your cut-off 
    Nearest_Sal_Val[i]<-Salinity[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=10000){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}


kona_map+geom_point(data=meep, aes(y=alt_lat, x=alt_lon,color=SBE.45.Salinity))+#, shape=SWD_PA))+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())

LonSamp=mini$LAT_DD_start
LatSamp=mini$LONG_DD_start
LonTemp=meep$LAT_DD_start
LatTemp=meep$LONG_DD_start
Salinity=meep$SBE.45.Salinity
Salinity_Locations<-cbind(LonTemp, LatTemp)
Nearest_Sal_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Salinity_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<10000){#select the distance you want as your cut-off 
    Nearest_Sal_Val[i]<-Salinity[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=10000){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}
