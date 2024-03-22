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


# to automate means from sette CSVS########################
#***on 9/14/22 Andrea went back though this and commented out lines that took
#*means of all considered depths and instead allowed the script to create an 
#*amalgam of all depths per each CSV. Andrea also removed the <40m depth filter step
#*finally, she commented out the old write.csv and changed it to have updated file names
#* this was done to allow for only surface temps to be utilized in the analysis
#* across all data sources
library(tidyverse)
library(lubridate)
library(readxl)
library(suncalc)
library(measurements)
###for 12_06 csvs#######
oes1206<-c("CTD001.XLS", "CTD002.XLS", "CTD003.XLS","CTD004.XLS","CTD005.XLS","CTD006.XLS",
           "CTD007.XLS","CTD008.XLS","CTD009.XLS","CTD010.XLS","CTD011.XLS","CTD012.XLS","CTD013.XLS")
setwd("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/CTD/CTD Log Sheets")

oes1206_function=function(input) {
  df<-read_excel(input,skip=4)
  index=which(oes1206==input)
  lat=as.character(df[9,2])
  long=as.character(df[10,2])
  date=as.character(df[6,2])
  cast=as.character(df[8,2])
  cruise=as.character(df[5,2])
  df2<-read_excel(input,skip=18)
  top40<-filter(df2, is.na(`Time  (UTC)`)==F)
  top40<-add_column(top40, date=date)
  top40<-unite(top40, col="date_time", c(date, `Time  (UTC)`), sep=" ")
  date_time<-mdy_hm(top40$date_time)
  #grep("Hawaii", OlsonNames(), value=TRUE)
  date_time<-with_tz(date_time, tz="US/Hawaii")
  forsun<-as.Date(date_time[1])
  sunlist<-getSunlightTimes(date=forsun, lat=21.315603, lon=	-157.858093, tz="US/Hawaii")
  day_night<-ifelse((date_time[1]<=sunlist$sunrise)==T,"night","day")
  day_night
  cols.num <- c("Pressure   (dbars)","Salinity","Temperature    (C)","DO", 
                "Fluoro", "Bottle", "Macro Ctrl-r","...10","...4","...6","...8")
  top40[cols.num] <- sapply(top40[cols.num],as.numeric)
  #top40<-as_tibble(filter(top40, `Pressure   (dbars)`<40))
  #top40_means<-top40 %>% summarise_at(vars(`Pressure   (dbars)`, `Temperature    (C)`, Salinity), c(min,max,mean))
  #top40_means<-top40_means %>% rename("min_pressure"=`Pressure   (dbars)_fn1`,"max_pressure"=`Pressure   (dbars)_fn2`,"mean_pressure"=`Pressure   (dbars)_fn3`,"min_sal"=Salinity_fn1,"max_sal"=Salinity_fn2,"mean_sal"=Salinity_fn3, "min_temp"=`Temperature    (C)_fn1`,"max_temp"=`Temperature    (C)_fn2`,"mean_temp"=`Temperature    (C)_fn3`)
  #newdf<-add_column(top40_means, lat=lat)
  newdf<-add_column(top40, lat=lat)
  newdf<-newdf %>% separate(lat, into = c("lat_deg","lat_minsec", "lat_dir"), sep=" ", remove=T)  #sept14th: V15,V16,V17
  newdf$lat_deg<-as.numeric(newdf$lat_deg)
  newdf$lat_minsec<-as.numeric(newdf$lat_minsec)
  newdf<-add_column(newdf,long=long)
  newdf<-newdf %>% separate(long, into = c("long_deg","long_minsec", "long_dir"), sep=" ",remove=T) #sept14th: V18,V19,V20
  newdf$long_deg<-as.numeric(newdf$long_deg)
  newdf$long_minsec<-as.numeric(newdf$long_minsec)
  newdf<-newdf %>% mutate(lat_dd=lat_deg+(lat_minsec/60)) #sept14th: V21
  newdf<-newdf %>% mutate(lon_dd=(long_deg+(long_minsec/60))*-1) #sept14th: V22
  newdf<-add_column(newdf,cast=cast) #sept14th: V23
  newdf<-add_column(newdf,day_night=day_night) #sept14th: V24
  newdf<-add_column(newdf,date=date) #sept14th: V25
  newdf<-add_column(newdf,cruise=cruise) #sept14th:V26
  str(newdf)
  #outname = paste("SE12_06",input, "_top40summary",'.csv', sep = "")
  outname = paste("SE12_06",input, "_sept14_alldepths",'.csv', sep = "")
  write.csv(x=newdf, file=outname)
}
for(i in 1:length(oes1206)) {
  oes1206_function(oes1206[i])
}

#new1206<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/CTD/CTD Log Sheets", pattern=".csv")
new1206<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/CTD/CTD Log Sheets/all_depths", pattern=".csv")

binding_fn<-function(input) {
  df2<-read.csv(input[1], header=T)
  top40df<-as.data.frame(matrix(NA, ncol = ncol(df2), nrow = length(input)))
  for(i in 1:length(input)){
    df<-read.csv(input[i], header=T)
    top40df[i,]<-df[1,]
  }
  print(top40df)
  #write.csv(x=top40df, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/top40df_1206.csv")
  write.csv(x=top40df, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/sept14_surface_1206.csv")
}
setwd("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/CTD/CTD Log Sheets/all_depths")
binding_fn(new1206)

tw<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/sept14_surface_1206.csv")
str(tw)
#tw <-tw %>% rename("cruise"=V26, "date"= V25, "day_night"=V24,"cast"=V23, "lon_dd"=V22, "lat_dd"=V21, "long_deg"=V18,"long_minsec"=V19, "long_dir"=V20,"lat_deg"=V15,"lat_minsec"=V16, "lat_dir"=V17, "Fluoro"=V10, "DO"=V8,"bottle"=V11,"depth"=V12,"trash"=V14,"salinity"=V6,"temperature"=V4,"Pressure   (dbars)"=V3)


###for 11_06 csvs##############
oes1106_function=function(input) {
  df<-read_excel(input,skip=4)
  index=which(oes1106==input)
  lat=as.character(df[9,2])
  long=as.character(df[10,2])
  date=as.character(df[6,2])
  cast=as.character(df[7,2])
  cruise=as.character(df[5,2])
  df2<-read_excel(input,skip=18)
  top40<-filter(df2, is.na(`Time  (UTC)`)==F) #not actually top40, just wanted to keep names consistent between 1206 and 1106 for ease of writing a new function
  top40<-add_column(top40, date=date)
  top40<-unite(top40, col="date_time", c(date, `Time  (UTC)`), sep=" ")
  date_time<-mdy_hm(top40$date_time)
  date_time<-with_tz(date_time, tz="US/Hawaii")
  forsun<-as.Date(date_time[1])
  sunlist<-getSunlightTimes(date=forsun, lat=21.315603, lon=	-157.858093, tz="US/Hawaii")
  day_night<-ifelse((date_time[1]<=sunlist$sunrise)==T,"night","day")
  day_night
  cols.num <- c("Pressure   (dbars)","Salinity","Temperature    (C)","DO","Fluoro", "Bottle", "Depth", "Macro Ctrl-r","...10","...4","...6","...8")
  top40[cols.num] <- sapply(top40[cols.num],as.numeric)
  #top40<-as_tibble(filter(top40, `Pressure   (dbars)`<40))
  #top40_means<-top40 %>% summarise_at(vars(`Pressure   (dbars)`, `Temperature    (C)`, Salinity), c(min,max,mean))
  #top40_means<-top40_means %>% rename("min_pressure"=`Pressure   (dbars)_fn1`,"max_pressure"=`Pressure   (dbars)_fn2`,"mean_pressure"=`Pressure   (dbars)_fn3`,"min_sal"=Salinity_fn1,"max_sal"=Salinity_fn2,"mean_sal"=Salinity_fn3,"min_temp"=`Temperature    (C)_fn1`,"max_temp"=`Temperature    (C)_fn2`,"mean_temp"=`Temperature    (C)_fn3`)
  #newdf<-add_column(top40_means, lat=lat)
  newdf<-add_column(top40, lat=lat)
  newdf<-newdf %>% separate(lat, into = c("lat_deg","lat_minsec", "lat_dir"), sep=" ", remove=T)
  newdf$lat_deg<-as.numeric(newdf$lat_deg)
  newdf$lat_minsec<-as.numeric(newdf$lat_minsec)
  newdf<-add_column(newdf,long=long)
  newdf<-newdf %>% separate(long, into = c("long_deg","long_minsec", "long_dir"), sep=" ",remove=T)
  newdf$long_deg<-as.numeric(newdf$long_deg)
  newdf$long_minsec<-as.numeric(newdf$long_minsec)
  newdf<-newdf %>% mutate(lat_dd=lat_deg+(lat_minsec/60))
  newdf<-newdf %>% mutate(lon_dd=(long_deg+(long_minsec/60))*-1)
  newdf<-add_column(newdf,cast=cast)
  newdf<-add_column(newdf,day_night=day_night)
  newdf<-add_column(newdf,date=date)
  newdf<-add_column(newdf,cruise=cruise)
  str(newdf)
  #outname = paste("SE11_06",input, "_top40summary",'.csv', sep = "")
  outname = paste("SE11_06",input, "_sept14_alldepths",'.csv', sep = "")
  write.csv(x=newdf, file=outname)
}
oes1106<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/Logs/CTDCastLogs/XLS", pattern=".XLS")
setwd("~/oes1106_1206/OS11-06/Logs/CTDCastLogs/XLS")
for(i in 1:length(oes1106)) {
  oes1106_function(oes1106[i])
}
#new1106<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/Logs/CTDCastLogs", pattern=".csv")
new1106<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/Logs/CTDCastLogs/XLS", pattern=".csv")
#setwd("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/Logs/CTDCastLogs")

binding_fn2<-function(input) {
  df2<-read.csv(input[1], header=T)
  top40df<-as.data.frame(matrix(NA, ncol = ncol(df2), nrow = length(input)))
  for(i in 1:length(input)){
    df<-read.csv(input[i], header=T)
    top40df[i,]<-df[1,]
  }
  print(top40df)
  #write.csv(x=top40df, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/summarydf_1106.csv")
  write.csv(x=top40df, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/sept14_surface_1106.csv")
}
binding_fn2(new1106)
el<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/sept14_surface_1106.csv")
#this is from before when means of top and bottom temps were recorded
#el<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/summarydf_1106.csv")
#str(el)
#el$X<-NULL
#el<-el %>% rename("cruise"=V22, "date"= V21, "day_night"=V20, "lon_dd"=V18, "lat_dd"=V17,"long_deg"=V14,"long_minsec"=V15, "long_dir"=V16,"lat_deg"=V11,"lat_minsec"=V12, "lat_dir"=V13,"min_pressure"=V2,"max_pressure"=V5,"mean_pressure"=V8,"min_sal"=V4,"max_sal"=V7,"mean_sal"=V10, "min_temp"=V3,"max_temp"=V6,"mean_temp"=V9)
#write.csv(x=el, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/summarydf_1106_named.csv")

###for tc32, cruise from 1967####
tc32<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/tc32/PIFSC_PIRO_bulk_data_download_InPort_5809/PIFSC_PIRO_bulk_data_download_InPort_5809/data/data/RB004AA1.csv")
str(tc32)
tc32<-tc32 %>% mutate(pa_uku=ifelse(STATION_NUMBER==3, 1, 0)) #26 64
tc32$pa_uku[tc32$STATION_NUMBER==26]<-1
tc32$pa_uku[tc32$STATION_NUMBER==64]<-1
ggplot(tc32, aes(x=TEMPERATURE_1, y=pa_uku))+geom_point(size=log(tc32$DEPTH_25DEG_ISOTHERM))
#Townsend Cromwell_32station8_1954
tc32<-tc32 %>% mutate(VESSEL_CODE="Townsend Cromwell") #just to make it match with weird formatting in main uku_pa sheet on GoogleSheets
tc32 <- tc32 %>% unite(sample_record_identifier, c(VESSEL_CODE,CRUISE_NUMBER,STATION_NUMBER,START_TIME), sep="_", remove=F)
tc32$sample_record_identifier[tc32$STATION_NUMBER==64]<-"USNM 322279"#64
tc32$sample_record_identifier[tc32$STATION_NUMBER==3]<-"USNM 322280" #3
tc32$sample_record_identifier[tc32$STATION_NUMBER==26]<-"USNM 322281" #26

#stopped here because no concrete depth data are associated with the temperatures shared in these data
#Dre input these by hand to the Relevant_Aprion_virescens_literature GoogleSheet many months ago, using 
#the surface temp as max_temp, surface salinity (as it was all that was available)
#CTD to SAMPLE LOCATION MATCH from Justin####
#making pseudo sample locations
LonSamp=sample(-125: -110, 20, replace=T)
LatSamp=sample(-5:30, 20, replace=T)

#making pseudo CTD locations and temp vals
LonTemp=sample(-125: -110, 5, replace=T)
LatTemp=sample(-5:30, 5, replace=T)
Temperature=sample(20:34, 5, replace=T)

#making location matrix for CTD collections
Temperature_Locations<-cbind(LonTemp, LatTemp)

#now identify the distance between each sample and all CTD locations
library(raster)
Nearest_Temp_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<1000000){#select the distance you want as your cut-off 
    Nearest_Temp_Val[i]<-Temperature[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=1000000){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}


##Andrea matching OES1206/1106 with CTD####
library(tidyverse)

##2012 sample locations####
#pa<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/stranges_pa_mocid_to_towid_means_aug5retry.csv")
pa<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/Relevant_Aprion_virescens_literature - presence_absence_sept22.csv")
tw_pa<-filter(pa, cruise_number=="12_06")
##checking tow distances
dist_1206<-as.numeric(tw_pa$sampling_distance_km_per_net_from_time)
dist_1206<-dist_1206*1000 #to convert from km to m
mean_samp_dist<-(mean(dist_1206))
LonSamp=(tw_pa$longitude_start_dd)
LatSamp=(tw_pa$latitude_start_dd)


# CTD locations and temp vals
#the following 7 lines were commented out because they took up the top 40m of the water column
#tw<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/top40df_1206.csv")
#summary order:min, max mean
#library(tidyverse)
#tww<-tw %>% rename("min_pressure"=V2,"max_pressure"=V5,"mean_pressure"=V8,"min_sal"=V4,"max_sal"=V7,"mean_sal"=V10,"min_temp"=V3,"max_temp"=V6,"mean_temp"=V9)
#LonTemp=(tw$V18)
#LatTemp=(tw$V17)
#Temperature=(tw$V9)
#this new csv is surface values only
tw<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/sept14_surface_1206.csv")
tw <-tw %>% rename("cruise"=V26, "date"= V25, "day_night"=V24,"cast"=V23, "lon_dd"=V22, "lat_dd"=V21, "long_deg"=V18,"long_minsec"=V19, "long_dir"=V20,"lat_deg"=V15,"lat_minsec"=V16, "lat_dir"=V17, "Fluoro"=V10, "DO"=V8,"bottle"=V11,"depth"=V12,"trash"=V14,"salinity"=V6,"temperature"=V4,"Pressure   (dbars)"=V3)
LonTemp=(tw$lon_dd)
LatTemp=(tw$lat_dd)
Temperature=(tw$temperature)

#making location matrix for CTD collections
Temperature_Locations<-cbind(LonTemp, LatTemp)


#now identify the distance between each sample and all CTD locations
library(raster)
Nearest_Temp_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<mean_samp_dist){#select the distance you want as your cut-off 
    Nearest_Temp_Val[i]<-Temperature[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=mean_samp_dist){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}

Nearest_Temp_Val

#2012 salinity##
Salinity=(tw$salinity)
#left "Temperature_Locations" the same name since it works for sal too here
Nearest_Sal_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<mean_samp_dist){#select the distance you want as your cut-off 
    Nearest_Sal_Val[i]<-Salinity[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=mean_samp_dist){
    Nearest_Sal_Val[i]<-NA#assign NA if too far away
  }
}
Nearest_Sal_Val
matched_1206<-tibble(tw_pa$sample_record_identifier, Nearest_Temp_Val,Nearest_Sal_Val, tw_pa$uku_present1_absent0)




##2011 sample locations####
#pa<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/stranges_pa_mocid_to_towid_means_aug5retry.csv")
pa<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/Relevant_Aprion_virescens_literature - presence_absence_sept22.csv")
el_pa<-filter(pa, cruise_number=="11_06")
##checking tow distances
dist_1106<-as.numeric(el_pa$sampling_distance_km_per_net_from_time)
dist_1106<-dist_1106*1000
mean_samp_dist<-(mean(dist_1106))

LonSamp=(el_pa$longitude_start_dd)
LatSamp=(el_pa$latitude_start_dd)

#CTD locations and temp vals
#el<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/summarydf_1106_named.csv")
el<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/sept14_surface_1106.csv")
el<-el %>% rename("cruise"=V26, "date"= V25, "day_night"=V24,"cast"=V23, "lon_dd"=V22, "lat_dd"=V21, "long_deg"=V18,"long_minsec"=V19, "long_dir"=V20,"lat_deg"=V15,"lat_minsec"=V16, "lat_dir"=V17, "Fluoro"=V10, "DO"=V8,"bottle"=V11,"depth"=V12,"trash"=V14,"salinity"=V6,"temperature"=V4,"Pressure   (dbars)"=V3)

LonTemp=(el$lon_dd)
LatTemp=(el$lat_dd)
Temperature=(el$temperature) #change to "mean_temp" if using top40 csv

#making location matrix for CTD collections
Temperature_Locations<-cbind(LonTemp, LatTemp)

#now identify the distance between each sample and all CTD locations
library(raster)
Nearest_Temp_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<mean_samp_dist){#select the distance you want as your cut-off 
    Nearest_Temp_Val[i]<-Temperature[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=mean_samp_dist){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}
Nearest_Temp_Val

Salinity=(el$salinity) #change to "mean_sal" if using top40 csv
#left "Temperature_Locations" the same name since it works for sal too here
Nearest_Sal_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<mean_samp_dist){#select the distance you want as your cut-off 
    Nearest_Sal_Val[i]<-Salinity[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=mean_samp_dist){
    Nearest_Sal_Val[i]<-NA#assign NA if too far away
  }
}

matched_1106<-tibble(el_pa$sample_record_identifier, Nearest_Temp_Val,Nearest_Sal_Val, el_pa$uku_present1_absent0)

###for tc32, cruise from 1967####
#originally Dre input these by hand to the Relevant_Aprion_virescens_literature GoogleSheet many months ago, using 
#the surface temp as max_temp, surface salinity (as it was all that was available)
#to ensure maximum reproducibility the following has been done
tc32<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/tc32/PIFSC_PIRO_bulk_data_download_InPort_5809/PIFSC_PIRO_bulk_data_download_InPort_5809/data/data/RB004AA1.csv")
str(tc32)
tc32<-tc32 %>% mutate(pa_uku=ifelse(STATION_NUMBER==3, 1, 0)) #26 64
tc32$pa_uku[tc32$STATION_NUMBER==26]<-1
tc32$pa_uku[tc32$STATION_NUMBER==64]<-1
#Townsend Cromwell_32station8_1954
tc32<-tc32 %>% mutate(VESSEL_CODE="Townsend Cromwell") #just to make it match with weird formatting in main uku_pa sheet on GoogleSheets
tc32 <- tc32 %>% unite(sample_record_identifier, c(VESSEL_CODE,CRUISE_NUMBER,STATION_NUMBER,START_TIME), sep="_", remove=F)
tc32$sample_record_identifier[tc32$STATION_NUMBER==64]<-"USNM 322279"#64
tc32$sample_record_identifier[tc32$STATION_NUMBER==3]<-"USNM 322280" #3
tc32$sample_record_identifier[tc32$STATION_NUMBER==26]<-"USNM 322281" #26
matched_tc32<-tibble(tc32$sample_record_identifier,tc32$SURFACE_TEMPERATURE,tc32$SURFACE_SALINITY, tc32$pa_uku)
library(lubridate)
library(tidyverse)
tc32<-tc32 %>% mutate(END_TIME=(as.character(END_TIME)))
tc32<-tc32 %>% mutate(END_TIMEd=ifelse(nchar(END_TIME)==3,(gsub("([0-9])([0-9])([0-9])", "\\1:\\2\\3", END_TIME)),(gsub("([0-9])([0-9])([0-9])([0-9])", "\\1\\2:\\3\\4", END_TIME))))
tc32<-tc32 %>% mutate(START_TIMEd=ifelse(nchar(START_TIME)==3,(gsub("([0-9])([0-9])([0-9])", "\\1:\\2\\3", START_TIME)),(gsub("([0-9])([0-9])([0-9])([0-9])", "\\1\\2:\\3\\4", START_TIME))))
tc32<-tc32 %>% unite("END_DATE", c(YEAR, MONTH, DAY,END_TIMEd), sep="-", remove=F)
tc32<-tc32 %>% unite("START_DATE", c(YEAR, MONTH, DAY,START_TIMEd), sep="-", remove=F)
tc32<-mutate(tc32,END_TIME=ymd_hm(tc32$END_DATE))
tc32<-mutate(tc32,START_TIME=ymd_hm(START_DATE)) 
tc32<-tc32 %>% mutate(total_tow_time=END_TIME-START_TIME)
tc32<-tc32 %>% mutate(total_tow_time=ifelse(total_tow_time<0, total_tow_time+24,total_tow_time)) #roughly 6 hours each tow
#for this datastream, speed listed in knots on InPort but all are NAs so will use the following:                     
#for tows in 1984 Ship speed was adjusted over tow speeds of about 0.9-1.1 m/second (source: GUID: gov.noaa.nmfs.inport:8792)
SPEED=2.5 #in knots; for an approximate result, divide the speed value by 1.944
tc32<-tc32 %>% dplyr::mutate(total_tow_distance=(tc32$total_tow_time)*3600*(SPEED/1.944)) #convert hours to seconds, then multiply by speed (1 m/s) to get distance value in meters
#cobb net area:from mfr27102.pdf,"Scuba divers observed during a shallow tow that the net opening was about 12 m wide by8 m high." equals 96m^2
tc32<-tc32 %>% dplyr::mutate(total_tow_volume=(tc32$total_tow_distance)*96)
#most tows were 21600m
dist_tc32<-21600

###matched tibbles#####
m11<- setNames(matched_1106, paste0("o", seq_along(matched_1106)))
df11<-data.frame("sample_record_identifier"=m11$o1,"Nearest_Temp_Val"=m11$o2,"Nearest_Sal_Val"=m11$o3, "pa_uku"=m11$o4)
m12<-setNames(matched_1206, paste0("o", seq_along(matched_1206)))
df12<-data.frame("sample_record_identifier"=m12$o1,"Nearest_Temp_Val"=m12$o2,"Nearest_Sal_Val"=m12$o3, "pa_uku"=m12$o4)
m32<-setNames(matched_tc32, paste0("o", seq_along(matched_tc32)))
df32<-data.frame("sample_record_identifier"=m32$o1,"Nearest_Temp_Val"=m32$o2,"Nearest_Sal_Val"=m32$o3, "pa_uku"=m32$o4)

matched_all<-base::rbind(df11, df12, df32)
matched_all
#write.csv(x=matched_all, file="C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/matched_oes11_12_tc32.csv")
#on sept 14th 2022 Andrea resaved matched_all as matchy so as to avoid changing the SEPTEMBER STATS script
#write.csv(x=matched_all, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/matched_all.csv")
###plotting matched tibbles#
ggplot(matched_all, aes(x=Nearest_Temp_Val))+geom_histogram(aes(fill=as.factor(pa_uku)))+scale_fill_manual(values=c('grey',"black"))+theme_minimal()
ggplot(matched_all, aes(x=Nearest_Sal_Val))+geom_histogram(aes(fill=as.factor(pa_uku)))+scale_fill_manual(values=c('lavender',"purple"))+theme_minimal()
ggplot(tc32, aes(x=TEMPERATURE_1, y=pa_uku))+geom_point(size=log(tc32$DEPTH_25DEG_ISOTHERM))
ggplot(matched_1206, aes(y=Nearest_Temp_Val, x=`tw_pa$uku_present1_absent0`))+geom_point()
ggplot(matched_1106, aes(y=Nearest_Temp_Val, x=`el_pa$uku_present1_absent0`))+geom_point()

#From uku project: adding in older/other larvae#####
larv<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/Relevant_Aprion_virescens_literature - larval_records.csv")
larv<-filter(larv, sampling_method!="MOCNESS")
larv<-filter(larv, region=="Main Hawaiian Islands")
larv<-filter(larv, life_stage=="larv")
mini_larv<-tibble(larv$sample_record_identifier,larv$length_mm, larv$collection_depth_m_max, larv$collection_day, larv$collection_month, larv$collection_year )
mini_larv<-dplyr::rename(mini_larv, sample_id= "larv$sample_record_identifier", length_mm="larv$length_mm", collection_depth_m_max="larv$collection_depth_m_max")
mini_larv<-mutate(mini_larv, length_mm=as.numeric(length_mm))
mini_larv<-mutate(mini_larv, collection_depth_m_max=as.numeric(collection_depth_m_max))
sizes2<-full_join(mini_larv, sizes)
sizes2<-right_join(joey, sizes2,by="sample_id")
#fix very nearshore points from Kahe Environmental Impact Survey
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-029")]<-797.04
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-030")]<-68.56
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-031")]<-988.11
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-032")]<-63.11
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-033")]<-63.11
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-034")]<-797.04
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-036")]<-160.50
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-035")]<-797.04
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-037")]<-797.04
ll<-tibble(sizes2$LAT_DD, sizes2$LON_DD, sizes2$LAT_DD_JL, sizes2$LON_DD_JL, sizes2$length_mm)
# to automate means from sette CSVS########################
#***on 9/14/22 Andrea went back though this and commented out lines that took
#*means of all considered depths and instead allowed the script to create an 
#*amalgam of all depths per each CSV. Andrea also removed the <40m depth filter step
#*finally, she commented out the old write.csv and changed it to have updated file names
#* this was done to allow for only surface temps to be utilized in the analysis
#* across all data sources
library(tidyverse)
library(lubridate)
library(readxl)
library(suncalc)
library(measurements)
###for 12_06 csvs#######
oes1206<-c("CTD001.XLS", "CTD002.XLS", "CTD003.XLS","CTD004.XLS","CTD005.XLS","CTD006.XLS",
           "CTD007.XLS","CTD008.XLS","CTD009.XLS","CTD010.XLS","CTD011.XLS","CTD012.XLS","CTD013.XLS")
setwd("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/CTD/CTD Log Sheets")

oes1206_function=function(input) {
  df<-read_excel(input,skip=4)
  index=which(oes1206==input)
  lat=as.character(df[9,2])
  long=as.character(df[10,2])
  date=as.character(df[6,2])
  cast=as.character(df[8,2])
  cruise=as.character(df[5,2])
  df2<-read_excel(input,skip=18)
  top40<-filter(df2, is.na(`Time  (UTC)`)==F)
  top40<-add_column(top40, date=date)
  top40<-unite(top40, col="date_time", c(date, `Time  (UTC)`), sep=" ")
  date_time<-mdy_hm(top40$date_time)
  #grep("Hawaii", OlsonNames(), value=TRUE)
  date_time<-with_tz(date_time, tz="US/Hawaii")
  forsun<-as.Date(date_time[1])
  sunlist<-getSunlightTimes(date=forsun, lat=21.315603, lon=	-157.858093, tz="US/Hawaii")
  day_night<-ifelse((date_time[1]<=sunlist$sunrise)==T,"night","day")
  day_night
  cols.num <- c("Pressure   (dbars)","Salinity","Temperature    (C)","DO", 
                "Fluoro", "Bottle", "Macro Ctrl-r","...10","...4","...6","...8")
  top40[cols.num] <- sapply(top40[cols.num],as.numeric)
  #top40<-as_tibble(filter(top40, `Pressure   (dbars)`<40))
  #top40_means<-top40 %>% summarise_at(vars(`Pressure   (dbars)`, `Temperature    (C)`, Salinity), c(min,max,mean))
  #top40_means<-top40_means %>% rename("min_pressure"=`Pressure   (dbars)_fn1`,"max_pressure"=`Pressure   (dbars)_fn2`,"mean_pressure"=`Pressure   (dbars)_fn3`,"min_sal"=Salinity_fn1,"max_sal"=Salinity_fn2,"mean_sal"=Salinity_fn3, "min_temp"=`Temperature    (C)_fn1`,"max_temp"=`Temperature    (C)_fn2`,"mean_temp"=`Temperature    (C)_fn3`)
  #newdf<-add_column(top40_means, lat=lat)
  newdf<-add_column(top40, lat=lat)
  newdf<-newdf %>% separate(lat, into = c("lat_deg","lat_minsec", "lat_dir"), sep=" ", remove=T)  #sept14th: V15,V16,V17
  newdf$lat_deg<-as.numeric(newdf$lat_deg)
  newdf$lat_minsec<-as.numeric(newdf$lat_minsec)
  newdf<-add_column(newdf,long=long)
  newdf<-newdf %>% separate(long, into = c("long_deg","long_minsec", "long_dir"), sep=" ",remove=T) #sept14th: V18,V19,V20
  newdf$long_deg<-as.numeric(newdf$long_deg)
  newdf$long_minsec<-as.numeric(newdf$long_minsec)
  newdf<-newdf %>% mutate(lat_dd=lat_deg+(lat_minsec/60)) #sept14th: V21
  newdf<-newdf %>% mutate(lon_dd=(long_deg+(long_minsec/60))*-1) #sept14th: V22
  newdf<-add_column(newdf,cast=cast) #sept14th: V23
  newdf<-add_column(newdf,day_night=day_night) #sept14th: V24
  newdf<-add_column(newdf,date=date) #sept14th: V25
  newdf<-add_column(newdf,cruise=cruise) #sept14th:V26
  str(newdf)
  #outname = paste("SE12_06",input, "_top40summary",'.csv', sep = "")
  outname = paste("SE12_06",input, "_sept14_alldepths",'.csv', sep = "")
  write.csv(x=newdf, file=outname)
}
for(i in 1:length(oes1206)) {
  oes1206_function(oes1206[i])
}

#new1206<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/CTD/CTD Log Sheets", pattern=".csv")
new1206<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/CTD/CTD Log Sheets/all_depths", pattern=".csv")

binding_fn<-function(input) {
  df2<-read.csv(input[1], header=T)
  top40df<-as.data.frame(matrix(NA, ncol = ncol(df2), nrow = length(input)))
  for(i in 1:length(input)){
    df<-read.csv(input[i], header=T)
    top40df[i,]<-df[1,]
  }
  print(top40df)
  #write.csv(x=top40df, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/top40df_1206.csv")
  write.csv(x=top40df, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/sept14_surface_1206.csv")
}
setwd("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/CTD/CTD Log Sheets/all_depths")
binding_fn(new1206)

tw<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/sept14_surface_1206.csv")
str(tw)
#tw <-tw %>% rename("cruise"=V26, "date"= V25, "day_night"=V24,"cast"=V23, "lon_dd"=V22, "lat_dd"=V21, "long_deg"=V18,"long_minsec"=V19, "long_dir"=V20,"lat_deg"=V15,"lat_minsec"=V16, "lat_dir"=V17, "Fluoro"=V10, "DO"=V8,"bottle"=V11,"depth"=V12,"trash"=V14,"salinity"=V6,"temperature"=V4,"Pressure   (dbars)"=V3)


###for 11_06 csvs##############
oes1106_function=function(input) {
  df<-read_excel(input,skip=4)
  index=which(oes1106==input)
  lat=as.character(df[9,2])
  long=as.character(df[10,2])
  date=as.character(df[6,2])
  cast=as.character(df[7,2])
  cruise=as.character(df[5,2])
  df2<-read_excel(input,skip=18)
  top40<-filter(df2, is.na(`Time  (UTC)`)==F) #not actually top40, just wanted to keep names consistent between 1206 and 1106 for ease of writing a new function
  top40<-add_column(top40, date=date)
  top40<-unite(top40, col="date_time", c(date, `Time  (UTC)`), sep=" ")
  date_time<-mdy_hm(top40$date_time)
  date_time<-with_tz(date_time, tz="US/Hawaii")
  forsun<-as.Date(date_time[1])
  sunlist<-getSunlightTimes(date=forsun, lat=21.315603, lon=	-157.858093, tz="US/Hawaii")
  day_night<-ifelse((date_time[1]<=sunlist$sunrise)==T,"night","day")
  day_night
  cols.num <- c("Pressure   (dbars)","Salinity","Temperature    (C)","DO","Fluoro", "Bottle", "Depth", "Macro Ctrl-r","...10","...4","...6","...8")
  top40[cols.num] <- sapply(top40[cols.num],as.numeric)
  #top40<-as_tibble(filter(top40, `Pressure   (dbars)`<40))
  #top40_means<-top40 %>% summarise_at(vars(`Pressure   (dbars)`, `Temperature    (C)`, Salinity), c(min,max,mean))
  #top40_means<-top40_means %>% rename("min_pressure"=`Pressure   (dbars)_fn1`,"max_pressure"=`Pressure   (dbars)_fn2`,"mean_pressure"=`Pressure   (dbars)_fn3`,"min_sal"=Salinity_fn1,"max_sal"=Salinity_fn2,"mean_sal"=Salinity_fn3,"min_temp"=`Temperature    (C)_fn1`,"max_temp"=`Temperature    (C)_fn2`,"mean_temp"=`Temperature    (C)_fn3`)
  #newdf<-add_column(top40_means, lat=lat)
  newdf<-add_column(top40, lat=lat)
  newdf<-newdf %>% separate(lat, into = c("lat_deg","lat_minsec", "lat_dir"), sep=" ", remove=T)
  newdf$lat_deg<-as.numeric(newdf$lat_deg)
  newdf$lat_minsec<-as.numeric(newdf$lat_minsec)
  newdf<-add_column(newdf,long=long)
  newdf<-newdf %>% separate(long, into = c("long_deg","long_minsec", "long_dir"), sep=" ",remove=T)
  newdf$long_deg<-as.numeric(newdf$long_deg)
  newdf$long_minsec<-as.numeric(newdf$long_minsec)
  newdf<-newdf %>% mutate(lat_dd=lat_deg+(lat_minsec/60))
  newdf<-newdf %>% mutate(lon_dd=(long_deg+(long_minsec/60))*-1)
  newdf<-add_column(newdf,cast=cast)
  newdf<-add_column(newdf,day_night=day_night)
  newdf<-add_column(newdf,date=date)
  newdf<-add_column(newdf,cruise=cruise)
  str(newdf)
  #outname = paste("SE11_06",input, "_top40summary",'.csv', sep = "")
  outname = paste("SE11_06",input, "_sept14_alldepths",'.csv', sep = "")
  write.csv(x=newdf, file=outname)
}
oes1106<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/Logs/CTDCastLogs/XLS", pattern=".XLS")
setwd("~/oes1106_1206/OS11-06/Logs/CTDCastLogs/XLS")
for(i in 1:length(oes1106)) {
  oes1106_function(oes1106[i])
}
#new1106<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/Logs/CTDCastLogs", pattern=".csv")
new1106<-list.files(path="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/Logs/CTDCastLogs/XLS", pattern=".csv")
#setwd("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/Logs/CTDCastLogs")

binding_fn2<-function(input) {
  df2<-read.csv(input[1], header=T)
  top40df<-as.data.frame(matrix(NA, ncol = ncol(df2), nrow = length(input)))
  for(i in 1:length(input)){
    df<-read.csv(input[i], header=T)
    top40df[i,]<-df[1,]
  }
  print(top40df)
  #write.csv(x=top40df, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/summarydf_1106.csv")
  write.csv(x=top40df, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/sept14_surface_1106.csv")
}
binding_fn2(new1106)
el<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/sept14_surface_1106.csv")
#this is from before when means of top and bottom temps were recorded
#el<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/summarydf_1106.csv")
#str(el)
#el$X<-NULL
#el<-el %>% rename("cruise"=V22, "date"= V21, "day_night"=V20, "lon_dd"=V18, "lat_dd"=V17,"long_deg"=V14,"long_minsec"=V15, "long_dir"=V16,"lat_deg"=V11,"lat_minsec"=V12, "lat_dir"=V13,"min_pressure"=V2,"max_pressure"=V5,"mean_pressure"=V8,"min_sal"=V4,"max_sal"=V7,"mean_sal"=V10, "min_temp"=V3,"max_temp"=V6,"mean_temp"=V9)
#write.csv(x=el, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/summarydf_1106_named.csv")

###for tc32, cruise from 1967####
tc32<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/tc32/PIFSC_PIRO_bulk_data_download_InPort_5809/PIFSC_PIRO_bulk_data_download_InPort_5809/data/data/RB004AA1.csv")
str(tc32)
tc32<-tc32 %>% mutate(pa_uku=ifelse(STATION_NUMBER==3, 1, 0)) #26 64
tc32$pa_uku[tc32$STATION_NUMBER==26]<-1
tc32$pa_uku[tc32$STATION_NUMBER==64]<-1
ggplot(tc32, aes(x=TEMPERATURE_1, y=pa_uku))+geom_point(size=log(tc32$DEPTH_25DEG_ISOTHERM))
#Townsend Cromwell_32station8_1954
tc32<-tc32 %>% mutate(VESSEL_CODE="Townsend Cromwell") #just to make it match with weird formatting in main uku_pa sheet on GoogleSheets
tc32 <- tc32 %>% unite(sample_record_identifier, c(VESSEL_CODE,CRUISE_NUMBER,STATION_NUMBER,START_TIME), sep="_", remove=F)
tc32$sample_record_identifier[tc32$STATION_NUMBER==64]<-"USNM 322279"#64
tc32$sample_record_identifier[tc32$STATION_NUMBER==3]<-"USNM 322280" #3
tc32$sample_record_identifier[tc32$STATION_NUMBER==26]<-"USNM 322281" #26

#stopped here because no concrete depth data are associated with the temperatures shared in these data
#Dre input these by hand to the Relevant_Aprion_virescens_literature GoogleSheet many months ago, using 
#the surface temp as max_temp, surface salinity (as it was all that was available)
#CTD to SAMPLE LOCATION MATCH from Justin####
#making pseudo sample locations
LonSamp=sample(-125: -110, 20, replace=T)
LatSamp=sample(-5:30, 20, replace=T)

#making pseudo CTD locations and temp vals
LonTemp=sample(-125: -110, 5, replace=T)
LatTemp=sample(-5:30, 5, replace=T)
Temperature=sample(20:34, 5, replace=T)

#making location matrix for CTD collections
Temperature_Locations<-cbind(LonTemp, LatTemp)

#now identify the distance between each sample and all CTD locations
library(raster)
Nearest_Temp_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<1000000){#select the distance you want as your cut-off 
    Nearest_Temp_Val[i]<-Temperature[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=1000000){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}


##Andrea matching OES1206/1106 with CTD####
library(tidyverse)

##2012 sample locations####
#pa<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/stranges_pa_mocid_to_towid_means_aug5retry.csv")
pa<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/Relevant_Aprion_virescens_literature - presence_absence_sept22.csv")
tw_pa<-filter(pa, cruise_number=="12_06")
##checking tow distances
dist_1206<-as.numeric(tw_pa$sampling_distance_km_per_net_from_time)
dist_1206<-dist_1206*1000 #to convert from km to m
mean_samp_dist<-(mean(dist_1206))
LonSamp=(tw_pa$longitude_start_dd)
LatSamp=(tw_pa$latitude_start_dd)


# CTD locations and temp vals
#the following 7 lines were commented out because they took up the top 40m of the water column
#tw<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/top40df_1206.csv")
#summary order:min, max mean
#library(tidyverse)
#tww<-tw %>% rename("min_pressure"=V2,"max_pressure"=V5,"mean_pressure"=V8,"min_sal"=V4,"max_sal"=V7,"mean_sal"=V10,"min_temp"=V3,"max_temp"=V6,"mean_temp"=V9)
#LonTemp=(tw$V18)
#LatTemp=(tw$V17)
#Temperature=(tw$V9)
#this new csv is surface values only
tw<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS12-06/sept14_surface_1206.csv")
tw <-tw %>% rename("cruise"=V26, "date"= V25, "day_night"=V24,"cast"=V23, "lon_dd"=V22, "lat_dd"=V21, "long_deg"=V18,"long_minsec"=V19, "long_dir"=V20,"lat_deg"=V15,"lat_minsec"=V16, "lat_dir"=V17, "Fluoro"=V10, "DO"=V8,"bottle"=V11,"depth"=V12,"trash"=V14,"salinity"=V6,"temperature"=V4,"Pressure   (dbars)"=V3)
LonTemp=(tw$lon_dd)
LatTemp=(tw$lat_dd)
Temperature=(tw$temperature)

#making location matrix for CTD collections
Temperature_Locations<-cbind(LonTemp, LatTemp)


#now identify the distance between each sample and all CTD locations
library(raster)
Nearest_Temp_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<mean_samp_dist){#select the distance you want as your cut-off 
    Nearest_Temp_Val[i]<-Temperature[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=mean_samp_dist){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}

Nearest_Temp_Val

#2012 salinity##
Salinity=(tw$salinity)
#left "Temperature_Locations" the same name since it works for sal too here
Nearest_Sal_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<mean_samp_dist){#select the distance you want as your cut-off 
    Nearest_Sal_Val[i]<-Salinity[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=mean_samp_dist){
    Nearest_Sal_Val[i]<-NA#assign NA if too far away
  }
}
Nearest_Sal_Val
matched_1206<-tibble(tw_pa$sample_record_identifier, Nearest_Temp_Val,Nearest_Sal_Val, tw_pa$uku_present1_absent0)




##2011 sample locations####
#pa<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/stranges_pa_mocid_to_towid_means_aug5retry.csv")
pa<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/Relevant_Aprion_virescens_literature - presence_absence_sept22.csv")
el_pa<-filter(pa, cruise_number=="11_06")
##checking tow distances
dist_1106<-as.numeric(el_pa$sampling_distance_km_per_net_from_time)
dist_1106<-dist_1106*1000
mean_samp_dist<-(mean(dist_1106))

LonSamp=(el_pa$longitude_start_dd)
LatSamp=(el_pa$latitude_start_dd)

#CTD locations and temp vals
#el<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/summarydf_1106_named.csv")
el<-read.csv("C:/Users/Andrea.Schmidt/Documents/oes1106_1206/OS11-06/sept14_surface_1106.csv")
el<-el %>% rename("cruise"=V26, "date"= V25, "day_night"=V24,"cast"=V23, "lon_dd"=V22, "lat_dd"=V21, "long_deg"=V18,"long_minsec"=V19, "long_dir"=V20,"lat_deg"=V15,"lat_minsec"=V16, "lat_dir"=V17, "Fluoro"=V10, "DO"=V8,"bottle"=V11,"depth"=V12,"trash"=V14,"salinity"=V6,"temperature"=V4,"Pressure   (dbars)"=V3)

LonTemp=(el$lon_dd)
LatTemp=(el$lat_dd)
Temperature=(el$temperature) #change to "mean_temp" if using top40 csv

#making location matrix for CTD collections
Temperature_Locations<-cbind(LonTemp, LatTemp)

#now identify the distance between each sample and all CTD locations
library(raster)
Nearest_Temp_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<mean_samp_dist){#select the distance you want as your cut-off 
    Nearest_Temp_Val[i]<-Temperature[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=mean_samp_dist){
    Nearest_Temp_Val[i]<-NA#assign NA if too far away
  }
}
Nearest_Temp_Val

Salinity=(el$salinity) #change to "mean_sal" if using top40 csv
#left "Temperature_Locations" the same name since it works for sal too here
Nearest_Sal_Val<-NULL
for (i in 1:length(LonSamp)){
  #calculate each of those distances
  Dist_Vals<-pointDistance(c(LonSamp[i], LatSamp[i]), Temperature_Locations, lonlat=T, allpairs = T)
  K<-which(Dist_Vals==min(Dist_Vals))#find the location of the minimum distance
  if (Dist_Vals[K]<mean_samp_dist){#select the distance you want as your cut-off 
    Nearest_Sal_Val[i]<-Salinity[K]# fill if below cut-off
  }
  else if (Dist_Vals[K]>=mean_samp_dist){
    Nearest_Sal_Val[i]<-NA#assign NA if too far away
  }
}

matched_1106<-tibble(el_pa$sample_record_identifier, Nearest_Temp_Val,Nearest_Sal_Val, el_pa$uku_present1_absent0)

###for tc32, cruise from 1967####
#originally Dre input these by hand to the Relevant_Aprion_virescens_literature GoogleSheet many months ago, using 
#the surface temp as max_temp, surface salinity (as it was all that was available)
#to ensure maximum reproducibility the following has been done
tc32<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/tc32/PIFSC_PIRO_bulk_data_download_InPort_5809/PIFSC_PIRO_bulk_data_download_InPort_5809/data/data/RB004AA1.csv")
str(tc32)
tc32<-tc32 %>% mutate(pa_uku=ifelse(STATION_NUMBER==3, 1, 0)) #26 64
tc32$pa_uku[tc32$STATION_NUMBER==26]<-1
tc32$pa_uku[tc32$STATION_NUMBER==64]<-1
#Townsend Cromwell_32station8_1954
tc32<-tc32 %>% mutate(VESSEL_CODE="Townsend Cromwell") #just to make it match with weird formatting in main uku_pa sheet on GoogleSheets
tc32 <- tc32 %>% unite(sample_record_identifier, c(VESSEL_CODE,CRUISE_NUMBER,STATION_NUMBER,START_TIME), sep="_", remove=F)
tc32$sample_record_identifier[tc32$STATION_NUMBER==64]<-"USNM 322279"#64
tc32$sample_record_identifier[tc32$STATION_NUMBER==3]<-"USNM 322280" #3
tc32$sample_record_identifier[tc32$STATION_NUMBER==26]<-"USNM 322281" #26
matched_tc32<-tibble(tc32$sample_record_identifier,tc32$SURFACE_TEMPERATURE,tc32$SURFACE_SALINITY, tc32$pa_uku)
library(lubridate)
library(tidyverse)
tc32<-tc32 %>% mutate(END_TIME=(as.character(END_TIME)))
tc32<-tc32 %>% mutate(END_TIMEd=ifelse(nchar(END_TIME)==3,(gsub("([0-9])([0-9])([0-9])", "\\1:\\2\\3", END_TIME)),(gsub("([0-9])([0-9])([0-9])([0-9])", "\\1\\2:\\3\\4", END_TIME))))
tc32<-tc32 %>% mutate(START_TIMEd=ifelse(nchar(START_TIME)==3,(gsub("([0-9])([0-9])([0-9])", "\\1:\\2\\3", START_TIME)),(gsub("([0-9])([0-9])([0-9])([0-9])", "\\1\\2:\\3\\4", START_TIME))))
tc32<-tc32 %>% unite("END_DATE", c(YEAR, MONTH, DAY,END_TIMEd), sep="-", remove=F)
tc32<-tc32 %>% unite("START_DATE", c(YEAR, MONTH, DAY,START_TIMEd), sep="-", remove=F)
tc32<-mutate(tc32,END_TIME=ymd_hm(tc32$END_DATE))
tc32<-mutate(tc32,START_TIME=ymd_hm(START_DATE)) 
tc32<-tc32 %>% mutate(total_tow_time=END_TIME-START_TIME)
tc32<-tc32 %>% mutate(total_tow_time=ifelse(total_tow_time<0, total_tow_time+24,total_tow_time)) #roughly 6 hours each tow
#for this datastream, speed listed in knots on InPort but all are NAs so will use the following:                     
#for tows in 1984 Ship speed was adjusted over tow speeds of about 0.9-1.1 m/second (source: GUID: gov.noaa.nmfs.inport:8792)
SPEED=2.5 #in knots; for an approximate result, divide the speed value by 1.944
tc32<-tc32 %>% dplyr::mutate(total_tow_distance=(tc32$total_tow_time)*3600*(SPEED/1.944)) #convert hours to seconds, then multiply by speed (1 m/s) to get distance value in meters
#cobb net area:from mfr27102.pdf,"Scuba divers observed during a shallow tow that the net opening was about 12 m wide by8 m high." equals 96m^2
tc32<-tc32 %>% dplyr::mutate(total_tow_volume=(tc32$total_tow_distance)*96)
#most tows were 21600m
dist_tc32<-21600

###matched tibbles#####
m11<- setNames(matched_1106, paste0("o", seq_along(matched_1106)))
df11<-data.frame("sample_record_identifier"=m11$o1,"Nearest_Temp_Val"=m11$o2,"Nearest_Sal_Val"=m11$o3, "pa_uku"=m11$o4)
m12<-setNames(matched_1206, paste0("o", seq_along(matched_1206)))
df12<-data.frame("sample_record_identifier"=m12$o1,"Nearest_Temp_Val"=m12$o2,"Nearest_Sal_Val"=m12$o3, "pa_uku"=m12$o4)
m32<-setNames(matched_tc32, paste0("o", seq_along(matched_tc32)))
df32<-data.frame("sample_record_identifier"=m32$o1,"Nearest_Temp_Val"=m32$o2,"Nearest_Sal_Val"=m32$o3, "pa_uku"=m32$o4)

matched_all<-base::rbind(df11, df12, df32)
matched_all
#write.csv(x=matched_all, file="C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/matched_oes11_12_tc32.csv")
#on sept 14th 2022 Andrea resaved matched_all as matchy so as to avoid changing the SEPTEMBER STATS script
#write.csv(x=matched_all, file="C:/Users/Andrea.Schmidt/Documents/oes1106_1206/matched_all.csv")
###plotting matched tibbles#
ggplot(matched_all, aes(x=Nearest_Temp_Val))+geom_histogram(aes(fill=as.factor(pa_uku)))+scale_fill_manual(values=c('grey',"black"))+theme_minimal()
ggplot(matched_all, aes(x=Nearest_Sal_Val))+geom_histogram(aes(fill=as.factor(pa_uku)))+scale_fill_manual(values=c('lavender',"purple"))+theme_minimal()
ggplot(tc32, aes(x=TEMPERATURE_1, y=pa_uku))+geom_point(size=log(tc32$DEPTH_25DEG_ISOTHERM))
ggplot(matched_1206, aes(y=Nearest_Temp_Val, x=`tw_pa$uku_present1_absent0`))+geom_point()
ggplot(matched_1106, aes(y=Nearest_Temp_Val, x=`el_pa$uku_present1_absent0`))+geom_point()

#From uku project: adding in older/other larvae#####
larv<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/Relevant_Aprion_virescens_literature - larval_records.csv")
larv<-filter(larv, sampling_method!="MOCNESS")
larv<-filter(larv, region=="Main Hawaiian Islands")
larv<-filter(larv, life_stage=="larv")
mini_larv<-tibble(larv$sample_record_identifier,larv$length_mm, larv$collection_depth_m_max, larv$collection_day, larv$collection_month, larv$collection_year )
mini_larv<-dplyr::rename(mini_larv, sample_id= "larv$sample_record_identifier", length_mm="larv$length_mm", collection_depth_m_max="larv$collection_depth_m_max")
mini_larv<-mutate(mini_larv, length_mm=as.numeric(length_mm))
mini_larv<-mutate(mini_larv, collection_depth_m_max=as.numeric(collection_depth_m_max))
sizes2<-full_join(mini_larv, sizes)
sizes2<-right_join(joey, sizes2,by="sample_id")
#fix very nearshore points from Kahe Environmental Impact Survey
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-029")]<-797.04
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-030")]<-68.56
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-031")]<-988.11
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-032")]<-63.11
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-033")]<-63.11
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-034")]<-797.04
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-036")]<-160.50
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-035")]<-797.04
sizes2$Dist2Shore_m[which(sizes2$sample_id=="I.25000-037")]<-797.04
ll<-tibble(sizes2$LAT_DD, sizes2$LON_DD, sizes2$LAT_DD_JL, sizes2$LON_DD_JL, sizes2$length_mm)