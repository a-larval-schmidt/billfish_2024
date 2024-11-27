
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


##larval data read in#######
larv<-re_mas%>%dplyr::select(c("specimen_identification","stage","Site","ID_morph_and_PCR","data.source", "count","comb_length","length_occurence"))

##metadatasheet as combo then subset to mini####
#add station number infomation for each cruise by matching start/end times of tows in combo to each cruise datasheet
#combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/Product1_LarvalBillfish_TowMetadata.csv")
#combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
combo<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_short12.csv")
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

#data to do list#####
summary(as.factor(combo[(which(is.na(combo$temp.1m==T))),4]))
#fill out mean values for temp/sal ect ect  from TSG for what I can find matches for
#fill out SST. sal, chla data for ALL data points from remote sensed data (use GLORYS to stay consistent with modelled salinityd data). 
#keep both columns to allow for comparisons
#OCean color ESA: occci_V6_8day_4km
#merge TSG env data#####
tsg1<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/merged_tsgs_redo.csv")
tsg2<-tsg1 %>%
  mutate(datetime=as_datetime(datetime),.keep="unused")%>%
  mutate(date=ymd(Day),.keep="unused")%>%
  mutate(time=hms(str_replace_all(time,"[:alpha:]","")))%>%
  mutate(local_datetime=with_tz(datetime, "UTC")) #view time as HST to match combo data
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
  mutate(local_datetime=with_tz(datetime, tz="HST"), .keep="all")%>%
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
  mutate(local_datetime=with_tz(datetime, tz="HST"), .keep="all")%>%
  mutate(Year=year(date))%>%
  mutate(Day_Time_Julian=decimal_date(date))%>%
  mutate(lon_degree=as.numeric(str_sub(Furuno.GP90_Longitude,1,3)),.keep="all")%>%
  mutate(lon_minute=as.numeric(str_sub(Furuno.GP90_Longitude,4,9)),.keep="all")%>%
  mutate(og_lon=as.numeric(str_replace(Furuno.GP90_Longitude,"[:alpha:]","")))%>%
  mutate(lon_dd=(-1*(lon_degree+(lon_minute/60))))%>%
  mutate(lat_degree=as.numeric(str_sub(Furuno.GP90_Latitude,1,2)),.keep="all")%>%
  mutate(lat_minute=as.numeric(str_sub(Furuno.GP90_Latitude,3,9)),.keep="all")%>%
  mutate(og_lat=as.numeric(str_replace(Furuno.GP90_Latitude,"[:alpha:]","")))%>%
  mutate(lat_dd=lat_degree+(lat_minute/60))
  #mutate(LAT_DD_START=og_lat/60)%>%
  #mutate(LON_DD_START=(-1*(og_lon/60))) 
meep2<-select(meep, c(colnames(tsg3)))
tsg8<-rbind(tsg7,meep2)

#kona_map+geom_point(data=meep, aes(x=LON_DD_START, y=LAT_DD_START))
kona_map+geom_point(data=meep, aes(x=lon_dd, y=lat_dd))

#time join TSG and mini, site join this to specimen data#######
#mini_time_join1<-left_join(tsg8,mini,join_by(local_datetime<=EndDateTime, local_datetime>=StartDateTime)) #join-by closest value
#add limit to date range for joining ^^ within one day ONLY
full_site_join<-left_join(combooo,mini_time_join1,
                          join_by(Year, Site, LAT_DD_start,LONG_DD_start,EndDateTime, StartDateTime),
                          relationship="many-to-many")
specimen_site_join<-left_join(larv,full_site_join,join_by(Site))
full<-specimen_site_join%>%
  mutate(DateTime=mdy_hm(DateTime))%>%
  mutate(Time=hms(Time))%>%
  mutate(Time.end=hms(Time.end))%>%
  mutate("moon_date"=suncalc::getMoonIllumination(date = StartDateTime,keep = "phase"))%>%
  mutate("phase"=moon_date$phase)%>%
  mutate("cat_moon"=lunar::lunar.phase(x = StartDateTime, shift=10, name=T))%>%
  mutate(dur=lubridate::interval(start=StartDateTime, end=EndDateTime))%>%
  mutate(duration=as.numeric(dur, "minutes"))%>%#filter(duration>0 & duration<60)%>%
  mutate("temp_fix"=ifelse(is.na(temp.1m)==T,TempC,temp.1m))%>% # let both exist in their own columns as well for comparison purposes
  mutate("sal_fix"=ifelse(is.na(sal.1m)==T,Salinity, sal.1m))%>%#let both exist in their own columns as well for comparison purposes
  mutate("density"=count/vol.m3)

ggplot(full, aes(x=year(DateTime), y=sal_fix,color=Year))+geom_point()
ggplot(full, aes(x=Habitat, y=density, color=temp_fix))+facet_grid(~ID_morph_and_PCR)+geom_point()+scale_color_viridis_c(option="turbo")
ggplot(full, aes(x=Cruise, y=temp_fix))+geom_point()
ggplot(full, aes(x=Cruise, y=density, color=temp_fix))+geom_point()+scale_color_viridis_c(option="turbo")
#station summary table#####
done<-full%>%group_by(Site)%>%summarize("mean_temp"=mean(temp_fix, NA.rm=T),"mean_sal"=mean(sal_fix,NA.rm=T)) # sig figs to 4 from TSG
donedone<-left_join(combooo, done, by="Site")
donedone<-donedone%>%
  mutate("temp"=ifelse(is.na(temp.1m)==T,mean_temp,temp.1m))%>%
  mutate("sal"=ifelse(is.na(sal.1m)==T,mean_sal,sal.1m))
#write.csv(donedone, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_short13.csv")
summary(as.factor(donedone[(which(is.na(donedone$temp==T))),4]))
donedone<-donedone%>%
  mutate("env_data_source"=ifelse(is.na(temp)==F,"TSG","remote sensing")) # rename to temp data source, add in 

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
#salty<-combooo%>%dplyr::select(c(LONG_DD_start,LAT_DD_start,sal.1m))
#mini3<-left_join(mini, salty)
minimini#<-mini%>%dplyr::select(c(GLORYS_sal, Site))
donedone<-left_join(donedone, minimini, by="Site")
donedone_glorys<-donedone%>%
  mutate("sal_data_source"=ifelse(is.na(sal)==T,"Modelled Salinity Data from GLORYS",'In situ measurement'),.keep="all")%>%
  mutate("salinity"=ifelse(is.na(sal)==T,GLORYS_sal,sal),.keep="all")%>%
  distinct(Site, .keep_all=T)
#write.csv(minimini, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/GLORYS_sal_matched_to_Site.csv")
##add sst##############################
summary(as.factor(donedone_glorys[(which(is.na(donedone_glorys$salinity==T))),4]))
f<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/WHIP_SWDs.csv")
str(f)
ff<-f%>%dplyr::select(c(Site,SST_Sat))
d<-ff%>%full_join(donedone_glorys,ff, by="Site")
dd<-d%>%
  mutate("temp_data_source"=ifelse(is.na(sal)==T,"SST Data from CRW",'In situ measurement'),.keep="all")%>%
  mutate("temp"=ifelse(is.na(temp)==T,SST_Sat,temp), .keep="all")
#write.csv(dd, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_short15.csv")
#add chla from Jessie!!!!!####
ddd<-dd%>%mutate(Date=mdy(Date))
mini<-ddd%>%dplyr::select(c(LAT_DD_start,LONG_DD_start,Site,Date))
summary(as.factor(dd[(which(is.na(dd$chla==T))),4]))
summary(as.factor(dd[(which(is.na(dd$chla==T))),7]))
summary(dd$LAT_DD_start)
summary(dd$LONG_DD_start)
library(raster)
library(tidyverse)
library(sp)
library(sf)
library(reshape2)

setwd("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/")

# read in dataset
df <- read.csv("combo_whip_slick_short16.csv")
df$Date = as.Date(df$Date, format = "%m/%d/%Y")

# read in 8-day 4km ocean color data (.nc file)
data = "ESA_OC_CCI_chla_kd490_1997_2021_8day.nc"
#data = "ESA_OC_CCI_chla_kd490_1997_2019_monthly.nc"

chlor <-stack(data, varname = "chlor_a")
kd490 <-stack(data, varname = "kd_490")

# need to mask shallow water cells (< 30m)
#bathy_file = "Hawaii_bathy_1km.nc"
#bathy <- raster(bathy_file)
#bathy[bathy[] < -30 ] = NA 
#bathy_poly = rasterToPolygons(bathy)
#plot(bathy_poly)

# add a buffer around 30m depth contour equal to 1/2 diagonal pixel distance of 500m GOCI data 
#[sqrt((1000^2) + (1000^2))/2] = 707.1068
#bathy_buffered_poly_1km <- raster::buffer(bathy_poly, width = 707.1068)
#save(bathy_buffered_poly_1km, file = paste0("envirn_data/bathymetry/bathy_HI_30m_buffered_poly_1km.RData"))

#plot(bathy_buffered_poly_1km)

# use buffered bathymetry file to mask shallow waters around MHI
load("bathy_HI_30m_buffered_poly_1km.RData")
chlor_masked <- raster::mask(chlor, bathy_buffered_poly_1km, inverse = TRUE)
kd490_masked <- raster::mask(kd490, bathy_buffered_poly_1km, inverse = TRUE)

# create new column of nearest 8-day value
library(data.table)

df1 = df
y=as.numeric(substr(names(chlor),2,5))
m=as.numeric(substr(names(chlor),7,8))
d=as.numeric(substr(names(chlor),10,11))
df2 = as.data.frame(ymd(paste(y,m,d)))
names(df2) <- "date"

setDT(df1)[,DT_DATE := Date] # name of date column in your dataset
setDT(df2)[,DT_DATE := date] # name of date column in df2

# merge d1 and df2 by matching the nearest date from df2 (ocean color 8-day dates) to the date in df1 (your data)
merged <- df2[df1,on=.(date=DT_DATE),roll="nearest"]
df_8day = merged[,c("Date","DT_DATE","LAT_DD_start","LONG_DD_start")]

# note 1997 data only goes back to 09/1997 but the cruise for that year is 04/1997, so wonʻt have chlor data for that cruise
df_8day = subset(df_8day, Date > "1997-12-31")

# create empty columns for chlorophyll and kd490
df_8day$chlor_8day <- NA
df_8day$kd490_8day <- NA

for(i in 1:nlayers(chlor)) {
  
  # i = 30
  
  year=as.numeric(substr(names(chlor_masked[[i]]),2,5))
  month=as.numeric(substr(names(chlor_masked[[i]]),7,8))
  day=as.numeric(substr(names(chlor_masked[[i]]),10,11))
  ymd = ymd(paste(year,month,day))
  
  idx <- which(df_8day$DT_DATE == ymd)
  
  # need to average layer to get rid of z-value that is causing issues in extract
  ch = mean(chlor_masked[[i]])
  k = mean(kd490_masked[[i]])
  
  if (length(idx)>0){
    
    pts <- SpatialPoints(df_8day[idx,c('LONG_DD_start', 'LAT_DD_start')], crs(ch))
    
    df_8day$chlor_8day[idx] <- raster::extract(ch, pts) 
    df_8day$kd490_8day[idx] <- raster::extract(k, pts) 
    
  }
  else if (length(idx)==0) {}
  
  print(paste("Completed",i,"of",nlayers(chlor_masked),"layers"))
  
}

# merge matched ocean color back with full dataframe
duh = left_join(df, df_8day[,-"DT_DATE"], by = c("Date","LONG_DD_start", "LAT_DD_start"))
#duh<-full_join(dd, all_env_df)#, relationship="one-to-one")
duh2<-duh%>%
  mutate("temp_data_source"=ifelse(is.na(sal)==T,"8 day ESA Chlorophyll",'In situ measurement'),.keep="all")%>%
  mutate("chla_mixed"=ifelse(is.na(chla)==T,chlor_8day,chla))%>%
  distinct(Site,.keep_all = T)
summary(as.factor(duh2[(which(is.na(duh2$chla_mixed==T))),6]))
#match monthly ocean color data to provide the more general productivity of the area at broader timescales 
#run this script=bathy_HI_30m_buffered_poly_1km.R
#You should be able to run the same script and just change the "data" file to the monthly file and change the "8day" classifiers to "monthly."
#final format for matching####
combo16<-duh2%>%
  #dplyr::select(!c(chla_ESA_CCIOceanColour,sal,mean_sal,mean_temp,SST_Sat,GLORYS_sal, GLORYS_sal.x, GLORYS_sal.y))%>%
  mutate(temp.mixed.data=temp,.keep="unused")%>% # allow for TSG and remotse sensing in  their own columns (fix in preceeding dfs)
  mutate(sal.mixed.data=salinity, .keep = "unused")%>%
  mutate(chla.mixed.data=chla_mixed, .keep = "unused")%>%
  mutate(lunar_illumination=lunar.illumination(mdy(Date)), .keep="all")%>%
  mutate("both_temp"=ifelse((is.na(SST_Sat)==F|is.na(temp)==F),"both","singleton"))%>%
  mutate("both_sal"=ifelse((is.na(GLORYS_sal)==F|is.na(sal)==F),"both","singleton"))%>%
  mutate("both_chla"=ifelse((is.na(chla)==F|is.na(chlor_8day)==F),"both","singleton"))%>%
  distinct(Site,.keep_all = T)
#write.csv(combo16, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_short16.csv")
##plot tests######
specimen_site_join<-left_join(larv, combo16, by="Site")
full<-specimen_site_join%>%
  mutate(DateTime=mdy_hm(DateTime))%>%
  mutate(Time=hms(Time))%>%
  mutate(Time.end=hms(Time.end))%>%
  mutate("larval_density_per_cubic_meter"=count/vol.m3)
full<-full%>%filter(is.na(env_data_source)==F)
ggplot(full, aes(x=Cruise, y=larval_density_per_cubic_meter, color=sal.1m))+geom_point()+scale_color_viridis_c(option="turbo")+facet_grid(~env_data_source)
#product2####
product2<-full%>%
  mutate(larval_identity=ID_morph_and_PCR, .keep="unused")%>%
  mutate(standard_length_mm=comb_length,.keep="unused")%>%
  mutate(unique_specimen_identifier=specimen_identification,.keep="unused")%>%
  dplyr::select(!c(data.source,Transect, sample, StartDateTime, EndDateTime))
#write.csv(product2, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/Billfish_Specimen_Environmenal_Data.csv")

##old############
nc<- nc_open(data)
names(nc$var)
v1 <- nc$var[[1]]
chla1618 <- ncvar_get(nc,v1) 
dim(chla1618)
dates<- as.POSIXlt(v1$dim[[3]]$vals,origin='1970-01-01',tz='UTC') #get the dates for each time step
lon <- v1$dim[[1]]$vals #gives vector of longitude
lat <- v1$dim[[2]]$vals #gives vector of latitude
chla<-tibble(lon,lat,dates)
nc_close(nc) #this step is important, otherwise you risk data loss

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

