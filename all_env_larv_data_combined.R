
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
library(dplyr)
library(stringr)

##larval data read in#######
mas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_20241122.csv")
og<-filter(mas, data.source!="box")
og %>% group_by(taxa)%>%summarize(sum(as.numeric(count)),na.rm=TRUE)
mas %>% group_by(taxa)%>%summarize(sum(as.numeric(count)),na.rm=TRUE) 
mas2<-mas %>%
  mutate("dre_length"=as.numeric(dre_length))%>%
  mutate("unknown.sizes"=as.numeric(unknown.sizes))%>%
  mutate("unknown.sizes"=ifelse(is.na(dre_length)==F,0,unknown.sizes))%>%
  mutate("ID.from.PCR2"=ifelse(ID.from.PCR=="", taxa,ID.from.PCR))%>% #trying to get pcr ids into genus column, alternatively just repalce Unk values alt. method from excel, filter out 'unknowns" in ID.from.PCR column
  mutate("ID_morph_and_PCR"=ifelse(ID.from.PCR2=="Mn", "Makaira nigricans",ifelse(ID.from.PCR2=="Ta", "Tetrapturus angustirostris", 
                                                                                  ifelse(ID.from.PCR2=="Ka","Kajikia audax",
                                                                                         ifelse(ID.from.PCR2=="Xg","Xiphias gladius",
                                                                                                ifelse(ID.from.PCR2=="Unk.Istiophoridae", "Unk.Istiophoridae",taxa))))))

mas2 %>%group_by(taxa)%>%summarize(sum(count),na.rm=TRUE) #values match those of bff; this is an UNDER COUNT
mas2%>%group_by(taxa)%>%summarize(sum(as.numeric(unknown.sizes)),na.rm=TRUE) #values match those of bff; this is an UNDER COUNT

mas_long<-mas2%>%
  dplyr::select(c(specimen_identification,X01mm:X100m))%>%
  pivot_longer(X01mm:X100m,names_to="paper_length",
               values_to="length_occurence",values_drop_na = T) #pivot longer &coalesce both lengths and frequencies columns or #melt, shape, reshape

mas_clean<-mas_long%>%
  mutate("paper_length_num"=gsub("mm","",paper_length), .keep="unused")%>%
  mutate("paper_length_num"=gsub("X","",paper_length_num))%>%
  mutate("paper_length_num"=gsub("m","",paper_length_num))%>%
  mutate("paper_length_num"=as.numeric(paper_length_num))#be sure to specify vials v.s. specimen IDs##

mas3<-mas2%>%dplyr::select(!X01mm:X100m)%>%
  distinct(specimen_identification,.keep_all = T)%>%
  mutate(unknown.sizes=ifelse(is.na(dre_length)==F,0, unknown.sizes)) 
mas_long_clean<-mas_clean%>%filter(length_occurence!=0)#distinct function eliminated vials where multiple size classes were counted
#no need to pull distinct here because the important part is the Site ID
re_mas<-left_join(mas3,mas_long_clean,by=c(specimen_identification="specimen_identification"))#, vial="vial", taxa="taxa"))#, relationship="many-to-many")

re_mas<-re_mas%>%
  mutate("length_occurence"=ifelse(is.na(dre_length)==F,1, length_occurence))%>%#give _dre_length its own frequency column
  mutate("comb_length"=ifelse(is.na(paper_length_num)==T,dre_length,paper_length_num))%>%
  mutate("freq_check"=length_occurence+unknown.sizes)
larv<-re_mas%>%dplyr::select(c("specimen_identification","stage","Site","ID_morph_and_PCR","data.source", "count","comb_length","length_occurence"))

##metadatasheet as combo then subset to mini####
#add station number information for each cruise by matching start/end times of tows in combo to each cruise datasheet
#combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/Product1_LarvalBillfish_TowMetadata.csv")
#combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
combo<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_short12.csv")
comboo<-combo%>%
  unite("dend",c(Date,Time.end),sep=" ",remove = F)%>%
  unite("send",c(Date,Time.start),sep=" ", remove=F)%>%
  mutate(Date=mdy(Date))%>%
  mutate(Time.end=hms(Time.end))%>%
  mutate(Time.start=hms(Time.start))
combooo<-comboo%>% #times in local
  mutate(EndDateTime=lubridate::mdy_hms(dend, tz="HST"), .keep="unused")%>%
  mutate(StartDateTime=lubridate::mdy_hms(send,tz="HST"), .keep="unused")

#with_tz() changes the time zone in which an instant is displayed. The clock time displayed for the instant changes, but the moment of time described remains the same.
#add column to d to start
mini<-combooo%>%dplyr::select(c(Site,Date,Time.end, Time.start,LAT_DD_start,LONG_DD_start))

#data to do list#####
summary(as.factor(combo[(which(is.na(combo$temp.1m==T))),4]))
#fill out mean values for temp/sal ect ect  from TSG for what I can find matches for
#fill out SST. sal, chla data for ALL data points from remote sensed data (use GLORYS to stay consistent with modelled salinityd data). 
#keep both columns to allow for comparisons
#OCean color ESA: occci_V6_8day_4km
#make tsg1#####
tsgcsv <- dir("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/processed_TSGs/", recursive=TRUE, full.names=TRUE, pattern="csv")
keeps <- c("Year", "Day_Time_Julian", "Day","TempC","Salinity","datetime","time")
b<-read.csv(tsgcsv[1])
tsg1<-b[keeps]
b<-read.csv(tsgcsv[2])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[3])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[4])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[5])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[6])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[7])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[8])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[9])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[10])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[11])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
b<-read.csv(tsgcsv[12])
b<-b[keeps]
tsg1<-rbind(tsg1,b)
for(i in seq_along(length(tsgcsv))){
  x<-read.csv(tsgcsv[i])
  keeps <- c("Year", "Day_Time_Julian", "Day","TempC","Salinity","datetime","time")
  b<-x[keeps]
  b <- rbind(b,x)
}
#merge TSG env data#####
tsg1<-filter(tsg1, Salinity<40)
tsg1<-filter(tsg1,30<Salinity)
tsg1<-filter(tsg1, TempC<40)
tsg1<-filter(tsg1,0<TempC)
tsg2<-tsg1 %>% 
  mutate(datetime=ymd_hms(datetime),.keep="all")%>%
  mutate(Date=lubridate::ymd(Day))%>%
  mutate(local_datetime=with_tz(datetime, "HST"))%>%##view time as HST to match combo data
  mutate(time2=str_replace_all(time,"[:alpha:]",":"))%>%mutate(time3=str_sub(time2, end=-2))%>%mutate(time=hms(time3))
tsg2<-tsg2%>%dplyr::select(c(Year, TempC, Salinity, datetime,time, local_datetime,Day_Time_Julian))
str(tsg2) #times in UTC; this includes 1999 to 2006
#QC##
#append9703 and 9804
o9804<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/CTD_TC_9804 CTD Station Log.csv", header=T)
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
sub_9804<-dplyr::select(e, c(colnames(tsg2)))
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
sub_9703<-dplyr::select(g, c(colnames(tsg3)))
tsg4<-rbind(tsg3,sub_9703)
str(tsg4)
#append 1704
sbe45<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/se1704_AllInclusiveContinuous.csv")
str(sbe45)
sbe46<-sbe45%>%
  mutate(date=mdy(Date))%>%
  mutate(time=hms(Time))%>%
  mutate(Year=year(date))%>%
  mutate(TempC=as.numeric(SBE.45.Remote.Temperature))%>%
  mutate(Salinity=as.numeric(SBE.45.Salinity))%>%
  mutate(lat=as.numeric(Furuno.GP90_Latitude))%>%
  mutate(Day_Time_Julian=decimal_date(date))%>%
  mutate(lon=as.numeric(Furuno.GP90_Longitude ))%>%
  mutate("datetime"=paste(date,time))%>%
  mutate(datetime=ymd_hms(datetime,tz="UTC"))%>%
  mutate(local_datetime=with_tz(datetime,tz="HST"),.keep="all")
sbe47<-dplyr::select(sbe46, c(colnames(tsg3)))
tsg5<-rbind(tsg4,sbe47)
tsg5<-filter(tsg5, Salinity<40)
tsg5<-filter(tsg5,30<Salinity)
tsg5<-filter(tsg5, TempC<38)
tsg5<-filter(tsg5,20<TempC)
str(tsg5)
#rawfiles####
sttfiles <- dir("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/", recursive=TRUE, full.names=TRUE, pattern="SAMOS-TSG-Temp")
stsfiles <- dir("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/", recursive=TRUE, full.names=TRUE, pattern="SAMOS-TSG-SAL")
drat<-read.csv(sttfiles[1])
colnames(drat)<-c("Date","Time", "Derivative","TempC","Temp2","idk","idk2","idk3")
drat2<-read.csv(sttfiles[2])
colnames(drat2)<-c("Date","Time", "Derivative","TempC","Temp2","idk","idk2","idk3")
stt<-rbind(drat, drat2)
stt2<-dplyr::select(stt, c(Date, Time, TempC))
stt3<-stt2%>%unite("datetime",Date, Time, sep=" ")%>% mutate(datetime=mdy_hms(datetime))

drat<-read.csv(stsfiles[1])
colnames(drat)<-c("Date","Time", "Derivative","Salinity","Salinity2","idk","idk2","idk3")
drat2<-read.csv(stsfiles[2])
colnames(drat2)<-c("Date","Time", "Derivative","Salinity","Salinity2","idk","idk2","idk3")
sts<-rbind(drat, drat2)
sts2<-dplyr::select(sts, c(Date, Time, Salinity))
sts3<-sts2%>%unite(datetime, Date:Time, sep=" ")%>% mutate(datetime=mdy_hms(datetime))
rawva<-full_join(sts3, stt3)
#####
rawva1<-rawva%>%
  mutate(datetime= with_tz(datetime,tz="UTC"), .keep="all")%>%
  mutate(local_datetime= with_tz(datetime,tz="HST"), .keep="all")%>%
  mutate(date=date(datetime))%>%
  mutate(Year=year(datetime)) %>%
  mutate( hour = hour(datetime),
    minute = minute(datetime),
    second = second(datetime)) %>% 
  mutate(time2 = paste(hour, minute, second, sep = ":"), .keep="unused")%>%
  mutate(time=lubridate::hms(time2))%>%
  mutate(Day_Time_Julian=decimal_date(datetime))
str(rawva1)#its okay that almost half of it is NAs, sal and temp measurements were not taken at exactly the same time so full_join was effectively more of an r bind
rawva2<-dplyr::select(rawva1, c(colnames(tsg5)))
tsg6<-rbind(tsg5,rawva2)
str(tsg6)
tsg6<-filter(tsg6, Salinity<40)
tsg6<-filter(tsg6,30<Salinity)
tsg6<-filter(tsg6, TempC<38)
tsg6<-filter(tsg6,20<TempC)
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
  mutate(datetime=lubridate::ymd_hms(datetime, tz="UTC"))%>%
  mutate(local_datetime=lubridate::with_tz(datetime, tz="HST"), .keep="all")%>%
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
meep2<-dplyr::select(meep, c(colnames(tsg3)))
tsg7<-rbind(tsg6,meep2)
#tsgwhip<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/fuzzy_joined_tsg_1216.csv")
#time join TSG and mini, site join this to specimen data#######
#~why are some days having the exact same values???? (se0412-...)
tsg8<-tsg7%>%mutate(date=as.Date(datetime))
tsgdfname<-tsg8
#joining from Jon#########
library(tidyverse)
library(fuzzyjoin)
#tsgwhip<-fuzzy_left_join(tsg8, mini,by = c("date"="Date","time" = "Time.start","time" = "Time.end"),match_fun = list(`==`, `>=`, `<=`))
#write.csv(tsgwhip, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/fuzzy_joined_tsgs_to_product1.csv")
#tsg_mini<-semi_join(tsgdfname,mini,join_by(date==Date))
#time_join<-full_join(tsg_mini, mini,join_by(between(time, Time.start,Time.end)), relationship="many-to-many")
#write.csv(time_join, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/tsgs_joined_to_product1_by_date_AND_time.csv")
#mini_time_join1<-left_join(tsgdfname,mini,join_by(date==Date, between(time, Time.start,Time.end)), relationship="many-to-many")#local_datetime<=EndDateTime, local_datetime>=StartDateTime)) #join-by closest value
tsg10<-time_join%>%dplyr::select(c(Site, TempC, Salinity))%>%filter(Salinity<40)%>%filter(TempC<40)%>%filter(TempC>0)%>%filter(Salinity>30)
#mean temp and sal per Site (one value per site)
tsg11 <- tsg10%>%
  group_by(Site) %>%
  summarise(TempC = mean(TempC, na.rm=T), Salinity= mean(Salinity, na.rm=T))
#tsg12<-left_join(mini_time_join1,tsg11, join_by(Site))
library(tidyverse)
library(fuzzyjoin)
full_site_join<-fuzzy_left_join(combooo,tsg11,by=(c("Site"="Site")),match_fun = `==`)#Year, Site, LAT_DD_start,LONG_DD_start,Time.start, Time.end),#EndDateTime, StartDateTime),
full_site_join<-left_join(combooo,tsg11,by=c("Site"="Site"))#Year, Site, LAT_DD_start,LONG_DD_start,Time.start, Time.end),#EndDateTime, StartDateTime),
specimen_site_join<-left_join(full_site_join,larv,join_by("Site"=="Site"))
#write.csv(specimen_site_join, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/specimen_site_join_121924.csv")
#presence/absence dataset#######
pa<-specimen_site_join%>%mutate("billfish_present"=ifelse(is.na(count)==F,T,F))%>%
  mutate("ta_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Tetrapturus angustirostris",T,F))%>%
  mutate("mn_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Makaira nigricans",T,F))%>%
  mutate("xg_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Xiphias gladius",T,F))%>%
  mutate("ka_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Kajikia audax",T,F))
pap<-pa%>%distinct(Site,.keep_all = TRUE)
#env and spec####
full<-specimen_site_join%>%
  mutate(DateTime=mdy_hm(DateTime))%>%
  mutate("moon_date"=suncalc::getMoonIllumination(date = StartDateTime,keep = "phase"))%>%
  mutate("phase"=moon_date$phase)%>%
  mutate("cat_moon"=lunar::lunar.phase(x = StartDateTime, shift=10, name=T))%>%
  mutate(dur=lubridate::interval(start=StartDateTime, end=EndDateTime))%>%
  mutate(duration=as.numeric(dur, "minutes"))%>%#filter(duration>0 & duration<60)%>%
  mutate("temp_fix"=ifelse(is.na(temp.1m)==T,TempC,temp.1m))%>% # let both exist in their own columns as well for comparison purposes
  mutate("sal_fix"=ifelse(is.na(sal.1m)==T,Salinity, sal.1m))%>%#let both exist in their own columns as well for comparison purposes
  mutate("density"=count/vol.m3)%>%
  rename("temp.is"=TempC)%>%
  rename("sal.is"=Salinity)%>%
  mutate("TempC_data_source"=ifelse(is.na(temp.1m)==F,"CTD/TSG","NOAA_CRW"))%>% 
  mutate("Salinity_data_source"=ifelse(is.na(sal.1m)==F,"CTD/TSG","GLORYS_SSS"))%>% 
  mutate("chla_data_source"=ifelse(is.na(chla)==F,"bottle collections","ESA_OC-CCI")) 
  
ggplot(full, aes(x=TempC_data_source, y=density, color=temp_fix))+geom_point()+scale_color_viridis_c(option="turbo")
ggplot(full, aes(x=Salinity_data_source, y=density, color=sal_fix))+geom_point()+scale_color_viridis_c(option="turbo")
ggplot(full, aes(x=chla_data_source, y=density, color=chla))+geom_point()+scale_color_viridis_c(option="turbo")

#station summary table#####
#full<-full%>%group_by(Site)%>%summarize("mean_temp"=mean(temp_fix, NA.rm=T),"mean_sal"=mean(sal_fix,NA.rm=T)) # sig figs to 4 from TSG
#donedone<-left_join(combooo, done, by="Site")
#from Jon: check 
tsg.sum <- full %>% group_by(Cruise, across(c(Year))) %>% summarise(Ntsg.miss=n_distinct(Site[is.na(temp_fix)]), Ntsg=n_distinct(Site[!is.na(temp_fix)])) %>%
  mutate(miss.prop = Ntsg.miss/(Ntsg+Ntsg.miss)) %>% arrange(desc(miss.prop))
ggplot(tsg.sum, aes(x=Cruise, y=miss.prop))+geom_col()+ylim(c(0,1))
#12-16 to do##########
#nothing cuz NO larvae at all form that cruise
#prep modeled salinity data####
# data=("C:/Users/Andrea.Schmidt/Desktop/for offline/cmems_mod_glo_phy_my_0.083deg_P1D-m_1727396375892.nc")
# #data = ("C:/Users/jessica.perelman/Downloads/salinity.nc")
# sal <-brick(data, varname = "so") # double check this is the actual variable name
# NAvalue(sal) <- -9999 # for whatever reason, the "no value" grid cells were stored as NA rather than a numeric in the glorys dataset, so you have to reclassify these.
# mini<-mini2%>%
#   filter(is.na(LONG_DD_start)==F)%>%
#   filter(is.na(LAT_DD_start)==F)
#   
# mini$GLORYS_sal <- NA
# 
# for(i in 1:nlayers(sal)) {
#   
#   # i = 291
#   year=as.numeric(substr(names(sal[[i]]),2,5))
#   month=as.numeric(substr(names(sal[[i]]),7,8))
#   day=as.numeric(substr(names(sal[[i]]),10,11))
#   ymd = as.Date(as.POSIXct(ymd(paste(year,month,day)), tz = "HST"), tz = "HST")
#   
#   idx <- which(as.Date(mini$StartDateTime) == ymd)
#   
#   if (length(idx)>0){
#     
#     pts <- SpatialPoints(mini[idx,c('LONG_DD_start', 'LAT_DD_start')], crs(sal))
#     mini$GLORYS_sal[idx] <- raster::extract(sal[[i]], pts)
#     
#   }
#   else if (length(idx)==0) {}
#   
#   print(paste("Completed",i,"of",nlayers(sal),"layers"))
#   
# }
# 
# saveRDS(mini, file = "mini_sal_GLORYS.rds")
#work with modelled salinity data#####
salty<-readRDS("mini_sal_GLORYS.rds")
mini_salty<-salty%>%dplyr::select(c(GLORYS_sal,Site))
donedone_glorys<-left_join(full, mini_salty,relationship = "many-to-many")
donedone_glorys<-donedone_glorys%>%
  mutate("sal.1m"=ifelse(is.na(sal.1m)==T,GLORYS_sal,sal.is),.keep="all")%>%
  distinct(Site, .keep_all=T)
#write.csv(minimini, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/GLORYS_sal_matched_to_Site.csv")
##add sst##############################
f<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/WHIP_SWDs.csv")
str(f)
ff<-f%>%dplyr::select(c(Site,SST_Sat))
d<-ff%>%full_join(donedone_glorys,ff, by="Site")
dd<-d%>%mutate("CRW_SST"=ifelse(is.na(temp.1m)==T,SST_Sat,temp.is), .keep="all")
#add chla from Jessie!!!!!####
library(raster)
library(tidyverse)
library(sp)
library(sf)
library(reshape2)
setwd("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/")
df<-dd
# read in dataset
df$Date = format(dd$Date, format = "%m/%d/%Y")

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

setDT(df1)[,DT_DATE := DateTime] # name of date column in your dataset
setDT(df2)[,DT_DATE := date] # name of date column in df2

# merge d1 and df2 by matching the nearest date from df2 (ocean color 8-day dates) to the date in df1 (your data)
df11<-df1%>%dplyr::select(!moon_date)
merged <- df2[df11,on=.(date=DT_DATE),roll="nearest"]
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
duh2<-duh%>%mutate("chla"=ifelse(is.na(chla)==T,chlor_8day,chla))%>%distinct(Site,.keep_all = T)
#match monthly ocean color data to provide the more general productivity of the area at broader timescales 
#run this script=bathy_HI_30m_buffered_poly_1km.R
#You should be able to run the same script and just change the "data" file to the monthly file and change the "8day" classifiers to "monthly."
#final format for matching####
duh3<-as.data.frame(duh2)
duh4<-duh3%>%dplyr::select(!c(specimen_identification,stage, ID_morph_and_PCR,data.source, count, comb_length, length_occurence))
#write.csv(duh4, "C:/github/billfish_2024/Deliverable3_1219.csv")
duh5<-duh4%>%left_join(larv, duh4,  by="Site")
#write.csv(duh5, "C:/github/billfish_2024/Specimen_and_Env_1219.csv")
duh6<-full_join(larv, duh4,  by="Site")
pa<-duh6%>%mutate("billfish_present"=ifelse(is.na(count)==F,T,F))%>%
  mutate("ta_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Tetrapturus angustirostris",T,F))%>%
  mutate("mn_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Makaira nigricans",T,F))%>%
  mutate("xg_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Xiphias gladius",T,F))%>%
  mutate("ka_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Kajikia audax",T,F))
pap<-pa%>%distinct(Site,.keep_all = TRUE)
#write.csv(pap, "C:/github/billfish_2024/PA_Env_1219.csv")

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

