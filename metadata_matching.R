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
str(slick_var)

slick<-slick %>%
  mutate("Date"=mdy(Date)) %>%
  mutate("DateTime"=mdy_hm(DateTime)) %>%
  mutate("Time"= parse_time(str_pad(as.character(Time), 4, pad = "0"), "%H:%M"))%>%
  rename("lat_start"=Latitude) %>%
  rename("long_start"=Longitude) %>%
  mutate("tow.depth.category"=ifelse(depth>-1,"surface","subsurface"),keep=T)

ggplot(slick, aes(x=Transect, y=tow.length, color=sample))+geom_point() #multiple samples/transect
goop<-group_by(slick, Transect, sample)
#need lat long from Jon for transects 53-59

str(slick_var)
slick_var <-slick_var %>%
  rename("Transect"=tran) %>%
  rename("Site"=site)

#if you want to merge the full dataset, skip this next step with select()
slick_var_short <-slick_var %>%
  select(Transect, sample, temp.1m, sal.1m, fluo.1m, chla, chltot)
slick_whole_short<-full_join(slick, slick_var_short, by = join_by(Transect, sample)) #join by transect number (not sample)

#prep whip#####
str(slick_whole_short)
str(whip)
whip_ik<- whip %>%
  filter(str_detect(Tow.type, "6' IK|paravane"))
#make whip names consistent with slick names
whip_ik <-whip_ik %>%
  mutate("Date"=mdy(Date)) %>%
  mutate("tow.length"=(Length_km*1000)) %>%
  mutate("Habitat"=slick, remove=T) %>%
  mutate("Latitude"=LAT_DD_start) %>%
  mutate("Longitude"=LONG_DD_start) %>%
  mutate("dist.shore"=Dist2Shore_km) %>%
  mutate("temp.1m"=coalesce(sst.mean.tsg, as.numeric(Surface.Temp)),keep = "none") %>%
  mutate("sal.1m"=coalesce(sss.mean.tsg, as.numeric(Surface.Salinity)),keep = "none") %>%
  mutate("Time"= parse_time(str_pad(as.character(Time.start), 4, pad = "0"), "%H%M"))%>%
  mutate("Time.end"= parse_time(str_pad(as.character(Time.end), 4, pad = "0"), "%H%M"))%>%
  mutate("sample"=as.numeric(Station))%>%
  mutate("Gear"=gear) %>% 
  mutate("speed.kts"=coalesce(speed.kts, tow.sog.kt,tow.sog.mean.scs.kt),keep = "none") %>%
  mutate("Site"=as.character(Sample)) %>%
  mutate("calc.speed.knts"=((tow.length/(tow.duration*60))*1.944))
#QC compare calculated to written speed values
#use dist.gps, duration (coverted to seconds)= m/s *1.944 to get speed in knts

#what parts of WHIP dataset do we really want? would it be easier to exclude the vales that we do not want?
whip_ik_short<-whip_ik%>%
  select(!(c(direction, slick.og, slick.paper, slick.cromwell, slick.chck, 
             Processed.count, pair.ID, tow.sog.kt, paravane, Tow.type,#(pull out notes elements then drop??)
             Fish.removed, #(keep for unabridged version, drop for public one)
             mechanism,TL, LJFL,EFL,weight,eggs,flag,OBJECTID,Notes.joey,FLAG,Year, Month,Day,
             Total.Fish.Larvae, Fish.eggs,Fish.eggs.large, remove,remove.flag, UID_tran,
             paper.notes, Tow.type.1, Gear.paper, tow.length))) #leave all iterations of lat, long in

whip_ik_short<-whip_ik_short%>%
  filter(depth !="midwater")%>%
  mutate("chla"=NA) %>%
  mutate("chltot"=NA)%>%
  mutate("Platform"="ship")%>%
  mutate("mechanism"=NA) %>%
  mutate("Transect"=NA)%>%
  mutate("fluo.1m"=NA)
str(whip_ik_short)

#join slick and whip#######
diff_list<-setdiff(colnames(whip_ik_short),colnames(slick_whole_short)) #in whip not in slick
diff_list
slick_whole_short[,diff_list] <- NA
diff_list2<-setdiff(colnames(slick_whole_short),colnames(whip_ik_short)) #in slick not in whip
whip_ik_short[,diff_list2] <- NA

combo_whip_slick_short<-rbind(slick_whole_short, whip_ik_short)
combo_whip_slick_short<-combo_whip_slick_short%>%
  mutate("is_slick"= replace(Habitat, "Inside" == "Slick", "Outside"== "Ambient")) #make Habitat values consistent (inside/ouside vs ambient/slick)

#continue to coalesce duplicates across new df#####
str(combo_whip_slick_short)
#write.csv(combo_whip_slick_short,"C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short.csv",row.names = F)
#see about coalesceing these with whip once merged
#unite("DateTime.old",c(Date, Time),sep=" ",remove=F)%>%
#mutate("DateTime"=ymd_hms(DateTime.old))%>% 