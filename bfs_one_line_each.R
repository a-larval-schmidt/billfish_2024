#extend df to one line per larva
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
library(fuzzyjoin)
#old strategy########
df<-read.csv("~/billfish_not_github/Product 3_ Envrionmental Data Paired with Billfish Specimen Data.csv")
df2<-df%>%
  distinct(specimen_identification,.keep_all=T)%>%
  mutate(length_occurence=
           ifelse(is.na(length_occurence),0,length_occurence))%>%
  unite("specimen_identification_len",c(specimen_identification, standard_length_mm), sep="_", remove=F)%>%
  uncount(length_occurence, .remove=F)%>%
  rename("vial_id"=specimen_identification)%>%
  mutate("length_occurence"=1)


df3<-df2%>% uncount(length_occurence,.id="id_modifier", .remove=F)%>%
unite("specimen_identification_new",c(specimen_identification_len,id_modifier), sep="_", remove = T)%>%
  mutate("real_count"=1)%>%
  mutate("is_or_is"=ifelse(count==real_count,T,F))

df4<-df3%>%distinct(specimen_identification_new, .keep_all = T)

df1<-anti_join(df,df4,by = join_by("specimen_identification"=="vial_id")) #uncount this using the count column and then rbind this to df3
df1<-df1%>%uncount(count, .id="id_modifier", .remove=F)%>%
  rename("vial_id"=specimen_identification)%>%
  unite("specimen_identification_new", c(vial_id,id_modifier), sep="_", remove = F)%>%
  mutate("real_count"=1)%>%
  dplyr::select(!(id_modifier))%>%
  mutate("is_or_is"=ifelse(count==real_count,T,F))

setdiff(colnames(df1),colnames(df4))
df5<-rbind(df1, df4)

summary(df1$is_or_is)
#write.csv(df5, "~/billfish_not_github/df5_01142025.csv")

#strategy which does not give unique IDs across different length values#######
df<-read.csv("~/billfish_not_github/Product 3_ Envrionmental Data Paired with Billfish Specimen Data.csv")
#make a different df without lengths and uncount by count
dfn<-df%>%filter(is.na(standard_length_mm))%>%
  uncount(count, .id="id_mod",.remove=F)%>%
  unite(new_id, c(specimen_identification, id_mod), remove=F)


#make a df of larvae with lengths and then uncount by length occurence
dfl<-df%>%filter(!is.na(standard_length_mm))%>%
  unite("specimen_identification",c(specimen_identification, standard_length_mm), sep="_", remove=F)%>%
  uncount(length_occurence, .id="id_mod", .remove=F)%>%
  unite(new_id, c(specimen_identification, id_mod), remove=F)


setdiff(colnames(dfl),colnames(dfn))

#rbind these together
dfr<-rbind(dfl,dfn)


df_anti<-anti_join(dfr,df5,by=join_by("new_id"=="specimen_identification_new"))


#strategy where we start completely from scratch#####
mas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_2025.csv")
#mas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_20241122.csv")
#mas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_20241217.csv")
og<-filter(mas, data.source!="box")
og %>% group_by(taxa)%>%summarize(sum(as.numeric(count)),na.rm=TRUE)
og%>%filter(Year<2019)%>%summarize(sum(count, na.rm=T))#gives 1597
mas%>%summarize(sum(as.numeric(count)),na.rm=TRUE)
mas %>% group_by(taxa)%>%summarize(sum(as.numeric(count)),na.rm=TRUE)
mas%>%summarize(sum(count, na.rm=T))#gives 2048
#mas<-mas%>%filter(Year<2019)
mas2<-mas %>%
  mutate("dre_length"=as.numeric(dre_length))%>%
  mutate("unknown.sizes"=as.numeric(unknown.sizes))%>%
  mutate("unknown.sizes"=ifelse(is.na(dre_length)==F,0,unknown.sizes))%>%
  mutate("ID.from.PCR2"=ifelse(ID.from.PCR=="", taxa,ID.from.PCR))%>% #trying to get pcr ids into genus column, alternatively just repalce Unk values alt. method from excel, filter out 'unknowns" in ID.from.PCR column
  mutate("ID_morph_and_PCR"=ifelse(ID.from.PCR2=="Mn", "Makaira nigricans",ifelse(ID.from.PCR2=="Ta", "Tetrapturus angustirostris", 
                                                                                  ifelse(ID.from.PCR2=="Ka","Kajikia audax",
                                                                                         ifelse(ID.from.PCR2=="Xg","Xiphias gladius",
                                                                                                ifelse(ID.from.PCR2=="Unk.Istiophoridae", "Unk.Istiophoridae",taxa))))))
mas2%>%summarize(sum(count, na.rm=T))#gives 2012, now gives 1958
mas2%>%distinct(specimen_identification, .keep_all = T)%>%summarise(sum(count, na.rm=T))#1901...soooo 111 duplicate specimen IDs?
xu <- mas2[duplicated(mas2$specimen_identification),]
xu$specimen_identification
mas2 %>%group_by(taxa)%>%summarize(sum(count),na.rm=TRUE) 
mas%>%distinct(specimen_identification, .keep_all = T)%>%group_by(taxa)%>%summarize(sum(count),na.rm=TRUE)
mas2%>%group_by(taxa)%>%summarize(sum(as.numeric(unknown.sizes),na.rm=TRUE)) #values match those of bff; this is an UNDER COUNT

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
larv%>%summarize(sum(count, na.rm=T))#gives 2048
larv%>%summarise(sum(length_occurence, na.rm=T))
for_counting<-re_mas%>%distinct(Site, .keep_all=T)
for_counting%>%
  group_by(ID_morph_and_PCR)%>%
  summarise(count=sum(count, na.rm=T), freq=sum(length_occurence,na.rm=T))
df<-re_mas
df2<-df%>%
  distinct(specimen_identification,.keep_all=T)%>%
  mutate(length_occurence=
           ifelse(is.na(length_occurence),0,length_occurence))%>%
  unite("specimen_identification_len",c(specimen_identification, comb_length), sep="_", remove=F)%>%
  uncount(length_occurence, .remove=F)%>%
  rename("vial_id"=specimen_identification)%>%
  mutate("length_occurence"=1)


df3<-df2%>% uncount(length_occurence,.id="id_modifier", .remove=F)%>%
  unite("specimen_identification_new",c(specimen_identification_len,id_modifier), sep="_", remove = T)%>%
  mutate("real_count"=1)%>%
  mutate("is_or_is"=ifelse(count==real_count,T,F))

df4<-df3%>%distinct(specimen_identification_new, .keep_all = T)

df1<-anti_join(df,df4,by = join_by("specimen_identification"=="vial_id")) #uncount this using the count column and then rbind this to df3
df1<-df1%>%uncount(count, .id="id_modifier", .remove=F)%>%
  rename("vial_id"=specimen_identification)%>%
  unite("specimen_identification_new", c(vial_id,id_modifier), sep="_", remove = F)%>%
  mutate("real_count"=1)%>%
  dplyr::select(!(id_modifier))%>%
  mutate("is_or_is"=ifelse(count==real_count,T,F))

setdiff(colnames(df1),colnames(df4))
df5<-rbind(df1, df4)

#add env data to df5####
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
mini<-combooo%>%dplyr::select(c(Site,Date,Time.end, Time.start,LAT_DD_start,LONG_DD_start))

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
tsg5<-filter(tsg5, TempC<40)
tsg5<-filter(tsg5,0<TempC)
str(tsg5)
#rawfiles#
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
tsg6<-filter(tsg6, TempC<40)
tsg6<-filter(tsg6,0<TempC)
#2016 env data
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
meep2<-dplyr::select(meep, c(colnames(tsg3)))
tsg7<-rbind(tsg6,meep2)
tsg8<-tsg7%>%mutate(date=as.Date(datetime))
tsgdfname<-tsg8
tsg_mini<-semi_join(tsgdfname,mini,join_by(date==Date))
time_join<-full_join(tsg_mini, mini,join_by(between(time, Time.start,Time.end)), relationship="many-to-many")
tsg10<-time_join%>%dplyr::select(c(Site, TempC, Salinity))%>%filter(Salinity<40)%>%filter(TempC<40)%>%filter(TempC>0)%>%filter(Salinity>30)
tsg11 <- tsg10%>%
  group_by(Site) %>%
  summarise(TempC = mean(TempC, na.rm=T), Salinity= mean(Salinity, na.rm=T))
full_site_join<-fuzzy_left_join(combooo,tsg11,by=(c("Site"="Site")),match_fun = `==`)#Year, Site, LAT_DD_start,LONG_DD_start,Time.start, Time.end),#EndDateTime, StartDateTime),
full_site_join<-left_join(combooo,tsg11,by=c("Site"="Site"))#Year, Site, LAT_DD_start,LONG_DD_start,Time.start, Time.end),#EndDateTime, StartDateTime),
full<-full_site_join%>%
  mutate(DateTime=mdy_hm(DateTime))%>%
  mutate("moon_date"=suncalc::getMoonIllumination(date = StartDateTime,keep = "phase"))%>%
  mutate("phase"=moon_date$phase)%>%
  mutate("cat_moon"=lunar::lunar.phase(x = StartDateTime, shift=10, name=T))%>%
  mutate(dur=lubridate::interval(start=StartDateTime, end=EndDateTime))%>%
  mutate(duration=as.numeric(dur, "minutes"))%>%#filter(duration>0 & duration<60)%>%
  mutate("temp_fix"=ifelse(is.na(temp.1m)==T,TempC,temp.1m))%>% # let both exist in their own columns as well for comparison purposes
  mutate("sal_fix"=ifelse(is.na(sal.1m)==T,Salinity, sal.1m))%>%#let both exist in their own columns as well for comparison purposes
  #mutate("density"=count/vol.m3,na.rm=T)%>%
  rename("temp.is"=TempC)%>%
  rename("sal.is"=Salinity)


#tsg merge check#####
tsg.sum <- full %>% group_by(Cruise, across(c(Year))) %>% summarise(Ntsg.miss=n_distinct(Site[is.na(temp_fix)]), Ntsg=n_distinct(Site[!is.na(temp_fix)])) %>%
  mutate(miss.prop =100*(1-(Ntsg.miss/(Ntsg+Ntsg.miss))))%>%arrange(miss.prop)
tsg.sum<-tsg.sum%>%
  filter(!is.na(Year))%>%
  rename("Number of Stations Missing TSG Data"=Ntsg.miss, "Number of TSG Files availible"=Ntsg)%>%
  mutate("Percentage of Stations with TSG Data"=round(miss.prop,1), .keep="unused")
library(kableExtra)
tsg.sum%>%kbl()%>%kable_styling()

#env+ larv#####
specimen_join<-left_join(df5,full,join_by("Site"=="Site")) #want larv into sites since the focus is on sites and then just if a bf was found there
hand<-dplyr::select(specimen_join,c(Site, Year.x, Cruise, station, data.source, vial_id, specimen_identification_new, stage, count,real_count, is_or_is, length_occurence, comb_length, paper_length_num, temp.is, sal.is,temp_fix, sal_fix, chla))
