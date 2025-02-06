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

#strategy where we start completely from scratch#####
mas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_01222025.csv")
#mas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_2025.csv")
#mas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_20241122.csv")
#mas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_20241217.csv")
og<-filter(mas, data.source!="box")
og %>% group_by(taxa)%>%summarize(sum(as.numeric(count)),na.rm=TRUE)
og%>%filter(Year<2019)%>%summarize(sum(count, na.rm=T))#gives 1597

mas%>%summarize(sum(as.numeric(count)),na.rm=TRUE)
mas %>% group_by(taxa)%>%summarize(sum(as.numeric(count)),na.rm=TRUE)
mas%>%summarize(sum(count, na.rm=T))
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

mas_long<-mas2%>% #2/5/25 added unknown sizes to line 46, downstream is all messed up, but this probab;y solves the issue we're having with dropped paper.notes larvae
  
  dplyr::select(c(specimen_identification,unknown.sizes,X01mm:X100m))%>%
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
#add dropped larvae######
larv<-df5%>%dplyr::select(c(Site, vial_id,specimen_identification_new,specimen_identification_edited_Nov,vial,ID_morph_and_PCR,taxa,family,stage, stage_QC_flag,comb_length,length_occurence,real_count,count,count.sum))
og<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/downloaded_Feb2024_MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240202.csv")
discrep_site<-anti_join(og, larv, by=join_by("sample"==Site))
disc<-discrep_site%>%filter(!is.na(Year))%>%
  dplyr::select(c(vial,X1mm:X100.mm))%>%
  pivot_longer(X1mm:X100.mm,names_to="paper_length",
               values_to="length_occurence",values_drop_na = T)
og<-og%>%dplyr::select(!c(X1mm:X100.mm))
disc2<-disc%>%
  filter(length_occurence>0)%>%
  mutate("paper_length_num"=gsub("mm","",paper_length), .keep="unused")%>%
  mutate("paper_length_num"=gsub("X","",paper_length_num))%>%
  mutate("paper_length_num"=gsub("m","",paper_length_num))%>%
  mutate("paper_length_num"=as.numeric(paper_length_num))
disc3<-left_join(discrep_site,disc2, by=join_by(vial))
disc3<-disc3%>%
  distinct(vial,.keep_all = T)%>%
  dplyr::select(!c(X1mm:X100.mm))%>%
  filter(!is.na(Year))%>%
  rename(c("comb_length"=paper_length_num,"specimen_identification"=vial))%>%
  uncount(count,.id="id_modifier", .remove=F)%>%
  unite("specimen_identification_new", c(specimen_identification,id_modifier), sep="_", remove = F)%>%
  mutate("real_count"=1)%>%
  mutate("tofix"=ifelse(length_occurence==unknown.sizes,T,F))

disc3<-disc3%>%mutate(unknown.sizes=ifelse((tofix==T & specimen_identification_new=="TC0203002BF01-02_1"),
                                           (gsub(1,0,unknown.sizes)),unknown.sizes))%>%
  mutate(length_occurence=ifelse((tofix==T & specimen_identification_new=="TC0203002BF01-02_2"),
                                 (gsub(1,0,length_occurence)),length_occurence))%>%
  mutate(comb_length=ifelse((tofix==T & specimen_identification_new=="TC0203002BF01-02_2" & length_occurence==0),NA,comb_length))%>%
  dplyr::select(!c(tofix,X,X.1,id_modifier,count))%>%
  rename(c("count"=real_count, "vial_id"=specimen_identification))%>%
  mutate("ID_morph_and_PCR"=taxa)

diff_list<-setdiff(colnames(df5),colnames(disc3)) #in whip not in slick
not_disc<-matrix(data=NA,ncol=length(diff_list),nrow=nrow(disc3),)
diff_list<-as.list(diff_list)
not_disc<-as.data.frame(not_disc)
names(not_disc)=diff_list
disc4<-disc3%>%add_column(not_disc)
setdiff(colnames(disc4),colnames(df5))
disc4<-disc4%>%dplyr::select(!sample)%>%
  mutate(real_count=1)
df5<-rbind(df5,disc4)
#add env data to df5####
#combo<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_short12.csv")
combo<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_longJan272025.csv")
combo<-combo%>%distinct(Site, .keep_all=T)
comboo<-combo%>%
  unite("dend",c(Date,Time.end),sep=" ",remove = F)%>%
  unite("send",c(Date,Time.start),sep=" ", remove=F)%>%
  mutate(Date=ymd(Date))%>%
  mutate(Time.end=str_sub(Time.end, end=8))%>%
  mutate(Time.start=str_sub(Time.start,end=8))
str(comboo)
combooo<-comboo%>% #times in local
  mutate(EndDateTime=lubridate::ymd_hms(dend, tz="HST"), .keep="unused")%>%
  mutate(StartDateTime=lubridate::ymd_hms(send,tz="HST"), .keep="unused")
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

#tsg2####
#tsg2<-tsg1%>% mutate(datetime=ymd_hms(datetime),.keep="all")%>%mutate(Date=lubridate::ymd(Day))%>%mutate(local_datetime=with_tz(datetime, "HST"))%>%##view time as HST to match combo datamutate(time2=str_replace_all(time,"[:alpha:]",":"))%>%mutate(time3=str_sub(time2, end=-2))%>%mutate(time=hms(time3))
tsg2<-tsg1%>% 
  mutate(datetime=ymd_hms(datetime),.keep="all")%>%
  mutate(time5=ifelse(grepl("H",time),time,paste("00:",time)))%>%
  separate_wider_delim(time5,"H", names = c("hours","minutesseconds"),too_few = "align_end")%>% #  mutate(time6=str_pad(as.character(time), 9, pad = "0","left"))%>%
  mutate(Date=lubridate::ymd(Day))%>%
  mutate(local_datetime=with_tz(datetime, "HST"))%>%##view time as HST to match combo data
  mutate(minutesseconds=str_sub(minutesseconds, end=-5))%>%
  mutate("minsec"=str_replace_all(minutesseconds,"[:alpha:]",""))%>%
  mutate("hourz"=str_pad(as.character(hours), 2, pad = "0","left"))%>%
  mutate(hourz=ifelse(is.na(hourz)==T, "00", hourz))%>%
  mutate(minz=str_replace_all(minsec,"00: ",""))%>%
  mutate(minz=str_replace_all(minz," ",""))%>%
  mutate(minz=str_pad(as.character(minz), 2, pad = "0","left"))%>%
  unite("time",c(hourz, minz), sep=":")  
tsg2<-tsg2%>%dplyr::select(c(Year, TempC, Salinity, datetime,time, local_datetime,Day_Time_Julian))
str(tsg2) #times in UTC; this includes 1999 to 2006
#QC##
#append9703 and 9804####
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
  mutate(time=TIME_HST)%>%
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
  mutate(time=as.character(Time))%>%
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
  mutate(time=as.character(time2))%>%#lubridate::hms(time2))%>%
  mutate(time=str_sub(time, end=7))%>%
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
#to do, make list of non 6ft IK tows to remove from larval list
whip<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/WHIP_TowMetadata_20240419 - WHIP_TowMetadata.csv") 
whip_antik<- whip %>%filter(!str_detect(Tow.type, "6' IK|paravane")) %>%mutate("Site"=Sample)#this is just the ! version of the filter step in metadata matching
anti_site_list<-whip_antik$Site
slick<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/TowData_2016-2018_20211203 - TowData_2016-2018_20211203.csv")
#slick<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/422_edit_TransectMetadata_Whitneyetal2020_SuppTableS6 - Copy.csv") #couldnʻt figure out duration in R so did it in excel
#add the following to pipe if using the SuppTable version of the data:   mutate("tow.depth.category"=ifelse(depth>-1,"surface","sub-surface"),.keep="all")%>%mutate("gear"=ifelse(Platform== "Small Boat","1m straight-conical ring-net","6'IK"))
slick_verts<-slick %>%
  mutate("Sample"=str_pad(as.character(Sample.ID), 3, pad = "0"))%>%
  unite("Site", c(Cruise, Sample), sep="-", remove = F)%>%
  filter(Sample.type!="Neuston")
anti_neuston<-slick_verts$Site
anti_new<-df5%>%filter(Year>2020)
anti<-anti_new$Site
#antijoin'/anti filter ik? then pull site numbers only, then use that vector to anti filter larvae
listy<-as.list(c(anti_neuston,anti_site_list,anti))
df6<-df5%>%filter(!Site %in% listy)
specimen_join<-left_join(df6,full,join_by("Site"=="Site")) #want larv into sites since the focus is on sites and then just if a bf was found there
specimen_join<-left_join(df5,full,join_by("Site"=="Site")) #want larv into sites since the focus is on sites and then just if a bf was found there


#remote sensing#####
library(raster)
library(tidyverse)
library(sp)
library(sf)
library(reshape2)
library(rerddap)
library(rerddapXtracto)
library(httr)
##SST#####
WHIP_Tows<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_longJan272025.csv")
WHIP_Tows$Year<-year(WHIP_Tows$Date)
WHIP_Tows$Month<-month(WHIP_Tows$Date)
Years<-unique(WHIP_Tows$Year)
Months<-unique(WHIP_Tows$Month)
Years<-Years[!is.na(Years)]
Months<-Months[!is.na(Months)]
WHIP_Tows<-WHIP_Tows[!is.na(WHIP_Tows$LONG_DD_start),]
WHIP_Tows<-WHIP_Tows[!is.na(WHIP_Tows$Date),]
swchlInfo <- as.info("noaacrwsstDaily", url="https://coastwatch.noaa.gov/erddap/")
SST_Match <- rxtracto(swchlInfo, parameter = 'analysed_sst', 
                      xcoord = WHIP_Tows$LONG_DD_start, ycoord = WHIP_Tows$LAT_DD_start, tcoord = WHIP_Tows$Date, 
                      xlen = .2, ylen = .2, progress_bar = TRUE)
WHIP_Tows$SST_Sat<-SST_Match$`mean analysed_sst`
f<-WHIP_Tows
ff<-f%>%dplyr::select(c(Site,SST_Sat))

##modeled salinity data#####
salty<-readRDS("C:/github/billfish_2024/mini_sal_GLORYS.rds")
mini_salty<-salty%>%dplyr::select(c(GLORYS_sal,Site))
donedone_glorys<-left_join(full, mini_salty,relationship = "many-to-many")
donedone_glorys<-donedone_glorys%>%
  mutate("Salinity_data_source"=ifelse(is.na(sal.1m)==F,"CTD/TSG","GLORYS_SSS"))%>% 
  mutate("sal.1m"=ifelse(is.na(sal.1m)==T,GLORYS_sal,sal.is),.keep="all")%>%
  distinct(Site, .keep_all=T)

d<-ff%>%full_join(donedone_glorys,ff, by="Site")
dd<-d%>%  mutate("TempC_data_source"=ifelse(is.na(temp.1m)==F,"CTD/TSG","NOAA_CRW"))%>%
  mutate("CRW_SST"=ifelse(is.na(temp.1m)==T,SST_Sat,temp.is), .keep="all")

#add chla from Jessie!!!!!####
library(raster)
library(tidyverse)
library(sp)
library(sf)
 library(reshape2)
setwd("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/")
 # read in dataset
df<-dd
df$Date = format(dd$Date, format = "%m/%d/%Y")
df$Date = as.Date(df$Date, format = "%m/%d/%Y")

# # read in 8-day 4km ocean color data (.nc file)
 data = "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/ESA_OC_CCI_chla_kd490_1997_2021_8day.nc"
 #data = "ESA_OC_CCI_chla_kd490_1997_2019_monthly.nc"
#
chlor <-stack(data, varname = "chlor_a")
kd490 <-stack(data, varname = "kd_490")
#
# # use buffered bathymetry file to mask shallow waters around MHI
load("bathy_HI_30m_buffered_poly_1km.RData")
chlor_masked <- raster::mask(chlor, bathy_buffered_poly_1km, inverse = TRUE)
kd490_masked <- raster::mask(kd490, bathy_buffered_poly_1km, inverse = TRUE)
#
# # create new column of nearest 8-day value
library(data.table)
 
 df1 = df
 y=as.numeric(substr(names(chlor),2,5))
 m=as.numeric(substr(names(chlor),7,8))
 d=as.numeric(substr(names(chlor),10,11))
 df2 = as.data.frame(ymd(paste(y,m,d)))
 names(df2) <- "date"
#
 df1<-df1%>%dplyr::select(!moon_date)%>%
   mutate("Date"=ymd(Date))
 setDT(df1)[,DT_DATE := Date] # name of date column in your dataset
 setDT(df2)[,DT_DATE := date] # name of date column in df2
#
# # merge d1 and df2 by matching the nearest date from df2 (ocean color 8-day dates) to the date in df1 (your data)
 merged <- df2[df1,on=.(date=DT_DATE),roll="nearest"]
 df_8day = merged[,c("Date","DT_DATE","LAT_DD_start","LONG_DD_start")]
#
# # note 1997 data only goes back to 09/1997 but the cruise for that year is 04/1997, so wonʻt have chlor data for that cruise
 df_8day = subset(df_8day, Date > "1997-12-31")
 df_8day = df_8day[complete.cases(df_8day),]
 df_8day = unique(df_8day) # this line removes duplicate date/lat/lon combinations
# # create empty columns for chlorophyll and kd490
 df_8day$chlor_8day <- NA
 df_8day$kd490_8day <- NA
#
 df_8day<-df_8day%>%
   filter(is.na(LAT_DD_start)!=T)%>%
   filter(is.na(LONG_DD_start)!=T)
#
 for(i in 1:nlayers(chlor)) {

    #i = 30

   year=as.numeric(substr(names(chlor_masked[[i]]),2,5))
   month=as.numeric(substr(names(chlor_masked[[i]]),7,8))
   day=as.numeric(substr(names(chlor_masked[[i]]),10,11))
   ymd = ymd(paste(year,month,day))

   idx <- which(df_8day$DT_DATE == ymd)

   # need to average layer to get rid of z-value that is causing issues in extract
   ch = mean(chlor_masked[[i]])
   k = mean(kd490_masked[[i]])
#
   if (length(idx)>0){

     pts <- SpatialPoints(df_8day[idx,c('LONG_DD_start', 'LAT_DD_start')], crs(ch))

     df_8day$chlor_8day[idx] <- raster::extract(ch, pts)
     df_8day$kd490_8day[idx] <- raster::extract(k, pts)

   }
   else if (length(idx)==0) {}

   print(paste("Completed",i,"of",nlayers(chlor_masked),"layers"))

 }
#write.csv(df_8day,"C:/Users/Andrea.Schmidt/Documents/billfish_not_github/df_8day_jan28.csv")

# merge matched ocean color back with full dataframe
#df1<-dd #GOT STUCK HERE
#df_8day<-df_8day%>%mutate(DT_Date=ymd(DT_DATE))
#mini8<-df_8day[,c("Date","LONG_DD_start", "LAT_DD_start")]
#duh = left_join(mini8, df1,by=c("Date"))#, all.y=T)

full_df= left_join(df1, df_8day[,-"DT_DATE"], by = c("Date","LONG_DD_start", "LAT_DD_start"),relationship = "many-to-many")
duh = left_join(df, df_8day[,-"DT_DATE"], by = c("Date","LONG_DD_start", "LAT_DD_start"))

#dull=left_join(df1, df_8day[,-"DT_DATE"],by =c('LONG_DD_start', 'LAT_DD_start'),relationship = "many-to-many")
duh2<-full_df%>%
  mutate("chla_data_source"=ifelse(is.na(chla)==F,"bottle collections","ESA_OC-CCI")) %>%
  mutate("chla"=ifelse(is.na(chla)==T,chlor_8day,chla))#%>%
duh3<-as.data.frame(duh2)
combo<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_short12.csv")
ik_list<-combo$Site
all_env_data<-duh3%>% 
  mutate("consistent_with_product_1"=ifelse(Site %in% ik_list,TRUE, FALSE))%>%
  distinct(Site,.keep_all=T)
#write.csv(all_env_data,("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/all_env_data.csv"))

#if you want dates/lats/longs for bet larvae#########
bet<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/PIFSC_billfish_larvae_2022.csv")
bet<-bet%>%dplyr::select(c(Site, cruise,Date_YYYYMMDD,Lat_In,Lon_In,Net_type,Time_In_HST,Time_Out_HST,Max_Depth_m))%>%
  rename(c("Cruise"=cruise, "Date"=Date_YYYYMMDD,"LAT_DD_start"=Lat_In,"LONG_DD_start"=Lon_In,
           "Gear"=Net_type,"Time"=Time_In_HST,"Time.end"=Time_Out_HST, "tow.depth.category"=Max_Depth_m))
diff_list<-setdiff(colnames(all_env_data),colnames(bet)) #in whip not in slick
not_bet<-matrix(data=NA,ncol=length(diff_list),nrow=nrow(bet),)
diff_list<-as.list(diff_list)
not_bet<-as.data.frame(not_bet)
names(not_bet)=diff_list
bet<-bet%>%add_column(not_bet)
hall_env_data<-rbind(bet,all_env_data)
#larv+ remote sensing#####
str(df5)
larv<-df5%>%dplyr::select(c(Site, vial_id,specimen_identification_new,
                            ID_morph_and_PCR,taxa,family,stage, stage_QC_flag,comb_length,
                            length_occurence,real_count,count,count.sum))%>%
  distinct(specimen_identification_new, .keep_all=T)
env_larv_data<-full_join(all_env_data,larv,join_by("Site"=="Site"),relationship = "many-to-many") #want sites into larv since the focus is on larv
env_larv<-env_larv_data%>%
  mutate("specimen_identification_final"=ifelse(is.na(specimen_identification_new)==T,
                                                "No billfish were recorded at this site",
                                                specimen_identification_new))
#selections for doublechecking######
thirdcheck<-env_larv%>%
  #filter(Year<=2022)%>%
  #filter(consistent_with_product_1==T)%>%
  distinct(specimen_identification_final,.keep_all=T)%>%
  rename("specimen_identification"=specimen_identification_final,
                              "standard_length_mm"=comb_length,"temp_is"=temp.is,"sal_is"=sal.is,
                               "chla_is"=chla, "species"=ID_morph_and_PCR)%>%
  mutate("datetime_txt"=as.character(DateTime))%>%
  mutate("date_txt"=as.character(Date))%>%
  mutate("density"=count/vol.m3)%>%
  mutate("count"=ifelse((specimen_identification!='No billfish were recorded at this site' & is.na(real_count)==T),1,real_count))%>%
  dplyr::select(c(specimen_identification,
                  species,
                  count,
                  standard_length_mm,
                  length_occurence,
                  stage,
                  Transect,
                  Station,
                  Site,
                  Cruise,
                  Year,
                  datetime_txt,
                  date_txt,
                  Time.start,
                  Time.end,
                  LAT_DD_start,
                  LONG_DD_start,
                  Habitat,
                  Platform,
                  Gear,
                  mesh,
                  vol.m3,
                  density,
                  tow.length,
                  dist.shore,
                  tow.duration,
                  tow.depth.category,
                  day.night,
                  tow.speed.kts,
                  LAT_DD_end,
                  LONG_DD_end,
                  temp.1m,
                  sal.1m,
                  fluo.1m,
                  #chla,
                  chltot,
                  TempC_data_source,
                  Salinity_data_source,
                  chla_data_source,
                  CRW_SST,
                  GLORYS_sal,
                  chlor_8day,
                  temp_is,
                  sal_is,
                  chla_is,
                  #lunar_illumination,
                  cat_moon,
                  `consistent_with_product_1`))%>%
  distinct(specimen_identification,.keep_all=T)

write.csv(thirdcheck,("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/product3_feb4.csv"))
#plotting#####
fin<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/CSVproduct3_final_all_tow_types.csv")
kona_map+geom_point(data=fin, aes(x=LONG_DD_start, y=LAT_DD_start, color=chla_is))+
  theme(legend.text = element_text(size=14))+scale_color_viridis_c(option="turbo")

nafin<-fin%>%filter(stage!="")
nafin$stage <- factor(nafin$stage, levels=c("preflexion", "flexion", "post flexion"))
ggplot(data=nafin, aes(x=stage, y=standard_length_mm))+
  geom_boxplot()+facet_grid(~species)+
  labs(x="Developmental Stage", y="Standard Length (mm)")

ggplot(data=fin, aes(x=species, y=standard_length_mm))+
  geom_boxplot()+
  labs(x="Species Identity", y="Standard Length (mm)")

library(treemap)
fin%>%
  mutate(count=1)%>%
  group_by(species)%>%
  summarise(Sum.spp=sum(count))%>%
  mutate(Species.Index=paste(species, Sum.spp, sep ="\n"))%>%
  treemap(index="Species.Index", vSize="Sum.spp")

ex<-fin%>%filter(consistent_with_product_1==T)%>%
  filter(is.na(stage)==F)%>%
  filter(is.na(standard_length_mm)==F)
  


#was qc, but clearly not quality######
og<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/downloaded_Feb2024_MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240202.csv")
larv<-df5%>%dplyr::select(c(Site, vial_id,specimen_identification_new,specimen_identification_edited_Nov,vial,ID_morph_and_PCR,taxa,family,stage, stage_QC_flag,comb_length,length_occurence,real_count,count,count.sum))
discrep_larv<-anti_join(og, larv, by=join_by("vial"==vial))
discrep_site<-anti_join(og, larv, by=join_by("sample"==Site))
nrow(discrep_larv) #so 82 larvae/vials are different from og than larv
nrow(discrep_site)
disc<-discrep_site%>%filter(!is.na(Year))
sum(disc$count)
sum(disc$count.sum, na.rm=T)

check<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/product3_2025_Jan28.csv")
submitted<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/Product 3_ Envrionmental Data Paired with Billfish Specimen Data.csv")
discrep_larv<-anti_join(check, submitted, by=join_by("specimen_identification"==specimen_identification))
discrep_site<-anti_join(check, submitted, by=join_by("Site"==Site))
nrow(discrep_larv)
nrow(discrep_site)

#QC emergencythe most original#########
og<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/original_billfish_xiphiidae_counts.csv")
mas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_01222025.csv")
mine<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/feb05_product3_final_all_tow_types.csv")
mine<-mine%>%filter(Year<=2020)
#missing_in_mas<-anti_join(og, mas, by=join_by("sample"=="Site"))
#missing_in_og<-anti_join(mas,og, by=join_by("Site"=="sample"))

mine<-mine%>%
  mutate(count=1)%>%
group_by(Site,species)%>%
  summarize(new_sum=sum(as.numeric(count), na.rm=T))

og<-og%>%
  mutate(taxa2=ifelse(taxa=="Unk.Istiophorid","Unk.Istiophoridae",taxa))%>%
  group_by(sample,taxa2)%>%
  summarize(og_sum=sum(count, na.rm=T))
#by <- join_by(mas, within())
q<-full_join(mine, og,by=join_by(Site==sample, species==taxa2))
q<-q%>%
  #take difference between counts from og to mas
  mutate(disc=og_sum-new_sum)%>%
  filter(disc!=0)%>%
  filter(species!="Unk.Istiophoridae"|disc<0)%>%
  filter(disc>0)
#find difference: to see what needs to be changed from unk. to actually IDd
#270 that were id


#update product to be finished

#STILL UNDER CONSTRUCTION: 01/22/25: presence-absence data set#####
#combo<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/combo_whip_slick_shortJan25.csv")
whip2<-whip%>%
  mutate("Site"=Sample, .keep="all")%>%
  filter(depth !="midwater")%>% #remove non-neustonic tows
  mutate("Habitat"=ifelse(slick== "Inside","Slick","Ambient"),.keep="unused")%>%
  mutate("tow.depth.category"=depth, .keep="all")%>%
  mutate("Date"=mdy(Date)) %>%
  mutate("tow.length"=(Length_km*1000),.keep = "unused") %>%
  mutate("LAT_DD_start"=ifelse(is.na(LAT_DD_start)==F,LAT_DD_start,LAT_DD_mid),.keep = "all" ) %>%
  mutate("LONG_DD_start"=ifelse(is.na(LONG_DD_start)==F,LONG_DD_start,LONG_DD_mid),.keep = "all") %>%
  mutate("dist.shore"=Dist2Shore_km*1000) %>%
  mutate("temp.1m"=coalesce(sst.mean.tsg, as.numeric(Surface.Temp)),.keep = "unused") %>%
  mutate("sal.1m"=coalesce(sss.mean.tsg, as.numeric(Surface.Salinity)),.keep = "unused") %>%
  mutate("Time.start"= ifelse(nchar(Time.start)<3, str_pad(as.character(Time.start), 3, side="right", pad = "0"), Time.start))%>% 
  mutate("Time.end"= ifelse(nchar(Time.end)<3, str_pad(as.character(Time.end), 3, side="right", pad = "0"), Time.end))%>% 
  mutate("Time.end"= parse_time(str_pad(as.character(Time.end), 4, side="left", pad = "0"), "%H%M"))%>% 
  mutate("Time"= parse_time(str_pad(as.character(Time.start), 4, side="left", pad = "0"), "%H%M"))%>% 
  mutate("Time.start"= Time, .keep="all")%>% 
  mutate("sample"=Sample,.keep="all")%>% #from v2 to v3 sample was as.numeric(station) is now as.numeric(Sample)
  mutate("Station"=as.numeric(Station),.keep="all")%>%
  unite("transect_sample_habitat",c(Cruise,UID_tran,sample,Habitat),sep = "_", remove=F)%>%
  mutate("Gear"=gear) %>% 
  mutate("tow.speed.kts"=coalesce(tow.sog.kt,tow.sog.mean.scs.kt,speed.kts),.keep = "unused") %>%
  mutate("Site"=as.character(Sample)) %>%
  mutate("calc.speed.knts"=((tow.length/(tow.duration*60))*1.944))%>% #use dist.gps, duration (converted to seconds)= m/s *1.944 to get speed in knts
  unite("DateTime.old",c(Date, Time),sep=" ",remove = F)%>%
  rename("Transect"=UID_tran)
whip3<-whip2%>% #times in local
  mutate(EndDateTime=lubridate::mdy_hms(Time.end, tz="HST"), .keep="unused")%>%
  mutate(StartDateTime=lubridate::mdy_hms(Time.start,tz="HST"), .keep="unused")%>%
  mutate("DateTime"=ymd_hms(DateTime.old))
whip4<-whip3%>%dplyr::select(c(Site,Cruise,DateTime,StartDateTime, EndDateTime,LAT_DD_start, LONG_DD_start, LAT_DD_end, LONG_DD_end))
micro<-anti_join(full,whip4)#,by=join_by(Site))
micro<-micro%>%dplyr::select(!X:X.77)
specimen_join<-specimen_join%>%dplyr::select(!X:X.77)
micro2<-left_join(specimen_join,micro)#,join_by("Site"=="Site"))#,relationship = "many-to-many")

##presence-absence
pa<-micro2%>%mutate("billfish_present"=ifelse(is.na(count)==F,T,F))%>%
  mutate("ta_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Tetrapturus angustirostris",T,F))%>%
  mutate("mn_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Makaira nigricans",T,F))%>%
  mutate("xg_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Xiphias gladius",T,F))%>%
  mutate("ka_present"=ifelse(is.na(count)==F & ID_morph_and_PCR=="Kajikia audax",T,F))

#wayyyy fewer sites than expected; needs to keep sites without larvae as well
pap<-pa%>%dplyr::select(c(billfish_present, ta_present, mn_present,xg_present, ka_present,Site,Cruise,DateTime,Time.start, Time.end,
                          LAT_DD_start, LONG_DD_start, LAT_DD_end, LONG_DD_end))%>%
  distinct(Site,.keep_all = TRUE) #write.csv(pap, "C:/github/billfish_2024/Sites_Env_bf_PA_1220.csv")

all_tows<-full


#trying to get NAs to parse from tsg1 with time 11/27/25###########
#%>%
  unite("time3",c(Date, time2),sep=" ")%>%
  mutate("timey"=as.POSIXlt(time3, tz="HST", format="%Y-%m-%d %H:%M"))


tsg2$time2
#%>%
  mutate(time3=str_replace_all(time2," ", ""))%>%
  mutate(time3=str_replace_all(time3,"[:alpha:]",""))%>%
  mutate(time3=str_sub(time3, end=-2))%>%
  mutate(time3=str_remove(time3, "^:"))%>%
  mutate(time4=str_replace_all(time3," ", ""))


tsg2$time7 <- ifelse(nchar(tsg2$time4)<6, (str_sub(tsg2$time4, start = 4,end=3, omit_na = FALSE) <- "0"),tsg2$time4)
tsg2$time7
ifelse(nchar(tsg2$time7)<5,str_sub(tsg2$time4, start = 3, end = 3, omit_na = FALSE) <- ":0",tsg2$time4)
tsg2<-tsg2%>%
    mutate(time6=str_pad(as.character(time7), 8, pad = "0","right"))#%>%
tsg2$time4
tsg2$time6

#COUNT CHECKS#####
summarize(specimen_join,sum(as.numeric(real_count, na.rm=T)), sum(count, na.rm=T),sum(length_occurence,na.rm = T))
summarize(df6,sum(real_count, na.rm=T), sum(count, na.rm=T),sum(length_occurence,na.rm = T))
summarize(df5,sum(real_count, na.rm=T), sum(count, na.rm=T),sum(length_occurence,na.rm = T))

nrow(distinct(specimen_join, Site))
nrow(distinct(whip, Sample))
nrow(distinct(whip_antik, Site))
nrow(distinct(full, Site))

sp_joi<-specimen_join%>%
  dplyr::select(c(ID_morph_and_PCR,#specimen_identification,
                  specimen_identification_new,
                  real_count,
                  comb_length, #standard_length_mm,
                  length_occurence,
                  stage,
                  Station,
                  Site,
                  Cruise,
                  Year.x,
                  DateTime,
                  Date,
                  Time.start,
                  Time.end,
                  LAT_DD_start,
                  LONG_DD_start,
                  Habitat,
                  Platform,
                  Gear,
                  mesh,
                  vol.m3,
                  tow.length,
                  dist.shore,
                  tow.duration,
                  tow.depth.category,
                  day.night,
                  tow.speed.kts,
                  LAT_DD_end,
                  LONG_DD_end))#, temp_is, sal_is, chla_is))
f<-is.na(sp_joi$Date)==T
#write.csv(specimen_join, "~/billfish_not_github/bf_prod3_triplecheck.csv")

