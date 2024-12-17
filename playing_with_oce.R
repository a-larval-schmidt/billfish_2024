#playing with oce
library(dplyr)
library(lubridate)
library(tidyverse)
library(oce)
library(gsw)
#TSG-Temp.LAB files#######
#CTD data is output in .cnv. The function, read.oce() will parse CTD data. oce has the ability to parse non-cnv data types, called oceMagic()...read.ctd("C:/github/billfish_2024/CTD_SE_1704/SE1704CTD/SE1704CastSheets/CTD0013CastSheet.csv")
setwd("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827")
#focus on 2004-2006
#note sure where to start??try:  ?`[[,oce-method`
#tsgfiles <- dir("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/", recursive=TRUE, full.names=TRUE, pattern="TSG.LAB")#"*TSG.ACO$")
#template<- dir("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/", recursive=TRUE, full.names=TRUE, pattern="TSG.ACO")

tsgfiles <- dir("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/", recursive=TRUE, full.names=TRUE, pattern="TSG-Temp.LAB")
b<-read.table("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/TC0006/TSG-Temp.LAB")
b
read.csv(tsgfiles[1])
i=13
b<-read.table(tsgfiles[i])
str(b)
colnames(b) <- c("Year", "Day_Time_Julian", "Day_Julian", "Time_frac_of_day", "TempC","Conductivity","Salinity")#,"uncertain")#,"Temp2","Salinity","uncertain")
newyearsday=as_datetime(paste(b$Year[1],"-01-01"))
df<-b %>%
  mutate(Day=as_date(Day_Julian,origin=newyearsday))%>%
  mutate(time24=(Time_frac_of_day*24),.keep="unused")%>%
  mutate(time=hm(paste(floor(time24), formatC(round(60*(time24%%1),),width=2,flag=0), sep = ":")),.keep="unused")%>%
  unite("datetime",Day:time, sep=" ", remove=F)%>%
  mutate(datetime=as_datetime(datetime)) #if timezones are the cause of things not lining up, fix it here
print(df)
str(df)
outname = paste(df$Year[1],month(df$datetime[1]),'prepped_TSG.csv', sep = "") 
#write.csv(x=df, file=outname)#decimal time broke up by time along a particular transect, then take mean of values from that time stamp)
#combine csvs#####
tsgcsv <- dir("~/billfish_not_github//HistoricCruiseData_ChrisTokita20190827/processed_TSGs/", recursive=TRUE, full.names=TRUE, pattern="csv")
tsgvalues<-as_tibble(c(Year=NA,
                     Day_Time_Julian=NA,
                     Day=NA,
                     TempC=NA,
                     Salinity=NA,
                     datetime=NA,
                     time=NA))

tsgvalues<-read.csv("~/merged_tsgs.csv")
merge_tsg_csv = function(input) {
b<-read.csv(tsgcsv[1])
keeps <- c("Year", "Day_Time_Julian", "Day","TempC","Salinity","datetime","time")
b2<-b[keeps]
str(b2)
df<-read.csv("~/merged_tsgs.csv")
str(df)
df2<-df[keeps]
df3<-rbind(b2,df2)
str(df3)
print(nrow(b2)+nrow(df2))
print(nrow(df3))
ifelse((nrow(df3)==(nrow(b2)+nrow(df2))),print("yes"),print("no"))
write.csv(df3, file="~/merged_tsgs.csv")
}
for (i in 1:length(tsgcsv)) {
     merge_tsg_csv(tsgcsv[i])
   }
df<-read.csv("~/merged_tsgs.csv")
d<-df %>%
  mutate(datetime=as_datetime(datetime),.keep="unused")%>%
  mutate(date=ymd(Day),.keep="unused")%>%
  mutate(time=hms(str_replace_all(time,"[:alpha:]","")))
str(d)
#write.csv(d, file="~/merged_tsgs2.csv")
# 
# 
# #attempt at a function#######
# make_stn_mean(tsgfiles[6])
# is.wholenumber<-function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
# make_stn_mean = function(input) {
#   b<-read.table(input)
#   colnames(b) <- c("Year", "Day_Time_Julian", "Day_Julian", "Time_frac_of_day", "TempC","Conductivity","Temp2","Salinity","dbar")
#   newyearsday=as_datetime(paste(b$Year[1],"-01-01"))
#   df<-b %>%
#     mutate(Day=as_date(Day_Julian,origin=newyearsday))%>%
#     mutate(time24=(Time_frac_of_day*24),.keep="unused")%>%
#     mutate(time=hm(paste(floor(time24), formatC(round(60*(time24%%1),),width=2,flag=0), sep = ":")),.keep="unused")%>%
#     unite("datetime",Day:time, sep=" ", remove=F)%>%
#     mutate(datetime=as_datetime(datetime)) #if timezones are the cause of things not lining up, fix it here
#     outname = paste("processed_",input, '.csv', sep = "") 
#   write.csv(x=df, file=outname)#decimal time broke up by time along a particular transect, then take mean of values from that time stamp)
# }
# 
# #merge 
# combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
# 
# #match/merge each cruise data sheet into combo11######
# #add station number infomation for each cruise by matching start/end times of tows in combo to each cruise datasheet
# #find mean salinity by station
# #making pseudo sample locations



for (i in 1:length(tsgfiles)) {
  make_stn_mean(tsgfiles[i])
}


#########STOPPED HERE 09/27!!!!#########
#.RAW files############
ssfiles <- dir("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/", recursive=TRUE, full.names=TRUE, pattern="*.Raw")
drat<-read.csv(ssfiles[2])
#note to self first few lines of .raw files are a mess likely bubbles and equilibrating
prep_raw_csvs = function(input) {
  drat<-read.csv(ssfiles[i])
  if(ncol(drat)==17){
  colnames(drat)<-c("Date","Time","Instrument","time_no_sep", "lat_dd","lat_direction","lon_dd","lon_direction","X1","X9","X2.4","Salinity","M","X", "M.1","X.1","hex")
  raw2<-drat%>%
    unite("datetime",Date: Time, sep=" ", remove=F)%>%
    mutate(datetime=mdy_hms(datetime,tz="HST"))%>%#,format="%m/%d/%y %H:%M"))
    mutate(local_datetime=datetime, .keep="all")%>%
    mutate(date=date(datetime))%>%
    mutate(Year=year(datetime))%>%
    mutate(time=hms(Time))%>%
    mutate(TempC=as.numeric(M))%>% #originally was "M"
    mutate(Salinity=as.numeric(Salinity))%>%
    mutate(lat=as.numeric(lat_dd))%>%
    mutate(Day_Time_Julian=decimal_date(datetime))%>%
    mutate(lon=as.numeric(lon_dd))
  merp=paste(i)
  }
  else if (ncol(drat)<17){
    colnames(drat)<-c("DateTime","M", "Salinity","Temp2","maybe_conductivity")
    raw2<-drat%>%
      mutate(datetime=mdy_hms(DateTime,tz="HST"))%>%#,format="%m/%d/%y %H:%M"))
      mutate(local_datetime=datetime, .keep="all")%>%
      mutate(date=date(datetime))%>%
      mutate(Year=year(datetime))%>%
      mutate(time=hms(datetime))%>%
      mutate(TempC=as.numeric(M))%>% #originally was "M"
      mutate(Salinity=as.numeric(Salinity))%>%
      filter(Salinity>30)
  }
  else if(ncol(drat)==3){
    colnames(drat)<-c("Date","Time","X_TempC_dbar_Salinity_Conductivity")
    raw2<-drat%>%
      unite("datetime",Date: Time, sep=" ", remove=F)%>%
      mutate(datetime=mdy_hms(datetime,tz="HST"))%>%#,format="%m/%d/%y %H:%M"))
      mutate(local_datetime=datetime, .keep="all")%>%
      mutate(date=date(datetime))%>%
      mutate(Year=year(datetime))%>%
      mutate(time=hms(Time))%>%
      separate(col=X_TempC_dbar_Salinity_Conductivity,into=c("X","TempC","dbar","Salinity","Conductivity"), sep="        ")%>%
      mutate(TempC=as.numeric(TempC))%>%
      mutate(Salinity=as.numeric(Salinity))%>%
      mutate(dbar=as.numeric(dbar))%>%
      mutate(Conductivity=as.numeric(Conductivity))
  }
  else if(ncol(drat)==1){print("skip")}
  outname = paste("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/processed_RAW_TSGs/processed_RAW_TSG_",raw2$Year[i],"_",merp, '.csv', sep = "") 
  write.csv(x=raw2, file=outname)
}

for (i in 1:length(ssfiles)) {
  drat<-read.csv(ssfiles[i])
  if(ncol(drat)==17){prep_raw_csvs(ssfiles[i])
  }
  else if (ncol(drat)<17){}
  print(paste("Completed",i,"of",length(ssfiles),"files"))
}

raw_csvs<-dir("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/processed_RAW_TSGs_09_11/", recursive=TRUE, full.names=TRUE, pattern=".csv") 
#rawvalues<-(c(Year=NA,TempC=NA,Salinity=NA,datetime=NA,time=NA,date=NA,local_datetime=NA))
as.data.frame(rawvalues)
#raw_to_tsg_match=function(input) {
i=1+i
drat<-read.csv(raw_csvs[i])# if(ncol(drat)==12){
    drat2<-drat%>%
      dplyr::select(c(Year,TempC,Salinity,datetime,time,date,local_datetime))
    rawvalues<-rbind(rawvalues,drat2)# }else if (ncol(drat)==27){
    drat_no_sal<-drat%>%
      dplyr::select(c(Year,TempC,datetime,time,date,local_datetime))%>%
      mutate(Salinity=NA)
    rawvalues<-rbind(rawvalues,drat_no_sal)#}
for (i in 1:length(raw_csvs)) {
  raw_to_tsg_match(raw_csvs[i])
  print(paste("united",i,"of",length(raw_csvs),"files"))
  }
  
rawvalues2<-rawvalues%>%
  filter(Salinity<36)%>%
  filter(Salinity>30)
#write.csv(x=rawvalues2, file="C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/combined_raw_files_2009_2011.csv")

##oce and cnv files#####
f<-("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/SE1206_Processed/ctd0010cfatlobsx.cnv")
d <- read.ctd(f, columns=list(
  salinity=list(name="SAL",
                unit=list(unit=expression(),
                          scale="PSS-78"))))
s=d[["salinity"]]
plot(d, which="temperature")
#FOR REFERENCE ONLY: UKU MOCNESS FUNCTIon###############
moc_read_8604 = function(input) {
  d<-read.table(input)
  index=which(files==input)
  d<-d %>%filter(str_detect(string=d$V1,negate=T,pattern="\"") & str_detect(string=d$V1,negate=T,pattern="^[[:upper:]]")) 
  discrep<-ifelse(is.wholenumber((nrow(d)/10)-3), 0,((nrow(d)/10-3)%%1)*10) 
  d<-d %>% head(d,n=floor(-discrep))
  V1<-matrix(d[1,], nrow=(nrow(d)/10), ncol = 1)
  V2<-matrix(d[2,], nrow=(nrow(d)/10), ncol = 1)
  V3<-matrix(d[3,], nrow=(nrow(d)/10), ncol = 1)
  num_offset<-10
  V4<-matrix(d[seq(from=4, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V5<-matrix(d[seq(from=5, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V6<-matrix(d[seq(from=6, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V7<-matrix(d[seq(from=7, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1) 
  V8<-matrix(d[seq(from=8, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V9<-matrix(d[seq(from=9, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V10<-matrix(d[seq(from=10, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V11<-matrix(d[seq(from=11, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V12<-matrix(d[seq(from=12, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V13<-matrix(d[seq(from=13, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  max.len = max(length(V1), length(V2),length(V3),length(V4),length(V5),length(V6),length(V7),
                length(V8),length(V9),length(V10),length(V11),length(V12),length(V13))#set max length based on longest vector here
  #pad nas to fill space discrepancies between shortest and longest
  V1=c(V1,rep(NA, max.len-length(V1)))
  V2=c(V2,rep(NA, max.len-length(V2)))
  V3=c(V3,rep(NA, max.len-length(V3)))
  V4=c(V4,rep(NA, max.len-length(V4)))
  V5=c(V5,rep(NA, max.len-length(V5)))
  V6=c(V6,rep(NA, max.len-length(V6)))
  V7=c(V7,rep(NA, max.len-length(V7)))
  V8=c(V8,rep(NA, max.len-length(V8)))
  V9=c(V9,rep(NA, max.len-length(V9)))
  V10=c(V10,rep(NA, max.len-length(V10)))
  V11=c(V11,rep(NA, max.len-length(V11)))
  V12=c(V12,rep(NA, max.len-length(V12)))
  V13=c(V13,rep(NA, max.len-length(V13)))
  newdf<-as.data.frame(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13)) #CREATE DATAFRAME WITH CBIND
  newdf[,5:13]<-lapply(c(newdf[,5:13]), as.numeric)
  newdf<-rename(newdf,"cruise"="V1", "date"="V2","station_number"="V3","sal"="V7", "temp_DegC"="V6","depth_m"="V5", "time"="V4", "net_number"="V13", "record_number"="V9","time_bins_in_sec"="V12","net_angle"="V8")
  newdf$date<-mdy(newdf$date)#as.Date(newdf$date, format="%m-%d-%Y")#turns date to YMD format
  newdf$date<-ifelse(str_starts(newdf$time,"00", negate = F),(newdf$date+days(1)),newdf$date)
  newdf$date<-as_date(newdf$date)
  newdf<-unite(newdf, "date_time", c("date","time"), remove = F)
  newdf$date_time<-as.POSIXct(newdf$date_time,format="%Y-%m-%d_%H:%M:%OS")
  newdf<-unite(newdf, "moc_id", c(cruise,station_number,net_number),sep="_", remove=F)
  newdf$moc_id<-gsub("-","",as.character(newdf$moc_id))
  newdf$moc_id<-str_to_upper(newdf$moc_id, locale = "en")
  newdf<-unite(newdf, "moc_id_noNet", c(cruise,station_number),sep="_", remove=F)
  newdf$moc_id_noNet<-gsub("-","",as.character(newdf$moc_id_noNet))
  newdf$moc_id_noNet<-str_to_upper(newdf$moc_id_noNet, locale = "en")
  outname = paste("TC",input, '.csv', sep = "") 
  write.csv(x=newdf, file=outname)
}