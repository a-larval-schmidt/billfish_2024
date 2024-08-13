#playing with oce
library(dplyr)
library(lubridate)
library(tidyverse)
library(oce)
library(gsw)
#CTD data is output in .cnv. The function, read.oce() will parse CTD data. oce has the ability to parse non-cnv data types, called oceMagic()
setwd("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827")
list.files()
#focus on 2004-2006,2017-2018
#2006 to start
#files <- dir("~/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/", recursive=TRUE, full.names=TRUE, pattern="\\.ACO$")

is.wholenumber<-function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
read_aco = function(input) {
  d<-read.csv(input)
  group_by(#decimal time bropke up by time along a particular transect, then take mean of values from that time stamp)
    #mutate(d, V3, lubridate::decimal_date(d[3,])
    #
}
for (i in 1:length(files)) {
  read_aco(files[i])
}

#FOR REFERENCE ONLY: UKU MOCNESS FUNCTIon
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