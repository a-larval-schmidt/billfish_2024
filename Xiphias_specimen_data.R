#Xiphias sizes/counts/env
#libraries#########
library(tidyverse)
library(lubridate)
library(suncalc)
library(lunar)
library(raster)
library(ncdf4)
library(httr)
library(sp)
#TO DO: analysis section, look for correlations####
##data read in and sanity check#######
ex<-read.csv("C:/github/billfish_2024/billfish genetics - tissue extractions (2).csv")
mas<-read.csv("C:/github/billfish_2024/MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240925 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240926.csv")
mas %>% # check that data display as they should/ are consistent with the MASTER google sheet, this is correct
  group_by(taxa)%>%
  summarize(sum(as.numeric(count)),na.rm=TRUE) 
summary(as.factor(mas$taxa)) #checks out with what is in the data sheet
bff<- mas %>% #move length data into own single column
  pivot_longer(X01mm:X100m, names_to="lengthy",values_to="freq",values_drop_na = TRUE)%>%
  mutate(frequency=as.numeric(freq))%>%
  mutate_at(vars(frequency), ~replace(., is.na(.), 0))
bff %>%group_by(taxa)%>%summarize(sum(frequency),na.rm=TRUE) #this is seems correct, fewer counts than length measurements are available for
bff%>%group_by(cruise)%>%summarize(sum(frequency)) #gives number of larvae/cruise 

#TO DO:check that length values are valid (dre and paper match when can )#####
mas2<-mas%>%
  mutate(length_check=ifelse((X01mm:X100m[,i]>=0),mas[, "col"],"no length"))


#extraction data with master datasheet###########
mas_long<-pivot_longer(mas,X01mm:X100m, names_to="paper_length", values_to="length_occurence",values_drop_na = TRUE)
mas_long %>%group_by(taxa)%>%summarize(sum(length_occurence),na.rm=TRUE) #values match those of bff
mas_clean<-mas_long%>%
  mutate("paper_length_num"=gsub("mm","",paper_length))%>%
  mutate("paper_length_num"=gsub("X","",paper_length_num))%>%
  mutate("paper_length_num"=gsub("m","",paper_length_num))%>%
  mutate("paper_length_num"=as.numeric(paper_length_num))%>%
  mutate("dre_length"=as.numeric(dre_length))
mas_clean2<-mas_clean%>%
  
#exmas<-left_join(mas_clean, ex, by="specimen_identification",relationship = "many-to-many")
#compare exmas to bff
exmas%>%group_by(vial)%>%summarize(sum(as.numeric(length_occurence)),na.rm=TRUE) #this is NOT correct
exmas2<-exmas%>%
  mutate("spp_id"=gsub("Mn","Makaira nigricans",spp.id. ))%>%
  mutate("spp_id"=gsub("Ka","Kajikia audax",spp_id))%>%
  mutate("spp_id"=gsub("Xg","Xiphias gladius",spp_id))%>%
  mutate("spp_id"=gsub("Ta","Tetapterus angustrirostris",spp_id))%>%
  mutate("taxa2"=gsub("#N/A" ,"Unk.Istiophoridae",taxa))%>%
  mutate("taxa3"=dplyr::coalesce(taxa2,spp_id))%>%
  mutate("comb_length"=dplyr::coalesce(paper_length_num,dre_length))%>%
  mutate("length_check"=ifelse(paper_length_num==dre_length, T, F))
#number of paper lengths
na.omit(length(mas_clean$paper_length_num))
#number of unique specimens or vials
length(unique(mas$specimen_identification))
#number of unique extracted larvae
length(unique(ex$specimen_identification))
#number of unique in combined dataframe, should be equal to master sheet.... ex is a subset of entries in master sheet
length(unique(exmas2$specimen_identification))
#number of specimens per unique vial
exmas3<-exmas2[which(exmas2$specimen_identification %in% unique(exmas2$specimen_identification)),]
length(unique(exmas3$specimen_identification))
hey<-exmas3%>%group_by(specimen_identification)%>%summarize(sum(as.numeric(length_occurence)),na.rm=TRUE) #this seems correct
sum(hey$`sum(as.numeric(length_occurence))`) #indicates that above is correct, sum of specimens almost = number of unique specimen identifiers (diff of 3)
lilx<-filter(exmas3, comb_length<11)
ggplot(lilx, aes(x=comb_length, y=length_occurence, fill=taxa3))+geom_col()
boxplot(lilx$comb_length~lilx$Year)
ggplot(data=lilx, aes(y=length_occurence, fill=Year, x=comb_length))+geom_col()+scale_fill_viridis_c(option="magma")+facet_grid(~taxa3)
#metadatasheet as combo then mini####
#add station number infomation for each cruise by matching start/end times of tows in combo to each cruise datasheet
#combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/Product1_LarvalBillfish_TowMetadata.csv")
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
str(combo)
comboo<-combo%>%
  unite("dend",c(Date,Time.end),sep=" ",remove = F)%>%
  unite("send",c(Date,Time.start),sep=" ", remove=F)
combooo<-comboo%>% #times in local
  mutate(EndDateTime=lubridate::as_datetime(dend, tz="HST"), .keep="unused")%>%
  mutate(StartDateTime=lubridate::as_datetime(send,tz="HST"), .keep="unused")%>%
  mutate("moon_date"=suncalc::getMoonIllumination(date = StartDateTime,keep = "phase"))%>%
  mutate("phase"=moon_date$phase)%>%
  mutate("cat_moon"=lunar::lunar.phase(x = StartDateTime, shift=10, name=T))
#with_tz() changes the time zone in which an instant is displayed. The clock time displayed for the instant changes, but the moment of time described remains the same.
#add column to d to start
mini_combo<-combooo%>%dplyr::select(c(Site,EndDateTime, StartDateTime,LAT_DD_start,LONG_DD_start, phase, cat_moon))
mini2<-mini_combo%>%
  mutate(dur=lubridate::interval(start=StartDateTime, end=EndDateTime))%>%
  mutate(duration=as.numeric(dur, "minutes"))%>%
  filter(duration>0 & duration<60)
#take mean in d3 ()over tow time durations
mas14<-dplyr::select(mas,c(Year, Site, cruise, station,specimen_identification,dre_length,taxa,count))
xg14<-filter(mas14, taxa=="Xiphias gladius")
lilx<-filter(exmas3, comb_length<11)
xg<-filter(lilx, taxa=="Xiphias gladius")
#full_env<-left_join(exmas3, lilx, relationship="many-to-many",join_by(Site)) # this will brick R
mini_xg<-dplyr::select(xg, c(Year, Site, dre_length, paper_length_num,comb_length, length_occurence,taxa, taxa3,specimen_identification,cruise, station))
xg_env<-left_join(mini2, xg14, relationship="many-to-many",join_by(Site))
omg<-left_join(mini2, mas14, relationship="many-to-many",join_by(Site))
str(xg_env)
#write.csv(xg_env,"C:/Users/Andrea.Schmidt/Desktop/for offline/xg_env.csv")
##each taxa as a df######
Xg<-exmas2%>%
  filter(taxa=="Xiphias gladius")
Mn<-exmas2%>%
  filter(taxa=="Makaira nigricans")
Ta<-exmas2%>%
  filter(taxa=="Tetapterus angustrirostris")
unk<-exmas2%>%
  filter(taxa=="Unk.Istiophoridae")
Ka<-exmas2%>%
  filter(taxa=="Kajikia audax")
#merge TSG env data#####
tsg1<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/merged_tsgs_redo.csv")
tsg2<-tsg1 %>%
  mutate(datetime=as_datetime(datetime),.keep="unused")%>%
  mutate(date=ymd(Day),.keep="unused")%>%
  mutate(time=hms(str_replace_all(time,"[:alpha:]","")))%>%
  mutate(local_datetime=with_tz(datetime, "HST")) #view time as HST to match combo data
str(tsg2) #times in UTC
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
tsg3<-rbind(tsg2,f)
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
tsg4<-rbind(tsg3,h)
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
sbe47<-select(sbe46, c(colnames(d3)))
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
ggplot(tsg6, aes(x=local_datetime, y=Salinity))+geom_point()

#map this against map/geography to see if signal is true vs artefact

##time join TSG and specimen data (mini)#######
mini_time_join1<-left_join(tsg6,mini,join_by(local_datetime<=EndDateTime, local_datetime>=StartDateTime)) #join-by closest value
mini$TempC = NA
mini$Salinity = NA

for(i in 1:nrow(mini)) {
  tsg <- tsg6 %>%
    subset(local_datetime >= mini[i,]$StartDateTime &
             local_datetime <= mini[i,]$EndDateTime)
  mini$TempC[i] = mean(tsg$TempC)
  mini$Salinity[i] = mean(tsg$Salinity)
  
  print(paste("Completed",i,"of",nrow(mini),"rows"))
}

#identify the time between each sample and all stations start times and match within a set time cut off
library(raster)
d5$Nearest_Temp_Val<-NA
d5$Nearest_Sal_Val<-NA
mini_no_na<-mini%>%
  filter(is.na(StartDateTime)==F)
sample_start=mini_no_na$StartDateTime#sample time points
sample_end=mini_no_na$EndDateTime
tsg7<-tsg6%>%
  filter(is.na(local_datetime)==F)
Time_env=tsg7$local_datetime #tsg timepoints
Temperature=tsg7$TempC
Salinity=tsg7$Salinity
time_cut_off=mini_no_na$duration
tryCatch.W.E <- function(expr)
{
  W <- NULL
  w.handler <- function(w) { # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}

for (i in 1:length(tsg7)){
  #calculate each of those time distances1
  time_vals<-as.numeric(int_standardize(interval(Time_env[i],sample_start[i])))#duration in seconds
  K<-which(time_vals==min(time_vals))#find the location of the minimum distance
  if (time_vals[K]<time_cut_off){#select the distance you want as your cut-off
    #Error in if (time_vals[K] < time_cut_off) { : argument is of length zero
    Nearest_Sal_Val[i]<-Salinity[K]# fill if below cut-off
    }
  else if (time_vals[K]>=time_cut_off){
    d5$Nearest_Sal_Val[i]<-NA# fill if below cut-off
    
  }
  #tryCatch.W.E("argument is of length zero")
}
#merge modelled salinity data#####
#updated time match script from Jessie##
mini$TempC = NA
mini$Salinity = NA
mini$tsg = NA

for(i in 1:nrow(mini)) {
  tsg <-tsg7%>%
    subset(local_datetime >= mini[i,]$StartDateTime - (24*60*60) &
             local_datetime <= mini[i,]$EndDateTime + (24*60*60))
  mini$tsg[i] = ifelse(nrow(tsg) > 0, "Yes","No")
  mini$TempC[i] = mean(tsg$TempC, na.rm=T)
  mini$Salinity[i] = mean(tsg$Salinity, na.rm=T)
  
  print(paste("Completed",i,"of",nrow(mini),"rows"))
}

mini$Salinity[is.nan(mini$Salinity)] <- NA
mini$TempC[is.nan(mini$TempC)] <- NA

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
mini<-mini3
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

# #This sort of approach should work for matching the modelled salinity values, but you would need lon and lat coordinates for the tow data (i.e. ʻminiʻ) in order to match.
## this is FROM SAT COURSE
#nc <- nc_open("C:/Users/Andrea.Schmidt/Desktop/for offline/cmems_mod_glo_phy_my_0.083deg_P1D-m_1727396375892.nc")
# v1 <- nc$var[[1]] #list of variables??
# glorysal<- ncvar_get(nc,v1)
# dim(glorysal)#examines the structure of sal
# sal_dates<- as.POSIXlt(v1$dim[[4]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step
# lon <- v1$dim[[1]]$vals #gives vector of longitude
# lat <- v1$dim[[2]]$vals #gives vector of latitude
# nc_close(nc)
# gs<-as.data.frame(c(lon,lat,sal_dates))
# #par(mar=c(2,2))
# I=which(lon>=201 & lon<=203) #subsets to specific lat/long
# J=which(lat>=20 & lat<=22)
# sal2=glorysal[I,J,]
# n=dim(glorysal)[3] #set limit on x-axis by creating a dynamic number (which will always be the number of dimensions in the 3rd element of the list of dimensions of sst2, 120  40  12, in this case 120 longitudes, 40 latitudes over 12 time steps)
# res=rep(NA,n)
# for (i in 1:n)
#   res[i]=mean(glorysal[,,i],na.rm=TRUE)#set limit on x-axis by creating a dynamic number (which will always be the number of dimensions in the 3rd element of the list of dimensions of sst2, 120  40  12, in this case 120 longitudes, 40 latitudes over 12 time steps)
# res=rep(NA,n)
# #for (i in 1:n)
# #res[i]=mean(glorysal[,,mean(1:7823)],na.rm=TRUE)
#  #in a for loop, the resolution for the plot is set by the ith value in the 3rd element of the sst2 matrix, in this case the 3rd element is time
# plot(1:n,res,axes=F,type='o', col="blue",pch=20,xlab='',ylab='modelled salinity') #creates plot  with the x axis going until n ends and then the y axis going for a range of res???? axes=F means no axes but also no weird box bounding the whole graph
# plot(d5$Salinity~month(d5$date), pch=15,col="red")
# axis(2) #adds axis lines on y axis. axis(3) gives an axis on top of chart and nothing on x/y, axis 4 gives it on y but right side
# axis(1,1:n,format(sal_dates,'%m')) #lets x-axis get filled in with the date column on the bottom of graph, uses lubirate to specify that only the month needs be shown
# #gs<-glorysal[,,mean(1:7823)] #okay so basically set row and columns names as lat long THEN pivot longer so these all become their own columns???
# rownames(gs, do.NULL = TRUE, prefix = "row")
# rownames(gs) <- lon
# colnames(gs, do.NULL = TRUE, prefix = "col")
# colnames(gs) <-lat
# gs<-as.data.frame(as.table(gs))
# colnames(gs) <- c("lon","lat","sal")
#CTD to SAMPLE LOCATION MATCH code from Justin
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


#plots###########
check<-mini%>%
  group_by(year(StartDateTime))%>%
  #summarise(stations_per_year = n_distinct(Site))%>%
  count(TempC, Salinity)
check
check2<-filter(check, Salinity<40)
ggplot(check2, aes(x=`year(StartDateTime)`,y=n,color=Salinity))+geom_point()#+scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 10")
ch<-mini%>%
  group_by(year(StartDateTime))%>%
  summarise(stations_per_year = n_distinct(Site))
ch+geom_point(data=check, aes(x=`year(StartDateTime)`,y=sval,color=sval))
ggplot()+geom_point(data=ch, aes(x=`year(StartDateTime)`,y=stations_per_year))


ggplot(data=xg_env, aes(x=month(StartDateTime),y=as.numeric(dre_length),size=count,color=phase))+
  geom_point()+scale_color_viridis_c(option="C")+facet_grid(~taxa)

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


