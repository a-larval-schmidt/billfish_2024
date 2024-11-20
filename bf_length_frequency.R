#length frequency with environmental variables

#libraries#########
library(tidyverse)
library(lubridate)
library(suncalc)
library(lunar)
library(raster)
library(ncdf4)
library(httr)
library(sp)
library(data.table)
###########
#distinct ex= 1090, mas=1462, mas2=1257, mas_long=771
#data read in and prep####
#mas<-read.csv("C:/github/billfish_2024/MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240925 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20241115.csv")
mas<-read.csv("C:/github/billfish_2024/BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20241119.csv")
#^all "dre measured" larvae rows had their paper length values deleted in the csv

og<-filter(mas, data.source!="box")
og %>% group_by(taxa)%>%summarize(sum(as.numeric(count)),na.rm=TRUE)
# check that data display as they should/ are consistent with the MASTER google sheet, this is correct
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
  mutate("freq_check"=length_occurence+unknown.sizes) #check that number of measured larvae plus unknown length larvae are consistent with overall count, #dplyr::select(!unknown.sizes)%>%#filter data frame to remove vials where all sizes are unknown (unknown sizes=count), leaves a few where count!= unknown,

#product 1 compatibility check#####
combo<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/combo_whip_slick_short11.csv")
larv<-re_mas%>%dplyr::select(c("Site","ID_morph_and_PCR","data.source", "count","comb_length","length_occurence"))
full<-left_join(larv, combo,by="Site")
#write.csv(re_mas, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/bf_main_and_genetics20241117.csv")

#check length frequency vs count data#####

re_mas%>%group_by(taxa)%>%summarize(sum((as.numeric(count)),na.rm=TRUE)) #values match those of bff; this is an UNDER COUNT
re_mas%>%group_by(taxa)%>%summarize(sum(as.numeric(length_occurence),na.rm=TRUE)) #values match those of bff; this is an UNDER COUNT
#summary(tibble(re_mas$length_occurence2, re_mas$dre_length, re_mas$length_occurence))
#summary(tibble(re_mas$comb_length, re_mas$dre_length, re_mas$paper_length_num))
e<-re_mas%>%group_by(taxa)%>%summarize(sum(count,na.rm=TRUE))
r<-re_mas%>%group_by(taxa)%>%summarize(sum(unknown.sizes,na.rm=TRUE))
l<-re_mas%>%group_by(taxa)%>%summarize(sum(length_occurence,na.rm=TRUE)) 
lr<-left_join(r,l)
erl<-left_join(lr,e)
erl<-erl%>%mutate("freq_check"=`sum(length_occurence, na.rm = TRUE)`+`sum(unknown.sizes, na.rm = TRUE)`)

count_only<-re_mas
library(treemap)
treemap(re_mas,
        index=c("taxa"),#group and subgroup...?
        vSize="count",
        type="index")


#ts plots###########
ggplot(full, aes(x=temp.1m, y=sal.1m, shape=ID_morph_and_PCR, color=ID_morph_and_PCR))+
  geom_point(size=10)+scale_color_viridis_d()+theme(axis.text= element_text(size=20),
                                                    axis.title=element_text(size=30,face="bold"),
                                                    legend.text = element_text(size=40))
tiny<-full%>%filter(comb_length<10)
ggplot(tiny, aes(x=temp.1m, y=sal.1m,color=comb_length, shape=ID_morph_and_PCR))+
  geom_point(size=5, alpha=0.3)+scale_color_viridis_c()#+facet_wrap(~ID_morph_and_PCR)
#stage check#####
clean_stage<-re_mas%>%filter(stage!="")%>%filter(stage!="preflexion/flexion???")%>%filter(stage!="flexion/post flexion, only eye available, 1mm across")%>%filter(stage!="unknown-dried")
ggplot(clean_stage, aes(y=comb_length, x=ID_morph_and_PCR, color=stage))+
  geom_point(alpha=0.3, size=4)+scale_color_viridis_d()
#need to QC stage data


#extraction decision making##########
#what has been checked for Mn???
filter(is.na(Mn_PCR_date)=F) #dropped from 1562 to 419....hmmmm
dist_exmas<-exmas2%>%distinct(specimen_identification,.keep_all = T)
dist_exmas%>%group_by(spp_id)%>%summarize(sum(count),na.rm=TRUE) #values match those of bff; this is an UNDER COUNT
dist_exmas%>%group_by(spp.id.)%>%summarize(sum(count),na.rm=TRUE) #values match those of bff; this is an UNDER COUNT

unk1<-filter(exmas4, taxa3=="Unk.Istiophoridae")%>%distinct(specimen_identification,.keep_all = T)
unk<-unk1%>%mutate("dry"=grepl("dry*|DRY*|dri*|dess*|rot*", specimen_notes))%>%filter(dry==F)
summary(as.factor(unk$spp_id)) 

#write.csv(unk, "C:/Users/Andrea.Schmidt/Documents/billfish_not_github/unk_bf_20241108.csv")
nrow(unk1)-nrow(unk)
unk2$specimen_notes
#length filter only
unk_big<-filter(unk, comb_length>=5)
nrow(unk_big)
unk_small<-filter(unk, comb_length<5)
nrow(unk_small)
nrow(unk)-(nrow(unk_small)+nrow(unk_big))
#total sample size filter
unk_big_samp<-filter(unk, larvae_per_station>20)
unk_big_champ<-filter(unk, larvae_per_station<20)
nrow(unk)-(nrow(unk_big_samp)+nrow(unk_big_champ))


stan<-unk%>%group_by(Site)%>%summarize("larvae_per_station_filt"=sum(count, na.rm=T))
unk3<-merge(x=unk,y=stan,by.x="Site", by.y ="Site",all= T)#group_by(cruise, station)%>%mutate("larvae_per_station"=(summarize(sum(count, na.rm=T))))%>%ungroup()

#length and sample size
le<-filter(unk_big, larvae_per_station>20)#larvae 5+mm, stations more than 20 larvae
nrow(filter(unk_small, larvae_per_station<20)) #larvae 5+mm, stations fewer than 20 larvae


df<-exmas4%>%distinct(specimen_identification,.keep_all = T)
df<-mutate(df,"ship"=ifelse(str_sub(specimen_identification, 1,2)=="TC","Cromwell","Sette"))
unk<-filter(df, taxa=="Unk.Istiophoridae")

unk_big<-filter(unk, comb_length>=5) #prioritize these samples
unk_big2<-filter(unk, paper_length_num>=5) #prioritize these samples
unk_big3<-filter(unk, dre_length>=5) #prioritize these samples

unk_small<-filter(df, comb_length<5)
df_nona<-df%>%mutate(len=as.numeric(comb_length)) %>%filter(is.na(len)==F)
df_nona%>% #no NAs for length
  group_by(ship)%>%
  summarize(mean=mean(paper_length_num,na.rm=TRUE))

stan<-re_mas%>%group_by(Site)%>%summarize("larvae_per_station"=sum(count, na.rm=T))
stan
exmas4<-merge(x=re_mas,y=stan,by.x="Site", by.y ="Site",all= T)#group_by(cruise, station)%>%mutate("larvae_per_station"=(summarize(sum(count, na.rm=T))))%>%ungroup()
#group by station number and count number of distinct specimen ids to get an idea of how many larvae/station in a given cruise
#count.sum is an unreliable metric

#incorporate all data types########
#results from plotting suggest temps and sal are not matching to dates/ months/
str(mas)
str(combooo)
WHIP_SWDs<-read.csv("C:/Users/Andrea.Schmidt/Desktop/for offline/WHIP_SWDs.csv")
mini_whip<- WHIP_SWDs%>%dplyr::select(c("Site","temp.1m", "vol.m3", "LAT_DD_start","LONG_DD_start",
                    "SWD_PA","sal.1m","SST_Sat", "Moon_Phase.phase","dist.shore",
                    "day.night","chltot"))
#merge SST_Sat with temp1.m and tsg data, use tsg6 dataframe from Xiphias_specimen_data script
env2<-left_join(mini_whip, re_mas2,join_by("Site"==Site))

mini_rds<-readRDS("mini_sal_GLORYS.rds")
mini_mini_rds<-mini_rds%>%select(c("Site","GLORYS_sal","cat_moon","StartDateTime","EndDateTime"))
env<-left_join(env2, mini_mini_rds, join_by("Site"==Site))
tsg6<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/HistoricCruiseData_ChrisTokita20190827/tsg6.csv")
tsg6<-tsg6%>%
mutate(datetime=ymd_hms(datetime,tz="HST"))%>%#,format="%m/%d/%y %H:%M"))
  mutate(local_datetime=datetime, .keep="all")%>%
  mutate(date=date(datetime))%>%
  mutate(Year=year(datetime))%>%
  mutate(time=hm(datetime))%>%
  mutate(Day_Time_Julian=decimal_date(datetime))
mini_tsg6<-tsg6%>%select(c("local_datetime","Salinity", "TempC"))%>%filter(year(local_datetime)!=2009)
count_only<-left_join(mas, mini_whip, join_by("Site"==Site))
count_only<-left_join(count_only,mini_mini_rds,join_by("Site"==Site))
sitedate<-combooo%>% mutate("DateTime"=ymd_hms(DateTime))%>%select(c(DateTime,Site))
tsg<-left_join(sitedate, mini_tsg6,join_by("DateTime"==local_datetime))
#setdiff(colnames(WHIP_SWDs),colnames(exmas2)) #in whip not in exmas; to double check column naminig conventions
env3<-left_join(env, tsg, join_by("Site"==Site), relationship="many-to-many")
count_only<-left_join(count_only,tsg,join_by("Site"==Site))
count_only<-count_only%>%mutate("temp_fix"=ifelse(is.na(temp.1m)==F,temp.1m,(ifelse(is.na(temp.1m)==T,SST_Sat,ifelse(is.na(SST_Sat)==T,TempC,NA)))))%>%
  mutate("sal_fix"=ifelse(is.na(sal.1m)==F,sal.1m,(ifelse(is.na(sal.1m)==T,Salinity,(ifelse(is.na(Salinity)==T,GLORYS_sal,NA))))))%>%
  mutate("cat_moon"=lunar::lunar.phase(x = DateTime, shift=10, name=T))%>%
  mutate("density"=count/vol.m3)
#sizes and seasonaltiy#########
#run salinity script to get it and then merge it
larv<-env3%>%
  #filter(comb_length<11)%>%
  mutate("temp_fix"=ifelse(is.na(temp.1m)==F,temp.1m,
                           (ifelse(is.na(temp.1m)==T,SST_Sat,ifelse(is.na(SST_Sat)==T,TempC,NA)))))%>%
  mutate("sal_fix"=ifelse(is.na(sal.1m)==F,sal.1m,(ifelse(is.na(sal.1m)==T,Salinity,(ifelse(is.na(Salinity)==T,GLORYS_sal,NA))))))%>%
  mutate("cat_moon"=lunar::lunar.phase(x = DateTime, shift=10, name=T))

#mutate("taxa_fix"-ifelse(is.na(taxa==T),print("Unk.Istiophoridae"),taxa))
xg<-filter(larv, taxa=="Xiphias gladius")

ggplot()+geom_point(data=xg, aes(x=LONG_DD_start, y=LAT_DD_start, colour=temp_fix),size=3)+#,size=comb_length))+
  scale_color_viridis_c(option="H")#+facet_grid(~month(StartDateTime))
ggplot()+geom_point(data=xg, aes(x=LONG_DD_start, y=LAT_DD_start, colour=sal_fix),size=3)+#,size=comb_length))+
  scale_color_viridis_c(option="A")

ggplot()+geom_point(data=xg, aes(x=temp_fix,y=sal_fix, size=paper_length_num))+
  scale_colour_viridis_c(option="H")+facet_grid(~taxa)

ggplot()+geom_bar(data=xg, aes(x=Moon_Phase.phase,fill=cat_moon))+
  scale_fill_viridis_d(option="C")

xg_larv<-filter(xg, comb_length<11)
ggplot()+geom_bar(data=xg_larv, aes(x=Moon_Phase.phase,fill=cat_moon))+
  scale_fill_viridis_d(option="C")#+facet_grid(~taxa)
ggplot()+geom_point(data=xg, aes(y=Moon_Phase.phase,x=SWD_PA))+
  scale_fill_viridis_d(option="C")


ggplot(larv, aes(x=TempC, y=Salinity, shape=taxa))+geom_point()


ggplot(larv, aes(x=temp.1m, y=sal.1m, shape=taxa, color=taxa3))+geom_point()+facet_grid()


#maps in a for loop###########
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
world <-ne_countries(scale="medium", returnclass = "sf")
# ne_countries(scale=10,returnclass = "sf")#generate high res coastlines 

#setwd("C:/JJS_Old_Lptp/HICEAS/IKMT_Processing/Genus_Level_Plots")
lerv<-filter(larv, !is.na(taxa)==T)
genera<-unique(larv$taxa)
merge_data<-larv%>%
  mutate(genus=taxa3)%>%
  mutate("Abund_num_1000m3"=(count/vol.m3))%>%
  mutate("Lon_West"=LONG_DD_start)%>%
  mutate("Lat_DD"=LAT_DD_start)
oahu_raster <- raster(file.path("C:/Users/Andrea.Schmidt/Desktop/for offline/billfish_hi_bathy.tiff")) #ETPO_2022(Bedrock) from https://www.ncei.noaa.gov/maps/bathymetry
oahu_df <- fortify(as.bathy(oahu_raster))
kona_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  scale_fill_gradient(high = "white", low = "white",limits=c(-6000,0))+new_scale_fill()+
  theme(panel.background=element_blank(),axis.title.x = "", axis.title.y = "")+#, panel.grid.major.x=element_line())+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-155.6,-156.5), ylim=c(18.8, 20.3))#+coord_sf(xlim=c(-154.8,-157), ylim=c(18.8, 20.3))
#point maps#####
kona_map+geom_point(data=larv, aes(y=LAT_DD_start, x=LONG_DD_start, size=count))+#, shape=SWD_PA))+
  facet_wrap(~cat_moon)+
  scale_color_viridis_c(option="A")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())

kona_map+geom_point(data=xg_larv, aes(y=LAT_DD_start, x=LONG_DD_start, color=temp_fix), size=3)+#, shape=SWD_PA))+
  #facet_wrap(~cat_moon)+
  scale_color_viridis_c(option="H")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())



knownly<-filter(lerv, taxa!="Unk.Istiophoridae")
kona_map+geom_point(data=knownly, aes(y=LAT_DD_start, x=LONG_DD_start, size=paper_length_num, color=cat_moon))+#, shape=SWD_PA))+
  facet_wrap(~taxa)+
  scale_color_viridis_d(option="E")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())

ggplot(data=knownly, aes(x=Moon_Phase.phase, group=taxa, fill=taxa))+
  geom_density(alpha=0.3)+scale_fill_viridis_d()
knownly_larv<-filter(knownly, comb_length<11)
ggplot(data=knownly_larv, aes(x=Moon_Phase.phase, group=taxa, fill=taxa))+
  geom_density(alpha=0.3)+xlim(c(0,1))+scale_fill_viridis_d()
#start over count data only#######
count_only<-larv
library(treemap)
treemap(larv,
        index=c("taxa"),#group and subgroup...?
        vSize="count",
        type="index")
f<-read.csv("C:/github/billfish_2024/MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20241305 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240905.csv")
f %>%group_by(taxa)%>%summarize(sum(count),na.rm=TRUE) #values match those of bff; this is an UNDER COUNT


kona_map+geom_point(data=xg, aes(y=LAT_DD_start, x=LONG_DD_start, size=density, color=sal_fix))+#, shape=SWD_PA))+
  facet_grid(~taxa)+
  scale_color_viridis_c(option="mako")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y=element_blank())
d<-filter(xg, (is.na(DateTime)==F))
kona_map+geom_point(data=d, aes(y=LAT_DD_start, x=LONG_DD_start, size=density, color=temp_fix))+#, shape=SWD_PA))+
  scale_color_viridis_c(option="H")+
  facet_wrap(~ year(DateTime)+ month(DateTime), labeller = 
               labeller(
                 year(DateTime)  ~ paste("Year: ", .),
                 month(DateTime)  ~ paste("Month: ", .),
                 .multi_line = FALSE))+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y=element_blank())

kona_map+geom_point(data=xg, aes(y=LAT_DD_start, x=LONG_DD_start, size=density, color=temp_fix))+#, shape=SWD_PA))+
  facet_grid(~year(DateTime))+
  scale_color_viridis_c(option="H")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y=element_blank())

ggplot()+geom_point(data=xg, aes(y=temp_fix, x=sal_fix, size=density))+
  labs(x="Salinity", y="Temperature")

ggplot()+geom_density(data=xg, aes(count, group=month(DateTime), fill=month(DateTime)),alpha=0.4)+
  scale_fill_viridis_c()


xg<-filter(larv, taxa=="Xiphias gladius")

kona_map+geom_point(data=xg, aes(y=LAT_DD_start, x=LONG_DD_start, size=density, color=temp_fix))+#, shape=SWD_PA))+
  facet_wrap(~taxa)+
  scale_color_viridis_c(option="H")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())

kona_map+geom_point(data=xg, aes(y=LAT_DD_start, x=LONG_DD_start, size=density, color=sal_fix))+#, shape=SWD_PA))+
  facet_wrap(~taxa)+
  scale_color_viridis_c(option="A")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())

kona_map+geom_point(data=xg, aes(y=LAT_DD_start, x=LONG_DD_start, size=density, color=chla))+#, shape=SWD_PA))+
  facet_wrap(~taxa)+
  scale_color_viridis_c(option="C")+
  theme(panel.background=element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())


##loop####
for(i in 1: length(genera)){
  Genus_count<-merge_data[merge_data$genus==genera[i],]
  
  png(paste0("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/plots",genera[i],"_BF_size_Plots.png"), height=5, width=7, units="in", res=300)
  p<-kona_map+
    geom_point(data=Genus_count,aes(x=Lon_West, y=Lat_DD, size =comb_fix, color = Abund_num_1000m3))+
    geom_sf(data = world)+facet_grid(~month(StartDateTime))+scale_color_viridis()+
    ylab("Latitude")+xlab("Longitude")+ggtitle(genera[i])
  print(p)
  dev.off()}
for(i in 1: length(genera)){
  Genus_count<-merge_data[merge_data$genus==genera[i],]
  
  png(paste0("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/plots",genera[i],"_BF_sst_density_Plots.png"), height=5, width=7, units="in", res=300)
  p<-kona_map() +
    geom_point(data=Genus_count,aes(x=Lon_West, y=Lat_DD, size = Abund_num_1000m3, color = SST_Sat))+
    geom_sf(data = world)+facet_grid(~month(StartDateTime))+
    scale_color_viridis_c(option="H")+
    ylab("Latitude")+xlab("Longitude")+ggtitle(genera[i])
  print(p)
  dev.off()}
for(i in 1: length(genera)){
  Genus_count<-merge_data[merge_data$genus==genera[i],]
  
  png(paste0("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/plots",genera[i],"_BF_sal_density_Plots.png"), height=5, width=7, units="in", res=300)
  p<-kona_map() +
    geom_point(data=Genus_count,aes(x=Lon_West, y=Lat_DD, size = Abund_num_1000m3, color = GLORYS_Sal))+
    geom_sf(data = world)+facet_grid(~month(StartDateTime))+
    scale_color_viridis_c(option="E")+
    ylab("Latitude")+xlab("Longitude")+ggtitle(genera[i])
  print(p)
  dev.off()}
for(i in 1: length(genera)){
  Genus_count<-merge_data[merge_data$genus==genera[i],]
  
  png(paste0("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/plots",genera[i],"_BF_year_size_Plots.png"), height=5, width=7, units="in", res=300)
  p<-kona_map() +
    geom_point(data=Genus_count,aes(x=Lon_West, y=Lat_DD, size =comb_fix, color = Abund_num_1000m3))+
    geom_sf(data = world)+facet_grid(~Year(StartDateTime))+
    scale_color_viridis()+
    ylab("Latitude")+xlab("Longitude")+ggtitle(genera[i])
  print(p)
  dev.off()}


#combine extraction ids with master sheet#####
#this is irrelevant as of 11/15 due to added ID.from.PCR column
ex<-read.csv("C:/github/billfish_2024/billfish genetics - tissue extractions.csv")
ex<-ex%>%filter(species=="Unk.Istiophoridae")%>%filter(extraction_qc_flag==0)
ggplot(re_mas2, aes(x=comb_length, y=comb_freq,color=taxa))+geom_point()
summary(as.factor(ex$spp.id.)) 
nrow(ex)
exx<-ex%>%distinct(specimen_identification,.keep_all = T)
diff_list<-setdiff(ex,exx) #in whip not in slick
diff_list$specimen_identification
nrow(exx)
nrow(re_mas2)
summary(as.factor(re_mas2$taxa)) 

#exmas<-left_join(re_mas2, ex, by="specimen_identification")#does joining this way drop the stations where no larvae were found????
exmas<-read.csv("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/exmas_20241108.csv")
exmas2<-exmas%>%
  mutate("spp.id."=gsub("Mn","Makaira nigricans",spp.id.))%>%
  mutate("spp.id."=gsub("Ka","Kajikia audax",spp.id.))%>%
  mutate("spp.id."=gsub("Xg","Xiphias gladius",spp.id.))%>%
  mutate("spp.id."=gsub("Ta","Tetapterus angustrirostris",spp.id.))%>%
  mutate("spp.id."=gsub("#N/A" ,"Unk.Istiophoridae",spp.id.))%>%
  #mutate("spp_id"=gsub("","Unk.Istiophoridae",spp_id))%>%
  mutate("taxa2"=ifelse(is.na(spp.id.)==F,spp.id., "Unk.Istiophoridae"))%>%
  #mutate("taxa3"=dplyr::coalesce(taxa2,spp_id))%>%
  mutate("paper_length_check"=ifelse(comb_length==paper_length_num, T, F))%>%
  mutate("dre_length_check"=ifelse(comb_length==dre_length,T,F))%>%
  mutate("comb_fix"=ifelse(paper_length_check==F|dre_length_check==F,paper_length_num,comb_length))
summary(as.factor(exmas2$taxa2)) 

exmas3<-exmas2%>% filter(is.na(Mn_PCR_date)==F)#filter out ones that we didn't do an mn PCR on
summary(as.factor(exmas2$spp.id.)) 
summary(as.factor(exmas2$spp_id)) 
summary(as.factor(exmas2$taxa2)) 

####poster#############
full<-full%>%
  mutate(density=count/vol.m3)

full<-full%>%
  mutate(month=month(mdy(Date)))
kona_map+geom_point(data=full, aes(y=LAT_DD_start, x=LONG_DD_start, size=density, color=ID_morph_and_PCR))+
  scale_color_viridis_d()+
  facet_wrap(~month)
