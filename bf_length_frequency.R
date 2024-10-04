#length frequency with environmental variables
#libraries
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
#data read in####
ex<-read.csv("C:/github/billfish_2024/billfish genetics - tissue extractions (2).csv")
mas<-read.csv("C:/github/billfish_2024/M1ASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240925 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20241004.csv")
mas %>% # check that data display as they should/ are consistent with the MASTER google sheet, this is correct
  group_by(taxa)%>%
  summarize(sum(as.numeric(count)),na.rm=TRUE) 
#drop unknown sizes columns#####
mas2<-mas %>%
  mutate("dre_length"=as.numeric(dre_length))%>%
  mutate("dl_freq"=ifelse(is.na(dre_length)==F,1,NA))%>%#give _dre_length its own frequency column
  mutate("unknown.sizes"=as.numeric(unknown.sizes))%>%
  mutate("unknown.sizes"=ifelse(is.na(dre_length)==F,0,unknown.sizes))%>%#fix unknown.size column to be consistent with dre-larvae butnot mess with the paper legnths
  mutate("count_discrep"=unknown.sizes-count)%>% #find vials/larvae where some but not all larvae have been counted
  filter(unknown.sizes<=count)%>%#data QC can't have more unknown size values than there are larvae
  filter(!unknown.sizes==count)%>%#filter data frame to remove vials where all sizes are unknown (unknown sizes=count), leaves a few where count!= unknown,
  #remaining unknowns: unk2, Xg 6, Ta, 3. Simply do not include these in pivot. deal with them as own rows after the fact as they won't be double counted
  mutate_at("X01mm":"X100m", as.numeric(X01mm:X100m))
mas2 %>%group_by(taxa)%>%summarize(sum(count),na.rm=TRUE) #values match those of bff; this is an UNDER COUNT
mas2%>%group_by(taxa)%>%summarize(sum(as.numeric(unknown.sizes)),na.rm=TRUE) #values match those of bff; this is an UNDER COUNT
#pivot longer &coalesce both lengths and frequencies columns######
#or #melt, shape, reshape
mas_long<-mas2%>%pivot_longer(X01mm:X100m,names_to="paper_length",
                              values_to="length_occurence",values_drop_na = TRUE)
mas_melt<-melt(setDT(mas2[,c(27,37:136)]),id.vars=c("specimen_identification","length_occurence"),variable.name="paper_length") #dataframe with taxa name and lengths
# for melting, col 27= taxa, col 6= vial #, col 7= specimen ID
mas_clean<-mas_long%>%
  mutate("paper_length_num"=gsub("mm","",paper_length))%>%
  mutate("paper_length_num"=gsub("X","",paper_length_num))%>%
  mutate("paper_length_num"=gsub("m","",paper_length_num))%>%
  mutate("paper_length_num"=as.numeric(paper_length_num))%>%
  mutate("dre_length"=as.numeric(dre_length))%>%
  coalesce(dl_freq,length_occurence)%>%
  coalesce(dre_length, paper_length_num)

#be sure to specify vials v.s. specimen IDs##
mas_clean %>%group_by(specimen_identification)%>%summarize(sum(unique(count)),na.rm=TRUE) #values match those of bff; this is an UNDER COUNT
mas_clean %>%group_by(specimen_identification)%>%summarize(sum(unique(vial)),na.rm=TRUE) #values match those of bff; this is an UNDER COUNT





#sizes and seasonaltiy




ggplot(mas_clean, aes(x=as.numeric(comb_length), y=length_occurence,color=taxa))+geom_point()

exmas<-left_join(mas_clean, ex, by="specimen_identification",relationship = "many-to-many")

