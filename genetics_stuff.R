#####genetics troubleshooting
ex<-read.csv("C:/github/billfish_2024/billfish genetics - tissue extractions (2).csv")
mas<-read.csv("C:/github/billfish_2024/MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240925 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240926.csv")
str(ex)
library(tidyverse)
library(lubridate)
#concentration vs band presence##########
ex1<-ex%>% filter(is.na(ex$pcr_product_qbit_2)==F)
ex2<-ex1%>% filter(extract.concentration==0.1)

ggplot()+
  geom_col(data=ex2,aes(y=extract_qbit_flex_1_ngul, x=extract_identificaiton, fill=pcr1_band.size..approx, color=qbit_qc))+
  facet_grid(~plate_number)#+scale_fill_manual(values=c("red","red3","red4","black"))
ggplot()+geom_col(data=ex2,aes(y=pcr_product_qbit_2, x=extract_identificaiton, fill=band.size..approx,color=qbit_qc))+
  facet_grid(~plate_number)+scale_fill_manual(values=c("red","red3","red4","black"))

surm<-ex %>%
  group_by(spp.id.,extraction_date,Mn_PCR_date)%>%
  count(Mn_PCR_band_239)
surm
ggplot(data=surm,aes(x=unique(extraction_date), y=Mn_PCR_band_239))+geom_col()
yes<-filter(boop, found.=="yes")
ist<-filter(yes, taxa=="Unk.Istiophoridae")


#join#######
mas_long<-pivot_longer(mas,X01mm:X100m, names_to="paper_length", values_to="length_occurence",values_drop_na = TRUE)
mas_clean<-mas_long%>%
  mutate("paper_length_num"=gsub("mm","",paper_length))%>%
  mutate("paper_length_num"=gsub("X","",paper_length_num))%>%
  mutate("paper_length_num"=gsub("m","",paper_length_num))%>%
  mutate("paper_length_num"=as.numeric(paper_length_num))%>%
  mutate("dre_length"=as.numeric(dre_length))
exmas<-left_join(mas_clean, ex)
exmas2<-exmas%>%
  mutate("spp_id"=gsub("Mn","Makaira nigricans",spp.id. ))%>%
  mutate("spp_id"=gsub("Ka","Kajikia audax",spp_id))%>%
  mutate("spp_id"=gsub("Xg","Xiphias gladius",spp_id))%>%
  mutate("spp_id"=gsub("Ta","Tetapterus angustrirostris",spp_id))%>%
  mutate("taxa2"=gsub("#N/A" ,"Unk.Istiophoridae",taxa))%>%
  mutate("taxa3"=dplyr::coalesce(taxa2,spp_id))%>%
  mutate("comb_length"=dplyr::coalesce(paper_length_num,dre_length))
  #select(c(id_from_band,dre_length))
ggplot(exmas2, aes(x=comb_length, y=length_occurence, fill=taxa3))+geom_col()

#treemap######
library(treemap)
treemap(exmas2, 
        index=c("Year","taxa"),#group and subgroup...?
        vSize="count",
        type="index")
x<-filter(omg, taxa=="Kajikia audax")
treemap(x, 
        index=("cat_moon"),#group and subgroup...?
        vSize="count",
        type="index")
