#####genetics troubleshooting
ex<-read.csv("C:/github/billfish_2024/billfish genetics - tissue extractions (2).csv")
str(ex)
library(tidyverse)
library(lubridate)
ex1<-ex%>% filter(is.na(ex$pcr_product_qbit_2)==F)
ex2<-ex1%>% filter(extract.concentration==0.1)

ggplot()+
  geom_col(data=ex2,aes(y=extract_qbit_flex_1_ngul, x=extract_identificaiton, fill=band.size..approx, color=qbit_qc))+
  facet_grid(~plate_number)+scale_fill_manual(values=c("red","red3","red4","black"))
ggplot()+geom_col(data=ex2,aes(y=pcr_product_qbit_2, x=extract_identificaiton, fill=band.size..approx,color=qbit_qc))+
  facet_grid(~plate_number)+scale_fill_manual(values=c("red","red3","red4","black"))
########
surm<-ex %>%
  group_by(spp.id.,extraction_date,Mn_PCR_date)%>%
  summarize(count(Mn_PCR_band_239))
ggplot()+geom_col(data=ex,aes(x=count(unique(extraction_date)), y=count(unique(spp.id.))))
###########
boop<-read.csv("C:/github/billfish_2024/MASTER_BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240925 - BillfishInventory_WHIP+Slicks_Istiophoridae-Xiphiidae_Counts-Sizes_20240926.csv")
yes<-filter(boop, found.=="yes")
ist<-filter(yes, taxa=="Unk.Istiophoridae")
#join#######
omg<-left_join(boop, ex)# join_by=specimen_identification==specimen_identification)
str(omg)
omg2<-pivot_longer(omg,X01mm:X100m, names_to="paper_length", values_to="occurence",values_drop_na = TRUE)
omg3<-omg2%>%
  str_replace_all(paper_length,"[:alpha:]","")
omfg<-omg3%>%
  coalesce(paper_length, dre_length)
  select(c(id_from_band,dre_length))