#extend df to one line per larva
library(dplyr)
library(tidyverse)
df<-read.csv("~/billfish_not_github/Product 3_ Envrionmental Data Paired with Billfish Specimen Data.csv")
str(df)

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
