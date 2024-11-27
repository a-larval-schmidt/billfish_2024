#by Jessie Perelman
library(raster)
library(tidyverse)
library(sp)
library(sf)
library(reshape2)

setwd("C:/Users/Andrea.Schmidt/Documents/billfish_not_github/")

# read in dataset
df <- read.csv("combo_whip_slick_short16.csv")
df$Date = as.Date(df$Date, format = "%m/%d/%Y")

# read in 8-day 4km ocean color data (.nc file)
data = "ESA_OC_CCI_chla_kd490_1997_2021_8day.nc"
#data = "ESA_OC_CCI_chla_kd490_1997_2019_monthly.nc"

chlor <-stack(data, varname = "chlor_a")
kd490 <-stack(data, varname = "kd_490")

# need to mask shallow water cells (< 30m)
#bathy_file = "Hawaii_bathy_1km.nc"
#bathy <- raster(bathy_file)
#bathy[bathy[] < -30 ] = NA 
#bathy_poly = rasterToPolygons(bathy)
#plot(bathy_poly)

# add a buffer around 30m depth contour equal to 1/2 diagonal pixel distance of 500m GOCI data 
#[sqrt((1000^2) + (1000^2))/2] = 707.1068
#bathy_buffered_poly_1km <- raster::buffer(bathy_poly, width = 707.1068)
#save(bathy_buffered_poly_1km, file = paste0("envirn_data/bathymetry/bathy_HI_30m_buffered_poly_1km.RData"))

#plot(bathy_buffered_poly_1km)

# use buffered bathymetry file to mask shallow waters around MHI
load("bathy_HI_30m_buffered_poly_1km.RData")
chlor_masked <- raster::mask(chlor, bathy_buffered_poly_1km, inverse = TRUE)
kd490_masked <- raster::mask(kd490, bathy_buffered_poly_1km, inverse = TRUE)

# create new column of nearest 8-day value
library(data.table)

df1 = df
y=as.numeric(substr(names(chlor),2,5))
m=as.numeric(substr(names(chlor),7,8))
d=as.numeric(substr(names(chlor),10,11))
df2 = as.data.frame(ymd(paste(y,m,d)))
names(df2) <- "date"

setDT(df1)[,DT_DATE := Date] # name of date column in your dataset
setDT(df2)[,DT_DATE := date] # name of date column in df2

# merge d1 and df2 by matching the nearest date from df2 (ocean color 8-day dates) to the date in df1 (your data)
merged <- df2[df1,on=.(date=DT_DATE),roll="nearest"]
df_8day = merged[,c("Date","DT_DATE","LAT_DD_start","LONG_DD_start")]

# note 1997 data only goes back to 09/1997 but the cruise for that year is 04/1997, so wonʻt have chlor data for that cruise
df_8day = subset(df_8day, Date > "1997-12-31")

# create empty columns for chlorophyll and kd490
df_8day$chlor_8day <- NA
df_8day$kd490_8day <- NA

for(i in 1:nlayers(chlor)) {
  
  # i = 30
  
  year=as.numeric(substr(names(chlor_masked[[i]]),2,5))
  month=as.numeric(substr(names(chlor_masked[[i]]),7,8))
  day=as.numeric(substr(names(chlor_masked[[i]]),10,11))
  ymd = ymd(paste(year,month,day))
  
  idx <- which(df_8day$DT_DATE == ymd)

  # need to average layer to get rid of z-value that is causing issues in extract
  ch = mean(chlor_masked[[i]])
  k = mean(kd490_masked[[i]])
  
  if (length(idx)>0){
    
    pts <- SpatialPoints(df_8day[idx,c('LONG_DD_start', 'LAT_DD_start')], crs(ch))
    
    df_8day$chlor_8day[idx] <- raster::extract(ch, pts) 
    df_8day$kd490_8day[idx] <- raster::extract(k, pts) 
    
  }
  else if (length(idx)==0) {}
  
  print(paste("Completed",i,"of",nlayers(chlor_masked),"layers"))
  
}

# merge matched ocean color back with full dataframe
full_df = left_join(df, df_8day[,-"DT_DATE"], by = c("Date","LONG_DD_start", "LAT_DD_start"))

# name and save file in whatever directory you prefer
#saveRDS(df, file = "C:/Users/jessica.perelman/Downloads/df_oceancolor.rds")


