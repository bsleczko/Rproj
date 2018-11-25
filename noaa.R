library(readr)
library(dplyr)
library(tidyr)
library(stringr)

noaa_df <- read.delim(file = "data/signif.txt", stringsAsFactors = FALSE)

NAto01 <- function(vector){
  sapply(vector, function(x){
    if(is.na(x)) return("01")
    else return(as.character(str_pad(x, width=2, side="left", pad="0")))
    })
}

eq_location_clean <- function(df){
  df <- df %>%
    mutate(LOCATION_NAME = str_match(LOCATION_NAME, "(?<=:  )([:graph:]|[:blank:])+")[,1]) %>%
    mutate(LOCATION_NAME = str_to_title(LOCATION_NAME))
  return(df)
}

noaa_df <- eq_location_clean(noaa_df)

#dt=as.Date(x*2-minus,origin="1970-01-01")+days(1) 
# Geo pos is already numeric
#noaa_df[,"LATITUDE"] <- sapply(noaa_df[,"LATITUDE"], function(x){as.numeric(x)})
#noaa_df[,"LONGITUDE"] <- sapply(noaa_df[,"LONGITUDE"], function(x){as.numeric(x)})

noaa_df %>%
  mutate(DATE = paste(YEAR, NAto01(MONTH), NAto01(DAY), sep = "-"))