# Function to tidy NIR output data
# equations taken from Moore and Undersander 2002
# see https://www.foragelab.com/Media/Relative_Forage_Quality.pdf


# library(googlesheets4)
library(tidyverse)

# NIR data
# googledrive link
# url <- "https://docs.google.com/spreadsheets/d/1z2B9jrX-pGm9YaUfVRTFczP-mI7UCfepGMw21QiH8f8/edit#gid=0"
# dat <- read_sheet(url,
#                   gs4_deauth())

tidy.nir.report <- function(perten_extensive_report) {
  # library(dplyr)
  perten_extensive_report %>% 
    rename_all(.,tolower) %>% 
    filter(`product name`=="Hay") %>%
    mutate(datetime=`date/time of analysis`,
           code=`sample id`,
           drymatter=`dry matter %, predicted`,
           protein=`protein as is %, predicted`,
           adf=`adf as is %, predicted`,
           ndf=`ndf as is %, predicted`,
           ndf48h = `48dndfr as is %, predicted`) %>%
    select(filename,datetime, code, drymatter,protein,adf,ndf,ndf48h) %>%
    mutate(datetime=as.POSIXct(datetime,
                               format="%m/%d/%Y %H:%M:%S %p")) %>% 
    mutate(protein=protein*drymatter/100,
           adf=adf*drymatter/100,
           ndf=ndf*drymatter/100,
           ndf48h=ndf48h*drymatter/100) %>% 
    return()
}

# Do not use %>% %>% %>% %>% %>%  this function on forages containing legumes!
calc.rfq.rfv.grass <- function(nir_tidy_data) {
  nir_tidy_data %>% 
    mutate(DM=drymatter,
           CP=protein,
           NDF=ndf,
           NDFD=ndf48h,
           ADF=adf,
           EE=2.05, #2.05 is constant, extractable ether
           FA=EE-1,
           Ash=100-DM,
           NFC=100-((0.93*NDF)+CP+EE+Ash),
           NDFn=NDF*0.93, 
           NDFDp=22.7+0.664*NDFD,
           TDN=(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10,
           DMI=(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+
             (.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF),
           rfq=DMI*TDN/1.23,
           rfv=DMI*((89.8-(0.779*ADF)))/1.29)
}


