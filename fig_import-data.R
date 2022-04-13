# Import in data for FIG experiment
rm(list=ls())

# library(googlesheets4)
library(tidyverse)


# wide format - forage yield and quality ----------------------------------


## wide format, original format during data collection
# data was compiled from multiple different excel and paper data sheets

# google drive option
# dat_wide <- read_sheet(
#   ss = "https://docs.google.com/spreadsheets/d/16qh8tlNDfsdnwmDvass7WY5EFLY_U0AzQZVDhPQMhjQ/edit#gid=0",
#   gs4_deauth()
# )

# dat_wide %>% 
#   write.csv("data_yield-quality_wide-format.csv",
#             row.names = F)

dat_wide <- read.csv("data_yield-quality_wide-format.csv")


# defining which columns are factors
dat_wide %>% 
  mutate_if(is.character,as.factor) %>%
  mutate_at("year", as.factor) %>%
  mutate_at("age", as.integer) %>%
  mutate_at("block", as.factor) %>%
  mutate_at("plot", as.factor) %>%
  mutate_at("trt.code", as.factor) -> dat_wide

# defining factor levels
dat_wide$timing.1cut <-
  factor(dat_wide$timing.1cut, levels = c("boot", "anthesis", "dough", "grain"))
dat_wide$follow.cut <-
  factor(dat_wide$follow.cut, levels = c("none", "september", "october"))
dat_wide$site <- factor(dat_wide$site, levels = c("I2", "R70", "R100"))
dat_wide$treatment <-
  factor(
    dat_wide$treatment,
    levels = c(
      "BN",
      "BS",
      "BO",
      "AN",
      "AS",
      "AO",
      "DN",
      "DS",
      "DO",
      "GN",
      "GS",
      "GO"
    )
  )


# cumulative forage yield -------------------------------------------------

# calculating cumulative forage yields only for plots where there is no missing
# data

oct <- dat_wide %>%
  filter(follow.cut=="october") %>%
  dplyr::select(id,yield.1cut,yield.2cut,yield.3cut) %>%
  drop_na() %>%
  pull(id)

sep <- dat_wide %>%
  filter(follow.cut=="september") %>%
  dplyr::select(id,yield.1cut,yield.2cut) %>%
  drop_na() %>%
  pull(id)

none <- dat_wide %>%
  filter(follow.cut=="none") %>%
  dplyr::select(id,yield.1cut) %>%
  drop_na() %>%
  pull(id)

goodid <- c(oct,sep,none)

dat_wideyt <- dat_wide %>%
  filter(dat_wide$id %in% goodid) %>%
  rowwise() %>%
  replace_na(list(yield.1cut=0,yield.2cut=0,yield.3cut=0)) %>%
  transmute(id=id,
            yield.total=sum(c(yield.1cut,yield.2cut,yield.3cut)))

dat_wide <- dplyr::left_join(dat_wide,dat_wideyt,by="id")
rm(none,oct,sep,dat_wideyt)



# forage quality calculations ---------------------------------------------

## Forage quality comparisons
# calculating relative feed quality (RFV) and relative forage quality (RFQ)
# equations taken from Moore and Undersander 2002
# see https://www.foragelab.com/Media/Relative_Forage_Quality.pdf

# note the values (i.e. protein.1cut) have already been converted to dry matter
# basis

# first cut
DM<-dat_wide$dry.matter.1cut
CP<-dat_wide$protein.1cut
NDF<-dat_wide$NDF.1cut
NDFD<-dat_wide$NDFD48.1cut
ADF<-dat_wide$ADF.1cut

EE=2.05 #2.05 is constant, extractable ether
FA=EE-1
Ash=100-DM
NFC=100-((0.93*NDF)+CP+EE+Ash)
NDFn=NDF*0.93
NDFDp=22.7+0.664*NDFD
TDN=(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10
DMI=(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+
  (.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF)

dat_wide$RFQ.1cut=DMI*TDN/1.23
dat_wide$RFV.1cut = DMI*((89.8-(0.779*ADF)))/1.29

# Second cut

DM<-dat_wide$dry.matter.2cut
CP<-dat_wide$protein.2cut
NDF<-dat_wide$NDF.2cut
NDFD<-dat_wide$NDFD48.2cut
ADF<-dat_wide$ADF.2cut

EE=2.05 #2.05 is constant, extractable ether
FA=EE-1
Ash=100-DM
NFC=100-((0.93*NDF)+CP+EE+Ash)
NDFn=NDF*0.93
NDFDp=22.7+0.664*NDFD
TDN=(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10
DMI=(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+
  (.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF)

dat_wide$RFQ.2cut=DMI*TDN/1.23
dat_wide$RFV.2cut = DMI*((89.8-(0.779*ADF)))/1.29

# third cut

DM<-dat_wide$dry.matter.3cut
CP<-dat_wide$protein.3cut
NDF<-dat_wide$NDF.3cut
NDFD<-dat_wide$NDFD48.3cut
ADF<-dat_wide$ADF.3cut

EE<-2.05
FA<-EE-1
Ash<-100-DM
NFC<-100-((0.93*NDF)+CP+EE+Ash)
NDFn<-NDF*0.93
NDFDp<-22.7+0.664*NDFD
TDN<-(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10
DMI<-(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+
  (.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF)

dat_wide$RFQ.3cut<-DMI*TDN/1.23
dat_wide$RFV.3cut<-DMI*((89.8-(0.779*ADF)))/1.29


# forage quality weighted by yield contribution ---------------------------

# weighting RFQ and RFV by their proportion to cumulative forage production 
dat_wide$RFQ.1cut.wt <- dat_wide$yield.1cut/dat_wide$yield.total*dat_wide$RFQ.1cut 
dat_wide$RFQ.2cut.wt <- dat_wide$yield.2cut/dat_wide$yield.total*dat_wide$RFQ.2cut 
dat_wide$RFQ.3cut.wt <- dat_wide$yield.3cut/dat_wide$yield.total*dat_wide$RFQ.3cut

dat_wide$RFV.1cut.wt <- dat_wide$yield.1cut/dat_wide$yield.total*dat_wide$RFV.1cut 
dat_wide$RFV.2cut.wt <- dat_wide$yield.2cut/dat_wide$yield.total*dat_wide$RFV.2cut 
dat_wide$RFV.3cut.wt <- dat_wide$yield.3cut/dat_wide$yield.total*dat_wide$RFV.3cut



# qaqc - RFQ ---------------------------------------------

# ensuring we only sum across rows where there is no missing data 

octq <- dat_wide %>%
  filter(follow.cut=="october") %>%
  dplyr::select(id,RFQ.1cut.wt,RFQ.2cut.wt,RFQ.3cut.wt) %>%
  drop_na() %>%
  pull(id)

sepq <- dat_wide %>%
  filter(follow.cut=="september") %>%
  dplyr::select(id,RFQ.1cut.wt,RFQ.2cut.wt) %>%
  drop_na() %>%
  pull(id)

noneq <- dat_wide %>%
  filter(follow.cut=="none") %>%
  dplyr::select(id,RFQ.1cut.wt) %>%
  drop_na() %>%
  pull(id)

goodidq <- c(octq,sepq,noneq)

datqt <- dat_wide %>%
  filter(dat_wide$id %in% goodidq) %>%
  rowwise() %>%
  replace_na(list(RFQ.1cut.wt=0,RFQ.2cut.wt=0,RFQ.3cut.wt=0)) %>%
  transmute(id=id,
            RFQ.total=sum(c(RFQ.1cut.wt,RFQ.2cut.wt,RFQ.3cut.wt))) %>%
  na_if(.,0)

dat_wide <- dplyr::left_join(dat_wide,datqt,by="id")
rm(noneq,octq,sepq,datqt)



# qaqc - RFV ---------------------------------------------

# ensuring we only sum across rows where there is no missing data 

# calculating cumulative relative forage quality only for plots where there is
# no missing data

octq <- dat_wide %>%
  filter(follow.cut=="october") %>%
  dplyr::select(id,RFV.1cut.wt,RFV.2cut.wt,RFV.3cut.wt) %>%
  drop_na() %>%
  pull(id)

sepq <- dat_wide %>%
  filter(follow.cut=="september") %>%
  dplyr::select(id,RFV.1cut.wt,RFV.2cut.wt) %>%
  drop_na() %>%
  pull(id)

noneq <- dat_wide %>%
  filter(follow.cut=="none") %>%
  dplyr::select(id,RFV.1cut.wt) %>%
  drop_na() %>%
  pull(id)

goodidq <- c(octq,sepq,noneq)

datqt <- dat_wide %>%
  filter(dat_wide$id %in% goodidq) %>%
  rowwise() %>%
  replace_na(list(RFV.1cut.wt=0,RFV.2cut.wt=0,RFV.3cut.wt=0)) %>%
  transmute(id=id,
            RFV.total=sum(c(RFV.1cut.wt,RFV.2cut.wt,RFV.3cut.wt))) %>%
  na_if(.,0)

dat_wide <- dplyr::left_join(dat_wide,datqt,by="id")
rm(noneq,octq,sepq,datqt)

# RFQ.total and RFV.total values that equaled zero are due to observations where
# forage quality data existed but yield data did not, resulting in an unknown
# weighted forage quality response, so it's recorded as NA in the dataset


# economic return estimates -----------------------------------------------

#incorporating $$$ using Hunter et al.
#Value of the hay in $ per kg
dat_wide$value.1cut<-34+1.02*dat_wide$RFV.1cut
dat_wide$value.2cut<-34+1.02*dat_wide$RFV.2cut
dat_wide$value.3cut<-34+1.02*dat_wide$RFV.3cut

#Economic net economic return in $ per ha
#subtracts cost of harvest ($143.06 per ha)
dat_wide$return.1cut<-dat_wide$value.1cut*dat_wide$yield.1cut*0.001-143.06
dat_wide$return.2cut<-dat_wide$value.2cut*dat_wide$yield.2cut*0.001-143.06
dat_wide$return.3cut<-dat_wide$value.3cut*dat_wide$yield.3cut*0.001-143.06


# qaqc - economic returns --------------------------------

# ensuring we only sum across rows where there is no missing data 


octr <- dat_wide %>%
  filter(follow.cut=="october") %>%
  dplyr::select(id,return.1cut,return.2cut,return.3cut) %>%
  drop_na() %>%
  pull(id)

sepr <- dat_wide %>%
  filter(follow.cut=="september") %>%
  dplyr::select(id,return.1cut,return.2cut) %>%
  drop_na() %>%
  pull(id)

noner <- dat_wide %>%
  filter(follow.cut=="none") %>%
  dplyr::select(id,return.1cut) %>%
  drop_na() %>%
  pull(id)

goodidr <- c(octr,sepr,noner)

datr <- dat_wide %>%
  filter(dat_wide$id %in% goodidr) %>%
  rowwise() %>%
  replace_na(list(return.1cut=0,return.2cut=0,return.3cut=0)) %>%
  transmute(id=id,
            return.total=sum(c(return.1cut,return.2cut,return.3cut))) %>%
  na_if(.,0)

dat_wide <- dplyr::left_join(dat_wide,datr,by="id")
rm(octr,sepr,noner,datr)


rm(goodid,goodidq,goodidr)
rm(DM,CP,NDF,NDFD,ADF,EE,FA,Ash,NFC,NDFn,NDFDp,TDN,DMI)


# long format - forage yield and quality ----------------------------------



## long format
# this is the data in format, done manually
# driveurl<-"https://docs.google.com/spreadsheets/d/1sRbJgHVK8YPWsqQXDi8jhn5P1qeqs1bfm0vvR4k-wC8/edit#gid=2030086613"
# dat_long<-read_sheet(driveurl,gs4_deauth())
# 
# # source from folder
# dat_long %>% 
#   # glimpse()
#   write.csv("data_yield-quality_long-format.csv",
#             row.names = F)

dat_long <- read.csv("data_yield-quality_long-format.csv")

dat_long <-dat_long%>%
  mutate_if(is.character,as.factor) %>%
  mutate_at("year", as.factor) %>%
  mutate_at("age", as.integer) %>%
  mutate_at("block", as.factor) %>%
  mutate_at("plot", as.factor) %>%
  mutate_at("trt.code", as.factor)

dat_long$timing.1cut <- factor(dat_long$timing.1cut, levels = c("boot", "anthesis", "dough", "grain"))
dat_long$follow.cut <- factor(dat_long$follow.cut, levels = c("none", "september", "october"))
dat_long$site <- factor(dat_long$site, levels = c("I2", "R70", "R100"))
dat_long$treatment <- factor(dat_long$treatment, levels = c("BN", "BS", "BO","AN", "AS", "AO","DN", "DS", "DO","GN", "GS", "GO"))
dat_long$xcut <- factor(dat_long$xcut, levels = c("1", "2", "3"))


# forage quality calculations ---------------------------------------------

DM<-dat_long$dry.matter
CP<-dat_long$protein
NDF<-dat_long$NDF
NDFD<-dat_long$NDFD48
ADF<-dat_long$ADF

EE<-2.05
FA<-EE-1
Ash<-100-DM
NFC<-100-((0.93*NDF)+CP+EE+Ash)
NDFn<-NDF*0.93
NDFDp<-22.7+0.664*NDFD
TDN<-(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10
DMI<-(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+(.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF)

dat_long$RFQ<-DMI*TDN/1.23
dat_long$RFV<-DMI*((89.8-(0.779*ADF)))/1.29



# removing I2 site --------------------------------------------------------

dat_long <- dat_long %>%
  filter(timing.1cut!="grain" & site!="I2")

rm(DM,CP,NDF,NDFD,ADF,EE,FA,Ash,NFC,NDFn,NDFDp,TDN,DMI)


# weather -----------------------------------------------------------------

dat_weather <- read.csv("data_weather.csv")

dat_weather <- dat_weather %>%
  mutate_at("year", as.factor) %>%
  mutate(year = recode_factor(year,
                              `30` = "30 year average")) %>%
  mutate(date=as.POSIXct(.$date, 
                         format="%m/%d/%Y"))
