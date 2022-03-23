# Import in data for FIG experiment
rm(list=ls())

library(googlesheets4)
library(tidyverse)


# forage yield and quality data -------------------------------------------

## wide format, original
# data was compiled from multiple different excel and paper data sheets

# google drive option
# dat_wide <- read_sheet(
#   ss = "https://docs.google.com/spreadsheets/d/16qh8tlNDfsdnwmDvass7WY5EFLY_U0AzQZVDhPQMhjQ/edit#gid=0",
#   gs4_deauth()
# )
# 
# dat_wide %>% 
#   write.csv("data_yield-quality_wide-format.csv",
#             row.names = F)
dat_wide <- read.csv("data_yield-quality_wide-format.csv")

# describe the job
# multiple columns have 2 pieces of information
# date.1cut has date (variable) and 1cut (variable)

dat_wide %>% 
  dplyr::select((ends_with("1cut"))) %>% 
  glimpse ()

dat_wide %>% 
  # glimpse()
  pivot_longer(
    data = .
    cols = ends_with("1cut"),
    
  )


## long format
# this is the data in format 
# driveurl<-"https://docs.google.com/spreadsheets/d/1sRbJgHVK8YPWsqQXDi8jhn5P1qeqs1bfm0vvR4k-wC8/edit#gid=2030086613"
# dat_long<-read_sheet(driveurl,gs4_deauth())
# 
# # source from folder
# dat_long %>% 
#   # glimpse()
#   write.csv("data_yield-quality_long-format.csv",
#             row.names = F)

dat_long <- read.csv("data_yield-quality_long-format.csv")



library(dplyr)
tdat<-tdat%>%
  mutate_if(is.character,as.factor) %>%
  mutate_at("year", as.factor) %>%
  mutate_at("age", as.integer) %>%
  mutate_at("block", as.factor) %>%
  mutate_at("plot", as.factor) %>%
  mutate_at("trt.code", as.factor)

tdat$timing.1cut <- factor(tdat$timing.1cut, levels = c("boot", "anthesis", "dough", "grain"))
tdat$follow.cut <- factor(tdat$follow.cut, levels = c("none", "september", "october"))
tdat$site <- factor(tdat$site, levels = c("I2", "R70", "R100"))
tdat$treatment <- factor(tdat$treatment, levels = c("BN", "BS", "BO","AN", "AS", "AO","DN", "DS", "DO","GN", "GS", "GO"))

# Forage quality

DM<-tdat$dry.matter
CP<-tdat$protein
NDF<-tdat$NDF
NDFD<-tdat$NDFD48
ADF<-tdat$ADF

EE<-2.05
FA<-EE-1
Ash<-100-DM
NFC<-100-((0.93*NDF)+CP+EE+Ash)
NDFn<-NDF*0.93
NDFDp<-22.7+0.664*NDFD
TDN<-(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10
DMI<-(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+(.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF)

tdat$RFQ<-DMI*TDN/1.23
tdat$RFV<-DMI*((89.8-(0.779*ADF)))/1.29

#there are now 3 replicates of a single id

#outlier elimination may still required.

#we will at least eliminate I2

tdat <- tdat %>%
  filter(timing.1cut!="grain" & site!="I2")

rm(DM,CP,NDF,NDFD,ADF,EE,FA,Ash,NFC,NDFn,NDFDp,TDN,DMI)

