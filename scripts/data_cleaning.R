#### Preamble ####
# Purpose: Clean the raw data
# Author: Qiao Zhou
# Data: 5 April 2022
# Contact: katherine.zhou@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(readr)
library(here)
library(tidyverse)

#read the raw data in
raw_data <- read_csv(here("inputs/data/HS_JPvars-only_11Jan2019_complete-cases-only.csv"))
#add two columns into the data, one represents the mean positive affect for each participate over 10days,
#the other one represents the mean negative affect for each participate over 10days.
raw_data<-raw_data%>%group_by(idPartID)%>%
  mutate(mean_pos_affect=(mean(tsHiArPosAffect)+mean(tsLoArPosAffect))*1/2,
         mean_neg_affect=(mean(tsHiArNegAffect)+mean(tsLoArNegAffect))*1/2)
#rename my interested column and add them as new columns in our data
raw_data<-raw_data %>% 
  mutate(sub_health=as.numeric(idSubOverallWellbeing),,age=as.numeric(idAge),gender=as.numeric(idGender),
         network_size=as.numeric(idSocNetworkSize),social_quality=as.numeric(idSocRelQuality),
         self_efficacy=as.numeric(idSocSelfEfficacy),self_reflection=as.numeric(idSelfReflection),
         anxiety=as.numeric(idSocAnxiety),solitude=as.numeric(idOverSolitude))
#select columns 
clean_data<-raw_data%>%
  select(idPartID,tsAssessNumber,sub_health,age,gender,network_size,social_quality,self_efficacy,self_reflection,solitude,anxiety,mean_pos_affect,mean_neg_affect)
#filter out replicate rows to get each participate's data
clean_data<-clean_data%>%
  filter(tsAssessNumber==1)
#filter out NAs
clean_data<-clean_data%>%
  filter(!is.na(sub_health)&!is.na(age)&!is.na(gender)&!is.na(network_size)&!is.na(social_quality)&!is.na(self_efficacy)&!is.na(self_reflection)&!is.na(solitude)&!is.na(anxiety)&!is.na(mean_pos_affect)&!is.na(mean_pos_affect)&!is.na(idPartID)&!is.na(tsAssessNumber))
#delete the column for assesment number and get my final data
final_data<-clean_data%>%
  select(idPartID,sub_health,age,gender,network_size,social_quality,self_efficacy,self_reflection,solitude,anxiety,mean_pos_affect,mean_neg_affect)
#write the final data in a csv file and put it in the input folder
write_csv(final_data, here("inputs/data/final_data.csv"))