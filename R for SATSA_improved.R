#Final Project- Tina Vo
install.packages("tidyverse") #If not installed
#Load packages
library(tidyverse)
library(tidyverse)
library(vroom)
library(visdat)
library(here)
library(visdat)
library(janitor)
install.packages("janitor")

library(readr)


rm(list = ls()) #Clean out workspace

SATSA_datascience <- read_csv("~/GitHub/Rrepos/SATSA_datascience.csv")
View(SATSA_datascience)

#using visdat package to explore the data
vis_dat(SATSA_datascience)

#Selected only the sleep variables of interest, created new dataset
SATSA<-SATSA_datascience %>% select(ID, AGE, ASLEEPN_Src, ASLEEPM_Src, ASLEEP1:ASLEEP12,
                                    ZSLEEPNrc, ZSLEEPMrc, ZSLEEP1:ZSLEEP12)
                                    
                                  
#create a function to calculate hours slept (duration) from
#bedtime and waketime
HRS_SLEPT <- function(a, b) {
  duration <- (b-a)+24
  return(duration)
  
}
SATSA$SleepDurationA<-HRS_SLEPT(SATSA$ASLEEPN_Src, SATSA$ASLEEPM_Src)
SATSA$SleepDurationZ<-HRS_SLEPT(SATSA$ZSLEEPNrc, SATSA$ZSLEEPMrc)
View(SATSA)

#PREVIOUS CODE

#data SATSA_num; set SATSAs;
#Abedtime= input (ASLEEPN_Src, time10.);
#Awaketime= input (ASLEEPM_Src, time10.);
#Zbedtime= input (ZSLEEPNrc, time10.);
#Zwaketime= input (ZSLEEPMrc, time10.);
#run;
#title 'Calculating sleep duration based on bedtime and waketime';
#/*calculate sleep duration based on waketime and bedtime*/
#  data SATSA_num2; set SATSA_num1;
#A_hrs_slept = mod(24+(Awaketime-Abedtime)/3600,24);
#Z_hrs_slept = mod(24+(Zwaketime-Zbedtime)/3600,24);
#run;


#VISUALIZE DATA/ formal check of data
SATSA %>% ggplot(aes(x = SATSA$AGE, y = SATSA$SleepDurationA)) + geom_point()
SATSA %>% ggplot(aes(x = SATSA$AGE, y = SATSA$SleepDurationZ)) + geom_point()
 

#checking data
range(SATSA$SleepDurationA, na.rm=T)     
range(SATSA$SleepDurationZ, na.rm=T)   

#See some outliers, will create new dataset setting individuals below
#two hours of sleep and above 15 hours of sleep to NA (according to national
#sleep foundation recommendations for what counts as healthy sleep duration)

SATSA_cleaned <- SATSA %>% mutate(
  SleepDurationA = ifelse(SleepDurationA >= 2 & SleepDurationA < 15, SleepDurationA, NA),
  SleepDurationZ = ifelse(SleepDurationZ >= 2 & SleepDurationZ < 15, SleepDurationZ, NA)
)

range(SATSA_cleaned$SleepDurationA, na.rm = T)
range(SATSA_cleaned$SleepDurationZ, na.rm = T)
#now the range for SleepDurationA is 5.3 and 12 and SleepDurationZ is 4.3 and 11.3
SATSA_cleaned %>% ggplot(aes(x = AGE, y = SleepDurationA)) + geom_point()
SATSA_cleaned %>% ggplot(aes(x = AGE, y = SleepDurationZ)) + geom_point()
#appears that people are sleeping more hours in the second wave (Z) compared to first wave (A)


#another way to visualize data
SATSA_cleaned %>% ggplot(aes(x = SleepDurationA)) + geom_histogram() 
SATSA_cleaned %>% ggplot(aes(x = SleepDurationZ)) + geom_histogram() 


#For Loop (graphing to visualize data by age group for sleep duration wave 1)

graphA<-function(i){
  p <-ggplot(i, aes(x=SleepDurationA)) + geom_histogram()+
    ggtitle(i$AGE)
  print(p)
}


SATSA_agegroup <-split(SATSA_cleaned, as.factor(SATSA_cleaned$AGE))
for(x in SATSA_agegroup) {
  graphA(x)
  
}

#For Loop (graphing to visualize data by age group for sleep duration wave 2)
graphZ<-function(q){
  z <-ggplot(q, aes(x=SleepDurationZ)) + geom_histogram()+
    ggtitle(q$AGE)
  print(z)
}


SATSA_agegroup <-split(SATSA_cleaned, as.factor(SATSA_cleaned$AGE))
for(h in SATSA_agegroup) {
  graphZ(h)
  
}



library(dplyr)
range(SATSA$ASLEEP1, na.rm=T)

a1<-c(5:16, 19:30)
as.character(a1)

#automation of recoding rather than if then statements
SATSA_RC<-SATSA %>%
  mutate_at(5:16, recode, '2'='1', '3'='1', '4'='1','5'='1',  '1'='0') %>%
  mutate_at(19:30, recode, '2'='1', '3'='1', '4'='1','5'='1', '1'='0')
range(SATSA_RC$ASLEEP1, na.rm=T)

#Previous code example
#if ASLEEP3=1 then ASLEEP3_h=0;
#if ASLEEP3 ge 2 then ASLEEP3_h=1;
#if ASLEEP1=1 then ASLEEP1_h=0;
#if ASLEEP1 ge 2 then ASLEEP1_h=1;
#if ASLEEP4=1 then ASLEEP4_h=0;
#if ASLEEP4 ge 2 then ASLEEP4_h=1;
#if ASLEEP8=1 then ASLEEP8_h=0;
#if ASLEEP8 ge 2 then ASLEEP8_h=1;
#if ASLEEP10=1 then ASLEEP10_h=0;
#if ASLEEP10 ge 2 then ASLEEP10_h=1;
#if ZSLEEP1=1 then ZSLEEP1_h=0;
#if ZSLEEP1 ge 2 then ZSLEEP1_h=1;
#if ZSLEEP3=1 then ZSLEEP3_h=0;
#if ZSLEEP3 ge 2 then ZSLEEP3_h=1;
#if ZSLEEP4=1 then ZSLEEP4_h=0;
#if ZSLEEP4 ge 2 then ZSLEEP4_h=1;
#if ZSLEEP8=1 then ZSLEEP8_h=0;
#if ZSLEEP8 ge 2 then ZSLEEP8_h=1;
#if ZSLEEP10=1 then ZSLEEP10_h=0;
#if ZSLEEP10 ge 2 then ZSLEEP10_h=1;
#if BSLEEP1=1 then BSLEEP1_h=0;
#if BSLEEP1 ge 2 then BSLEEP1_h=1;
#if BSLEEP3=1 then BSLEEP3_h=0;
#if BSLEEP3 ge 2 then BSLEEP3_h=1;
#if BSLEEP4=1 then BSLEEP4_h=0;
#if BSLEEP4 ge 2 then BSLEEP4_h=1;
#if BSLEEP8=1 then BSLEEP8_h=0;
#if BSLEEP8 ge 2 then BSLEEP8_h=1;
#if BSLEEP10=1 then BSLEEP10_h=0;
#if BSLEEP10 ge 2 then BSLEEP10_h=1;

#having clean names and same naming conventions (lettercase etc.) will
#allow for efficient merging with other datasets
SATSA_clean_names<-SATSA_RC%>%
  janitor::clean_names()



