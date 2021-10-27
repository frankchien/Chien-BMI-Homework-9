library(tidyr)
library(dplyr)
read_num <- -1 #use -1 to read the entire file, otherwise the number of rows to read from the csv
filepath <- "/Users/frankchien/Desktop/Homework 9/LABEVENTS.csv"
columns <- c("NULL", NA, "NULL",  NA, NA, "NULL", NA, "NULL", "NULL")
#the columns variable selects only the 2nd, 4th, 5th, and 6h columns of the original csv file
#this selects the subj_ID, the lab ID, chart time, and lab values

#50912 - Creatinine
#50889 - C-reactive protein
#51144 - Bands
#51300 - WBC
#50813 - Lactate

first_aki <- read.csv(filepath, colClasses = columns, nrows=read_num) %>% 
  filter(ITEMID==50912 & VALUENUM > 1.5) %>%  #selecting only the columns we need, then filtering only if patient reaches definition for AKI
  group_by(SUBJECT_ID) %>%  #groups by patient ID
  arrange(CHARTTIME) %>%  # arranges within group by when the lab was obtained
  slice(1) %>%  #obtains only the first time creatinine was > 1.5
  select(SUBJECT_ID,CHARTTIME) %>% #selects only the 2 columns we need, subject_id and charttime
  rename(pt=SUBJECT_ID, aki_time=CHARTTIME) #rename columns to pt, aki_time

first_lab1 <- read.csv(filepath, colClasses = columns, nrows = read_num) %>% #read the csv again
  filter(ITEMID==50813 & SUBJECT_ID %in% first_aki$pt) %>% #selects only lactates in patients with AKI
  group_by(SUBJECT_ID) %>%
  arrange(CHARTTIME) %>%
  slice(1) %>% 
  filter(CHARTTIME < first_aki[which(first_aki$pt==SUBJECT_ID), 2]) %>% #selects the lactate only if earlier than first_aki
  select(SUBJECT_ID, VALUENUM) %>% #selects the patient ID, and the lactate values
  rename(pt=SUBJECT_ID, lactate=VALUENUM) #renames columns to 'pt' and 'lacate

first_lab2 <- read.csv(filepath, colClasses = columns, nrows = read_num) %>%
  filter(ITEMID==50889 & SUBJECT_ID %in% first_aki$pt) %>% #selects the c-reactive protein (crp) in patients with AKI
  group_by(SUBJECT_ID) %>% 
  arrange(CHARTTIME) %>%
  slice(1) %>%
  filter(CHARTTIME < first_aki[which(first_aki$pt==SUBJECT_ID), 2]) %>% #again, selects crp only if it happened earlier than first_aki
  select(SUBJECT_ID, VALUENUM) %>% #selects the patient ID and the CRP values
  rename(pt=SUBJECT_ID, crp=VALUENUM) #renames

merge(first_lab1, first_lab2) %>%  #joins on "pt", previously called SUBJEC_ID
  na.omit() %>%  
  write.csv("/Users/frankchien/Desktop/RishiHW/output.csv") #writes to file
  