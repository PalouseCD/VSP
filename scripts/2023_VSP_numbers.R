# VSP numbers through WY 2022
# Gerrit Bass  created: July 2023
# Script to look at pre and post 2011 streamflow and temperature averages in Whitman County (Hopper, Potlach, & South Fork @ Pullman)
# and Garfield County (Alpowa, Pataha, and Deadman)

#SETUP--------------------------------------------------------------------------

#libraries
library(tidyverse)
library(readxl)
library(dataRetrieval)
library(tibbletime)
library(dplyr)

#DATA --------------------------------------------------------------------------

# Garfield data from datasheets
alpowa <- read_excel("./data/2022_Alpowa.xlsx", sheet = 3) %>% 
  drop_na() %>% 
  mutate(month = month(Date),
         site = "alpowa") %>% 
  rename(Flow = 2, `Water Temp` = 3 )

deadman <- read_excel("./data/2022_Deadman.xlsx", sheet = 3) %>% 
  drop_na() %>% 
  mutate(month = month(Date),
         site = "deadman")

pataha <- read_excel("./data/2022_Pataha.xlsx", sheet = 3) %>% 
  drop_na() %>% 
  mutate(month = month(Date),
         site = "pataha")

# Whitman flow data from USGS gauge stations
hopper <- readNWISdv(siteNumbers = "13351000",
                     parameterCd = "00060",
                     startDate = "1988-05-01",
                     endDate = "2022-09-30") %>% 
  rename(flow = 4) %>% 
  mutate(month = month(Date),
         site_no = "hopper") %>% 
  select(2:4,6)

potlatch <- readNWISdv(siteNumbers = "13345000",
                     parameterCd = "00060",
                     startDate = "1987-07-17",
                     endDate = "2022-09-30") %>% 
  rename(flow = 4) %>% 
  mutate(month = month(Date),
         site_no = "potlatch") %>% 
  select(2:4,6)

sf_pullman <- readNWISdv(siteNumbers = "13348000",
                        parameterCd = "00060",
                        startDate = "2001-05-25",
                        endDate = "2022-09-30") %>% 
  rename(flow = 4) %>% 
  mutate(month = month(Date),
         site_no = 'sf_pullman') %>% 
  select(2:4,6)

# Hooper Temperature data from EIM. Combining discrete and continuous
hopper_temp_discrete <- read.csv("./data/Hopper_EIM_Discrete_Temp.csv") %>% 
  select(5,9,51:52) %>% 
  rename("Field_Collection_Date" = 2)

hopper_temp_cont <- read.csv("./data/Hopper_EIM_Cont_Temp.csv") %>% 
  select(5,10,20:21)

hopper_temp_full <- rbind(hopper_temp_discrete,hopper_temp_cont) %>% 
  mutate(date = as.Date(Field_Collection_Date, format = "%m/%d/%Y")) %>% 
  mutate(month = month(date))

# GARFIELD STATS ---------------------------------------------------------------

#combine all the garfield data
garfield_data <- rbind(alpowa, deadman, pataha)

# pre 2011 flow and temp averages
garfield_pre2011 <- as_tbl_time(garfield_data, index = Date) %>% 
  filter(month > 5 & month < 10) %>%
  arrange(Date) %>% 
  filter_time('start' ~ '2011') %>% 
  group_by(site) %>% 
  summarise(mean_temp = mean(`Water Temp`), mean_flow = mean(Flow))

#post 2011 flow and temp averages
garfield_post2011 <-  as_tbl_time(garfield_data, index = Date) %>% 
  filter(month > 5 & month < 10) %>%
  arrange(Date) %>% 
  filter_time('2012' ~ 'end') %>% 
  group_by(site) %>% 
  summarise(mean_temp = mean(`Water Temp`), mean_flow = mean(Flow)) 

# WHITMAN STATS ---------------------------------------------------------------

#Combine all whitman county flow data
whitman_data <- rbind(hopper,potlatch,sf_pullman)

#pre 2011 flow averages for whitman county
whitman_pre2011_flow <- as_tbl_time(whitman_data, index = Date) %>% 
  filter(month > 5 & month < 10) %>%
  arrange(Date) %>% 
  filter_time('start' ~ '2011') %>% 
  group_by(site_no) %>% 
  summarise(mean_flow = mean(flow))

#post 2011 flow averages for whitman county
whitman_post2011_flow <-  as_tbl_time(whitman_data, index = Date) %>% 
  filter(month > 5 & month < 10) %>%
  arrange(Date) %>% 
  filter_time('2012' ~ 'end') %>% 
  group_by(site_no) %>% 
  summarise(mean_flow = mean(flow))


# pre 2011 average temperature at hooper only
hopper_pre2011_temp <- as_tbl_time(hopper_temp_full, index = date) %>% 
  filter(month > 5 & month < 10) %>%
  arrange(date) %>% 
  filter_time('start' ~ '2011') %>% 
  summarise(mean_temp = mean(Result_Value))

# post 2011 average temp at hooper only
hopper_post2011_temp <- as_tbl_time(hopper_temp_full, index = date) %>% 
  filter(month > 5 & month < 10) %>%
  arrange(date) %>% 
  filter_time('2012' ~ 'end') %>% 
  summarise(mean_temp = mean(Result_Value))
