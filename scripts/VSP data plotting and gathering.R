# Plotting and Gathering VSP data for Julia


library(dplyr)
library(plotly)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(here)
library(tidyr)

# Flow and Temp analysis

#function to import Flow and Temp analysis

FT <- function(file,site){
  dat <- read_csv(here("data",file),skip = 1, 
                col_names = c("date",
                              "Flow_cfs",
                              "Water.temp_C"))
dat$date <- mdy(dat$date)
dat$site <- site

return(dat)
}

alpow <- FT("Alpowa Creek_Temp_Flow.csv","Alpowa Creek")
dead <- FT("Deadman Creek_Temp_Flow.csv", "Deadman Creek")

all<-bind_rows(alpow, dead)
  all <- all[,c(4,1:3)]

write_csv(all, here("outputs","Alpowa_Deadman_Flow_Temp_2022.csv"))

Flow <- ggplot(all,aes(date,Flow_cfs,color = site))+
  geom_point()+
  geom_line()+
  facet_grid(rows = vars(site))
Flow

Temp <- ggplot(all,aes(date,Water.temp_C,color = site))+
  geom_point()+
  geom_line()+
  facet_grid(rows = vars(site))
Temp

longall <- all %>% pivot_longer(cols = !site & !date)
  colnames(longall)[3] <-"Parameter"
  colnames(longall)[4] <- "Value"

longall <- longall[,c(2,1,3,4)]

#### EIM Data Cleaning

EIMr <- function(file,site){
  a <- read_csv(here("data","EIM",file)) #alpowa data
  names(a)
    a <- a[,c(4,9,46,51,52,57,58,69)]
      colnames(a) <- c("site",
                       "date",
                       "Parameter",
                       "Value",
                       "Units",
                       "Data_Qual",
                       "Data_Q_Desc",
                       "Result_comment")
    a$site <- site
    a$date <- mdy(a$date)
        a2 <- a %>% filter(Parameter %in% c("Ammonia",
                                            "Dissolved Oxygen",
                                            "Fecal Coliform",
                                            "Nitrate + Nitrite as N",
                                            "Ortho-Phosphate",
                                            "pH",
                                            "Phosphorus",
                                            "Total Persulfate Nitrogen",
                                            "Total Phosphorus",
                                            "Total Suspended Solids"))
    return(a2)
}

alpow <- EIMr("Alpowa_DiscreteResults_20230322.csv","Alpowa Creek")
dead <- EIMr("Deadman_DiscreteResults_20230322.csv", "Deadman Creek")
  combo <- bind_rows(alpow,dead)
    write_csv(combo,here("outputs","Alpowa_Deadman_EIM_Data.csv"))

combo$year <- year(combo$date)
    
sum <- combo %>% 
  group_by(site,Parameter,year) %>% 
  summarize(min = min(Value),
            mean = round(mean(Value),2),
            max = max(Value),
            median = median(Value),
            samples = n())

for (var in unique(combo$Parameter)) { # points a lines of all parameters
  print( ggplot(combo[combo$Parameter==var,], 
                aes(date, Value,color=site)) + 
           geom_line() + 
           geom_point()+
           facet_grid(~Parameter) )
}    

names(longall)
names(combo)       
combo <- combo[,c(1:4)]

