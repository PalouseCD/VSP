### Sorting through and exporting RCPP data from the PCD Data Conservation 
### Datebase

# Created April 2023 by RDB

library(dplyr)
library(plotly)
library(ggplot2)
library(lubridate)
library(here)
library(tidyr)
library(readr)
library(janitor)
library(curl)

source("R:/_04_Project_Data/R/PCD_Functions/Smartsheet to Csv.R")


data<- get_smartsheet(4701405874284420,"GxrvfnkIi07vvAAk775hUigIcFeQC7dxUzyj7")

#startd <- mdy("1/1/2021")
#endd <- mdy("12/31/2022")

  
data <- data %>% 
  clean_names() 

data <- data %>%
  select(cooperator_name,
         funding_source,
         project_category,
         total_project_cost,
         start_date,
         completion_date,
         project_year,
         wria,
         huc_12_name,
         starts_with("practice_type"),
         -contains(c("stir","t_value","_sci_","co2_equivalent","nitrogen_use")))


data$start_date <-mdy(data$start_date)
data$completion_date <- mdy(data$completion_date)



#data <- mutate(data, paste(cooperator_name, completion_date, sep=' '))
#colnames(data)[50]="PracID"

data <-
  data %>% filter(project_year > '2020')

data <- 
  data %>% filter(start_date <= '2022-12-31')

#removing duplicate rows
data <- data %>% distinct()

#data <- 
 # data %>% filter(between(start_date, as.Date('2021-01-01'), as.Date('2022-12-31')))


pull<-function(data,cname){
  data <- data %>% 
    select(funding_source,
           project_category,
           total_project_cost,
           start_date,
           completion_date,
           project_year,
           wria,
           huc_12_name,
           contains(cname),) %>% 
    rename(nrcs_practice = 9,
           measurement = 10,
           unit = 11,
           cost = 12) %>% 
    remove_empty("rows") %>% 
    mutate(
      across("measurement",
              as.numeric)
    )
      return(data)
}


a <- pull(data,"_type_1_")
b <- pull(data,"_type_2_")
c <- pull(data,"_type_3_")
d <- pull(data,"_type_4_")
e <- pull(data,"_type_5_")
f <- pull(data,"_type_6_")
g <- pull(data,"_type_7_")
h <- pull(data,"_type_8_")
i <- pull(data,"_type_9_")
j <- pull(data,"_type_10_")


li <- list(a,b,c,d,e,f,g,h,i,j)

new <- bind_rows(li)

new <- new %>% 
  drop_na(nrcs_practice) %>% 
  mutate(
    across("cost",
           parse_number))

#write_csv(new,here("outputs","workable_database.csv"))

## Calculating number of projects for each NRCS practice
table(new$nrcs_practice)


## Calculating acres/feet/number of these practices implemented

new$measurement <- as.numeric(new$measurement)

practicesum <- new %>% 
  group_by(nrcs_practice) %>%
  summarise(
    prac_sum = sum(measurement, na.rm = TRUE)
  )


##Unit Convert

new <- new %>% 
  filter(unit != "Feet")



new$measurement  <- ifelse(new$unit == "Square Feet",
                       new$measurement/43560,
                       new$measurement)
new <- new %>% 
  mutate(unit = recode(unit,
                       "Square Feet" = "Acres"))

new$commet <- ifelse(new$nrcs_practice == "391\tRiparian Forest Buffer",
                     new$measurement*5.77,
                     ifelse(new$nrcs_practice == "329\tResidue and Tillage Management, No-Till",
                            new$measurement*0.16,
                            ifelse(new$nrcs_practice == "345\tResidue and Tillage Management, Reduced Till",
                                   new$measurement*0.09,
                                   ifelse(new$nrcs_practice == "340\tCover Crop",
                                          new$measurement*0.02,
                                          ifelse(new$nrcs_practice == "612\tTree/Shrub Establishment",
                                                 new$measurement*5.11,
                                                 ifelse(new$nrcs_practice == "327\tConservation Cover",
                                                        new$measurement*0.06,
                                                        ifelse(new$nrcs_practice == "484\tMulching",
                                                               new$measurement*0.32,0)))))))


write_csv(new,here("outputs","PCD_contracts_commet.csv"))


  
