#YTD Analysis of EBRPD 459s at start of Q4

#Load packages
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)

#Import 459 YTD data
YTD_459_2022 <- read_csv("Downloads/459 YTD - Sheet1.csv")

#For analysis purposes, create separate columns for the Month, Day, and Year each incident occurred.
V2_2022 <- separate(YTD_459_2022, Date, into = c("Month", "Day", "Year"))

#Entries 135, 136, and 137 have the wrong Year entered. Replace the Years so they are consistent with the others.
V2_2022["Year"][V2_2022["Year"]=="21"] <- "22"  

#Want names of the months instead of numbers.
V2_2022["Month"][V2_2022["Month"]=='1'] <- "January"
V2_2022["Month"][V2_2022["Month"]=='2'] <- "February"
V2_2022["Month"][V2_2022["Month"]=='3'] <- "March"
V2_2022["Month"][V2_2022["Month"]=='4'] <- "April"
V2_2022["Month"][V2_2022["Month"]=='5'] <- "May"
V2_2022["Month"][V2_2022["Month"]=='6'] <- "June"
V2_2022["Month"][V2_2022["Month"]=='7'] <- "July"
V2_2022["Month"][V2_2022["Month"]=='8'] <- "August"
V2_2022["Month"][V2_2022["Month"]=='9'] <- "September"
V2_2022["Month"][V2_2022["Month"]=='10'] <- "October"

#Identify which parks have had the most 459's
Park_459s <- V2_2022 %>% 
              count(Park)

#Based on this table, it appears that Tilden is by far the most frequent spot for 459's with 25
#This is followed by Briones with 13 and then MLK with 9

#Visualization of the 459's by park
ggplot(V2_2022, aes(x=Park))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "EBRPD 459's by Park 2022")

#How does the 459 data from 2022 compare to 2021? 

#Import data from 2021
#IMPORTANT NOTE: 2021 data is final end of year report and only contains total 459 counts per park. 
YTD_21 <- read_csv("Downloads/459 YTD '21 - Sheet1.csv")

#Import total 459s from 2022
YTD_22_Total <- read_excel("Documents/YTD_22_Total.xlsx")

#Join 2021 data with 2022 data

YTD_22v21 <- full_join(YTD_21,YTD_22_Total, by = "Park") %>% 
              rename(ID = ID.x) %>% 
              rename(Total_21 = Total.x) %>% 
              rename(Total_22 = Total.y)

#The ID.y column is not needed, so I removed it.
#This final table will allow data from 2021 to be compared to 2022
V2_YTD_22v21 <- subset(YTD_22v21, select = -c(ID.y))

 
#ALL REMANING CODE WILL BE FOR 2022 DATA

#Along with which park the 459 occurred in, I also included which Staging Area the 459 occurred at.
#Given that Tilden, Briones, and MLK saw the most 459s, I filtered the data for these parks. 
#This will allow me to identify which Staging Areas are hotspots. 

Tilden_459s <- V2_2022 %>% 
                filter(Park == "Tilden")

Briones_459s <- V2_2022 %>% 
                filter(Park == "Briones")

MLK_459s <- V2_2022 %>% 
            filter(Park == "MLK")

#For Tilden, Briones, and MLK, identify which Staging Areas are most common for 459s 

Tilden_459s %>% 
  count(Staging_area)

#Tilden has many Staging Areas that are often hit. Brazil Room saw the most 459s with 4, followed by Loan Oak, Quarry Lot, and Steam Trains, which all had 3.
#These parks either lack cameras or have poor coverage of the Staging Area.

Briones_459s %>% 
  count(Staging_area)

#The Lafayette Ridge Staging Area is the most commonly hit in Briones with 5, however this is EBMUD property.
#For regular patrol, Bear Creak Staging Area is the most commonly hit. Given the size of this Staging Area, the only camera is on the enterance kiosk.

MLK_459s %>% 
  count(Staging_area)

#For MLK, the most commonly hit Staging Area is Damon Slough. Possible explination is becuase this Staging Area is right next to a freeway entrance. 
#The cameras at MLK are usually dead, we've talked with staff about this multiple times.


#Identify which months are the most active for 459s
Month_459_2022 <- V2_2022 %>% 
              count(Month)
  
#Visualization of 459s by month 
ggplot(V2_2022, aes(x=factor(V2_2022$Month, levels = month.name)))+
  geom_bar()+
  labs(x= 'Month', title = "EBRPD 459's by Month")

#Based on visualization, May was the most active month for 459's
    
