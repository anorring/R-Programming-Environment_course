######################################################################################################################
### Author: Anni Norring                                                                                           ###
### Date: 2018 						                                                                                         ###
### Content: This script contains the R code for The R programming environment course                              ###                                                               ###
######################################################################################################################

# Access all the needed libraries:
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(readr)
library(lubridate)

######################################################################################################################
###             Week 4: QUIZ
######################################################################################################################



#Use the readr package to read the daily_SPEC_2014.csv.bz2 data file in to R. This file contains daily levels of fine
#particulate matter (PM2.5) chemical constituents across the United States. The data are measured at a network of 
#federal, state, and local monitors and assembled by the EPA.

#In this dataset, the "Sample.Value" column provides the level of the indicated chemical constituent and the 
#"Parameter.Name" column provides the name of the chemical constituent. The combination of a "State.Code", a 
#"County.Code", and a "Site.Num", uniquely identifies a monitoring site (the location of which is provided by the 
#"Latitude" and "Longitude" columns).

spec <- read_csv("/Users/anninorring/Downloads/data-2/daily_SPEC_2014.csv.bz2")

head(spec)

spec_2 <- spec %>% select(`Parameter Name`, `Arithmetic Mean`, `State Code`, `State Name`, `County Code`, `Site Num`)  

head(spec_2)

#For all of the questions below, you can ignore the missing values in the dataset, so when taking averages, just remove
#the missing values before taking the average (i.e. you can use na.rm = TRUE in the mean() function)

######################################################################################################################
### QUESTION 1: 

#What is average Arithmetic.Mean for “Bromine PM2.5 LC” in the state of Wisconsin in this dataset?

spec_3 <- spec_2 %>% filter(`Parameter Name` == "Bromine PM2.5 LC" & `State Name` == "Wisconsin")

head(spec_3)

#ANSWER: 
mean(spec_3$`Arithmetic Mean`, na.rm=TRUE)

# 0.003960482

######################################################################################################################
### QUESTION 2

#Calculate the average of each chemical constituent across all states, monitoring sites and all time points.
#Which constituent Parameter.Name has the highest average level?

spec_4 <- spec_2 %>% filter(grepl("PM", `Parameter Name`)) %>% 
  group_by(`Parameter Name`) %>% 
  summarize(ave_level = mean(`Arithmetic Mean`))

head(spec_4)

auxinfo_1 <- spec_4 %>% summarize(highest_level = max(spec_4$ave_level))
auxinfo_1

#ANSWER:
filter(spec_4, ave_level>=50)

# OC CSN Unadjusted PM2.5 LC TOT      67.8 

######################################################################################################################
### QUESTION 3

#Which monitoring site has the highest average level of “Sulfate PM2.5 LC” across all time? Indicate the state code, 
#county code, and site number.

spec_5 <- spec_2 %>% mutate(monitoringsite = paste(`Site Num`, `State Code`, `County Code`, sep="_")) %>%
  filter(grepl("Sulfate", `Parameter Name`)) %>%
  group_by(monitoringsite) %>%
  summarize(ave_level = mean(`Arithmetic Mean`))
head(spec_5)

auxinfo_2 <- spec_5 %>% summarize(highest_sulfate = max(ave_level))
auxinfo_2

#ANSWER:
filter(spec_5, ave_level>=3)

#State 39, County 081, Site 0017

######################################################################################################################
### QUESTION 4

#What is the absolute difference in the average levels of “EC PM2.5 LC TOR” between the states California and 
#Arizona, across all time and all monitoring sites?

spec_6 <- spec_2 %>% filter(grepl("EC PM", `Parameter Name`)) %>%
  filter(`State Name` == "California" | `State Name` == "Arizona") %>%
  group_by(`State Name`) %>%
  summarize(ave_level = mean(`Arithmetic Mean`))
head(spec_6)  

ariz <- spec_6[1,2]
cali <- spec_6[2,2]

#ANSWER:
cali - ariz

#0.01856696

######################################################################################################################
### QUESTION 5

#What is the median level of “OC PM2.5 LC TOR” in the western United States, across all time? Define western as any 
#monitoring location that has a Longitude LESS THAN -100.

head(spec)

answer_5 <- spec %>% 
  select(`Parameter Name`, `Arithmetic Mean`, `State Code`, `State Name`, `County Code`, `Site Num`, Longitude) %>%
  filter(Longitude < -100) %>%
  filter(grepl("OC PM", `Parameter Name`)) %>%
  summarize(ave_level = median(`Arithmetic Mean`))
answer_5

#0.430

######################################################################################################################
### QUESTION 6

#Use the readxl package to read the file aqs_sites.xlsx into R (you may need to install the package first). This file 
#contains metadata about each of the monitoring sites in the EPA’s monitoring system. In particular, the "Land Use" 
#and "Location Setting" variables contain information about what kinds of areas the monitors are located in (i.e. 
#“residential” vs. “forest”).

getwd()

setwd("/Users/anninorring/Downloads")

aqs <- read_excel("aqs_sites.xlsx")

head(aqs)

#How many monitoring sites are labelled as both RESIDENTIAL for "Land Use" and SUBURBAN for "Location Setting"?

aqs_1 <- aqs %>% filter(`Land Use`== "RESIDENTIAL") %>%
  filter(`Location Setting` == "SUBURBAN") 

head(aqs_1)

#ANSWER:
str(aqs_1)

#Number of observations: 3527

######################################################################################################################
### QUESTION 7

#What is the median level of “EC PM2.5 LC TOR” amongst monitoring sites that are labelled as both “RESIDENTIAL” and 
#“SUBURBAN” in the eastern U.S., where eastern is defined as Longitude greater than or equal to -100?

#I need to merge the two data frames. I'll use Longitude and Latitude as identifiers.

spec_aqs <- inner_join(spec, aqs, by = c("Longitude", "Latitude")) %>%
  select(`Parameter Name`, `Arithmetic Mean`, Longitude, Latitude, `Site Num`, `State Code.x`, `County Code.x`, 
         `State Name.x`, `Land Use`, `Location Setting`, `Date Local`)
head(spec_aqs)

#Find the median level of EC:

answer_7 <- spec_aqs %>% 
  filter(`Land Use`== "RESIDENTIAL") %>%
  filter(`Location Setting` == "SUBURBAN") %>%
  filter(grepl("EC PM", `Parameter Name`)) %>%
  filter(Longitude >= -100) %>%
  summarize(ave_level = median(`Arithmetic Mean`))
answer_7

#0.610

######################################################################################################################
### QUESTION 8

#Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use", which month of the year has the highest 
#average levels of "Sulfate PM2.5 LC"?

spec_9 <- spec_aqs %>% 
  filter(`Land Use`== "COMMERCIAL") %>%
  filter(grepl("Sulfate PM", `Parameter Name`)) %>%
  mutate(Month = months(`Date Local`)) %>%
  select(`Parameter Name`, `Land Use`, Month, `Arithmetic Mean`) %>%
  group_by(Month)
  
head(spec_9)

#ANSWER:
summarize(spec_9, ave_level = mean(`Arithmetic Mean`))

#February

######################################################################################################################
### QUESTION 9

#Take a look at the data for the monitoring site identified by State Code 6, County Code 65, and Site Number 8001 
#(this monitor is in California). At this monitor, for how many days is the sum of "Sulfate PM2.5 LC" and 
#"Total Nitrate PM2.5 LC" greater than 10?

#For each of the chemical constituents, there will be some dates that have multiple `Arithmetic Mean` values at this 
#monitoring site. When there are multiple values on a given date, take the average of the constituent values for that 
#date.

spec_10 <- spec_aqs %>%
  filter(`State Code.x` == "06") %>%
  filter(`County Code.x` == "065") %>%
  filter(`Site Num` == "8001") %>%
  filter(`Parameter Name`%in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  select(`Parameter Name`, `Arithmetic Mean`, `Site Num`, `State Code.x`, `County Code.x`, 
         `Date Local`) %>%
  mutate(Year = year(`Date Local`),
         YearDay = yday(`Date Local`)) %>%
  group_by(Year, YearDay) %>%
  mutate('Daily Mean' = mean(`Arithmetic Mean`)) %>%
  
  ungroup()

head(spec_10)  
  
  group_by(`Parameter Name`) %>%
  summarize(ave_value = mean(`Arithmetic Mean`)) %>%
  group_by(`Date Local`) %>%
  summarize(sulf_nitr = sum(`Arithmetic Mean`))
head(spec_10)

answer_9 <- filter(spec_10, sulf_nitr > 10)
answer_9

#STILL WRONG

######################################################################################################################
### QUESTION 10

#Which monitoring site in the dataset has the highest correlation between "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 
#LC" across all dates? Identify the monitoring site by it's State, County, and Site Number code.

#For each of the chemical constituents, there will be some dates that have multiple Sample.Value's at a monitoring 
#site. When there are multiple values on a given date, take the average of the constituent values for that date.

#Correlations between to variables can be computed with the cor() function.


