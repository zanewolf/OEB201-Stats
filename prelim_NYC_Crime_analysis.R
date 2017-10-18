########
  # Author: Zane Wolf
  # Date Created: 9/25/2017
  # Purpose: To explore and clean 

  # Date Updated: 

########

#housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("C:/Users/zane/Documents/School/Classes/Harvard/OEB 201 Experimental Design and Stats/NYC_Crime")

# libraries
library(tidyverse)
  #includes ggplot2, tibble, tidyr, readr, purrr, dplyr
library(plyr)
library(stringr)
library(lubridate)

###############################################

#start Work

###############################################
nyc <- read_csv("NYPD_Complaint_Data_Historic.csv")
View(nyc)

headers <- names(nyc)

###############################################

#Data Cleaning

###############################################

# delete the following columns: CMPLNT_TO_DT, CMPLNT_TO_TM, RPT_DT, X_COORD_CD, Y_COORD_CD, 
# and Lat_Lon

bad_vars <- names(nyc) %in% c("CMPLNT_TO_DT", "CMPLNT_TO_TM", "RPT_DT", "X_COORD_CD", "Y_COORD_CD", "Lat_Lon")
nyc <- nyc[!bad_vars]

#select for Completed Crimes
as.data.frame(table(nyc$CRM_ATPT_CPTD_CD)) #find out just how many were attempted....negligible, ~90,000 out of 5.5million
unique(nyc$CRM_ATPT_CPTD_CD) #Completed, attempted, NA
nyc <- subset(nyc, CRM_ATPT_CPTD_CD=="COMPLETED")

#Reduce parks to binary
  #if it occurred in a park, doesn't matter which park, -> 1
  #if it did not occur in a park -> 0
nyc$PARKS_NM[is.na(nyc$PARKS_NM)] <- 0
nyc$PARKS_NM[nyc$PARKS_NM!=0] <- 1

#Reduce housing developments to binary
  #if it occurred in a housing development -> 1
  #if it did not -> 0
nyc$HADEVELOPT[is.na(nyc$HADEVELOPT)] <- 0
nyc$HADEVELOPT[nyc$HADEVELOPT!=0] <- 1

#Reduce jurisdiction to binary
  #if NY Police Department -> 1
  #if any other department -> 0 
      # unique(nyc$JURIS_DESC)
      # [1] "1"                                   "N.Y. HOUSING POLICE"                 "N.Y. TRANSIT POLICE"                
      # [4] "N.Y. STATE POLICE"                   "DEPT OF CORRECTIONS"                 "TRI-BORO BRDG TUNNL"                
      # [7] "OTHER"                               "PORT AUTHORITY"                      "NYC PARKS"                          
      # [10] "HEALTH & HOSP CORP"                  "METRO NORTH"                         "LONG ISLAND RAILRD"                 
      # [13] "STATN IS RAPID TRANS"                "N.Y. STATE PARKS"                    "U.S. PARK POLICE"                   
      # [16] "NEW YORK CITY SHERIFF OFFICE"        "NYS DEPT TAX AND FINANCE"            "AMTRACK"                            
      # [19] "CONRAIL"                             "POLICE DEPT NYC"                     "FIRE DEPT (FIRE MARSHAL)"           
      # [22] "NYC DEPT ENVIRONMENTAL PROTECTION"   "SEA GATE POLICE DEPT"                "DISTRICT ATTORNEY OFFICE"           
      # [25] "NYS DEPT ENVIRONMENTAL CONSERVATION"
nyc$JURIS_DESC[nyc$JURIS_DESC=='N.Y. POLICE DEPT'] <- 1
nyc$JURIS_DESC[nyc$JURIS_DESC!=1] <- 0

#Reduce location of occurance description to binary
  #if inside -> 1
  #if anything else -> 0 
      # as.data.frame(table(nyc$LOC_OF_OCCUR_DESC))
      # Var1    Freq
      # 1    FRONT OF 1276501
      # 2      INSIDE 2731916
      # 3 OPPOSITE OF  151301
      # 4     OUTSIDE    2962
      # 5     REAR OF  118900

#MISSING DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#based on first 100 entries, I'm setting anything with a premise description as Street, Park/playground, or Gas Station as outside
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='STREET'] <- 0
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='PARK/PLAYGROUND'] <- 0
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='GAS STATION'] <- 0
#define all othe others
nyc$LOC_OF_OCCUR_DESC[nyc$LOC_OF_OCCUR_DESC!='INSIDE'] <- 0
nyc$LOC_OF_OCCUR_DESC[nyc$LOC_OF_OCCUR_DESC=='INSIDE'] <- 1
#there are still a bunch of NAs...277,996 to be exact
sum(is.na(nyc$LOC_OF_OCCUR_DESC))
#which type of premise descriptions have NA as location description....
unique(nyc$PREM_TYP_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)])
#interesting, there are some that have NA for both...how many? 
sum(is.na(nyc$LOC_OF_OCCUR_DESC)&is.na(nyc$PREM_TYP_DESC))
#...5,897
#are they all from the same borrow? 
as.data.frame(table(nyc$BORO_NM[(is.na(nyc$LOC_OF_OCCUR_DESC)&is.na(nyc$PREM_TYP_DESC))]))
#they seem to be pretty evenly distributed, except for staten island. 
as.data.frame(table(nyc$BORO_NM))
#but staten island has a lower amount of crimes reported anyway, so having a reduced number of missing location seems to be in proportion

#since part of my goal is to model based off of location, crime reports without locational description data is rather useless.
#I'm going to go ahead and delete the 5,897 incidences without that data
nyc <- nyc[!with(nyc,is.na(nyc$LOC_OF_OCCUR_DESC) & is.na(nyc$PREM_TYP_DESC)),]

#now back to the others...
na_premises <- unique(nyc$PREM_TYP_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)])
# [1] "OTHER"                        "TRANSIT - NYC SUBWAY"         "RESIDENCE - APT. HOUSE"       "BUS STOP"                    
# [5] "GROCERY/BODEGA"               "TUNNEL"                       "RESIDENCE-HOUSE"              "BRIDGE"                      
# [9] "AIRPORT TERMINAL"             "PUBLIC BUILDING"              "FOOD SUPERMARKET"             "BUS (NYC TRANSIT)"           
# [13] "OPEN AREAS (OPEN LOTS)"       "PARKING LOT/GARAGE (PUBLIC)"  "PARKING LOT/GARAGE (PRIVATE)" "HIGHWAY/PARKWAY"             
# [17] "DRY CLEANER/LAUNDRY"          "HOTEL/MOTEL"                  "CLOTHING/BOUTIQUE"            "STORAGE FACILITY"            
# [21] "COMMERCIAL BUILDING"          "BAR/NIGHT CLUB"               "CONSTRUCTION SITE"            "FAST FOOD"                   
# [25] "BANK"                         "CHAIN STORE"                  "TAXI (LIVERY LICENSED)"       "HOSPITAL"                    
# [29] "SMALL MERCHANT"               "TAXI (YELLOW LICENSED)"       "TAXI/LIVERY (UNLICENSED)"     "TRANSIT FACILITY (OTHER)"    
# [33] "BUS TERMINAL"                 "PUBLIC SCHOOL"                "BUS (OTHER)"                  "RESTAURANT/DINER"            
# [37] "BEAUTY & NAIL SALON"          "MARINA/PIER"                  NA                             "RESIDENCE - PUBLIC HOUSING"  
# [41] "DEPARTMENT STORE"             "CANDY STORE"                  "TELECOMM. STORE"              "STORE UNCLASSIFIED"          
# [45] "DRUG STORE"                   "GYM/FITNESS FACILITY"         "CHURCH"                       "BOOK/CARD"                   
# [49] "CHECK CASHING BUSINESS"       "ABANDONED BUILDING"           "SYNAGOGUE"                    "LIQUOR STORE"                
# [53] "OTHER HOUSE OF WORSHIP"       "DOCTOR/DENTIST OFFICE"        "FACTORY/WAREHOUSE"            "ATM"                         
# [57] "PRIVATE/PAROCHIAL SCHOOL"     "CEMETERY"                     "JEWELRY"                      "SOCIAL CLUB/POLICY"          
# [61] "VARIETY STORE"                "TRAMWAY"                      "FERRY/FERRY TERMINAL"         "PHOTO/COPY"                  
# [65] "VIDEO STORE"                  "SHOE"                         "MOSQUE"                       "LOAN COMPANY" 

#going to set the following as outside....
na_prem_outside <- na_premises[c(2,4,6,8,12:16,23,27,29:33,35,38,55,57,61,62)]
na_premises[c(2,4,6,8,12:16,23,27,29:33,35,38,55,57,61,62)]

nyc$LOC_OF_OCCUR_DESC[with(nyc, nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC %in% na_prem_outside & is.na(nyc$LOC_OF_OCCUR_DESC)])] <- 0

# #set these as the inside...
na_prem_inside <-  na_premises[c(3,5,7,9:11,17:22,24:26,28,34,36,37,39:54,56,58:60,63,64,66,67)]
na_prem_inside
nyc$LOC_OF_OCCUR_DESC[with(nyc, nyc$PREM_TYP_DESC %in% na_prem_inside & is.na(nyc$LOC_OF_OCCUR_DESC))] <- 1

# #what's left....hmm, Just 'SHOE' and 'OTHER'....what the hell type is shoe? maybe shoe store?? 
unique(nyc$PREM_TYP_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)])
as.data.frame(table(nyc$PREM_TYP_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)]))
#set 'SHOE' to inside....why not, only a couple hundred of them
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='SHOE'] <- 1

#so what happens at Other?? 
as.data.frame(table(nyc$PD_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)])) 
# a freaking lot
# lets just set it all to outside, the crimes with high frequencies (>500 occurrences) are ones that occur in 'open areas'
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='OTHER'] <- 1
#no more 'missing' data and all locations set to either inside (1) or outside (0)...whew. 

#Time for Time


# na_premises[!(na_premises %in% na_prem_outside | na_premises %in% na_prem_inside)]
# 
# unique(nyc$PREM_TYP_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)])
# nyc$LOC_OF_OCCUR_DESC[c(na_premises[])] <- 0
#goal: split CMPLNT_FR_DT into three columns: Year, Month, Day
# 
# ggplot(data=nyc, mapping=aes(x=BORO_NM, fill=BORO_NM))+geom_bar()
# ggplot(data=nyc, mapping=aes(x=LAW_CAT_CD, fill=LAW_CAT_CD))+geom_bar()+facet_wrap(~BORO_NM)
# 
# ggplot(data=nyc, mapping=aes(x=))
