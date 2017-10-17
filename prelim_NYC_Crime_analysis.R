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
nyc$LOC_OF_OCCUR_DESC[nyc$LOC_OF_OCCUR_DESC=='INSIDE'] <- 1
nyc$LOC_OF_OCCUR_DESC[nyc$LOC_OF_OCCUR_DESC!=1] <- 0

#goal: split CMPLNT_FR_DT into three columns: Year, Month, Day
# 
# ggplot(data=nyc, mapping=aes(x=BORO_NM, fill=BORO_NM))+geom_bar()
# ggplot(data=nyc, mapping=aes(x=LAW_CAT_CD, fill=LAW_CAT_CD))+geom_bar()+facet_wrap(~BORO_NM)
# 
# ggplot(data=nyc, mapping=aes(x=))
