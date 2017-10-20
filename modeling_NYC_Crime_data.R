########
# Author: Zane Wolf
# Date Created: 10/20/2017
# Purpose: To model NYC Crime Data


# Date Updated: 

########

####################################################################################################################################################

#                                                                 HOUSEKEEPING

####################################################################################################################################################
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("C:/Users/zane/Documents/School/Classes/Harvard/OEB 201 Experimental Design and Stats/NYC_Crime")

# libraries
library(tidyverse)
#includes ggplot2, tibble, tidyr, readr, purrr, dplyr
library(plyr)
library(stringr)
library(lubridate)

####################################################################################################################################################

#                                                                   IMPORT

####################################################################################################################################################
nyc <- read_csv("NYPD_Crime_Data_CLEAN.csv")
View(nyc)

headers <- names(nyc)

####################################################################################################################################################

#                                                               PLOTTING CODE

####################################################################################################################################################
#Let's see what we have, shall we? Before modeling it is important to plot your data to see 

#goal: split CMPLNT_FR_DT into three columns: Year, Month, Day
# 
# ggplot(data=nyc, mapping=aes(x=BORO_NM, fill=BORO_NM))+geom_bar()
# ggplot(data=nyc, mapping=aes(x=LAW_CAT_CD, fill=LAW_CAT_CD))+geom_bar()+facet_wrap(~BORO_NM)
# 
# ggplot(data=nyc, mapping=aes(x=))