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
library(rstan)
library(rstanarm)
library(rstantools)
library(mlogit)
library(nnet)
library(zoo)
library(arm)
library(dotwhisker)
library(broom)
library(ChoiceModelR)
library(brms)
library(Rcpp)


####################################################################################################################################################

#                                                                   IMPORT

####################################################################################################################################################
nyc <- read_csv("NYPD_Crime_Data_CLEAN_2.csv")
View(nyc)

names(nyc)

nyc_small <- subset(nyc, nyc$Year == 2006)

unique(nyc$OFNS_DESC)
unique(nyc_small$OFNS_DESC)
#import test data
# child_iq <- read_csv("~/School/Classes/Harvard/child.iq.csv")
# View(child_iq)

ggplot(data=nyc_small, mapping=aes(x=OFNS_DESC, fill=OFNS_DESC))+geom_bar()+facet_wrap(~BORO_NM)

#fake one set of data based on parameters of your real data 

# m1 <- stan_glm(LAW_CAT_CD ~ as.factor(BORO_NM) + as.numeric(Hour), data = nyc_small)

m2 <- stan_glm(as.numeric(as.factor(OFNS_DESC)) ~ Month, data = nyc_small)

m_hour <- stan_glm(as.numeric(as.factor(OFNS_DESC)) ~ Hour, data = nyc_small)
m_everything <- stan_glmer(as.numeric(as.factor(OFNS_DESC)) ~ Weekend + Holiday + PARKS_NM + Holiday:PARKS_NM + HADEVELOPT+ Hour + (1|BORO_NM), data = nyc_small)
# 
# xcoding <- c(0,0)
# mcmc=list(R=10, use=10)
# options=list(none=FALSE, save=TRUE, keep=1)
# 
# multi1 <- choicemodelr(nyc_small, xcoding, mcmc=mcmc, options=options, constraints=NONE)
summary(m2)

multi_1 <- brm(formula=as.factor(OFNS_DESC)~Hour+Holiday+(1|BORO_NM), data=nyc_small, 
              family=categorical(), link=logit, warmup=1000, iter=2000, chains=4)
# m1b <- stan_glmer(LAW_CAT_CD ~ BORO_NM + (1|Hour), data = nyc_small)

# m2 <- multinom(LAW_CAT_CD ~ factor(BORO_NM) + factor(Hour), data = nyc_small)

# test_model <- stan_glm(ppvt~educ_cat+momage, data=child_iq)




####################################################################################################################################################

#                                                               PLOTTING CODE

####################################################################################################################################################
#Let's see what we have, shall we? Before modeling it is important to plot your data to see 

#goal: split CMPLNT_FR_DT into three columns: Year, Month, Day
# 
ggplot(data=nyc, mapping=aes(x=BORO_NM, fill=BORO_NM))+geom_bar()
ggplot(data=nyc, mapping=aes(x=LAW_CAT_CD, fill=LAW_CAT_CD))+geom_bar()+facet_grid(BORO_NM~Year)

ggplot(data=nyc, mapping=aes(x=OFNS_DESC, fill=OFNS_DESC))+geom_bar()+facet_wrap(~BORO_NM)

ggplot(data=nyc, mapping=aes(x=OFNS_DESC, fill=OFNS_DESC))+geom_bar()

as.data.frame(table(nyc$OFNS_DESC))

coefplot(m_hour, intercept=TRUE)
dwplot(m2)

dwplot(m_hour)#make a plot of the number of crimes committed on each date, different lines for different years
# dates <- as.Date(nyc$Date, '%m/%d/%Y')
# yr <- year(dates)
# monyr <- as.yearmon(dates)
# lst <- lapply(list(dates, yr, monyr), function(x) 
#   transform(nyc, Count=ave(seq_along(x), x, FUN= length)))
# names(lst) <- paste0('newdf', seq_along(lst))

crimes_by_day <- as.data.frame(table(nyc$Year, nyc$MonthDay))
head(crimes_by_day)

avg_cby <- as.data.frame(aggregate(crimes_by_day[, 3], list(crimes_by_day$Var2), mean))
sd_cby <- aggregate(crimes_by_day[, 3], list(crimes_by_day$Var2), sd)

ggplot(avg_cby, aes(x=Group.1, y=x)) + geom_point()+theme_bw()
  # stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill=type), alpha=0.3) +
  # theme_bw()

crimes_by_day$Year <- crimes_by_day$Var1
crimes_by_day$MonthDay <- crimes_by_day$Var2
crimes_by_day$Count <- crimes_by_day$Freq
# holidays_list <- c("01-01", "02-14", "07-04", "09-04", "10-31", "12-25" )
# easter_list <- c("2006-04-23", "2007-04-08", "2008-03-27", "2009-04-19", "2010-04-04", "2011-04-24", "2012-04-15", "2013-05-05", "2014-04-20", "2015-04-12")
# thanksgiving_list <- c("2005-11-24", "2006-11-23", "2007-11-22", "2008-11-27", "2009-11-26", "2010-11-25", "2011-11-24", "2012-11-22", "2013-11-28", "2014-11-27", "2015-11-26", "2016-11-24")

crimes_by_hour <- as.data.frame(table(nyc$LAW_CAT_CD, nyc$Hour))
#create Holiday indicator variable, 0 if not holiday, 1 if it matches any of the holidays specified above
# crimes_by_day$Holiday <- as.integer(crimes_by_day$MonthDay %in% holidays_list)
# crimes_by_day <- crimes_by_day[c("Year", "MonthDay", "Holiday", "Weekend", "Count")]

# crimes_2005 <- subset(crimes_by_day, crimes_by_day$Year==2005)
# crimes_2015 <- subset(crimes_by_day, crimes_by_day$Year==2015)
ggplot(data=crimes_by_day, mapping=aes(x=Var2, y=Freq, group=Var1, colour=Var1))+geom_line()+xlab("Day")+ylab("Number of Crimes")+ggtitle("Crimes per Day")
# ggplot(data=crimes_2015, mapping=aes(x=MonthDay, y=Count))+geom_point()
crimes_by_month <- as.data.frame(table(nyc_small$Year, nyc_small$Month))
head(crimes_by_month)
ggplot(data=crimes_by_month, mapping=aes(x=Var2, y=Freq))+geom_point()+xlab("Month")+ylab("Number of Crimes")+ggtitle("Crimes per Month")

avg_cby <- as.data.frame(aggregate(crimes_by_month[, 3], list(crimes_by_day$Var2), mean))
sd_cby <- aggregate(crimes_by_day[, 3], list(crimes_by_day$Var2), sd)
crimes_by_month$Year <- crimes_by_day$Var1
crimes_by_day$MonthDay <- crimes_by_day$Var2
crimes_by_day$Count <- crimes_by_day$Freq

#####################
# compare non-holidays to holidays



# 
# ggplot(data=nyc, mapping=aes(x=))