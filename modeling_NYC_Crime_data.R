########
# Author: Zane Wolf
# Date Created: 10/20/2017
# Purpose: To model NYC Crime Data


# Date Updated: 11/25sta/2017

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
# library(stringr)
# library(lubridate)
library(rstan)
library(rstanarm)
library(rstantools)
library(arm)
library(brms)
library(shinystan)
setwd("C:/Users/zane/Documents/School/Classes/Harvard/OEB 201 Experimental Design and Stats/NYC_Crime")
####################################################################################################################################################

#                                                                   IMPORT

####################################################################################################################################################
nyc <- read_csv("NYPD_Crime_Data_CLEAN_2010.csv")

# nyc_big <- read_csv("NYPD_Crime_Data.csv")
View(nyc)

names(nyc)
# as.data.frame(table(nyc$LAW_CAT_CD))
# unique(nyc$Day)



#narrow it down to modeling differences between Misc. Offenses and criminal mischief
  #the ratio between these two, which one is more likely, varies by hours and neighborhoods
# nyc_M <- subset(nyc, nyc$LAW_CAT_CD =="MISDEMEANOR")
# 
# # nyc_M_small <- subset(nyc_M, nyc_M$OFNS_DESC %in% c("ASSAULT 3 & RELATED OFFENSES", "CRIMINAL MISCHIEF & RELATED OF"))
# nyc_M_small <- subset(nyc_M, nyc_M$OFNS_DESC %in% c("MISC. OFFENSES", "CRIMINAL MISCHIEF & RELATED OF"))
# 
# nyc_2010 <- subset(nyc_M_small, nyc_M_small$Year==2010)
# 
# write.csv(nyc_2010, "NYPD_Crime_Data_CLEAN_2010.csv")
# 
# 
# as.data.frame(table(nyc_M_small$OFNS_DESC, nyc_M_small$Year))
# 
# 
# 
# as.data.frame(table(nyc_M_small$OFNS_DESC, nyc_M_small$BORO_NM))
# 
# as.data.frame(table(nyc_M_small$OFNS_DESC, nyc_M_small$Hour, nyc_M_small$PARKS_NM))
# as.data.frame(table(nyc_M_small$OFNS_DESC ,nyc_M_small$LOC_OF_OCCUR_DESC))
# as.data.frame(table(nyc_M_small$OFNS_DESC ,nyc_M_small$Holiday))
# as.data.frame(table(nyc_M_small$OFNS_DESC ,nyc_M_small$Weekend))
# as.data.frame(table(nyc_M_small$OFNS_DESC ,nyc_M_small$HADEVELOPT))
# as.data.frame(table(nyc_M_small$OFNS_DESC ,nyc_M_small$JURIS_DESC))
# as.data.frame(table(nyc_M_small$OFNS_DESC ,nyc_M_small$Hour))

ggplot(data=nyc, mapping=aes(x=BORO_NM, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=nyc, mapping=aes(x=Time2, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=nyc, mapping=aes(x=, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")+facet_grid(BORO_NM~LOC_OF_OCCUR_DESC)

as.data.frame(table(nyc$Time2))

hist(aggregate(nyc$OFNS_DESC))

# df[sample(nrow(df), 3), ]
nyc_2010_sample=nyc[sample(nrow(nyc),10000),]

m2_no_int <- stan_glmer(as.factor(OFNS_DESC) ~ -1 +Hour+LOC_OF_OCCUR_DESC+(1|BORO_NM), data=nyc_2010_sample, family=binomial(link="logit"))

# m3_with_int <- stan_glmer(as.factor(OFNS_DESC) ~ -1 + Hour+LOC_OF_OCCUR_DESC+Hour:LOC_OF_OCCUR_DESC+(1|BORO_NM), data=nyc_2010_sample, family=binomial(link="logit"))

m4_comp_pool <- stan_glm(as.factor(OFNS_DESC) ~ -1+ Hour+LOC_OF_OCCUR_DESC+Hour:LOC_OF_OCCUR_DESC+BORO_NM, data=nyc_2010_sample, family=binomial(link="logit"))

m5_part_pool <- stan_glmer(as.factor(OFNS_DESC) ~ -1+ Hour+LOC_OF_OCCUR_DESC+Hour:LOC_OF_OCCUR_DESC+(1|BORO_NM), data=nyc_2010_sample, family=binomial(link="logit"))

m6_pp_no_int <- stan_glmer(as.factor(OFNS_DESC) ~ -1+ Hour+LOC_OF_OCCUR_DESC+(1|BORO_NM), data=nyc_2010_sample, family=binomial(link="logit"))

####################################################################################################################################################

#                                                               FAKE DATA

####################################################################################################################################################

#create predictor vectors

location=c("Inside", "Outside")
prop.table(table(nyc$LOC_OF_OCCUR_DESC))
dim(hours)
hours=c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
hours
hour_prob <- prop.table(table(nyc$Hour))
hour_prob
police=c(1,0)
popo_prob <- prop.table(table(nyc$JURIS_DESC))
hood=c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
hood_prob <- prop.table(table(nyc$BORO_NM))
hood_fac=factor(hood)
crimes=c("Mischief", "Miscellaneous")
crime_prob <- prop.table(table(nyc$OFNS_DESC))


LOC_OF_OCCUR_DESC <- sample(x = location, size=5000, replace=TRUE, prob=rep(1/2,2))
Hour <- sample(x = hours, size=5000, replace=TRUE, prob=hour_prob)
JURIS_DESC <- sample(x=police, size=5000, replace=TRUE, prob=c(0.124, 0.876))
BORO_NM <- sample(x=hood, size=5000, replace=TRUE, prob=c(0.208, 0.065, 0.198, 0.233, 0.296))
OFNS_DESC <- sample(x=crimes, size=5000, replace=TRUE, prob=rep(1/2,2))


head(var_hood)

 

test_df1 <- cbind.data.frame(OFNS_DESC, JURIS_DESC, BORO_NM, Hour, LOC_OF_OCCUR_DESC, deparse.level=1, stringsAsFactors=TRUE)

LOC_OF_OCCUR_DESC <- sample(x = location, size=5000, replace=TRUE, prob=rep(1/2,2))
Hour <- sample(x = hours, size=5000, replace=TRUE, prob=rep(1/24,24))
JURIS_DESC <- sample(x=police, size=5000, replace=TRUE, prob=rep(1/2, 2))
BORO_NM <- sample(x=hood, size=5000, replace=TRUE, prob=rep(1/5,5))
OFNS_DESC <- sample(x=crimes, size=5000, replace=TRUE, prob=rep(1/2,2))


head(var_hood)



test_df2 <- cbind.data.frame(OFNS_DESC, JURIS_DESC, BORO_NM, Hour, LOC_OF_OCCUR_DESC, deparse.level=1, stringsAsFactors=TRUE)


ggplot(data=test_df1, mapping=aes(x=BORO_NM, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=test_df1, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=test_df1, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")+facet_grid(BORO_NM~LOC_OF_OCCUR_DESC)


ggplot(data=test_df2, mapping=aes(x=BORO_NM, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=test_df2, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=test_df2, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")+facet_grid(BORO_NM~LOC_OF_OCCUR_DESC)

##make a completely numerical model by giving each variable value it's own vectore, between zeros and ones

values <- c(0,1)

#variables
colnum <- 32
nsims <- 5000

fake_data1 <- matrix(0, nsims, colnum)

my_names <- c('Crimes', 'Outside', 'Inside', 'Queens', 'Manhattan', 'Staten Island', 'Bronx', 'Brooklyn', 'H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'H7', 'H8', 'H9', 'H10', 'H11', 'H12', 'H13', 'H14', 'H15', 'H16', 'H17', 'H18', 'H19', 'H20', 'H21', 'H22', 'H23')

colnames(fake_data1) <- my_names

loc_names <- c('Outside', 'Inside')
boro_names <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
hour_names <- c('H0','H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'H7', 'H8', 'H9', 'H10', 'H11', 'H12', 'H13', 'H14', 'H15', 'H16', 'H17', 'H18', 'H19', 'H20', 'H21', 'H22', 'H23')

for (i in 1:nsims){
  random_boro <- sample(x=boro_names, size=1, replace=TRUE, prob=hood_prob)
  random_loc <- sample(x=loc_names, size=1, replace=TRUE, prob=rep(1/2,2))
  random_hour <- sample(x=hour_names, size=1, replace=TRUE, prob=hour_prob)
  
  fake_data1[i, random_boro] <- 1
  fake_data1[i, random_loc] <- 1
  fake_data1[i, random_hour] <- 1
  
}

head(fake_data1)

mu_out <- -2
mu_in <- 2
mu_Bronx <- 0.2
mu_Brook <- -0.4
mu_Queen <- -0.4
mu_Man <- 0.4
mu_SI <- 0.05
mu_0 <- -1
mu_1 <- -0.5
mu_2 <- -0.5
mu_3 <- -0.3
mu_4 <- -0.3
mu_5  <- -0.3
mu_6  <- -0.2
mu_7 <- -0.2
mu_8 <- -0.1
mu_9 <- 0.6
mu_10 <- 0.6
mu_11 <- 0.7
mu_12 <- 1
mu_13 <- 0.6
mu_14  <- 0.6
mu_15 <- 0.6
mu_16 <- 0.1
mu_17 <- -0.1
mu_18 <- -0.3
mu_19 <- -0.4
mu_20 <- -0.5
mu_21 <- -0.4
mu_22  <- 1
mu_23 <- -1


fake_data1b <- fake_data1
for (i in 1:nsims){
#   if (fake_data1a[i, 'Queens']==1 || fake_data1a[i, 'Brooklyn']==1 ){
#     my_prob=c(6/10, 4/10)
#   }
#   else if (fake_data1a[i, 'Bronx']==1 || fake_data1a[i, 'Staten Island']==1){
#     my_prob=c(1/2, 1/2)
#   }
#   else if (fake_data1a[i, 'Manhattan']==1){
#     my_prob=c(4/10, 6/10)
#   }
#   mu_boro <- sample(x=mus_boro, size=1, replace=TRUE, prob=my_prob)
#   
#   if (any(fake_data1a[i,9:17])==1||any(fake_data1a[i,25:32])==1){
#     mu_hour <- -1/2
#   }
#   else if (any(fake_data1a[i,18:24])==1){
#     mu_hour <- 1/2
#   }
#   
  y <- mu_out*as.numeric(fake_data1b[[i, 'Outside']])+mu_in*as.numeric(fake_data1b[[i, 'Inside']])+mu_Queen*as.numeric(fake_data1b[[i, 'Queens']])+mu_Brook*as.numeric(fake_data1b[[i, 'Brooklyn']])+mu_Bronx*as.numeric(fake_data1b[[i, 'Bronx']])+mu_Man*as.numeric(fake_data1b[[i, 'Manhattan']])+mu_SI*as.numeric(fake_data1b[[i, 'Staten Island']])+mu_0*as.numeric(fake_data1b[[i, 'H0']])+mu_1*as.numeric(fake_data1b[[i, 'H1']])+mu_2*as.numeric(fake_data1b[[i, 'H2']])+mu_3*as.numeric(fake_data1b[[i, 'H3']])+mu_4*as.numeric(fake_data1b[[i, 'H4']])+mu_5*as.numeric(fake_data1b[[i, 'H5']])+mu_6*as.numeric(fake_data1b[[i, 'H6']])+mu_7*as.numeric(fake_data1b[[i, 'H7']])+mu_8*as.numeric(fake_data1b[[i, 'H8']])+mu_9*as.numeric(fake_data1b[[i, 'H9']])+mu_10*as.numeric(fake_data1b[[i, 'H10']])+mu_11*as.numeric(fake_data1b[[i, 'H11']])+mu_12*as.numeric(fake_data1b[[i, 'H12']])+mu_13*as.numeric(fake_data1b[[i, 'H13']])+mu_14*as.numeric(fake_data1b[[i, 'H14']])+mu_15*as.numeric(fake_data1b[[i, 'H15']])+mu_16*as.numeric(fake_data1b[[i, 'H16']])+mu_17*as.numeric(fake_data1b[[i, 'H17']])+mu_18*as.numeric(fake_data1b[[i, 'H18']])+mu_19*as.numeric(fake_data1b[[i, 'H19']])+mu_20*as.numeric(fake_data1b[[i, 'H20']])+mu_21*as.numeric(fake_data1b[[i, 'H21']])+mu_22*as.numeric(fake_data1b[[i, 'H22']])+mu_23*as.numeric(fake_data1b[[i, 'H23']])
  if (y<0){
    my_crime='Criminal Mischief'
  }
  else if (y>=0){
    my_crime='Misc. Offenses'
  }
  fake_data1b[[i, 'Crimes']] <- my_crime
  message(i)
}

df1 <- as.data.frame(fake_data1a)
ggplot(data=df1, mapping=aes(x=Inside, group=Crimes, fill=Crimes))+geom_bar(position="dodge")

b_Queens <- sample(x = values, size=5000, replace=TRUE, prob=rep(1/2,2))
b_Manhattan <- sample(x = values, size=5000, replace=TRUE, prob=rep(1/2,2))
b_StattenIsland <- sample(x = values, size=5000, replace=TRUE, prob=rep(1/2,2))
b_Bronx <- sample(x = values, size=5000, replace=TRUE, prob=rep(1/2,2))
b_Brooklyn <- sample(x = values, size=5000, replace=TRUE, prob=rep(1/2,2))
l_Inside <- sample(x = values, size=5000, replace=TRUE, prob=rep(1/2,2))
l_Outside <- sample(x = values, size=5000, replace=TRUE, prob=rep(1/2,2))
h_1 
h_2
h_3
h_4
h_5
h_6 
h_7 
h_8 
h_9
h_10
h_11
h_12
h_13
h_14
h_15 
h_16
h_17
h_18
h_19
h_20 
h_21
h_22 
h_23


# fake_m3 <- stan_glmer(as.factor(var_crimes) ~ var_hours+var_loc+var_hours:var_loc+(1|var_hood), data=test_df1, family=binomial(link="logit"))
# 
# no_pooling_m4 <- stan_glmer(as.factor(var_crimes) ~ var_hours+var_loc+var_hours:var_loc+(1|var_hood), data=test_df1, family=binomial(link="logit"))
# 
# # 
# # sim_fake=posterior_predict(m3, test_df1)
# 
# sims <- as.matrix(m3)
# nsims <- nrow(sims)
# 
# (m3)# var_loc2 <- sample(x = location, size=5000, replace=TRUE, prob=c(1/6,5/6))
# var_hours2 <- sample(x = hours, size=5000, replace=TRUE, prob=rep(1/24, 24))
# var_popo2 <- sample(x=police, size=5000, replace=TRUE, prob=c(1/8, 7/8))
# var_hood2 <- sample(x=hood, size=5000, replace=TRUE, prob=c(1/5, 1/10, 1/10, 2/5, 1/5))
# var_crimes2 <- sample(x=crimes, size=5000, replace=TRUE, prob=rep(1/2,2))
# 
# test_df2 <- cbind.data.frame(var_crimes2,var_loc2, var_hours2, var_popo2, var_hood2, deparse.level=1, stringsAsFactors=TRUE)
# 
# fake_m2 <- stan_glmer(as.factor(var_crimes) ~ var_hours+var_loc+var_hours:var_loc+(1|var_hood), data=test_df2, family=binomial(link="logit"))

# beta_loc <- c(0.495, 0.505)
# beta_hours <- hour_prob
# beta_popo <- popo_prob
# beta_hood <- hood_prob
# 
# ints=c(-1, 3, -5, 4, 2)
# 
# for (i in 1:5000){
#   y=sample(x=ints, size=1, replace=TRUE, prob=rep(1/5, 5))+factor(var_loc[i])+coef_hours*var_hours[i]+coef_popo*var_popo[i]+factor(var_hood[i])
# }

# nyc_M <- subset(nyc, nyc$LAW_CAT_CD =="MISDEMEANOR")
# 
# nyc_M_small_2 <- subset(nyc_M, nyc_M$OFNS_DESC %in% c("MISC. OFFENSES", "CRIMINAL MISCHIEF & RELATED OF"))
# 
# as.data.frame(table(nyc_M_small_2$BORO_NM))
# 
# nyc_M_small_Q2 <- subset(nyc_M_small_2, nyc_M_small_2$BORO_NM == "QUEENS")
# 
# as.data.frame(table(nyc_M_small_Q2$OFNS_DESC ,nyc_M_small_Q2$PARKS_NM))
# as.data.frame(table(nyc_M_small_Q2$OFNS_DESC ,nyc_M_small_Q2$LOC_OF_OCCUR_DESC))
# as.data.frame(table(nyc_M_small_Q2$OFNS_DESC ,nyc_M_small_Q2$Holiday))
# as.data.frame(table(nyc_M_small_Q2$OFNS_DESC ,nyc_M_small_Q2$Weekend))
# as.data.frame(table(nyc_M_small_Q2$OFNS_DESC ,nyc_M_small_Q2$HADEVELOPT))
# as.data.frame(table(nyc_M_small_Q2$OFNS_DESC ,nyc_M_small_Q2$JURIS_DESC))
# as.data.frame(table(nyc_M_small_Q2$OFNS_DESC ,nyc_M_small_Q2$Hour))
# 
# ggplot(data=nyc_M_small_Q2, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar()
# as.data.frame(table(nyc_M_small_Q2$OFNS_DESC))

#####################
# compare non-holidays to holidays
####################################################################################################################################################

#                                                               FAKE DATA

####################################################################################################################################################

N <- 10000
u <- rlogis(N)
x <- rnorm(N)



# #lets look at some histos
# # ggplot(data,aes(x=diff))+geom_histogram()+facet_grid(~type)+theme_bw()
# # ggplot(data=nyc, mapping=aes(x=Time2))+geom_histogram()+facet_grid(~nyc$LAW_CAT_CD)+theme_bw()
# ggplot(data=nyc, mapping=aes(x=Time2, y=LAW_CAT_CD, group=LAW_CAT_CD))+geom_histogram()+theme_bw()
# #get rid of misdemeanors
# nyc_FV <- subset(nyc, nyc$LAW_CAT_CD %in% c("FELONY", "VIOLATION"))
# 
# 
# nyc_2007_FV <- subset(nyc_FV, nyc_FV$Year == 2007)
# 
# unique(nyc$OFNS_DESC)
# unique(nyc_small$OFNS_DESC)
# #import test data
# # child_iq <- read_csv("~/School/Classes/Harvard/child.iq.csv")
# # View(child_iq)
# 
# ggplot(data=nyc, mapping=aes(x=OFNS_DESC, fill=OFNS_DESC))+geom_bar()+facet_wrap(~BORO_NM)
# 
# crimes_by_time <- as.data.frame(table(nyc$Time2, nyc$LAW_CAT_CD))
# #Var1 is the time
# #Var2 is the type of crime, Felony, Misdemeanor, or Violation
# #Freq is the count for that day
# 
# ggplot(data=crimes_by_time, mapping=aes(x=Var1, y=Freq))+geom_point()+facet_wrap(~Var2)
# 
# avg_cby <- as.data.frame(aggregate(crimes_by_time[, 3], list(crimes_by_time$Var2), mean))
# 
# #fake one set of data based on parameters of your real data 
# 
# # m1 <- stan_glm(LAW_CAT_CD ~ as.factor(BORO_NM) + as.numeric(Hour), data = nyc_small)
# 
# # m2 <- stan_glm(as.numeric(as.factor(OFNS_DESC)) ~ Month, data = nyc_small)
# # 
# # m_hour <- stan_glm(as.numeric(as.factor(OFNS_DESC)) ~ Hour, data = nyc_small)
# # m_everything <- stan_glmer(as.numeric(as.factor(OFNS_DESC)) ~ Weekend + Holiday + PARKS_NM + Holiday:PARKS_NM + HADEVELOPT+ Hour + (1|BORO_NM), data = nyc_small)
# # 
# # xcoding <- c(0,0)
# # mcmc=list(R=10, use=10)
# # options=list(none=FALSE, save=TRUE, keep=1)
# # 
# # multi1 <- choicemodelr(nyc_small, xcoding, mcmc=mcmc, options=options, constraints=NONE)
# summary(m2)

m1 <- stan_glmer(as.factor(LAW_CAT_CD) ~ Hour+Holiday+(1|BORO_NM), data=nyc_2007_FV, family=binomial(link="logit"))

# multi_1 <- brm(formula=as.factor(LAW_CAT_CD)~Hour+Holiday+(1|BORO_NM), data=nyc_2007_FV, 
# family=categorical(), warmup=1000, iter=1000, chains=4)
# m1b <- stan_glmer(LAW_CAT_CD ~ BORO_NM + (1|Hour), data = nyc_small)

# m2 <- multinom(LAW_CAT_CD ~ factor(BORO_NM) + factor(Hour), data = nyc_small)

# test_model <- stan_glm(ppvt~educ_cat+momage, data=child_iq)




####################################################################################################################################################

#                                                               PLOTTING CODE

####################################################################################################################################################
#Let's see what we have, shall we? Before modeling it is important to plot your data to see 

#goal: split CMPLNT_FR_DT into three columns: Year, Month, Day
# 
# ggplot(data=nyc, mapping=aes(x=BORO_NM, fill=BORO_NM))+geom_bar()
# ggplot(data=nyc, mapping=aes(x=LAW_CAT_CD, fill=LAW_CAT_CD))+geom_bar()+facet_grid(BORO_NM~Year)
# 
# ggplot(data=nyc, mapping=aes(x=OFNS_DESC, fill=OFNS_DESC))+geom_bar()+facet_wrap(~BORO_NM)
# 
# ggplot(data=nyc, mapping=aes(x=OFNS_DESC, fill=OFNS_DESC))+geom_bar()
# 
# as.data.frame(table(nyc$OFNS_DESC))
# 
# coefplot(m_hour, intercept=TRUE)
# dwplot(m2)
# 
# dwplot(m_hour)#make a plot of the number of crimes committed on each date, different lines for different years
# # dates <- as.Date(nyc$Date, '%m/%d/%Y')
# # yr <- year(dates)
# # monyr <- as.yearmon(dates)
# # lst <- lapply(list(dates, yr, monyr), function(x) 
# #   transform(nyc, Count=ave(seq_along(x), x, FUN= length)))
# # names(lst) <- paste0('newdf', seq_along(lst))
# 
# crimes_by_day <- as.data.frame(table(nyc$Year, nyc$MonthDay))
# head(crimes_by_day)
# 
# avg_cby <- as.data.frame(aggregate(crimes_by_day[, 3], list(crimes_by_day$Var2), mean))
# sd_cby <- aggregate(crimes_by_day[, 3], list(crimes_by_day$Var2), sd)
# 
# ggplot(avg_cby, aes(x=Group.1, y=x)) + geom_point()+theme_bw()
#   # stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill=type), alpha=0.3) +
#   # theme_bw()
# 
# crimes_by_day$Year <- crimes_by_day$Var1
# crimes_by_day$MonthDay <- crimes_by_day$Var2
# crimes_by_day$Count <- crimes_by_day$Freq
# # holidays_list <- c("01-01", "02-14", "07-04", "09-04", "10-31", "12-25" )
# # easter_list <- c("2006-04-23", "2007-04-08", "2008-03-27", "2009-04-19", "2010-04-04", "2011-04-24", "2012-04-15", "2013-05-05", "2014-04-20", "2015-04-12")
# # thanksgiving_list <- c("2005-11-24", "2006-11-23", "2007-11-22", "2008-11-27", "2009-11-26", "2010-11-25", "2011-11-24", "2012-11-22", "2013-11-28", "2014-11-27", "2015-11-26", "2016-11-24")
# 
# crimes_by_hour <- as.data.frame(table(nyc$LAW_CAT_CD, nyc$Hour))
# #create Holiday indicator variable, 0 if not holiday, 1 if it matches any of the holidays specified above
# # crimes_by_day$Holiday <- as.integer(crimes_by_day$MonthDay %in% holidays_list)
# # crimes_by_day <- crimes_by_day[c("Year", "MonthDay", "Holiday", "Weekend", "Count")]
# 
# # crimes_2005 <- subset(crimes_by_day, crimes_by_day$Year==2005)
# # crimes_2015 <- subset(crimes_by_day, crimes_by_day$Year==2015)
# ggplot(data=crimes_by_day, mapping=aes(x=Var2, y=Freq, group=Var1, colour=Var1))+geom_line()+xlab("Day")+ylab("Number of Crimes")+ggtitle("Crimes per Day")
# # ggplot(data=crimes_2015, mapping=aes(x=MonthDay, y=Count))+geom_point()
# crimes_by_month <- as.data.frame(table(nyc_small$Year, nyc_small$Month))
# head(crimes_by_month)
# ggplot(data=crimes_by_month, mapping=aes(x=Var2, y=Freq))+geom_point()+xlab("Month")+ylab("Number of Crimes")+ggtitle("Crimes per Month")
# 
# avg_cby <- as.data.frame(aggregate(crimes_by_month[, 3], list(crimes_by_day$Var2), mean))
# sd_cby <- aggregate(crimes_by_day[, 3], list(crimes_by_day$Var2), sd)
# crimes_by_month$Year <- crimes_by_day$Var1
# crimes_by_day$MonthDay <- crimes_by_day$Var2
# crimes_by_day$Count <- crimes_by_day$Freq
# 
# crimes_by_hour <- as.data.frame(table(nyc$LAW_CAT_CD, nyc$Hour))
# ggplot(data=crimes_by_hour, mapping=aes(x=Var2, y=Freq, group=Var1, colour=Var1, fill=Var1))+geom_point()+theme_bw()
# 
# as.data.frame(table(nyc$OFNS_DESC, nyc$LAW_CAT_CD))

# 
# ggplot(data=nyc, mapping=aes(x=))