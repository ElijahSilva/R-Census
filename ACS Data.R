#################
# LOAD PACKAGES #
#################
library(tidycensus)
library(acs)
library(dplyr)

######################
# LOAD ACS VARIABLES #
######################
v15 <- load_variables(2015, "acs5", cache=TRUE) # view all variables

################
# GET ACS DATA #
################
medincome <- get_acs(geography="tract", variables = "B19013A_001E",year=2015,output="tidy", state="IN", county=141)
highschool <- get_acs(geography="tract", variables = "B15003_017E",year=2015,output="tidy", state="IN", county=141)
totalpop <- get_acs(geography="tract", variables = "B01003_001E",year=2015,output="tidy", state="IN", county=141)
houserent <- get_acs(geography="tract", variables = "B07013_003E",year=2015,output="tidy", state="IN", county=141)
housebuilt <- get_acs(geography="tract", variables = "B25035_001E",year=2015,output="tidy", state="IN", county=141)
blackpop <- get_acs(geography="tract", variables = "B02001_003E",year=2015,output="tidy", state="IN", county=141)
hisppop <- get_acs(geography="tract", variables = "B03001_003E",year=2015,output="tidy", state="IN", county=141)
poverty <- get_acs(geography="tract", variables = "B05010_002E",year=2015,output="tidy", state="IN", county=141)
totalhouseyear <- get_acs(geography="tract", variables = "B25034_001E",year=2015,output="tidy", state="IN", county=141)
house50_59<- get_acs(geography="tract", variables = "B25034_009E",year=2015,output="tidy", state="IN", county=141)
house40_49<- get_acs(geography="tract", variables = "B25034_010E",year=2015,output="tidy", state="IN", county=141)
house39_ <- get_acs(geography="tract", variables = "B25034_011E",year=2015,output="tidy", state="IN", county=141)

####################################################
# REMOVE MOE & VARIABLE AND CONVERT TO PERCENTAGES # (ALL IN ONE STEP!!!!)
####################################################
Fblackpop <- transmute(blackpop, GEOID, NAME, 'Black' = round(estimate/totalpop$estimate,3)*100)
Fhisppop <- transmute(hisppop, GEOID, NAME, 'Hispanic' = round(estimate/totalpop$estimate,3)*100)
Fhighschool <- transmute(highschool, GEOID, NAME, 'HS' = round(estimate/totalpop$estimate,3)*100)
Fhouserent <- transmute(houserent, GEOID, NAME, 'Rent' = round(estimate/totalpop$estimate,3)*100)
Fhouse50_ <- transmute(totalhouseyear, GEOID, NAME, '1950' = round((house39_$estimate+house40_49$estimate)/estimate * 100,1))
Fhouse60_ <- transmute(totalhouseyear, GEOID, NAME, '1960' = round((house39_$estimate+house40_49$estimate+house50_59$estimate)/estimate * 100,1))
Fmedincome <- transmute(medincome, GEOID, NAME, 'Income' = estimate)
Fhousebuilt <- transmute(housebuilt, GEOID, NAME, 'YearBuilt' = estimate)
Fpoverty <- transmute(poverty, GEOID, NAME, 'Poverty' = round(estimate/totalpop$estimate,3)*100)

######################
# CREATE FINAL TABLE #
######################
final <- transmute(blackpop, GEOID, NAME, 'Black Population' = Fblackpop$'Black',
                   'Hispanic Population' = Fhisppop$'Hispanic',
                   'High School Attainment Population' = Fhighschool$'HS',
                   'Percent Renter-Occupied Housing' = Fhouserent$'Rent',
                   'Percentage of House Built Before 1950' = Fhouse50_$'1950',
                   'Percentage of House Built Before 1960' = Fhouse60_$'1960',
                   'Median Income' = Fmedincome$'Income',
                   'Median Year House was Built' = Fhousebuilt$'YearBuilt',
                   'Percentage Below Poverty Line' = Fpoverty$'Poverty'
)
