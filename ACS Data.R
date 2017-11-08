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
highschool <- get_acs(geography="tract", variables = "B06009_003E",year=2015,output="tidy", state="IN", county=141)
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

#########################
# REMOVE MOE & VARIABLE #
#########################
blackpop$moe <- NULL
highschool$moe <- NULL
hisppop$moe <- NULL
house39_$moe <- NULL
house40_49$moe <- NULL
house50_59$moe <- NULL
housebuilt$moe <- NULL
houserent$moe <- NULL
medincome$moe <- NULL
poverty$moe <- NULL
totalhouseyear$moe <- NULL
totalpop$moe <- NULL
blackpop$variable <- NULL
highschool$variable <- NULL
hisppop$variable <- NULL
house39_$variable <- NULL
house40_49$variable <- NULL
house50_59$variable <- NULL
housebuilt$variable <- NULL
houserent$variable <- NULL
medincome$variable <- NULL
poverty$variable <- NULL
totalhouseyear$variable <- NULL
totalpop$variable <-NULL

####################################
# CHANGE COLNAMES INTO PROPER NAME #
####################################
blackpop <- dplyr::rename(blackpop, 'Black Population'=estimate)
highschool <- dplyr::rename(highschool, 'Population that Received HS Diploma'=estimate)
hisppop <- dplyr::rename(hisppop, 'Hispanic Population'=estimate)
house39_ <- dplyr::rename(house39_, 'Number of Pre-1939 Housing'=estimate)
house40_49 <- dplyr::rename(house40_49, 'Number of 1940-1949 Housing'=estimate)
house50_59 <- dplyr::rename(house50_59, 'Number of 1950-1959 Housing'=estimate)
housebuilt <- dplyr::rename(housebuilt, 'Median Year Housing Built'=estimate)
houserent <- dplyr::rename(houserent, 'Householder Lived in Renter-Occupied Housing Units'=estimate)
medincome <- dplyr::rename(medincome, 'Median Household Income'=estimate)
poverty <- dplyr::rename(poverty, 'Number of Households Below the Poverty Line'=estimate)
totalhouseyear <- dplyr::rename(totalhouseyear, 'Total Number of Houses'=estimate)
totalpop <- dplyr::rename(totalpop, 'Total Population'=estimate)

#######################################
# CREATE PRE-1960 and PRE-1950 TABLES #
#######################################
pre1960 <- merge(house39_, house40_49, by=c("GEOID", "NAME"))
pre1950final <- pre1960
pre1960final <- merge(pre1960, house50_59)
total1950 <- rowSums(pre1950final[,c('Number of Pre-1939 Housing','Number of 1940-1949 Housing')])  # I sum them rows into 1 total
totalpre1950 <- cbind(pre1950final, total1950)
totalpre1950$`Number of Pre-1939 Housing` <- NULL #I delete the individual data and keep the totals
totalpre1950$`Number of 1940-1949 Housing` <- NULL #I delete the individual data and keep the totals
total1960 <- rowSums(pre1960final[,c('Number of Pre-1939 Housing','Number of 1940-1949 Housing','Number of 1950-1959 Housing')]) 
totalpre1960 <- cbind(pre1960final, total1960)
totalpre1960$`Number of Pre-1939 Housing` <- NULL
totalpre1960$`Number of 1940-1949 Housing` <- NULL
totalpre1960$`Number of 1950-1959 Housing` <- NULL

#####################################
# CONVERT ALL TOTALS TO PERCENTAGES #
#####################################
###########################
# 1960 HOUSING PERCENTAGE #
###########################
Percent1960 <- round(totalpre1960$total1960 / totalhouseyear$`Total Number of Houses` * 100, digits = 1)
totalpre1960 <- cbind(totalpre1960, Percent1960)
totalpre1960$'total1960' <- NULL # remove the total and only have percentage

###########################
# 1950 HOUSING PERCENTAGE #
###########################
Percent1950 <- round(totalpre1950$total1950 / totalhouseyear$`Total Number of Houses` * 100, digits = 1)
totalpre1950 <- cbind(totalpre1950, Percent1950)
totalpre1950$'total1950'<-NULL

################
# HOUSING RENT #
################
PercentRent <- round(houserent$'Householder Lived in Renter-Occupied Housing Units' / totalpop$`Total Population` * 100, digits = 1)
PercRent <- cbind(houserent, PercentRent) #Make a new value for percentage black NEXT TIME!!
PercRent$'Householder Lived in Renter-Occupied Housing Units'<-NULL


###############################
# BLACK POPULATION PERCENTAGE #
###############################
PercentBlack <- round(blackpop$'Black Population' / totalpop$`Total Population` * 100, digits = 1)
PercBlack <- cbind(blackpop, PercentBlack) #Make a new value for percentage black NEXT TIME!!
PercBlack$'Black Population'<-NULL

#############################
# HS ATTAINEMENT PERCENTAGE #
#############################
PercentHS <- round(highschool$'Population that Received HS Diploma' / totalpop$`Total Population` * 100, digits = 1)
PercHS <- cbind(highschool, PercentHS)
PercHS$'Population that Received HS Diploma'<-NULL

##################################
# HISAPNIC POPULATION PERCENTAGE #
##################################
PercentHispanic <- round(hisppop$'Hispanic Population' / totalpop$`Total Population` * 100, digits = 1)
PercHispanic <- cbind(hisppop, PercentHispanic)
PercHispanic$'Hispanic Population'<-NULL

######################
# POVERTY PERCENTAGE #
######################
PercentPoverty <- round(poverty$'Number of Households Below the Poverty Line' / totalpop$`Total Population` * 100, digits = 1)
PercPoverty <- cbind(poverty, PercentPoverty)
PercPoverty$'Number of Households Below the Poverty Line'<-NULL

##################
# MERGE ALL DATA #
##################
merged <- merge(PercHispanic, PercBlack) 
merged <- merge(merged, PercHS)
merged <- merge(merged, PercPoverty)
merged <- merge(merged, PercRent)
merged <- merge(merged, medincome)
merged <- merge(merged, housebuilt)
merged <- merge(merged, totalpre1950)
merged <- merge(merged, totalpre1960)

# Dr. Sisk, to look at the final graph, go into the data and look for "merged". HS seems to be a bit
# low and I'm wondering if that only includes people who ONLY received a HS diploma. That would make more
# sense as I believe I got the right variable table from ACS. Please let me know if you have any questions
# about the coding. Also, I fixed the NA's in Renter-Occupied. I didn't have the right variable.

# WAYS TO IMPROVE MY CODING
# > Find a way to delete (NULL) columns (2+ more efficiently)
# > Find a way to merge the final data more efficiently

# TO-DO
# > Fix HS Educational Attainment
# > Some percent poverty is 0.0, so would like to figure out if nobody answered the survey there or what..
