v15 <- load_variables(2015, "acs5", cache=TRUE) # view all variables
medincome<- get_acs(geography="tract", variables = "B19013A_001E",year=2015,output="tidy", state="IN", county=141)
highschool<- get_acs(geography="tract", variables = "B15003_017E",year=2015,output="tidy", state="IN", county=141)
totalpop<- get_acs(geography="tract", variables = "B01003_001E",year=2015,output="tidy", state="IN", county=141)
houserent<- get_acs(geography="tract", variables = "B07013PR_003E",year=2015,output="tidy", state="IN", county=141)
housebuilt<- get_acs(geography="tract", variables = "B25035_001E",year=2015,output="tidy", state="IN", county=141)
blackpop<- get_acs(geography="tract", variables = "B02001_003E",year=2015,output="tidy", state="IN", county=141)
hisppop<- get_acs(geography="tract", variables = "B03001_003E",year=2015,output="tidy", state="IN", county=141)
poverty<- get_acs(geography="tract", variables = "B05010_002E",year=2015,output="tidy", state="IN", county=141)

#house
totalhouseyear <- get_acs(geography="tract", variables = "B25034_001E",year=2015,output="tidy", state="IN", county=141)
house50_59<- get_acs(geography="tract", variables = "B25034_009E",year=2015,output="tidy", state="IN", county=141)
house40_49<- get_acs(geography="tract", variables = "B25034_010E",year=2015,output="tidy", state="IN", county=141)
house39_ <- get_acs(geography="tract", variables = "B25034_011E",year=2015,output="tidy", state="IN", county=141)

# I want to remove the moe (Margin of Error) in each table
blackpop$moe <- NULL
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
totalpop$moe <-NULL

#At this point, I want to rename the columns in all the tables before merging so it's more organized
library(dplyr)
blackpop <- rename(blackpop, 'Black Variable'=variable, 'Black Population'=estimate)
highschool <- rename(highschool,'HS Variable'=variable, 'Population that Received HS Diploma'=estimate)
hisppop <- rename(hisppop, 'Hispanic Variable'=variable, 'Hispanic Population'=estimate)
house39_ <- rename(house39_, 'Pre-1939 Housing Variable'=variable, 'Number of Pre-1939 Housing'=estimate)

#Realized I don't need the table number variables either so removing those
blackpop$'Black Variable'<- NULL
highschool$'HS Variable' <- NULL
hisppop$'Hispanic Variable' <- NULL
house39_$'Pre-1939 Housing Variable' <- NULL

#Now I can continue the one's which I haven't changed the variable name
house40_49$variable <- NULL
house50_59$variable <- NULL
housebuilt$variable <- NULL
houserent$variable <- NULL
medincome$variable <- NULL
poverty$variable <- NULL
totalhouseyear$variable <- NULL
totalpop$variable <-NULL

#I will now continue changing the estimate variable into its proper name. This time, I only need to change one variable.
house40_49 <- rename(house40_49, 'Number of 1940-1949 Housing'=estimate)
house50_59 <- rename(house50_59, 'Number of 1950-1959 Housing'=estimate)
housebuilt <- rename(housebuilt, 'Median Year Housing Built'=estimate)
houserent <- rename(houserent, 'Householder Lived in Renter-Occupied Housing Units'=estimate)
medincome <- rename(medincome, 'Median Household Income'=estimate)
poverty <- rename(poverty, 'Number of Households Below the Poverty Line'=estimate)
totalhouseyear <- rename(totalhouseyear, 'Total Number of Houses'=estimate)
totalpop <- rename(totalpop, 'Total Population'=estimate)

#I want to create my pre-1960 & pre-1950 housing total. Need to combine all to one graph.
pre1960 <- merge(house39_, house40_49, by=c("GEOID", "NAME"))
pre1950final <- pre1960
pre1960final <- merge(pre1960, house50_59)

#Now I need to sum them into 1
total1950 <- rowSums(pre1950final[,c('Number of Pre-1939 Housing','Number of 1940-1949 Housing')]) 
totalpre1950 <- cbind(pre1950final, total1950)
#I can also delete the individual data in that table and keep only the totals
totalpre1950$`Number of Pre-1939 Housing` <- NULL
totalpre1950$`Number of 1940-1949 Housing` <- NULL
#Same process has to be done for pre-1960 houses now
total1960 <- rowSums(pre1960final[,c('Number of Pre-1939 Housing','Number of 1940-1949 Housing','Number of 1950-1959 Housing')]) 
totalpre1960 <- cbind(pre1960final, total1960)
totalpre1960$`Number of Pre-1939 Housing` <- NULL
totalpre1960$`Number of 1940-1949 Housing` <- NULL
totalpre1960$`Number of 1950-1959 Housing` <- NULL
#I was about to to rename the total1950 to Total Number of Houses Built Before 1950s but I can always do that at the end. Its a hassle to code with long names like that
#I need to convert everything to %, so let's start off with housing percentage
Percent1960 <- round(totalpre1960$total1960 / totalhouseyear$`Total Number of Houses` * 100, digits = 1)
totalpre1960 <- cbind(totalpre1960, Percent1960)
totalpre1960$'total1960' <- NULL # remove the total and only have percentage
#Now lets work on 1950 percentage
Percent1950 <- round(totalpre1950$total1950 / totalhouseyear$`Total Number of Houses` * 100, digits = 1)
totalpre1950 <- cbind(totalpre1950, Percent1950)
totalpre1950$'total1950'<-NULL
#Black Population Percentage
PercentBlack <- round(blackpop$'Black Population' / totalpop$`Total Population` * 100, digits = 1)
blackpop <- cbind(blackpop, PercentBlack) #Make a new value for percentage black NEXT TIME!!
blackpop$'Black Population'<-NULL
#High School Population Percentage
PercentHS <- round(highschool$'Population that Received HS Diploma' / totalpop$`Total Population` * 100, digits = 1)
PercHS <- cbind(highschool, PercentHS)
PercHS$'Population that Received HS Diploma'<-NULL

#Hispanic School Population Percentage
PercentHispanic <- round(hisppop$estimate / totalpop$`Total Population` * 100, digits = 1)
PercHispanic <- cbind(hisppop, PercentHispanic)
PercHispanic$'estimate'<-NULL

#Percent Poverty
PercPoverty <- round(poverty$'Number of Households Below the Poverty Line' / totalpop$`Total Population` * 100, digits = 1)
PercPoverty <- cbind(poverty, PercPoverty)
PercPoverty$'Number of Households Below the Poverty Line'<-NULL
  

#Merge all together
merged <- merge(PercHispanic,PercHS)
merged <- merge(merged,PercPoverty)
merged <- merge(merged, medincome)
merged <- merge(merged, housebuilt)
merged <- merge(merged, totalpre1950)
merged <- merge(merged, totalpre1960)