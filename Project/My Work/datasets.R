# 21/01/2020 to 19/02/2020



#------------------ VARIABLES -----------------#

####   Daily Data   ####
#   (cols = no. of days)      ---->     Tables = 3 each   (9)

#   One Country (rows = No. of State) =         One.Country.States.daily.<Confirmed/Deaths/Recovered>
#   One Country (aggrigate i.e. 1-row) =        One.Country.Aggregate.daily.<Confirmed/Deaths/Recovered>
#   All Country (rows = No. of Countries) =     All.Countries.daily.<Confirmed/Deaths/Recovered>


####   Till-Date Data   ####
#   (cols = Confirm, Deaths, Recovered)      ---->     Tables = 1 each    (3)

#   One Country (rows = No. of State) =         One.Country.States.summary
#   One Country (aggrigate i.e. 1-row) =        One.Country.Aggregate.summary
#   All Country (rows = No. of Countries) =     All.Countries.summary



####   Bulk Data   ####
#   TO BE CREATED

#----------------------------------------------#



#####  LIBRARIES  #####
# loading library for string operations
library(stringr)


#----------------------------------------------#


# Setting the working directory
setwd("/home/ravi/Documents/Data-Science/Project/My Work/")

# data files (csv)
Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
Deaths <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
Recovered <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

# Removing outlier i.e. Hubei (13th)
Confirmed = rbind(Confirmed[1:12,], Confirmed[14:nrow(Confirmed),])
Confirmed = Confirmed[order(Confirmed$Country.Region),]
row.names(Confirmed) <- NULL

Deaths = rbind(Deaths[1:12,], Deaths[14:nrow(Deaths),])
Deaths = Deaths[order(Deaths$Country.Region),]
row.names(Deaths) <- NULL

Recovered = rbind(Recovered[1:12,], Recovered[14:nrow(Recovered),])
Recovered = Recovered[order(Recovered$Country.Region),]
row.names(Recovered) <- NULL


#----------------------------------------------#


#######################
#####  functions  #####
#######################

###   processing daily data   ###

##################
country.spread.daily <- function(dfName, country) {
  country
  df <- get(dfName)
  df = df[which(str_detect(df$Country.Region, country)),]
  df = cbind(States = df[,1], Country = df[,2], df[,5:ncol(df)])
  row.names(df) <- NULL
  
  return (df)
}

##################
country.aggregate.daily  <-  function(dfName, country) {
  
  temp = country.spread.daily(dfName, country)            # all states' data of a country
  df = temp[1,]                                           # structure of required dataframe
  
  df[3:ncol(temp)] = apply(   temp[,3:ncol(temp)],
                            2,
                            sum
                        )                               # applying sum of all the states' values
  df = df[2:ncol(df)]                                   # removing column 'States'
  row.names(df) <- NULL
  
  return(df)
}

##################
countries.daily <-  function(dfName, cList = Countries) {
  
  n = length(cList)       # number of countries
  df = country.aggregate.daily(dfName, cList[1])
  
  for (i in 2:n) {
    temp = country.aggregate.daily(dfName, cList[i])
    df = rbind(df, temp)
  }
  row.names(df) <- NULL
  
  return(df)
}




###   processing data till date   ###
states.summarizer = function(cName) {
  
  C <- country.spread.daily("Confirmed", cName)
  D <- country.spread.daily("Deaths", cName)
  R <- country.spread.daily("Recovered", cName)
  
  allStates <- as.character(C$State)   # list of states in given country
  
  ###### overall data of the country (all states) ######
  df = data.frame(
    States = allStates,
    Confirmed = C[,ncol(C)],
    Deaths = D[,ncol(D)],
    Recovered = R[,ncol(R)]
  )
  
  return(df)
}



###########
country.summarizer = function(cName) {
  df = states.summarizer(cName)
  temp = df[1,]
  
  c = data.frame(Country = factor(cName))
  temp[2:ncol(df)] = apply(   df[,2:ncol(df)],
                              2,
                              sum
  )
  df = cbind(c, temp[2:ncol(temp)])               # replace state name with country
  
  return(df)
}

# country wise
total.summarizer <-  function(cList = Countries) {
  
  n = length(cList)       # number of countries
  df = country.summarizer(cList[1])
  
  for (i in 2:n) {
    temp = country.summarizer(cList[i])
    df = rbind(df, temp)
  }
  row.names(df) <- NULL
  
  return(df)
}



## finds the aggrigate data of any particular country (1 row o/p)
find.Country.Summery <- function(cName) { # cName is the name of the country
  Country = cName
  
  Confirmed.temp <- country.spread.daily("Confirmed", Country)
  Deaths.temp <- country.spread.daily("Deaths", Country)
  Recovered.temp <- country.spread.daily("Recovered", Country)
  
  allStates <- as.character(Confirmed.temp$States)
  allCountries <- as.character(Confirmed.temp$Country)
  
  summary.temp = data.frame(
    States = allStates,
    Country = allCountries,
    Confirmed = Confirmed.temp[,ncol(Confirmed.temp)],
    Deaths = Deaths.temp[,ncol(Deaths.temp)],
    Recovered = Recovered.temp[,ncol(Recovered.temp)]
  )
  
  t = apply(summary.temp[1:nrow(summary.temp),3:5], 2, sum)
  temp = summary.temp[1,]
  
  #### returning data of a single country
  return(temp)
}


#-----------------------------------------------------------------------#

# Some interesting facts
locations <- Deaths[,1:2]   # all states of different countries
Countries <- levels(locations$Country.Region)



#//////////////////////////////////////////////////////////////////////#



#################################################
#####    all about a particular Country     #####
#################################################

# just change the country name to get desired data
Country = "Mainland China"


###### country's (all states') daily data ######
One.Country.States.daily.Confirmed <- country.spread.daily("Confirmed", Country)
One.Country.States.daily.Deaths <- country.spread.daily("Deaths", Country)
One.Country.States.daily.Recovered <- country.spread.daily("Recovered", Country)

str(One.Country.States.daily.Confirmed)

View(One.Country.States.daily.Confirmed)
View(One.Country.States.daily.Deaths)
View(One.Country.States.daily.Recovered)

################################################
One.Country.Aggregate.daily.Confirmed <- country.aggregate.daily("Confirmed", Country)
One.Country.Aggregate.daily.Deaths <- country.aggregate.daily("Deaths", Country)
One.Country.Aggregate.daily.Recovered <- country.aggregate.daily("Recovered", Country)

str(One.Country.Aggregate.daily.Confirmed)

View(One.Country.Aggregate.daily.Confirmed)
View(One.Country.Aggregate.daily.Deaths)
View(One.Country.Aggregate.daily.Recovered)

################################################
All.Countries.daily.Confirmed <- countries.daily("Confirmed")
All.Countries.daily.Deaths <- countries.daily("Deaths")
All.Countries.daily.daily.Recovered <- countries.daily("Recovered")

str(All.Countries.daily.Confirmed)

View(All.Countries.daily.Confirmed)
View(All.Countries.daily.Deaths)
View(All.Countries.daily.Recovered)

###### overall data of the country (all states) ######
One.Country.States.summary = states.summarizer(Country)
One.Country.Aggregate.summary = country.summarizer(Country)
All.Countries.summary = total.summarizer(Countries)

str(One.Country.States.summary)

View(One.Country.States.summary)
View(One.Country.Aggregate.summary)
View(All.Countries.summary)




#//////////////////////////////////////////////////////////#



##########################################
#####    about all the Countries     #####
##########################################

allStates <- as.character(Confirmed$Province.State)
allCountries <- as.character(Confirmed$Country.Region)

####### all states of all the countries #######
bulk.summary = data.frame(
    States = allStates,
    Country = allCountries,
    Confirmed = Confirmed[,ncol(Confirmed)],
    Deaths = Deaths[,ncol(Deaths)],
    Recovered = Recovered[,ncol(Recovered)]
  )
View(bulk.summary)
str(bulk.summary)




#//////////////////////////////////////////////////////////#




###### overall data of all countries ######
Countries.summary = bulk.summary[-c(1:nrow(bulk.summary)),]
country.summarizer(Country)

# countries' list
countries = levels(Confirmed$Country.Region)


###### all countries' daily data ######
Country = "Mainland China"

Confirmed.temp <- country.spread.daily("Confirmed", Country)
Deaths.temp <- country.spread.daily("Deaths", Country)
Recovered.temp <- country.spread.daily("Recovered", Country)

## clculating the aggrigate data of all the countries (irrespactive of the states, in them)
for (i in 1:length(countries)) {
  # temp has data total for country iteratively
  temp = find.Country.Summery(countries[i])
  x = Countries.summary
  Countries.summary = rbind(x, temp)
}
# removing States as it's unnecessary
Countries.summary = Countries.summary[,2:ncol(Countries.summary)]



#////////////////////////////////////////////////////////////////////////#



##########################################
#####    about all the Countries     #####
##########################################

allStates <- as.character(Confirmed$Province.State)
allCountries <- as.character(Confirmed$Country.Region)

####### all states of all the countries #######
bulk.summary = data.frame(
  States = allStates,
  Country = allCountries,
  Confirmed = Confirmed[,ncol(Confirmed)],
  Deaths = Deaths[,ncol(Deaths)],
  Recovered = Recovered[,ncol(Recovered)]
)
View(bulk.summary)
str(bulk.summary)






#--------------------------------------------------------------------#





#################################
###   Writing to Data File    ###
#################################


write.csv(Confirmed, file = "ready_to_use/COVID-19/Confirmed.csv")
write.csv(Deaths, file = "ready_to_use/COVID-19/Deaths.csv")
write.csv(Recovered, file = "ready_to_use/COVID-19/Recovered.csv")

write.csv(bulk.summary, file = "ready_to_use/COVID-19/bulk_summary.csv")

#   12 files  ---->  daily(3x3) + overall(3) -> (9+3)   ==>   12
write.csv(check.Confirmed, file = "ready_to_use/COVID-19/One_Country_States_daily_Confirmed.csv", row.names = FALSE)
write.csv(check.Recovered, file = "ready_to_use/COVID-19/One_Country_States_daily_Deaths.csv", row.names = FALSE)
write.csv(check.Deaths, file = "ready_to_use/COVID-19/One_Country_States_daily_Recovered.csv", row.names = FALSE)

write.csv(check.Confirmed, file = "ready_to_use/COVID-19/One_Country_Aggregate_daily_Confirmed.csv", row.names = FALSE)
write.csv(check.Recovered, file = "ready_to_use/COVID-19/One_Country_Aggregate_daily_Deaths.csv", row.names = FALSE)
write.csv(check.Deaths, file = "ready_to_use/COVID-19/One_Country_Aggregate_daily_Recovered.csv", row.names = FALSE)

write.csv(check.Confirmed, file = "ready_to_use/COVID-19/All_Countries_daily_Confirmed.csv", row.names = FALSE)
write.csv(check.Recovered, file = "ready_to_use/COVID-19/All_Countries_daily_Deaths.csv", row.names = FALSE)
write.csv(check.Deaths, file = "ready_to_use/COVID-19/All_Countries_daily_Recovered.csv", row.names = FALSE)



write.csv(check.Confirmed, file = "ready_to_use/COVID-19/One_Country_States_summary.csv", row.names = FALSE)
write.csv(check.Recovered, file = "ready_to_use/COVID-19/One_Country_Aggregate_summary.csv", row.names = FALSE)
write.csv(check.Deaths, file = "ready_to_use/COVID-19/All_Countries_summary.csv", row.names = FALSE)


