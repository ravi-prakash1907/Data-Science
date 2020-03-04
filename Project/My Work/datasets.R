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

### data files (csv) ###

# Main
Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
Deaths <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
Recovered <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

# sorting Countrywise
Confirmed = Confirmed[order(Confirmed$Country.Region),]
Deaths = Deaths[order(Deaths$Country.Region),]
Recovered = Recovered[order(Recovered$Country.Region),]

row.names(Confirmed) <- NULL
row.names(Deaths) <- NULL
row.names(Recovered) <- NULL
#####################


# Hubei
Hubei.Confirmed = read.csv("cleaned/Hubei/time_series_19-covid-Confirmed.csv")
Hubei.Deaths = read.csv("cleaned/Hubei/time_series_19-covid-Recovered.csv")
Hubei.Recovered = read.csv("cleaned/Hubei/time_series_19-covid-Deaths.csv")

# Cruise
Diamond.Princess.Confirmed = read.csv("cleaned/Diamond-Princess/time_series_19-covid-Confirmed.csv")
Diamond.Princess.Deaths = read.csv("cleaned/Diamond-Princess/time_series_19-covid-Recovered.csv")
Diamond.Princess.Recovered = read.csv("cleaned/Diamond-Princess/time_series_19-covid-Deaths.csv")


#View(Confirmed)
#View(Deaths)
#View(Recovered)

#View(Hubei.Confirmed)
#View(Hubei.Deaths)
#View(Hubei.Recovered)

#View(Diamond.Princess.Confirmed)
#View(Diamond.Princess.Deaths)
#View(Diamond.Princess.Recovered)


#----------------------------------------------#


# Removing outlier i.e. Hubei
Hubei.Confirmed <- cbind(Hubei.Confirmed[,1], Hubei.Confirmed[,5:ncol(Hubei.Confirmed)])
Hubei.Deaths <- cbind(Hubei.Deaths[,1], Hubei.Deaths[,5:ncol(Hubei.Deaths)])
Hubei.Recovered <- cbind(Hubei.Recovered[,1], Hubei.Recovered[,5:ncol(Hubei.Recovered)])

names <- c("State", colnames(Hubei.Confirmed[2:ncol(Hubei.Confirmed)]))
colnames(Hubei.Confirmed) <- names
colnames(Hubei.Deaths) <- names
colnames(Hubei.Recovered) <- names
#----------------------------------------------#

# Removing outlier i.e. Diamond Princess
Diamond.Princess.Confirmed <- cbind(Diamond.Princess.Confirmed[,1], Diamond.Princess.Confirmed[,5:ncol(Diamond.Princess.Confirmed)])
Diamond.Princess.Deaths <- cbind(Diamond.Princess.Deaths[,1], Diamond.Princess.Deaths[,5:ncol(Diamond.Princess.Deaths)])
Diamond.Princess.Recovered <- cbind(Diamond.Princess.Recovered[,1], Diamond.Princess.Recovered[,5:ncol(Diamond.Princess.Recovered)])

names <- c("Loaction", colnames(Diamond.Princess.Confirmed[2:ncol(Diamond.Princess.Confirmed)]))
colnames(Diamond.Princess.Confirmed) <- names
colnames(Diamond.Princess.Deaths) <- names
colnames(Diamond.Princess.Recovered) <- names
#----------------------------------------------#

# Some interesting facts
Countries = Countries[which(str_detect(Countries, "Diamond Princess cruise ship", negate = T))]


#######################
#####  functions  #####
#######################

###   processing daily data   ###

##################
country.spread.daily <- function(dfName, country) {
  df <- get(dfName)
  df = df[which(str_detect(df$Country.Region, country)),]
  df = cbind(States = df[,1], Country = df[,2], df[,5:ncol(df)])
  row.names(df) <- NULL
  
  return (df)
}


##################
country.aggregate.daily  <-  function(dfName, country) {
  
  temp = country.spread.daily(dfName, country)            # all states' data of a country
  df = temp[-(1:nrow(temp)),]                             # structure of required dataframe
  
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
  
  temp = country.aggregate.daily(dfName, cList[1])
  df = temp[-(1:nrow(temp)),] 
  View(df)
  
  for (i in 1:n) {
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


#date wise country data
countries.daily.bulk.summary = function(cList) {
  
  # structure of resulting dataset (initially blank)
  df <- data.frame(
    Country = NULL,
    Day = NULL,           # day no.
    Date = NULL,
    Confirmed = NULL,
    Deaths = NULL,
    Recovered = NULL
  )
  
  # calculating all countries' data (date wise) through iteration
  for(i in cList) {
    this.one.confirmed = country.aggregate.daily("Confirmed", i)
    this.one.deaths = country.aggregate.daily("Deaths", i)
    this.one.recovered = country.aggregate.daily("Recovered", i)
    
    times = ncol(this.one.confirmed)-1      # no. of days
    day = 1:times
    d = as.Date("20-01-2020", format(c("%d-%m-%Y")))
    
    date = as.character((day + d), format(c("%d-%m-%Y")))      # its lenngth is equal to --> no. of days
    date = factor(c(date), levels = date)
    
    
    confirmed = as.character(this.one.confirmed[1,2:ncol(this.one.confirmed)])
    
    deaths = as.character(this.one.deaths[1,2:ncol(this.one.deaths)])
    
    recovered = as.character(this.one.recovered[1,2:ncol(this.one.recovered)])
    
    dataset <- data.frame(
      Country = rep(i, times),
      Day = factor(c(1:length(date)), levels = 1:length(date)),
      Date = date,
      Confirmed = confirmed,
      Deaths = deaths,
      Recovered = recovered
    )
    
    # joining this country
    df = rbind(df, dataset)
  }
  
  
  return(df)
}


#date wise WORLD data
world.daily.bulk.summary = function(dfC, dfD, dfR) {
  
  this.one.confirmed = find.aggrigate(dfC, "World")
  this.one.deaths = find.aggrigate(dfD, "World")
  this.one.recovered = find.aggrigate(dfR, "World")
  
  times = ncol(this.one.confirmed)-1      # no. of days
  day = 1:times
  d = as.Date("20-01-2020", format(c("%d-%m-%Y")))
  
  date = as.character((day + d), format(c("%d-%m-%Y")))      # its lenngth is equal to --> no. of days
  date = factor(c(date), levels = date)
  
  
  confirmed = as.character(this.one.confirmed[1,2:ncol(this.one.confirmed)])
  
  deaths = as.character(this.one.deaths[1,2:ncol(this.one.deaths)])
  
  recovered = as.character(this.one.recovered[1,2:ncol(this.one.recovered)])
  
  df <- data.frame(
    Location = rep("World", times),
    Day = factor(c(1:length(date)), levels = 1:length(date)),
    Date = date,
    Confirmed = confirmed,
    Deaths = deaths,
    Recovered = recovered
  )
  
  
  return(df)
}

######    Datewise
datewise <- function(Country, yesORno, cList = countries) {
  df = countries.daily.bulk.summary(countries)
  df = df[ which(str_detect(df$Country, Country, negate = yesORno)), ]
  
  return(df)
}


##  adder
find.aggrigate <- function(df1, colName) {
  first <- get(df1)
  temp = first[1,]
  
  c = data.frame(Location = colName)
  temp[2:ncol(first)] = apply(   first[,2:ncol(first)],
                              2,
                              sum
  )
  df = cbind(c, temp[2:ncol(temp)])  
  
  return(df)
}


####
outlier.datewise <- function(Name, df1, df2, df3) { # hubei, dim. pr. etc..
  get(df1) -> dfC
  get(df2) -> dfD
  get(df3) -> dfR
  
  day = 1:(ncol(dfC)-1)
  d = as.Date("20-01-2020", format(c("%d-%m-%Y")))
  
  date = as.character((day + d), format(c("%d-%m-%Y")))      # its lenngth is equal to --> no. of days
  date = factor(c(date), levels = date)
  
  confirmed = as.character(dfC[1,2:ncol(dfC)])
  deaths = as.character(dfD[1,2:ncol(dfD)])
  recovered = as.character(dfR[1,2:ncol(dfR)])
  
  df <- data.frame(
    State = rep(Name, (ncol(dfC)-1)),
    Day = factor(c(1:length(date)), levels = 1:length(date)),
    Date = date,
    Confirmed = confirmed,
    Deaths = deaths,
    Recovered = recovered
  )
  
  return(df)
}

#-----------------------------------------------------------------------#





#################################################
#####    all about a particular Country     #####
#################################################

# just change the country name to get desired data
Country = "China"


###### country's (all states') daily data ######
One.Country.States.daily.Confirmed <- country.spread.daily("Confirmed", Country)
One.Country.States.daily.Deaths <- country.spread.daily("Deaths", Country)
One.Country.States.daily.Recovered <- country.spread.daily("Recovered", Country)

View(One.Country.States.daily.Confirmed)
#View(One.Country.States.daily.Deaths)
#View(One.Country.States.daily.Recovered)

################################################
One.Country.Aggregate.daily.Confirmed <- country.aggregate.daily("Confirmed", Country)
One.Country.Aggregate.daily.Deaths <- country.aggregate.daily("Deaths", Country)
One.Country.Aggregate.daily.Recovered <- country.aggregate.daily("Recovered", Country)

View(One.Country.Aggregate.daily.Confirmed)
#View(One.Country.Aggregate.daily.Deaths)
#View(One.Country.Aggregate.daily.Recovered)

########  All states of all Countries (Spread of all countries)  #########
All.Countries.daily.Confirmed <- countries.daily("Confirmed")
All.Countries.daily.Deaths <- countries.daily("Deaths")
All.Countries.daily.Recovered <- countries.daily("Recovered")
#str(All.Countries.daily.Confirmed)
All.Countries.daily.Confirmed = na.omit(All.Countries.daily.Confirmed)
All.Countries.daily.Deaths = na.omit(All.Countries.daily.Deaths)
All.Countries.daily.Recovered = na.omit(All.Countries.daily.Recovered)
# removing Cruise row --> NA
row.names(All.Countries.daily.Confirmed) <- NULL
row.names(All.Countries.daily.Deaths) <- NULL
row.names(All.Countries.daily.Recovered) <- NULL

View(All.Countries.daily.Confirmed)
#View(All.Countries.daily.Deaths)
#View(All.Countries.daily.Recovered)

###### overall data of the country (all states) ######
One.Country.States.summary = states.summarizer(Country)
One.Country.Aggregate.summary = country.summarizer(Country)
All.Countries.summary = total.summarizer(Countries)

Hubei.summary = data.frame(
  State = "Hubei",
  Confirmed = Hubei.Confirmed[,ncol(Hubei.Confirmed)],
  Deaths = Hubei.Deaths[,ncol(Hubei.Deaths)],
  Recovered = Hubei.Recovered[,ncol(Hubei.Recovered)]
)
Diamond.Princess.summary = data.frame(
  Location = "Diamond Princess",
  Confirmed = Diamond.Princess.Confirmed[,ncol(Diamond.Princess.Confirmed)],
  Deaths = Diamond.Princess.Deaths[,ncol(Diamond.Princess.Deaths)],
  Recovered = Diamond.Princess.Recovered[,ncol(Diamond.Princess.Recovered)]
)

#View(One.Country.States.summary)
#View(One.Country.Aggregate.summary)
#View(All.Countries.summary)

#View(Hubei.summary)
#View(Diamond.Princess.summary)

##########################################
#########    about the world     #########
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
#View(bulk.summary)



###########################
##### on daily basis ######
###########################

# date wise summary of all the countries
dataset.countryWise = countries.daily.bulk.summary(countries)
dataset.dateWise = dataset.countryWise[order(dataset.countryWise$Date),]

#View(dataset)
#View(dataset.dateWise)

#--------------------------------------------------------------------#
One.country.dataset.dateWise = datewise(Country, FALSE, countries)
Rest.world.dataset.dateWise = datewise(Country, TRUE, countries)
#View(One.country.dataset.dateWise)
#View(Rest.world.dataset.dateWise)

############
Hubei.datewise = outlier.datewise("Hubei", "Hubei.Confirmed", "Hubei.Deaths", "Hubei.Recovered")
Diamond.Princess.datewise = outlier.datewise("Diamond Princess", "Diamond.Princess.Confirmed", "Diamond.Princess.Deaths", "Diamond.Princess.Recovered")



####################################################################################################################
####################################################################################################################





#   NOTHING TO BE ALTERED BELOW!!!


#################################
###   Writing to Data File    ###
#################################
#############  41  ##############

cName = "China"

################      HUBEI     #################
write.csv(Hubei.Confirmed, file = "ready_to_use/COVID-19/Hubei/Hubei_daily_Confirmed.csv", row.names = FALSE)
write.csv(Hubei.Deaths, file = "ready_to_use/COVID-19/Hubei/Hubei_daily_Deaths.csv", row.names = FALSE)
write.csv(Hubei.Recovered, file = "ready_to_use/COVID-19/Hubei/Hubei_daily_Recovered.csv", row.names = FALSE)

write.csv(Hubei.summary, file = "ready_to_use/COVID-19/Hubei/Hubei_summary.csv", row.names = FALSE)
write.csv(Hubei.datewise, file = "ready_to_use/COVID-19/Hubei/Hubei_dataset_dateWise_summary.csv", row.names = FALSE)

################      DIAMOND PRINCESS     #################
write.csv(Diamond.Princess.Confirmed, file = "ready_to_use/COVID-19/Cruise/Diamond_Princess_daily_Confirmed.csv", row.names = FALSE)
write.csv(Diamond.Princess.Deaths, file = "ready_to_use/COVID-19/Cruise/Diamond_Princess_daily_Deaths.csv", row.names = FALSE)
write.csv(Diamond.Princess.Recovered, file = "ready_to_use/COVID-19/Cruise/Diamond_Princess_daily_Recovered.csv", row.names = FALSE)

write.csv(Diamond.Princess.summary, file = "ready_to_use/COVID-19/Cruise/Diamond_Princess_summary.csv", row.names = FALSE)
write.csv(Diamond.Princess.datewise, file = "ready_to_use/COVID-19/Cruise/Diamond_Princess_dataset_dateWise_summary.csv", row.names = FALSE)

################      CHINA     #################
write.csv(country.spread.daily("Confirmed", cName), file = "ready_to_use/COVID-19/China/China_States_daily_Confirmed.csv", row.names = FALSE)
write.csv(country.spread.daily("Deaths", cName), file = "ready_to_use/COVID-19/China/China_States_daily_Deaths.csv", row.names = FALSE)
write.csv(country.spread.daily("Recovered", cName), file = "ready_to_use/COVID-19/China/China_States_daily_Recovered.csv", row.names = FALSE)

write.csv(country.aggregate.daily("Confirmed", cName), file = "ready_to_use/COVID-19/China/China_Aggregate_daily_Confirmed.csv", row.names = FALSE)
write.csv(country.aggregate.daily("Deaths", cName), file = "ready_to_use/COVID-19/China/China_Aggregate_daily_Deaths.csv", row.names = FALSE)
write.csv(country.aggregate.daily("Recovered", cName), file = "ready_to_use/COVID-19/China/China_Aggregate_daily_Recovered.csv", row.names = FALSE)

write.csv(states.summarizer(cName), file = "ready_to_use/COVID-19/China/China_States_summary.csv", row.names = FALSE)
write.csv(country.summarizer(cName), file = "ready_to_use/COVID-19/China/China_Aggregate_summary.csv", row.names = FALSE)

write.csv(datewise(cName, FALSE, countries), file = "ready_to_use/COVID-19/China/China_dataset_dateWise_summary.csv", row.names = FALSE)


################      WORLD     #################
Non.China.Countries.daily.Confirmed = All.Countries.daily.Confirmed[ which(str_detect(All.Countries.daily.Confirmed$Country, cName, negate = T)),]
Non.China.Countries.daily.Deaths = All.Countries.daily.Deaths[ which(str_detect(All.Countries.daily.Deaths$Country, cName, negate = T)),]
Non.China.Countries.daily.Recovered = All.Countries.daily.Recovered[ which(str_detect(All.Countries.daily.Recovered$Country, cName, negate = T)),]
Non.China.Countries.summary = All.Countries.summary[ which(str_detect(All.Countries.summary$Country, cName, negate = T)),]
Non.China.datewise = world.daily.bulk.summary("Non.China.Countries.daily.Confirmed", "Non.China.Countries.daily.Deaths", "Non.China.Countries.daily.Recovered")

write.csv(Non.China.Countries.daily.Confirmed, file = "ready_to_use/COVID-19/World/World_Countries_daily_Confirmed.csv", row.names = FALSE)
write.csv(Non.China.Countries.daily.Deaths, file = "ready_to_use/COVID-19/World/World_Countries_daily_Deaths.csv", row.names = FALSE)
write.csv(Non.China.Countries.daily.Recovered, file = "ready_to_use/COVID-19/World/World_Countries_daily_Recovered.csv", row.names = FALSE)

write.csv(find.aggrigate("Non.China.Countries.daily.Confirmed", "World"), file = "ready_to_use/COVID-19/World/World_Aggregate_daily_Confirmed.csv", row.names = FALSE)
write.csv(find.aggrigate("Non.China.Countries.daily.Deaths", "World"), file = "ready_to_use/COVID-19/World/World_Aggregate_daily_Deaths.csv", row.names = FALSE)
write.csv(find.aggrigate("Non.China.Countries.daily.Recovered", "World"), file = "ready_to_use/COVID-19/World/World_Aggregate_daily_Recovered.csv", row.names = FALSE)

write.csv(Non.China.Countries.summary, file = "ready_to_use/COVID-19/World/World_Countries_summary.csv", row.names = FALSE)
write.csv(find.aggrigate("Non.China.Countries.summary", "World"), file = "ready_to_use/COVID-19/World/World_Aggregate_summary.csv", row.names = FALSE)

write.csv(Non.China.datewise, file = "ready_to_use/COVID-19/World/World_dataset_dateWise_summary.csv", row.names = FALSE)



############################################



write.csv(Confirmed, file = "ready_to_use/COVID-19/Confirmed.csv", row.names = FALSE)
write.csv(Deaths, file = "ready_to_use/COVID-19/Deaths.csv", row.names = FALSE)
write.csv(Recovered, file = "ready_to_use/COVID-19/Recovered.csv", row.names = FALSE)

write.csv(bulk.summary, file = "ready_to_use/COVID-19/Mixed/bulk_summary.csv")

####
write.csv(One.Country.States.daily.Confirmed, file = "ready_to_use/COVID-19/One_Country_States_daily_Confirmed.csv", row.names = FALSE)
write.csv(One.Country.States.daily.Recovered, file = "ready_to_use/COVID-19/One_Country_States_daily_Deaths.csv", row.names = FALSE)
write.csv(One.Country.States.daily.Deaths, file = "ready_to_use/COVID-19/One_Country_States_daily_Recovered.csv", row.names = FALSE)
write.csv(One.Country.Aggregate.daily.Confirmed, file = "ready_to_use/COVID-19/One_Country_Aggregate_daily_Confirmed.csv", row.names = FALSE)
write.csv(One.Country.Aggregate.daily.Recovered, file = "ready_to_use/COVID-19/One_Country_Aggregate_daily_Deaths.csv", row.names = FALSE)
write.csv(One.Country.Aggregate.daily.Deaths, file = "ready_to_use/COVID-19/One_Country_Aggregate_daily_Recovered.csv", row.names = FALSE)
####

write.csv(All.Countries.daily.Confirmed, file = "ready_to_use/COVID-19/Mixed/All_Countries_daily_Confirmed.csv", row.names = FALSE)
write.csv(All.Countries.daily.Recovered, file = "ready_to_use/COVID-19/Mixed/All_Countries_daily_Deaths.csv", row.names = FALSE)
write.csv(All.Countries.daily.Deaths, file = "ready_to_use/COVID-19/Mixed/All_Countries_daily_Recovered.csv", row.names = FALSE)


write.csv(One.Country.States.summary, file = "ready_to_use/COVID-19/One_Country_States_summary.csv", row.names = FALSE)
write.csv(One.Country.Aggregate.summary, file = "ready_to_use/COVID-19/One_Country_Aggregate_summary.csv", row.names = FALSE)
write.csv(All.Countries.summary, file = "ready_to_use/COVID-19/Mixed/All_Countries_summary.csv", row.names = FALSE)


write.csv(dataset.countryWise, file = "ready_to_use/COVID-19/Mixed/countryWise_bulk_summary.csv", row.names = FALSE)
write.csv(dataset.dateWise, file = "ready_to_use/COVID-19/Mixed/dateWise_bulk_summary.csv", row.names = FALSE)


################################



#################################
########   Main Four    ########
###   (Hubei, China, World)   ###
#################################

# 3 daily(aggregate) confirmed/deaths/recovered   ->> 3rows each
a = Hubei.Confirmed
b = country.aggregate.daily("Confirmed", "China")
c = find.aggrigate("Non.China.Countries.daily.Confirmed", "World")
d = Diamond.Princess.Confirmed
colnames(a) <- colnames(c)
colnames(b) <- colnames(c)
colnames(d) <- colnames(c)

Four.daily.Confirmed <- rbind(a, b, c, d)
##
a = Hubei.Deaths
b = country.aggregate.daily("Deaths", "China")
c = find.aggrigate("Non.China.Countries.daily.Deaths", "World")
d = Diamond.Princess.Deaths
colnames(a) <- colnames(c)
colnames(b) <- colnames(c)
colnames(d) <- colnames(c)

Four.daily.Deaths <- rbind(a, b, c, d)
##
a = Hubei.Recovered
b = country.aggregate.daily("Recovered", "China")
c = find.aggrigate("Non.China.Countries.daily.Recovered", "World")
d = Diamond.Princess.Recovered
colnames(a) <- colnames(c)
colnames(b) <- colnames(c)
colnames(d) <- colnames(c)

Four.daily.Recovered <- rbind(a, b, c, d)


# 3 summary    ->> 3rows,4cols
a = Hubei.summary
b = country.summarizer("China")
c = find.aggrigate("Non.China.Countries.summary", "World")
d = Diamond.Princess.summary
colnames(a) <- colnames(c)
colnames(b) <- colnames(c)
colnames(d) <- colnames(c)

Four.Summary <- rbind(a, b, c, d)



# 3 dataset datewise 3XnoOfDays -> rows, 6cols(name, dayno., date, confirm, death, recover)
a = Hubei.datewise
b = datewise("China", FALSE, countries)
c = Non.China.datewise
d = Diamond.Princess.datewise
colnames(a) <- colnames(c)
colnames(b) <- colnames(c)
colnames(d) <- colnames(c)

Four.dataset.dateWise <- rbind(a, b, c, d)


# writting (5)
write.csv(Four.daily.Confirmed, file = "ready_to_use/COVID-19/FOUR/Four_daily_Confirmed.csv", row.names = FALSE)
write.csv(Four.daily.Deaths, file = "ready_to_use/COVID-19/FOUR/Four_daily_Deaths.csv", row.names = FALSE)
write.csv(Four.daily.Recovered, file = "ready_to_use/COVID-19/FOUR/Four_daily_Recovered.csv", row.names = FALSE)

write.csv(Four.Summary, file = "ready_to_use/COVID-19/FOUR/Four_Summary.csv", row.names = FALSE)
write.csv(Four.dataset.dateWise, file = "ready_to_use/COVID-19/FOUR/Four_dataset_dateWise.csv", row.names = FALSE)




#--------------------  ENDS  --------------------#
