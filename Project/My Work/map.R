# visualization on map
library(countrycode)
library(stringr)

# Setting the working directory
setwd("/home/ravi/Documents/Data-Science/Project/My Work/")


## TO PLOT COUNTRIES
#generating List of countries
Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
Countries <- levels(Confirmed$Country.Region)
# removing Cruise from country list
Countries = Countries[which(str_detect(Countries, "Diamond Princess cruise ship", negate = T))]

country.code = countrycode(
  sourcevar = Countries,
  origin = "country.name",
  destination = "iso2c",
  nomatch = NULL
)

country.code   # this can be easily plotted on graph



####################
#  TO PLOT STATES (of a country)




