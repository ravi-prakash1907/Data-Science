# 21/01/2020 to yesterday

# Setting the working directory
setwd("/home/ravi/Documents/Data-Science/Project/My Work/")


## replace new time series files first, then run following command -----> 'n your dataset is updated
check.Confirmed = read.csv("/home/ravi/Documents/Data-Science/John H. University/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
check.Recovered = read.csv("/home/ravi/Documents/Data-Science/John H. University/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
check.Deaths = read.csv("/home/ravi/Documents/Data-Science/John H. University/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

# loading
Confirmed <- read.csv("old/time_series_2019-ncov-Confirmed.csv")
Recovered <- read.csv("old/time_series_2019-ncov-Recovered.csv")
Deaths <- read.csv("old/time_series_2019-ncov-Deaths.csv")

# removing NAs
for (i in 1:nrow(Confirmed)) {
  for (j in 5:ncol(Confirmed)) {
    Confirmed[i,j] = ifelse(is.na(Confirmed[i, j]), 0, Confirmed[i,j])
  }
}

for (i in 1:nrow(Recovered)) {
  for (j in 5:ncol(Recovered)) {
    Recovered[i,j] = ifelse(is.na(Recovered[i, j]), 0, Recovered[i,j])
  }
}

for (i in 1:nrow(Deaths)) {
  for (j in 5:ncol(Deaths)) {
    Deaths[i,j] = ifelse(is.na(Deaths[i, j]), 0, Deaths[i,j])
  }
}

###########################
#View(Confirmed)
#View(Deaths)
#View(Recovered)


####################
#View(check.Confirmed)
#View(check.Deaths)
#View(check.Recovered)



####################################
# Appending new rows (country/state)
####################################
joiner <- function(newDF, oldDF) {
  
  get(newDF) -> dfNew
  get(oldDF) -> dfOld
  newName = "X1.21.20"
  #newName = colnames(dfOld[5])         # because it is "X1.21.20.22.00"
  newRows = nrow(dfNew) - nrow(dfOld) # no. of new rows
  for (i in 1:newRows) {
    dfOld <- rbind(dfOld[,], dfOld[nrow(dfOld),])
  }
  
  dfOld$Province.State <- dfNew$Province.State
  dfOld$Country.Region <- dfNew$Country.Region
  dfOld$Lat <- dfNew$Lat
  dfOld$Long <- dfNew$Long
  
  row.names(dfOld) <- NULL      # re-indexing
  
  
  dfNew <- cbind(dfNew[,1:4], dfOld[,5], dfNew[,5:ncol(dfNew)]) # 4-col, col(21/01/2020), rest col(till-date)
  
  temp <- c(colnames(dfNew[1:4]), newName, colnames(dfNew[6:ncol(dfNew)]))
  colnames(dfNew) <- temp
  
  
  return(dfNew)
}

###########################
###########################

check.Confirmed = joiner("check.Confirmed", "Confirmed")
check.Deaths = joiner("check.Deaths", "Deaths")
check.Recovered = joiner("check.Recovered", "Recovered")

###############################

#View(check.Confirmed)
#View(check.Deaths)
#View(check.Recovered)

###############################
str(check.Confirmed)


# replacing blank in states
for (i in 1:length(levels(check.Confirmed$Province.State))) {
  if(levels(check.Confirmed$Province.State)[i]=="") {
    levels(check.Confirmed$Province.State)[i] = "Others" }
}

for (i in 1:length(levels(check.Deaths$Province.State))) {
  if(levels(check.Deaths$Province.State)[i]=="") {
    levels(check.Deaths$Province.State)[i] = "Others" }
}

for (i in 1:length(levels(check.Recovered$Province.State))) {
  if(levels(check.Recovered$Province.State)[i]=="") {
    levels(check.Recovered$Province.State)[i] = "Others" }
}
#####################

# renaming countries etc...
for (i in 1:length(levels(check.Confirmed$Country.Region))) {
  if(levels(check.Confirmed$Country.Region)[i]=="Others") {
    levels(check.Confirmed$Country.Region)[i] = "Diamond Princess cruise ship" }
  
  if(levels(check.Confirmed$Country.Region)[i]=="US") {
    levels(check.Confirmed$Country.Region)[i] = "United States" }
  
  if(levels(check.Confirmed$Country.Region)[i]=="UK") {
    levels(check.Confirmed$Country.Region)[i] = "United Kingdom" }
  
  if(levels(check.Confirmed$Country.Region)[i]=="Mainland China") {
    levels(check.Confirmed$Country.Region)[i] = "China" }
}

for (i in 1:length(levels(check.Deaths$Country.Region))) {
  if(levels(check.Deaths$Country.Region)[i]=="Others") {
    levels(check.Deaths$Country.Region)[i] = "Diamond Princess cruise ship" }
  
  if(levels(check.Deaths$Country.Region)[i]=="US") {
    levels(check.Deaths$Country.Region)[i] = "United States" }
  
  if(levels(check.Deaths$Country.Region)[i]=="UK") {
    levels(check.Deaths$Country.Region)[i] = "United Kingdom" }
  
  if(levels(check.Deaths$Country.Region)[i]=="Mainland China") {
    levels(check.Deaths$Country.Region)[i] = "China" }
}

for (i in 1:length(levels(check.Recovered$Country.Region))) {
  
  if(levels(check.Recovered$Country.Region)[i]=="Others") {
    levels(check.Recovered$Country.Region)[i] = "Diamond Princess cruise ship" }
  
  if(levels(check.Recovered$Country.Region)[i]=="US") {
    levels(check.Recovered$Country.Region)[i] = "United States" }
  
  if(levels(check.Recovered$Country.Region)[i]=="UK") {
    levels(check.Recovered$Country.Region)[i] = "United Kingdom" }
  
  if(levels(check.Recovered$Country.Region)[i]=="Mainland China") {
    levels(check.Recovered$Country.Region)[i] = "China" }
}

#####################

# creating new .csv file after cleaning the data
write.csv(check.Confirmed, file = "cleaned/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(check.Recovered, file = "cleaned/time_series_19-covid-Recovered.csv", row.names = FALSE)
write.csv(check.Deaths, file = "cleaned/time_series_19-covid-Deaths.csv", row.names = FALSE)



#############################################################

cleaned.Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
cleaned.Deaths <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
cleaned.Recovered <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

str(cleaned.Confirmed)


View(cleaned.Confirmed)
View(cleaned.Deaths)
View(cleaned.Recovered)





##########    TEST CODE     ###########






