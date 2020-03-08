# 21/01/2020 to yesterday

# Setting the working directory
setwd("/home/ravi/Documents/Data-Science/Project/My Work/")

#####  LIBRARIES  #####
# loading library for string operations
library(stringr)
library(RFmarkerDetector) # random forest  ---> for autoscale()


## replace new time series files first, then run following command -----> 'n your dataset is updated
check.Confirmed = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
check.Recovered = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
check.Deaths = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

# loading
Confirmed <- read.csv("old/time_series_2019-ncov-Confirmed.csv")
Recovered <- read.csv("old/time_series_2019-ncov-Recovered.csv")
Deaths <- read.csv("old/time_series_2019-ncov-Deaths.csv")

# removing NAs

# new files
for (i in 1:nrow(check.Confirmed)) {
  for (j in 5:ncol(check.Confirmed)) {
    check.Confirmed[i,j] = ifelse(is.na(check.Confirmed[i, j]), 0, check.Confirmed[i,j])
  }
}

for (i in 1:nrow(check.Recovered)) {
  for (j in 5:ncol(check.Recovered)) {
    check.Recovered[i,j] = ifelse(is.na(check.Recovered[i, j]), 0, check.Recovered[i,j])
  }
}

for (i in 1:nrow(check.Deaths)) {
  for (j in 5:ncol(check.Deaths)) {
    check.Deaths[i,j] = ifelse(is.na(check.Deaths[i, j]), 0, check.Deaths[i,j])
  }
}

# old files
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

#View(check.Confirmed)
#View(check.Deaths)
#View(check.Recovered)


###############################
#str(check.Confirmed)

## Closed cases (i.e. Recovered or Death cases)
cases.Closed = cbind(check.Confirmed[,1:4],  (check.Deaths[,5:ncol(check.Deaths)] + check.Recovered[,5:ncol(check.Recovered)]))
## Active cases 
cases.Active = cbind(check.Confirmed[,1:4],  (check.Confirmed[,5:ncol(check.Confirmed)] - cases.Closed[,5:ncol(cases.Closed)]))


# Removing outlier i.e. Diamond.Princess
Diamond.Princess.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Diamond Princess cruise ship", negate = F)), ]
check.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Diamond Princess cruise ship", negate = T)), ]

Diamond.Princess.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Diamond Princess cruise ship", negate = F)),]
check.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Diamond Princess cruise ship", negate = T)), ]

Diamond.Princess.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Diamond Princess cruise ship", negate = F)), ]
check.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Diamond Princess cruise ship", negate = T)), ]


## Rectifying Row sequences
row.names(check.Confirmed) <- NULL
row.names(check.Deaths) <- NULL
row.names(check.Recovered) <- NULL

###  When and Where COVID-19 ever.Affected / still.Affected  --->  excluding Diamond Princess
ever.Affected = check.Confirmed
# Unit scaling
for (i in row.names(ever.Affected)) {
  for (j in 5:ncol(ever.Affected)) {
    if(ever.Affected[i,j] != 0)
      ever.Affected[i,j] = 1
  }
}

still.Affected = check.Confirmed
# Unit scaling
for (i in row.names(still.Affected)) {
  for (j in 5:ncol(still.Affected)) {
    if(still.Affected[i,j] != 0)
      still.Affected[i,j] = 1
  }
}

#View(ever.Affected)
#View(still.Affected)


# Removing outlier i.e. Hubei
Hubei.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Hubei", negate = F)), ]
check.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Hubei", negate = T)), ]

Hubei.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Hubei", negate = F)),]
check.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Hubei", negate = T)), ]

Hubei.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Hubei", negate = F)), ]
check.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Hubei", negate = T)), ]



## Rectifying Row sequences
row.names(check.Confirmed) <- NULL
row.names(check.Deaths) <- NULL
row.names(check.Recovered) <- NULL

row.names(Diamond.Princess.Confirmed) <- NULL
row.names(Diamond.Princess.Deaths) <- NULL
row.names(Diamond.Princess.Recovered) <- NULL

row.names(Hubei.Confirmed) <- NULL
row.names(Hubei.Deaths) <- NULL
row.names(Hubei.Recovered) <- NULL


###


#----------------------------------------------#




# creating new .csv file after cleaning the data

## Diamond Princess
write.csv(Diamond.Princess.Confirmed, file = "cleaned/Diamond-Princess/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(Diamond.Princess.Deaths, file = "cleaned/Diamond-Princess/time_series_19-covid-Recovered.csv", row.names = FALSE)
write.csv(Diamond.Princess.Recovered, file = "cleaned/Diamond-Princess/time_series_19-covid-Deaths.csv", row.names = FALSE)

## Hubei
write.csv(Hubei.Confirmed, file = "cleaned/Hubei/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(Hubei.Deaths, file = "cleaned/Hubei/time_series_19-covid-Recovered.csv", row.names = FALSE)
write.csv(Hubei.Recovered, file = "cleaned/Hubei/time_series_19-covid-Deaths.csv", row.names = FALSE)


## Main files
write.csv(check.Confirmed, file = "cleaned/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(check.Recovered, file = "cleaned/time_series_19-covid-Recovered.csv", row.names = FALSE)
write.csv(check.Deaths, file = "cleaned/time_series_19-covid-Deaths.csv", row.names = FALSE)

###   For map plot & gif
write.csv(ever.Affected, file = "cleaned/ever.Affected.csv", row.names = FALSE)
write.csv(still.Affected, file = "cleaned/still.Affected.csv", row.names = FALSE)



#############################################################


cleaned.Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
cleaned.Deaths <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
cleaned.Recovered <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

cleaned.ever.Affected <- read.csv("cleaned/ever.Affected.csv")
cleaned.still.Affected <- read.csv("cleaned/still.Affected.csv")

#str(cleaned.Confirmed)


View(cleaned.Confirmed)
View(cleaned.Deaths)
View(cleaned.Recovered)

View(cleaned.ever.Affected)
View(cleaned.still.Affected)





##########    TEST CODE     ###########






