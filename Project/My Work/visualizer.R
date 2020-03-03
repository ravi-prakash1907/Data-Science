#####  LIBRARIES  #####

# Setting the working directory
setwd("/home/ravi/Documents/Data-Science/Project/My Work/")

#library(ggplot2)   # gg menns ->   grammer of graphics
library(tidyverse)  # includes ggplot2
library(lattice)

#------------------------------------------------------------------------#

### Loading files ###

Confirmed = read.csv("ready_to_use/COVID-19/Confirmed.csv")
Deaths = read.csv("ready_to_use/COVID-19/Deaths.csv")
Recovered = read.csv("ready_to_use/COVID-19/Recovered.csv")


countryWise.bulk.summary = read.csv("ready_to_use/COVID-19/countryWise_bulk_summary.csv")
# train on 80% dates, test on 20%
dateWise.bulk.summary = read.csv("ready_to_use/COVID-19/dateWise_bulk_summary.csv")


Four.daily.Confirmed = read.csv("ready_to_use/COVID-19/FOUR/Four_daily_Confirmed.csv")
Four.daily.Deaths = read.csv("ready_to_use/COVID-19/FOUR/Four_daily_Deaths.csv")
Four.daily.Recovered = read.csv("ready_to_use/COVID-19/FOUR/Four_daily_Recovered.csv")
Four.Summary = read.csv("ready_to_use/COVID-19/FOUR/Four_Summary.csv")
Four.dataset.dateWise = read.csv("ready_to_use/COVID-19/FOUR/Four_dataset_dateWise.csv")

#---------------------------------------------------------------#
### Functions ###

# Recovery/Death over Confirm cases
plot.recovered.in = function(cName, yesORno = FALSE) {
  
  
  countryWise.bulk.summary.country = countryWise.bulk.summary[
                                        which(str_detect(countryWise.bulk.summary$Country,
                                                         cName,
                                                         negate = yesORno)),
                                        ]
  
  ############################################
  
  d <- countryWise.bulk.summary.country %>% 
    as_tibble()
  
  d_ends <- countryWise.bulk.summary.country %>% 
    group_by(Country) %>% 
    top_n(1, Confirmed) %>% 
    pull(Recovered)    # col can be changed
  
  d %>% 
    ggplot(aes(Confirmed, Recovered, color = Country)) +
    geom_line(size = 2, alpha = .8) +
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
    ggtitle("Few(specify type of such countries) reporting more recovered for the confirmed cases",
            subtitle = "Based on the dataset of given country") +
    labs(x = "Confirmed Cases", y = "Recovered", caption = "Plot by @ravi")
  
}


plot.deaths.in = function(cName, yesORno = FALSE) {
  
  
  countryWise.bulk.summary.country = countryWise.bulk.summary[
    which(str_detect(countryWise.bulk.summary$Country,
                     cName,
                     negate = yesORno)),
    ]
  
  ############################################
  
  d <- countryWise.bulk.summary.country %>% 
    as_tibble()
  
  d_ends <- countryWise.bulk.summary.country %>% 
    group_by(Country) %>% 
    top_n(1, Confirmed) %>% 
    pull(Deaths)    # col can be changed
  
  d %>% 
    ggplot(aes(Confirmed, Deaths, color = Country)) +
    geom_line(size = 2, alpha = .8) +
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
    ggtitle("Few(specify type of such countries) reporting more deaths for the confirmed cases",
            subtitle = "Based on the dataset of given country") +
    labs(x = "Confirmed Cases", y = "Deaths", caption = "Plot by @ravi")
  
}


# Cases daily

# further last date could be passed
confirmed.till.date = function(cName, yesORno = FALSE) {
  
  
  countryWise.bulk.summary.country = countryWise.bulk.summary[
    which(str_detect(countryWise.bulk.summary$Country,
                     cName,
                     negate = yesORno)),
    ]
  
  #####################################################
  
  d <- countryWise.bulk.summary.country %>% 
    as_tibble()
  
  d_ends <- countryWise.bulk.summary.country %>% 
    group_by(Country) %>% 
    top_n(1, Day) %>% 
    pull(Confirmed)    # col can be changed
  
  temp = as.character(countryWise.bulk.summary$Date[1:nlevels(countryWise.bulk.summary$Date)])
  new = c(temp[10], temp[20], temp[30], temp[40])
  
  d %>% 
    ggplot(aes(Day, Confirmed, color = Country)) +
    geom_line(size = 2, alpha = .8) +
    theme_minimal() +
    scale_x_continuous(label = new, breaks = c(10, 20, 30, 40)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Few(specify type of such countries) reporting more deaths for the confirmed cases",
            subtitle = "Based on the dataset of given country") +
    labs(x = "Dates", y = "Confirmed", caption = "Plot by @ravi")
  
}


deaths.till.date = function(cName, yesORno = FALSE) {
  
  
  countryWise.bulk.summary.country = countryWise.bulk.summary[
    which(str_detect(countryWise.bulk.summary$Country,
                     cName,
                     negate = yesORno)),
    ]
  
  #####################################################
  
  d <- countryWise.bulk.summary.country %>% 
    as_tibble()
  
  d_ends <- countryWise.bulk.summary.country %>% 
    group_by(Country) %>% 
    top_n(1, Day) %>% 
    pull(Deaths)    # col can be changed
  
  temp = as.character(countryWise.bulk.summary$Date[1:nlevels(countryWise.bulk.summary$Date)])
  new = c(temp[10], temp[20], temp[30], temp[40])
  
  d %>% 
    ggplot(aes(Day, Deaths, color = Country)) +
    geom_line(size = 2, alpha = .8) +
    theme_minimal() +
    scale_x_continuous(label = new, breaks = c(10, 20, 30, 40)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Few(specify type of such countries) reporting more deaths for the confirmed cases",
            subtitle = "Based on the dataset of given country") +
    labs(x = "Dates", y = "Deaths", caption = "Plot by @ravi")
  
}

recovery.till.date = function(cName, yesORno = FALSE) {
  
  
  countryWise.bulk.summary.country = countryWise.bulk.summary[
    which(str_detect(countryWise.bulk.summary$Country,
                     cName,
                     negate = yesORno)),
    ]
  
  #####################################################
  
  d <- countryWise.bulk.summary.country %>% 
    as_tibble()
  
  d_ends <- countryWise.bulk.summary.country %>% 
    group_by(Country) %>% 
    top_n(1, Day) %>% 
    pull(Recovered)    # col can be changed
  
  temp = as.character(countryWise.bulk.summary$Date[1:nlevels(countryWise.bulk.summary$Date)])
  new = c(temp[10], temp[20], temp[30], temp[40])
  
  d %>% 
    ggplot(aes(Day, Recovered, color = Country)) +
    geom_line(size = 2, alpha = .8) +
    theme_minimal() +
    scale_x_continuous(label = new, breaks = c(10, 20, 30, 40)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Few(specify type of such countries) reporting more deaths for the confirmed cases",
            subtitle = "Based on the dataset of given country") +
    labs(x = "Dates", y = "Recovered", caption = "Plot by @ravi")
  
}

# for world data
till.date = function(yAxis, dataSet, cName = c("Hubei", "World", "Mainland China", "Diamond Princess"), yesORno = FALSE) {
  
  get(dataSet) -> plotFrom
  
  plotFrom = plotFrom[
    which(str_detect(plotFrom$Location,
                     cName,
                     negate = yesORno)),
  ]
  
  #####################################################
  
  d <- plotFrom %>% 
    as_tibble()
  
  d_ends <- plotFrom %>% 
    group_by(Location) %>% 
    top_n(1, Day) %>% 
    pull(yAxis)    # col can be changed to    ---------->    yAxis
  
  temp = as.character(plotFrom$Date[1:nlevels(plotFrom$Date)])
  new = c(temp[10], temp[20], temp[30], temp[40])
  
  d %>% 
    ggplot(aes(Day, Recovered, color = Location)) +
    geom_line(size = 2, alpha = .8) +
    theme_minimal() +
    scale_x_continuous(label = new, breaks = c(10, 20, 30, 40)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Hubei, China(except Hubei) & World polt, on the daily basis.",
            subtitle = "Based on the summary dataset") +
    labs(x = "Dates", y = yAxis, caption = "Plot by @ravi")
  
}

#---------------------------------------------------------------#

View(countryWise.bulk.summary)

#################################
#######  VISUALIZATIONS  ########
#################################

bulk.summary = read.csv("ready_to_use/COVID-19/bulk_summary.csv")
#  Plotting state VS deaths --> country wise
ggplot(bulk.summary, aes(x = States, y = Confirmed, color = Country)) +
  geom_jitter(alpha = 0.7) +
  scale_y_continuous(breaks=seq(0,1500,50))   # sets y's range from 0 to 1500, with teak of 51


#################################
#########  Line Plots  ##########
#################################

## Visualizing 'deaths over confirmed' COVID-19 cases
# for single country
plot.recovered.in("Hong Kong")
plot.deaths.in("Mainland China")

# for multiple country
plot.recovered.in(c("Hong Kong", "Italy", "South Korea", "Iran"))
plot.deaths.in(c("Hong Kong", "Italy", "South Korea"))

# esle then the specified country
plot.recovered.in("Mainland China", TRUE)
plot.deaths.in("Mainland China", TRUE)

# esle then the specified countries
countryList = levels(countryWise.bulk.summary$Country)
# plot.recovered.in(c("Vietnam", "US", "Taiwan"), TRUE)  # not working
# plot.deaths.in(c("Vietnam", "US", "Taiwan"), TRUE)  # not working


############### day wise
confirmed.till.date("India")
confirmed.till.date(c("Italy", "South Korea", "Hong Kong"))

deaths.till.date("South Korea")
deaths.till.date(c("Italy", "South Korea", "Hong Kong"))

recovery.till.date("Mainland China")
recovery.till.date(c("Italy", "South Korea", "Hong Kong"))



########################
## a Histogram or Bar plot will be better for --> Four.dataset.dateWise
View(Four.dataset.dateWise)

till.date(yAxis = "Confirmed", dataSet = "Four.dataset.dateWise")
till.date(yAxis = "Deaths", dataSet = "Four.dataset.dateWise", cName)
till.date(yAxis = "Recovered", dataSet = "Four.dataset.dateWise")




