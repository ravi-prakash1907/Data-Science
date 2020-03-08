#####  LIBRARIES  #####

# Setting the working directory
setwd("/home/ravi/Documents/Data-Science/Project/My Work/")

#library(ggplot2)   # gg menns ->   grammer of graphics
library(tidyverse)
library(lattice)
library(plyr) # used to calculate the average weight of each group 

#------------------------------------------------------------------------#

### Loading files ###

Confirmed = read.csv("ready_to_use/COVID-19/Confirmed.csv")
Deaths = read.csv("ready_to_use/COVID-19/Deaths.csv")
Recovered = read.csv("ready_to_use/COVID-19/Recovered.csv")


countryWise.bulk.summary = read.csv("ready_to_use/COVID-19/Mixed/countryWise_bulk_summary.csv")
# train on 80% dates, test on 20%
dateWise.bulk.summary = read.csv("ready_to_use/COVID-19/Mixed/dateWise_bulk_summary.csv")


Four.daily.Confirmed = read.csv("ready_to_use/COVID-19/FOUR/Four_daily_Confirmed.csv")
Four.daily.Deaths = read.csv("ready_to_use/COVID-19/FOUR/Four_daily_Deaths.csv")
Four.daily.Recovered = read.csv("ready_to_use/COVID-19/FOUR/Four_daily_Recovered.csv")
Four.Summary = read.csv("ready_to_use/COVID-19/FOUR/Four_Summary.csv")

Four.dataset.locationWise = read.csv("ready_to_use/COVID-19/FOUR/Four_dataset_locationWise.csv")
Four.dataset.dateWise = read.csv("ready_to_use/COVID-19/FOUR/Four_dataset_dateWise.csv")

#---------------------------------------------------------------#
### Functions ###


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
till.date = function(yAxis, dataSet, cName = c("Hubei", "World", "China", "Diamond Princess"), yesORno = FALSE) {
  
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

till.date.Confirmed = function(lName = c("Hubei", "World", "China", "Diamond Princess"), yesORno = FALSE) {
  
  
  locationWise.bulk.summary.Four = Four.dataset.locationWise[
    which(str_detect(Four.dataset.locationWise$Location,
                     lName,
                     negate = yesORno)),
    ]
  
  #####################################################
  
  d <- locationWise.bulk.summary.Four %>% 
    as_tibble()
  
  d_ends <- locationWise.bulk.summary.Four %>% 
    group_by(Location) %>% 
    top_n(1, Day) %>% 
    pull(Confirmed)    # col can be changed
  
  temp = as.character(Four.dataset.locationWise$Date[1:nlevels(Four.dataset.locationWise$Date)])
  new = c(temp[10], temp[20], temp[30], temp[40])
  
  d %>% 
    ggplot(aes(Day, Confirmed, color = Location)) +
    geom_line(size = 2, alpha = 0.8) +
    theme_minimal() +
    scale_x_continuous(label = new, breaks = c(10, 20, 30, 40)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Few(specify type of such countries) reporting more deaths for the confirmed cases",
            subtitle = "Based on the dataset of given country") +
    labs(x = "Dates", y = "Confirmed", caption = "Plot by @ravi")
  
}

till.date.Deaths = function(lName = c("Hubei", "World", "China", "Diamond Princess"), yesORno = FALSE) {
  
  
  locationWise.bulk.summary.Four = Four.dataset.locationWise[
    which(str_detect(Four.dataset.locationWise$Location,
                     lName,
                     negate = yesORno)),
    ]
  
  #####################################################
  
  d <- locationWise.bulk.summary.Four %>% 
    as_tibble()
  
  d_ends <- locationWise.bulk.summary.Four %>% 
    group_by(Location) %>% 
    top_n(1, Day) %>% 
    pull(Deaths)    # col can be changed
  
  temp = as.character(Four.dataset.locationWise$Date[1:nlevels(Four.dataset.locationWise$Date)])
  new = c(temp[10], temp[20], temp[30], temp[40])
  
  d %>% 
    ggplot(aes(Day, Deaths, color = Location)) +
    geom_line(size = 2, alpha = 0.8) +
    theme_minimal() +
    scale_x_continuous(label = new, breaks = c(10, 20, 30, 40)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Few(specify type of such countries) reporting more deaths for the confirmed cases",
            subtitle = "Based on the dataset of given country") +
    labs(x = "Dates", y = "Deaths", caption = "Plot by @ravi")
  
}

till.date.Recovered = function(lName = c("Hubei", "World", "China", "Diamond Princess"), yesORno = FALSE) {
  
  
  locationWise.bulk.summary.Four = Four.dataset.locationWise[
    which(str_detect(Four.dataset.locationWise$Location,
                     lName,
                     negate = yesORno)),
    ]
  
  #####################################################
  
  d <- locationWise.bulk.summary.Four %>% 
    as_tibble()
  
  d_ends <- locationWise.bulk.summary.Four %>% 
    group_by(Location) %>% 
    top_n(1, Day) %>% 
    pull(Recovered)    # col can be changed
  
  temp = as.character(Four.dataset.locationWise$Date[1:nlevels(Four.dataset.locationWise$Date)])
  new = c(temp[10], temp[20], temp[30], temp[40])
  
  d %>% 
    ggplot(aes(Day, Recovered, color = Location)) +
    geom_line(size = 2, alpha = 0.8) +
    theme_minimal() +
    scale_x_continuous(label = new, breaks = c(10, 20, 30, 40)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Few(specify type of such countries) reporting more deaths for the confirmed cases",
            subtitle = "Based on the dataset of given country") +
    labs(x = "Dates", y = "Recovered", caption = "Plot by @ravi")
  
}
#---------------------------------------------------------------#


#######  VISUALIZATIONS  ########


#################################
#########  Line Plots  ##########
#################################


############### day wise
confirmed.till.date("Italy")
confirmed.till.date(c("Italy", "South Korea", "Hong Kong"))

deaths.till.date("South Korea")
deaths.till.date(c("Italy", "South Korea", "Hong Kong"))

recovery.till.date("China")
recovery.till.date(c("Italy", "South Korea", "Hong Kong"))



########################
## a Histogram or Bar plot will be better for --> Four.dataset.locationWise
View(Four.dataset.locationWise)

# Lines
till.date.Confirmed(c("Hubei", "China"))
till.date.Deaths("Hubei", T)
till.date.Recovered()

###########################################################################







#################################
#########  Histigrams  ##########
#################################

# 1) Active / Closed cases  (World)
View(Four.dataset.locationWise)
world = Four.dataset.locationWise[which(str_detect(Four.dataset.locationWise$Location, "World")),]
world.df = data.frame(
  Day = c(rep(world$Day, 2)),
  Date = c(rep(as.Date(world$Date), 2)),
  "Case Status" = factor(c(rep("Active", nrow(world)), rep("Closed", nrow(world))), levels = c("Active", "Closed")),
  Numbers = c(world$Active.Cases, world$Closed.Cases)
)

# Change line type
mean.caseWise <- ddply(world.df, "Case.Status", summarise, grp.mean=mean(Numbers))
head(mean.caseWise)

ggplot(world.df, aes(x=Numbers, color=Case.Status)) +
  geom_histogram(binwidth = 2500, fill="white", alpha=0.5, position="dodge") +
  geom_vline(data=mean.caseWise, aes(xintercept = grp.mean, color = Case.Status), linetype = "dashed") +
  theme(legend.position = "top") +scale_color_grey() + theme_classic() +
  theme(legend.position="top")


ggplot(world.df, aes(x=Numbers, color=Case.Status)) +
  geom_histogram(binwidth = 2500, fill = "white", position = "dodge") +
  geom_vline(data=mean.caseWise, aes(xintercept = grp.mean, color = Case.Status),
             linetype = "dashed") +
  scale_color_brewer(palette="Paired") +
  theme_classic()+theme(legend.position="top")


ggplot(world.df, aes(x=Numbers, color=Case.Status, fill=Case.Status)) +
  geom_histogram(binwidth = 3500, aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Case.Status),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Cases histogram plot",x="Cases", y = "Density")+
  theme_classic()






