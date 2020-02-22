#####  LIBRARIES  #####

#library(ggplot2)   # gg menns ->   grammer of graphics
library(tidyverse)  # includes ggplot2
library(lattice)

#------------------------------------------------------------------------#



### Loading files ###

Confirmed = read.csv("ready_to_use/COVID-19/Confirmed.csv")
Deaths = read.csv("ready_to_use/COVID-19/Deaths.csv")
Recovered = read.csv("ready_to_use/COVID-19/Recovered.csv")

bulk.summary = read.csv("ready_to_use/COVID-19/bulk_summary.csv")

############################################

One.Country.States.daily.Confirmed = read.csv("ready_to_use/COVID-19/One_Country_States_daily_Confirmed.csv")
One.Country.States.daily.Deaths = read.csv("ready_to_use/COVID-19/One_Country_States_daily_Deaths.csv")
One.Country.States.daily.Recovered = read.csv("ready_to_use/COVID-19/One_Country_States_daily_Recovered.csv")

One.Country.Aggregate.daily.Confirmed = read.csv("ready_to_use/COVID-19/One_Country_Aggregate_daily_Confirmed.csv")
One.Country.Aggregate.daily.Deaths = read.csv("ready_to_use/COVID-19/One_Country_Aggregate_daily_Deaths.csv")
One.Country.Aggregate.daily.Recovered = read.csv("ready_to_use/COVID-19/One_Country_Aggregate_daily_Recovered.csv")

All.Countries.daily.Confirmed = read.csv("ready_to_use/COVID-19/All_Countries_daily_Confirmed.csv")
All.Countries.daily.Deaths = read.csv("ready_to_use/COVID-19/All_Countries_daily_Deaths.csv")
All.Countries.daily.Recovered = read.csv("ready_to_use/COVID-19/All_Countries_daily_Recovered.csv")


One.Country.States.summary = read.csv("ready_to_use/COVID-19/One_Country_States_summary.csv")
One.Country.Aggregate.summary = read.csv("ready_to_use/COVID-19/One_Country_Aggregate_summary.csv")
All.Countries.summary = read.csv("ready_to_use/COVID-19/All_Countries_summary.csv")




#---------------------------------------------------------------#




#################################
#######  VISUALIZATIONS  ########
#################################

# line graph    --->   in states of country
plot(x = 1:nrow(One.Country.States.summary), y = One.Country.States.summary$Confirmed, type = "o", col = "blue", xlab = "States", ylab = "No. of People",
     main = "COVID19 Mainland China")
lines(x = 1:nrow(One.Country.States.summary), y = One.Country.States.summary$Recovered, type = "o", col = "green")
lines(x = 1:nrow(One.Country.States.summary), y = One.Country.States.summary$Deaths, type = "o", col = "red")

# scatter plot
ggplot(One.Country.States.summary) + # a blank canvas
  geom_point(mapping = aes(x = One.Country.States.summary$States, y = One.Country.States.summary$Recovered)) +  # a layer of points
  xlab("States") +
  ylab("COVID19 Recovered") +
  ggtitle("Mainland China") +
  labs(fill = "Recovered")



#  Plotting state VS deaths --> country wise
ggplot(bulk.summary, aes(x = States, y = Confirmed, color = Country)) +
  geom_jitter(alpha = 0.7) +
  scale_y_continuous(breaks=seq(0,1500,50))   # sets y's range from 0 to 1500, with teak of 51



# scatter plot
ggplot(Countries.summary) + # a blank canvas
  geom_point(mapping = aes(x = Countries.summary$Deaths, y = Countries.summary$Deaths)) +  # a layer of points
  xlab("Country") +
  ylab("COVID19 Deaths") +
  ggtitle("Countries") +
  labs(fill = "Deaths") +
  scale_x_continuous(breaks=seq(0,1000,50)) 



