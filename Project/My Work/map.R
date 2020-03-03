# visualization on map
library(stringr)
library(countrycode)
library(ggplot2)
library(ggmap)
library(maps)

# Setting the working directory
setwd("/home/ravi/Documents/Data-Science/Project/My Work/")


## TO PLOT COUNTRIES
#generating List of countries
Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
Countries <- levels(Confirmed$Country.Region)
# removing Cruise from country list
Countries = Countries[which(str_detect(Countries, "Diamond Princess cruise ship", negate = T))]

# generating country code
country.code = countrycode(
  sourcevar = Countries,
  origin = "country.name",
  destination = "iso2c",
  nomatch = NULL
)

country.code   # this can be easily plotted on graph

iso.expand(country.code, regex=TRUE)

####################
#  TO PLOT STATES (of a country)

## https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html



map.world <- map_data("world") # usa map, similarl;,     "world" give high res world map centered on the Pacific Ocean 

to_append <- data.frame(
  rank = 1:length(country.code),
  country = Countries
)

map.world_joined <- left_join(map.world, to_append, by = c('region' = 'country'))

map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(rank),F,T))
head(map.world_joined)


# countries/locations affected by coronavirus
ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +
  geom_point(data = Confirmed, aes(x = Long, y = Lat), color = "black") +
  scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  labs(title = 'Countries and their provinces, affected by "COVID-19"',
       subtitle = "Source: https://github.com/ravi-prakash1907/Data-Science",
       caption = "Plot by @ravi") +
  
  theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 30)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )

ggsave("PLOTS/affected locations.png", width = 16, height = 9, scale = 1)



