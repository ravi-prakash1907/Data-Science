# visualization on map
library(stringr)
library(countrycode)
library(dplyr) # left join
library(rlist)
library(ggplot2)
library(ggmap)
library(maps)

# Setting the working directory
setwd("/home/ravi/Documents/Data-Science/Project/My Work/")


## TO PLOT COUNTRIES
#generating List of countries
Affected <- read.csv("cleaned/Affected.csv")

Countries <- levels(affected.Today$Country.Region)




###############################################################################
###############################################################################

# generating country code
country.code = countrycode(
  sourcevar = Countries,
  origin = "country.name",
  destination = "iso3c",
  nomatch = NULL
)
print(country.code)
# country.code   # this can be easily plotted on graph

iso.expand(country.code, regex=TRUE)

###############################################################################
###############################################################################








columns <- colnames(Affected)

# Testing loop
for (today in 4+seq_along(columns)) {
  if(today == 6) {
    affected.Today = subset(Affected, Affected[today] == T, select = c(Province.State, Country.Region, Lat, Long))
    myList = list(affected.Today)
  } else if(today > ncol(Affected)) {
    break
  } else {
    affected.Today = subset(Affected, Affected[today] == T, select = c(Province.State, Country.Region, Lat, Long))
    myList = list.append(myList, affected.Today)
  }
}


##############################
# to be formated
for (df in myList) {
  View(df)
  readline("Press!")
}
##############################


i = 1

while(T) {
  if(i>length(myList)){
    print("Stoppppppp!!!!!!!!!!!!!!!!")
    break
  }

  affected.Today = as.data.frame(myList[i])  # myList[1]
  row.names(affected.Today) <- NULL
  
  Countries <- factor(as.character(affected.Today$Country.Region))
  
  to_append <- data.frame(
    rank = 1:length(Countries),
    country = Countries
  )
  
  
  map_generater("to_append", "affected.Today")
  pic = paste("pic",i, sep = "")
  imageName = paste(pic,"png", sep = ".")
  
  ggsave(path = "PLOTS", filename = imageName, width = 16, height = 9, scale = 1)
  i = i+1
}
  
  
  
map_generater = function(df1, df2) {
  
  get(df1) -> to_append
  get(df2) -> affected.Today
  
  
  map.world <- map_data("world") # usa map, similarl;,     "world" give high res world map centered on the Pacific Ocean 
  
  map.world_joined <- left_join(map.world, to_append, by = c('region' = 'country'))
  map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(rank),F,T))
  
  
  
  # countries/locations affected by coronavirus
  # plot daywise to create gif
  
  ggplot() +
    geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +   #      , alpha = 0.4
    geom_point(data = affected.Today, aes(x = Long, y = Lat), color = "black") +
    scale_fill_manual(values = c("#ffffff","#e60000")) +
    scale_alpha_manual(values = c(0.1, 0.1)) +
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
  
}






#ggsave("PLOTS/affected locations.png", width = 16, height = 9, scale = 1)





#####      PREDICTIVE vs GENERATIVE        #####

# cronejob ---->  command in linux


# fOR sUNNY
# reutors ---> text classiffication ---> this book is------ Religious, Fiction etc..
# Image Cat/Dog  -------->   AV articles on img classification
# own Search engin
# Bihar election with Delhi etc...

