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
columns <- colnames(Affected)

# storing data frames
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

#############################################################################
# colour & fonts can be customized

map_generator = function(df1, df2, Date) {
  
  get(df1) -> to_append
  get(df2) -> this.affected.Today
  
  
  map.world <- map_data("world") 
  
  map.world_joined <- left_join(map.world, to_append, by = c('region' = 'country'))
  map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(rank),F,T))
  
  date.today = paste("Date:", Date, sep = " ")
  
  # countries/locations affected by coronavirus
  ggplot() +
    geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +   #      , alpha = 0.4
    geom_point(data = this.affected.Today, aes(x = Long, y = Lat), color = "black") +
    scale_fill_manual(values = c("#cccccc","#e60000")) +
    scale_alpha_manual(values = c(0.1, 0.1)) +
    labs(title = '"COVID-19"',
         subtitle = date.today,
         caption = "Plot by @ravi") +
    
    theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
          ,panel.background = element_rect(fill = "#444444")
          ,plot.background = element_rect(fill = "#444444")
          ,panel.grid = element_blank()
          ,plot.title = element_text(size = 25)
          ,plot.subtitle = element_text(size = 15, color = "red")
          ,plot.caption = element_text(size = 10)
          ,axis.text = element_blank()
          ,axis.title = element_blank()
          ,axis.ticks = element_blank()
          ,legend.position = "none"
    )
  
}
#############################################################################

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
  
  
  map_generator("to_append", "affected.Today", columns[i+5])
  pic = paste("pic",i, sep = "")
  imageName = paste(pic,"png", sep = ".")
  
  ggsave(path = "PLOTS", filename = imageName, width = 16, height = 9, scale = 1)
  i = i+1
}



####################################################################

# Creating GIF
library(caTools) ##  read.gif(), write.gif()



####      TASKS
# 1) GIF
# 2) % change ---->  req. datasets (chapter 2)








