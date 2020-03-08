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
# generating List of countries
ever.Affected <- read.csv("cleaned/ever.Affected.csv")
still.Affected <- read.csv("cleaned/still.Affected.csv")



#####################################################################
#                             Function                              #
#####################################################################

# MAIN : for visualization
visualize_on_map <- function(df, Path) {
  get(df) -> Affected
  
  ##########################################
  
  # storing column names
  columns <- colnames(Affected)
  
  day = 1:(length(columns)-4)
  d = as.Date("20/01/2020", format(c("%d/%m/%Y")))
  date = as.character((day + d), format(c("%d/%m/%Y")))
  
  # storing data frames datewise
  for (today in 4+seq_along(columns)) {
    if(today == 5) {
      affected.Today = subset(Affected, Affected[today] == T, select = c(Province.State, Country.Region, Lat, Long))
      myList = list(affected.Today)
    } else if(today > ncol(Affected)) {
      break
    } else {
      affected.Today = subset(Affected, Affected[today] == T, select = c(Province.State, Country.Region, Lat, Long))
      myList = list.append(myList, affected.Today)
    }
  }
  
  
  # generating plots, datewise
  i = 1
  while(T) {
    if(i>length(myList)){ # length(myList)
      print("Stoppppppp!!!!!!!!!!!!!!!!")
      break
    }
    
    affected.Today = as.data.frame(myList[i])
    row.names(affected.Today) <- NULL
    
    Countries <- factor(as.character(affected.Today$Country.Region))
    
    to_append <- data.frame(
      rank = 1:length(Countries),
      country = Countries
    )
    
    
    ############################################################################################
    
    # getting world map
    map.world <- map_data("world") 
    
    map.world_joined <- left_join(map.world, to_append, by = c('region' = 'country'))
    map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(rank),F,T))
    
    # countries/locations affected by coronavirus
    ggplot() +
      geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg), color = "#252525") +
      geom_point(data = affected.Today, aes(x = Long, y = Lat), color = "white") +
      scale_fill_manual(values = c("#414141","#af0404")) +
      labs(title = "2019-nCoV",
           subtitle = date[i],
           caption = "by @ravi") +
      
      theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
            ,panel.background = element_rect(fill = "#252525")
            ,plot.background = element_rect(fill = "#252525")
            ,panel.grid = element_blank()
            ,plot.title = element_text(size = 20, face = "bold", color = "#ff0000", hjust = 0.5)
            ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
            ,plot.caption = element_text(size = 15, family = "Bookman", face = "italic", hjust = 0.9)
            ,axis.text = element_blank()
            ,axis.title = element_blank()
            ,axis.ticks = element_blank()
            ,legend.position = "none"
      )
    
    ##########################################################################################
    
    
    pic = paste("pic",i, sep = "")
    imageName = paste(pic,"png", sep = ".")
    
    ggsave(path = Path, filename = imageName, width = 16, height = 9, scale = 1)
    i = i+1
  }
}

#####################################################################

visualize_on_map("ever.Affected", "PLOTS/pngs/ever")
visualize_on_map("still.Affected", "PLOTS/pngs/still")

####      TASKS
# 2) % change ---->  req. datasets (chapter 2)














