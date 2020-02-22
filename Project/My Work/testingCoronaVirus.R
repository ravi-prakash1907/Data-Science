# Setting the working directory
setwd("/home/ravi/Documents/Data-Science/Project/My Work/")

# loading

test <- read.csv("test/cases_current.csv")

# work on string data
library(stringr)

china.data <- test[which(str_detect(test$Country_Region, "Mainland China")),]

china.data <- cbind(States = china.data[,1], china.data[,6:8])

china.states <- china.data$States[2:31]
china.deaths <- china.data$Deaths[2:31]
china.recovered <- china.data$Recovered[2:31]
china.confirmed <- china.data$Confirmed[2:31]

countries <- as.factor(unique(test$Country_Region))

# line graph    --->   in states of china
plot(x = 2:31, y = china.confirmed,type = "o", col = "blue", xlab = "States", ylab = "No. of People",
     main = "COVID19")
lines(x = 2:31, y = china.recovered, type = "o", col = "green")
lines(x = 2:31, y = china.deaths, type = "o", col = "red")

