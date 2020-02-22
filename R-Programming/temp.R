library(gtrendsR)
library(tidyverse)

topic <- readline("Enter topic you want stats on: ")

# Gathering data
df <- gtrends(topic, geo = c("IN", "US", "RU", "CN"), onlyInterest = T)
df <- df$interest_over_time
df$country <- ifelse(df$geo == "IN", "INDIA",
                     ifelse(df$geo == "US", "USA", 
                            ifelse(df$geo == "RU", "RUSSIA",
                                   "CHINA")))

# some preprocessing
df$hits <- as.integer(df$hits) 
df <- filter(df, df$hits >= 0)
df <- df[order(df$hits), ]

# Show off data
head(df)
dim(df)

# Plot Results
ggplot(data = df, 
       mapping = aes(x = date, y = hits, color = country)) +
  geom_smooth() + ggtitle(sprintf("INTERESTS IN (%s) OVER TIME IN VARIOUS COUNTRIES", topic))
