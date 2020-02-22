# pre processing

x <- summary(airquality)
air = airquality

# 3 ways to remove NAs
# 1) manually
# 2) Replace

air$Ozone = ifelse(is.na(air$Ozone), median(air$Ozone, na.rm = TRUE), air$Ozone)

##### TRANSFORMATION

brks = c(0, 50, 100, 150, 200)
air$S = cut(air$Ozone, breaks = brks)

library(ggplot2)
table(air$Month)


ggplot(air, aes(x = Temp, fill = factor(Month))) +
  geom_histogram(binwidth = 0.5) +
  xlab("Temp") +
  ylab("Month") +
  labs(fill = "Month")