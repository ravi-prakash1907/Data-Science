systolic.blood.pressure.Midpoint = c(rep(95.5,5),rep(105.5,8),rep(115.5,22),rep(125.5,27),rep(135.5,17),rep(145.5,9),rep(155.5, 5),rep(165.5,5),rep(175.5,2))

# Measures of dispersion:
# they represent that how much the data is dispersed
# these are of 2 types: ABSOLUTE & RELATIVE measures of dispersion
  # these can be represented in terms of:
    # Range
    # Standered Deviation
    # Variance
    # Quartiles and Quartile Deviation
    # Mean and Mean Deviation

##############################3



range = c(min(systolic.blood.pressure.Midpoint), max(systolic.blood.pressure.Midpoint))  # range
mean = mean(systolic.blood.pressure.Midpoint)  # mean
standardDev = sd(systolic.blood.pressure.Midpoint) # Standered deviation
quartile = quantile(systolic.blood.pressure.Midpoint)      # Quartile

ul <- unique(systolic.blood.pressure.Midpoint)

# mean derviation
print("Term : Mean Deviation")
for(i in 1:n) {
  cat(ul[i], " : ", ul[i]-mean) 
}
meanDev

# Quartile Deviation
Q1 = quartile[[1]]
Q3 = quartile[[3]]
quartileDev = (Q3-Q1)/(Q3+Q1) 
quartileDev
