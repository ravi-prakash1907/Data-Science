d1 <- data.frame(
  consumerID = c(9, 2, 3, 7, 5, 6),
      Name = c(rep("Lakme", 3), "Elle", "Elle", "Elle")
)

d2 <- data.frame(
  consumerID = c(11:16),
  Name = c("Ponds", "Ponds", "Nivea", "Nivea", rep(c("Ponds"), 2))
)

###################

RD <- rbind(d1, d2) # appends row-wise
CD <- cbind(d1, d2) # appends column-wise

#################################################
#################################################

d <- data.frame(
  Name = c('A', 'B', 'C', 'D', 'E', 'F', 'G'),
  TOC = c(20, 30, 40, 20, 50, 60, 30),
  OS = c(25, 35, 45, 15, 10, 28, 39),
  DOB = as.Date(c('1995-06-16', '1992-01-01', rep('1998-03-04', 5)))
)

d

print(sapply(d, class))   # returns vector
d$"Week Day" = weekdays(d$DOB)  

d$"Year Month" <- format(d$DOB, "%y-%m")

d$"Exponential Score" = exp(d$TOC)

############################

#########


d$Month <- c(1:6, 1)
d$Month = gsub(1, "January", d$Month)











