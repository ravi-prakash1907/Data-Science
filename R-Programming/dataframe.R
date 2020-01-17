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
