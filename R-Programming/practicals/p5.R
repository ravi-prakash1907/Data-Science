########
# put desired list here

l1 <- list(2, 4, 6, 9)

#############

runningTotal <- c(NULL)
sum = 0
class(sum)

for (i in 1:length(l1)) {
  sum = sum + as.numeric(l1[i])
  runningTotal = c(runningTotal, sum)
}

runningTotal <- as.list(runningTotal)

##############

cat("List: \n")
print(l1)

cat("Running total is: \n")
print(runningTotal)