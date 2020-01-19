# sum 1:n
num <- readline("Enter the number to get the sum upto that num. (from 1): ")
num <- as.numeric(num)

sum = 0
for (i in 1:num) {
  sum <- sum + i
}

cat("Sum is (1:", num, "): ", sum)