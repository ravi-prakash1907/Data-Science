# to be edited

t <- as.numeric(readline("Enter the number of elements in list: "))

l <- list(NULL)

for(i in 1:t) {
  num <- as.numeric(readline("Enter a numbers:"))
  l <- c(l, num)
  l[[i]] = num
}

## Max element in the list

max(as.numeric(l))
