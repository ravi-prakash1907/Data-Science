fib <- function(num) {
  if(num <= 1)
    return(num)
  else
    return(fib(num-1)+fib(num-2))
}

n <- readline(prompt = "Enter no. of terms for fibonacci numbers: ")
as.numeric(n)

for(i in 0:n)
 cat(fib(i), " ")