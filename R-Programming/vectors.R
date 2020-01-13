# Working with Vectors

# functions

total <- function(a, b) {
  a + b
}

product <- function(x, y) {
  prod = x*y
  return (prod)
}

reverse <- function(v) {
  sort(v, decreasing = TRUE) -> revVector
  return (revVector)
}


# creating vctors

v1 = c(4, 12, 10)
v2 <- 2:4
seq(6,8) -> v3


# Applications

cat("v1 = ", v1, "\n")
cat("v2 = ", v2, "\n")
cat("v3 = ", v3, "\n")

print("Displaying vector v1 in reverse order:")
print(reverse(v1))

sprintf("Sum of values in v2 = %d", sum(v2))

cat("v1 + v2 + v3 = ", total(total(v1, v2), v3), "\n")

cat("Product of v2 and v3 is : ", product(v2, v3), "\n")
