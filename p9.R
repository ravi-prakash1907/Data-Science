matrixAdd <- function(A, B) {
  return(A+B)
}

matrixSub <- function(A, B) {
  return(A-B)
}

matrixMul <- function(A, B) {
  return(A%*%B)
}


matrixAdd(A, B)
matrixSub(A, B)
matrixMul(A, B)



m1 = matrix(1:16, nrow = 4, ncol = 4)
m2 = matrix(17:32, nrow = 4, ncol = 4)

######

print("m1 + m2:")
m1+m2

######

print("m1 - m2:")
m1-m2

######

print("m1 * m2:")
m1%*%m2
