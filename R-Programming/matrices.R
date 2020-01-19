M1 <- matrix(c(1:15), nrow = 3, ncol = 5, byrow = TRUE)
M1[1] = "xyz"

M2 = rbind(2:4, 5:7, 16:18)
print(M2[2:3,])   # 2 rows
print(M2[2,3])   # ele in real [2][3]


#  array

arr = array(c(1:12), dim=c(2, 3, 2))
arr

#  excep hen in fun
devide <- function(a, b) {
  tryCatch(
    a/b,
    err <- function(e) {
      if(is.character(a) || is.character(b))
      print("Not possible!")
    }
  )
}
devide(4, 2)