table <- c(NULL)
columns <- c(NULL)

for (i in 1:12) {
  for (j in 1:10) {
    table = c(table, i*j)
  }
  
  columns = c(columns, as.character(i))
}

table <- matrix(table, nrow = 10, ncol = 12)
colnames(table) <- columns

View(table)
