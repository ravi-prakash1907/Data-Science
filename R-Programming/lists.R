# Working with Lists

list1 <- list('Xyz', 'is', 10, "years", "old!")
list2 <- list(c('Jan', 'Fab', 'Jun', 'July'), matrix(c(17, 22, 30, 19), nrow = 2, ncol = 2))

list3 <- list('one', 'two', 'three', 'four')

# naming the list1 elements

names(list1) = c("Name", "Helping Verb", "Age", "Unit for Age", "Status")

c('Fav Months', 'Dob matrix (Row Wise)') -> names(list2)


### handler functions ###

listOne <- function() {
  print(list1)  # prints list1
  
  print(list1["Age"]) # prints list1 item with index or named as 'Age'
  
  # For loop to print full list1
  print("By for loop: ")
  cat("\n")
  for(i in list1) {
    cat(i, " ")
  }
  cat("\n\n")
  
  cat("Size of First list is : ", length(list1)) # Size of the list1
  
  ######################
  cat("\n\n#########################################\n\n")
}

listTwo <- function() {
  print(list2[2]) # prints list2 item with index or named as 'Age'
  
  # For loop to print full list2
  print("By for loop: ")
  cat("\n")
  
  for(i in list2) {
    cat(i, "\n")
  }
  cat("\n")
  
  cat("Size of Second list is : ", length(list2)) # Size of the list2
  
  #################
  cat("\n\n#########################################\n\n")
}

listThree <- function() {
  # creating vector from a list
  cat("Creating vector from list3:\n")
  v <- unlist(list3)
  
  # using Repeat Loop
  cat("v = (")
  i = 0
  repeat {
    if(length(v) < i)
      break
    cat(v[i], " ")
    i = i+1
  }
  cat(") is a vector.\n\n")
  ####################
  
  # list manipulation
  list3[3] = 3
  list3[5] <- 5
  NULL -> list3[6]
  
  print(list3[6]) #NULL
  
  print("By for loop: ")
  cat("  Manipulated list-\n  [ ")
  for(i in list3) {
    cat(i, " ")
  }
  cat("]\n\n")
  
  # merging lists
  tempList = 6:10
  merged.list <- c(list3, tempList)
  
  print("Merged list :-")
  for (index in merged.list) {
    cat(index, " ")
  }
  
  cat("\n\n#########################################\n\n")
}


### Application ###

listOne()
listTwo()
listThree()
