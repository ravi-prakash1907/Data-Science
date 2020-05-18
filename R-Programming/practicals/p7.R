# Binary Search

# search
lsearch <- function(list, input){
  flag = F
  
  for (i in list){
    if(i == input){
      flag = T
      break
    }
  }
  
  ifelse(flag, print("Found!!"), print("Not Found!!"))
}

###############

list = list(1, 2, 3, 7 ,5, 9)
input = as.numeric(readline("Enter the num. to search: "))

#######
lsearch(list, input)