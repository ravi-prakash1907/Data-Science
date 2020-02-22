# Binary Search

list = list(1, 2, 3, 7 ,5, 9)

input = as.numeric(readline("Enter the num. to search: "))

# search
lsearch <- function(input){
  for (i in list){
    if(as.numeric(i) == input){
      print("Found!!")
      break
    } else {
      print("Not Found!!")
      break
    }
  }
}

#######
lsearch(input)