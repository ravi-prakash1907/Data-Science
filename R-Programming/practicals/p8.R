# Binary Search

# search
bsearch <- function(list, input){
  
  beg = 1
  end = as.numeric(length(list))
  
  while(beg <= end) {
    mid = as.integer((beg+end)/2)
    
    if(list[[mid]] == input) {
      return(print("Found!!"))
    }
    else if(input < list[[mid]]) {
      end <- mid-1
    } else {
      beg <- mid+1
    }
  }
  
  return(print("Not Found!!"))
}
#######

list = list(1, 2, 3, 7 ,5, 9)
input = as.numeric(readline("Enter the num. to be searched: "))

#######

bsearch(list, input)
