# palindrome

library(stringr)

str1 = "RAVI"
str2 ="Naman"
tolower(str1)   # ignoring case
tolower(str2)   # ignoring case

len1 = str_length(str1)
len2 = str_length(str2)

###########

plindromeCheck <- function(str, len) {
  flag = TRUE
  strlist = strsplit(str, "")[[1]]
  strlist <- as.vector(strlist)
  
  for(i in 1:as.numeric(len%/%2)) {
    revIndex = as.numeric(len+1-i)
    
    if(strlist[i] == strlist[revIndex]) {
      next
    }
    else {
      flag = FALSE
      break
    }
  }
  
  ifelse(flag == T, print("String is Palindrome"), print("String is NOT Palindrome"))
}

##########

plindromeCheck(str1, len1)
plindromeCheck(str2, len2)

