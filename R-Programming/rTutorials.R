# Hello World

c("Hello", "World", "!!") -> greetings

# functions

greet1 <- function(x) {
  say = ""
  
  for (word in x) {
    print(word)
  }
}

greet2 <- function(x) {
  print(x, sep = " ")
}

greet3 <- function() {
  print("Hello World!!")
}


# calls

greet1(greetings)

greet2(greetings)

greet3()

################################