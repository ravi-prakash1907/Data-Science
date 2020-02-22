# Create a vector.
apple <- c('red','green',"yellow")
print(apple)

# Get the class of the vector.
print(class(apple))

# List
list1 <- list(c(2,3,4),21.3,sin)

print(list)

# Matrix
M1 <- matrix(c(1, 2, 3, 'a', 'b', 'c'), nrow = 2, ncol = 3, byrow = TRUE)

M2 = M1 %o% t(M1)
print(M1)

# Array
arr <- array(c('green','yellow'),dim = c(2,3,3))

print(arr)

# Factors
colours <- c('red', 'red', 'blue', 'red', 'pink', 'red', 'pink', 'pink', 'blue', 'yellow')

colourFactor <- factor(colours)

print(colourFactor)
print(nlevels(colourFactor))

# Data Frames
df <- data.frame(
    classes = c("XII-A", "XII-B"),
    def = c('Science', 'Commerce'),
    techSub = c('C.S.', 'I.P.'),
    lang = c('C++', 'Java')
)

print(df$classes)
