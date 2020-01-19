# loading

train <- read.csv("CSV/train.csv")
test <- read.csv("CSV/test.csv")

# Add servived
test.survived <- data.frame(test[,1], Survived = rep("None", nrow(test)), test[,2:11])
names(test.survived)[1] <- names(train[1])

# joining data sets
data.combined <- rbind(train, test.survived)

# working with some R datatypes
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


# gross servival rate for titanic disaster ---> a table
table(data.combined$Survived)

# distribution accross classes of passengers
table(data.combined$Pclass)

library(ggplot2)

# pay more, get higher place, be closer to life bags   --->
        #   checking no of people in 3 classes and no of survivers

#train$Survived <- as.factor(train$Survived)
#train$Pclass <- as.factor(train$Pclass)
table(train$Pclass)
table(train$Survived)

### plotting  ###

# devision of survivers in each class
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_histogram(binwidth = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# separation of classes based of survival
ggplot(train, aes(x = Survived, fill = factor(Pclass))) +
  geom_histogram(binwidth = 0.5) +
  xlab("Survived") +
  ylab("Total Count") +
  labs(fill = "Pclass")


# dealing with names in training dataset
head(as.character(train$Name))


length(as.character(data.combined$Name))            # 1309
length(unique(as.character(data.combined$Name)))    # 1307    ----->  unique
# here we have 2 duplicate names

# extract (2) duplicate names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])#     ----->  unique

# look all records having duplicate names
data.combined[which(data.combined$Name %in% dup.names),]


# work on string data
library(stringr)

# devision based on sex & marital status (title)
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
masters <- data.combined[which(str_detect(data.combined$Name, "Master.")),]
misters <- data.combined[which(str_detect(data.combined$Name, "Mr.")),]

females <- data.combined[which(data.combined$Sex == "female"),]
males <- data.combined[which(data.combined$Sex == "male"),]

head(females)
head(males)



# relation b/w title and Survival as well as Pclass

# extract all the possible titles
get.titles <- function(name) {
    
  name = as.character(name)
    
  # can use switch too
  if(length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if(length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if(length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if(length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}
  
all.titles <- NULL
for (i in 1:nrow(data.combined)) {
  all.titles <- c(all.titles, get.titles(data.combined[i, "Name"]))
}
  
data.combined$Titles <- as.factor(all.titles) # appending new 'title' column
  nlevels(data.combined$Titles)                 # no of all titles



# visualization       --->      servival per class for every title
  
#  Differentiate b/w histogram and bargraph, after running the plot, below
ggplot(data.combined[1:819,], aes(x = Titles, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Titles") +
  ylab("Total") +
  labs(fill = "Survived")
  



