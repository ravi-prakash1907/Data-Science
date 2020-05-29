setwd("~/Documents/Data-Science/datasets/")

## Library
library(stringr)
library(RCurl)
library(tibble)
library(ggplot2)
library(dplyr)



## Creating dataset Dataset

name = NULL
Eng = NULL
Acc = NULL
BST = NULL
Eco = NULL
IP = NULL

Eng = round(runif(40, min = 0, max = 100))
Acc = round(runif(40, min = 0, max = 100))
BST = round(runif(40, min = 0, max = 100))
Eco = round(runif(40, min = 0, max = 100))
IP = round(runif(40, min = 0, max = 100))

for (num in c(1:40)) {
  name = c(name, paste("Name", num, sep = ""))
}

df <- data.frame(
            Name = name,
            English = Eng,
            Accountancy = Acc,
            "Business Studies" = BST,
            Economics = Eco,
            "Informatics Practices" = IP
        )

dim(df)
str(df)
View(df)


## Now, to get some meaning ful names, let's puul them form one of my online available dataset at GitHub

##works if internet is available
if(url.exists("https://raviprakashravi.cf/")){  
  marksDataset <- read.csv(text = getURL("https://raw.githubusercontent.com/ravi-prakash1907/Data-Science/master/datasets/marks.csv"), quote = "")
  
  
  ## looking onto the fetched dataset
  dim(marksDataset)
  str(marksDataset)
  
  ## raplacing the names in our previously created dataset
  df <- cbind(Names = marksDataset$Name, df[2:6])
  
}

## Viewing the dataset
View(df)
# writing into a file  ----->   once done for persistency
# write.csv(df, file = "marksData.csv", row.names = FALSE, quote = F)


# we have interoduced some missing values here in the above file
# so finally, our dataset is ready!!


#################################################


## Reading the above saved file as every time on running the script the dataset will change due to the random num. ganaration

readDF <- read.csv("marksData.csv", quote = "")
View(readDF)

##################################################


######        *** Pre-Processing ***        ######


#### Missing values
is.na(readDF)  ### shows which loc has NAs

sum(is.na(readDF))  ## total missing values in dataframe
colSums(is.na(readDF))  # columns having missing values
rowSums(is.na(readDF))  # row having missing values

#####

## looking if any of the standered statistical

#### Normalization and standerdization can be helpful for outlier detection and filling the missing values

# considering the normalized values to look onto the 
for (v in c(2:6)) {
  x = readDF[,v]
  y = (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
  
  print(colnames(readDF[v]))
  print(y)
}

## looking onto the summary of the features
summary(readDF)  

# considering the standardized values
for (v in c(2:6)) {
  x = readDF[,v]
  
  mean = mean(x, na.rm = T)
  standard_deviation = sqrt( sum( (x - mean)^2 , na.rm = T) / (length(x)-sum(is.na(x))))
  y = standard_deviation
  
  print(colnames(readDF[v]))
  print(y)
  
  print("Standerdized Values")
  print((x - mean) / standard_deviation)
}

###   as here are certain variencies (as from normalization) i.e. 
      # thigns are outlying the trend, 
      # we can not directly put mean, median or modes at the place of the missing values

### we'll fill the missing values with standered daeviations instead of mean, median or mode

####   Feature Selection   ####

# selecting col with missing values
missingCol <- readDF[,-c(1,5)]
missingCol
temp = missingCol

# considering the standardized values --->  te
for (v in seq_along(colnames(missingCol))) {
  x = missingCol[,v]
  x[is.na(x)] <- sqrt( sum( (x - mean(x, na.rm = T))^2 , na.rm = T) / (length(x)-sum(is.na(x))))
  missingCol[v] = round(x)
}


###############################################################

#  Some other ways to deal with the missing values:
## na.omit(dataframe)    ## it removes the rows where the data is not available
## we could have filled missing values with median using 'median()' or 
## with mode using:
      #   getmode <- function(v) {
      #                uniqv <- unique(x)
      #                uniqv[which.max(tabulate(match(x, uniqv)))]
      #              }
      #   Here, x is the single column

missingCol ## now the missing values are removed


###############################################################

#### Feature Extraction ####

## preparing another datasets with replaced missing values
cleaned <- cbind(readDF[,1], missingCol[,1:3], readDF[,5], missingCol[,4]) ## extracting the features from the 2 dataframes
colnames(cleaned) <- colnames(readDF) # Renaming the features/columns
cleaned


################################################
################################################

#######       *** Data Analysis***      ########

#######  Feature Creation

# to find the average of the students,
  # we need to have the total marks obtained by every students
cleaned = mutate(cleaned, Total = apply(cleaned[,2:6], 1, sum))
View(cleaned)


#####
Average <- data.frame(
                  
                  # Average marks of class/students
                  avgEng = mean(cleaned$English), # in English
                  avgAcc = mean(cleaned$Accountancy), # in Accountancy
                  avgBST = mean(cleaned$Business.Studies), # in Business Studies
                  avgEco = mean(cleaned$Economics), # in Economics
                  avgIP = mean(cleaned$Informatics.Practices), # in Informatics Practices
                  
                  "Total Average" = mean(cleaned$Total) # in Total
            )

print("Average Marks: ")
print(Average)

#####

print("Overall summary including the Minimum and Maximum marks obtained in every subject: ")
summary(cleaned)


#######################################################
#######################################################

#######       ***Data Visualization***         ########

## assigning the final dataset
df <- cleaned
View(df)


## Visualizing the marks obtained by every students

#########   Scatter plots

##########################    in different subjects

# English
ggplot(df, aes(x = c(1:40), y = English)) +
  geom_point(color = "blue") +
  
  labs(x="Names", y="Marks in English") +
  scale_x_continuous(label = df$Names, breaks = c(1:40)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    text = element_text(family = "Gill Sans")
    ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
    ,axis.text = element_text(size = 12)
    ,axis.title = element_text(size = 20)
  )


# Accountancy
ggplot(df, aes(x = c(1:40))) +
  geom_point(aes(y = Accountancy), color = "red") +
  
  labs(x="Names", y="Marks in Accountancy") +
  scale_x_continuous(label = df$Names, breaks = c(1:40)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    text = element_text(family = "Gill Sans")
    ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
    ,axis.text = element_text(size = 12)
    ,axis.title = element_text(size = 20)
  )


# Business.Studies
ggplot(df, aes(x = c(1:40))) +
  geom_point(aes(y = Business.Studies), color = "black") +
  
  labs(x="Names", y="Marks in Business Studies") +
  scale_x_continuous(label = df$Names, breaks = c(1:40)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    text = element_text(family = "Gill Sans")
    ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
    ,axis.text = element_text(size = 12)
    ,axis.title = element_text(size = 20)
  )


# Economics
ggplot(df, aes(x = c(1:40))) +
  geom_point(aes(y = Economics), color = "green") +
  
  labs(x="Names", y="Marks in Economics") +
  scale_x_continuous(label = df$Names, breaks = c(1:40)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    text = element_text(family = "Gill Sans")
    ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
    ,axis.text = element_text(size = 12)
    ,axis.title = element_text(size = 20)
  )

# Informatics.Practices
ggplot(df, aes(x = c(1:40))) +
  geom_point(aes(y = Informatics.Practices), color = "brown") +
  
  labs(x="Names", y="Marks in Informatics Practices") +
  scale_x_continuous(label = df$Names, breaks = c(1:40)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    text = element_text(family = "Gill Sans")
    ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
    ,axis.text = element_text(size = 12)
    ,axis.title = element_text(size = 20)
  )

#########   for overall marks obtained

# Total
ggplot(df, aes(x = c(1:40))) +
  geom_point(aes(y = Total), color = "black", size = 3) +
  geom_point(aes(y = Total), color = "brown", size = 3) +
  
  labs(x="Names", y="Total Marks (MM 500)") +
  scale_x_continuous(label = df$Names, breaks = c(1:40)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    text = element_text(family = "Gill Sans")
    ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
    ,axis.text = element_text(size = 12)
    ,axis.title = element_text(size = 20)
  )

############################################3

## Boxplot subject wise   --->  to detact outliers

# English
ggplot(df, aes(x = c(1:40), y = English)) +
  geom_point() +
  geom_boxplot() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "red") +
  labs(x = "Names", y = "Marks in English", title = "Boxplot to find the outliers")


# Accountancy
ggplot(df, aes(x = c(1:40), y = Accountancy)) +
  geom_point() +
  geom_boxplot() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "green") +
  labs(x = "Names", y = "Marks in Accountancy", title = "Boxplot to find the outliers")


##   similarlly for other 3 subjects..
##   we would like to find Outlier based on the overall scores --> out of 500
# Total
ggplot(df, aes(x = c(1:40), y = Total)) +
  geom_point(size = 2) +
  geom_boxplot() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "green") +
  labs(x = "Names", y = "Total Marks (MM 500)", title = "Boxplot to find the outliers")


cat("By this, we can infer that: \n",
    "1) Most of the students have scored marks in range 'more than 200 and less than 310(appx)'\n",
    "2) Only 1 student has scored a little ove 450\n",
    "3) No one has even scored marks more than 400, except 1 (topper)\n",
    "4) 10 out 40 students have scored less than 200 marks out of 500")



topper = as.character(df$Names[which(df$Total > 450)])
topper


cat("\n\n The topper is: ", topper,
    "\n Marks obtained by ", topper, ": ", df$Total[which(df$Total > 450)], "/500")

