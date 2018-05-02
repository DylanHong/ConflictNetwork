rm(list=ls())

#Import necessary libraries
library(readr)
library(igraph)
#install.packages("tidyr")
library(tidyr)
library(dplyr)

#Read in the raw CSV
AC <- read_csv("data/ACLED_Africa.csv")

#Quick overview of the data
class(AC)
summary(AC)

#Add new columns to dataframe

#Gives each event a unique event ID
AC$EVENT_ID <- seq.int(nrow(AC))

#Gives each relation a positive or negative relationship
AC$RELATION <- "neg"

#Seperate the allies into multiple columns if there are more than 1
#NA if there is no allies
ally1 <- c("Ally_1a", "Ally_1b", "Ally_1c", "Ally_1d","Ally_1e", "Ally_1f",
           "Ally_1g")
ally2 <- c("Ally_2a", "Ally_2b", "Ally_2c", "Ally_2d","Ally_2e", "Ally_2f")
AC <- AC %>% separate(ASSOC_ACTOR_1, ally1, sep = ";", fill = "right")
AC <- AC %>% separate(ASSOC_ACTOR_2, ally2, sep = ";", fill = "right")

# smalltest <- data.frame("actor1" = AC$ACTOR1,
#                         "actor2" = AC$ACTOR2,
#                         "edge1" = AC$LOCATION,
#                         "edge2" = AC$YEAR)
# smalltest <- smalltest[complete.cases(smalltest),]
# testGraph <- graph_from_data_frame(smalltest, directed = FALSE)
# 
# vcount(testGraph)
# ecount(testGraph)
# edge_attr_names(testGraph)
# 
# 
# allytest <- data.frame("actor1" = AC$ACTOR1,
#                        "ally1" = AC$Ally_1a)
# allytest <- allytest[complete.cases(allytest),]
# testAlly <- graph_from_data_frame(allytest, directed = FALSE)
# 
# vcount(testAlly)
# ecount(testAlly)

#Loop this for all

#Need to Make the final graph
#What is a good format for the final graph

AC <- AC[,c(8:24,1:7,25:41)]

finalAC <- AC[,c(1,10,9,17,18:41)]

#Need complete edge list with the relevant edge attributes
#What edge attribute are relevant? 
#For actual conflicts they are all relevant
#For ally relationships only need to know if positive or negative

#Puts the allys into the main dataframe
print(nrow(finalAC))
for (i in c(2:8)){
  print(i)
  tempdata <- AC[,c(1,i,9,17,18:41)]
  tempdata <- tempdata[complete.cases(tempdata[,1:2]),]
  colnames(tempdata)[2] <- 'ACTOR2'
  tempdata$RELATION <- "pos"
  finalAC <- rbind(finalAC,tempdata)
  print(nrow(finalAC))
}

for (i in c(11:16)){
  print(i)
  tempdata <- AC[,c(10,i,9,17,18:41)]
  tempdata <- tempdata[complete.cases(tempdata[,1:2]),]
  colnames(tempdata)[1] <- 'ACTOR1'
  colnames(tempdata)[2] <- 'ACTOR2'
  tempdata$RELATION <- "pos"
  finalAC <- rbind(finalAC,tempdata)
  print(nrow(finalAC))
}

finalAC
finalGraph <- graph_from_data_frame(finalAC, directed = FALSE)
vcount(finalGraph)
ecount(finalGraph)


#Now finalAC should be the complete edgelist


