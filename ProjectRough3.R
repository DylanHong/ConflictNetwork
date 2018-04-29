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

#Seperate the allies into multiple columns if there are more than 1
#NA if there is no allies
ally1 <- c("Ally_1a", "Ally_1b", "Ally_1c", "Ally_1d","Ally_1e", "Ally_1f",
           "Ally_1g")
ally2 <- c("Ally_2a", "Ally_2b", "Ally_2c", "Ally_2d","Ally_2e", "Ally_2f")
AC <- AC %>% separate(ASSOC_ACTOR_1, ally1, sep = ";", fill = "right")
AC <- AC %>% separate(ASSOC_ACTOR_2, ally2, sep = ";", fill = "right")

smalltest <- data.frame("actor1" = AC$ACTOR1,
                        "actor2" = AC$ACTOR2,
                        "edge1" = AC$LOCATION,
                        "edge2" = AC$YEAR)
smalltest <- smalltest[complete.cases(smalltest),]
testGraph <- graph_from_data_frame(smalltest, directed = FALSE)

vcount(testGraph)
ecount(testGraph)
edge_attr_names(testGraph)


allytest <- data.frame("actor1" = AC$ACTOR1,
                       "ally1" = AC$Ally_1a)
allytest <- allytest[complete.cases(allytest),]
testAlly <- graph_from_data_frame(allytest, directed = FALSE)

vcount(testAlly)
ecount(testAlly)

#Loop this for all

#Need to Make the final graph
#What is a good format for the final graph

AC <- AC[,c(8:24,1:7,25:39)]

finalAC <- AC[,c(1,10,9,17,18:39)]
for (i in c(2:8)){
  print(i)
  tempdata <- AC[,c(1,i,18:39)]
  tempdata <- tempdata[complete.cases(tempdata[,1:2]),]
  print(class(tempGraph))
  finalGraph1 <- graph.union(finalGraph1,tempGraph, byname = F)
}

finalGraph2 = make_empty_graph(n=0, directed = FALSE)
for (i in c(11:16)){
  print(i)
  tempdata <- AC[,c(10,i,18:39)]
  tempdata <- tempdata[complete.cases(tempdata[,1:2]),]
  tempGraph <- graph_from_data_frame(tempdata, directed = FALSE)
  print(class(tempGraph))
  finalGraph2 <- graph.union(finalGraph2,tempGraph, byname = F)
}

class(finalGraph1)
vcount(finalGraph1)
ecount(finalGraph1)
class(finalGraph2)
vcount(finalGraph2)
ecount(finalGraph2)
edge_attr_names(finalGraph1)
vertex_attr_names(finalGraph1)

random <- AC[,c(8:24)]


#Dynamically naming varaibles in R?

#Create all the necessary graphs with the allies
#Then merge all the graphs to get master graph
