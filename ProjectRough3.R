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
#AC$EVENT_ID <- seq.int(nrow(AC))

#Gives each relation a positive or negative relationship
AC$RELATION <- "neg"

#Seperate the allies into multiple columns if there are more than 1
#NA if there is no allies
ally1 <- c("Ally_1a", "Ally_1b", "Ally_1c", "Ally_1d","Ally_1e", "Ally_1f",
           "Ally_1g")
ally2 <- c("Ally_2a", "Ally_2b", "Ally_2c", "Ally_2d","Ally_2e", "Ally_2f")
AC <- AC %>% separate(ASSOC_ACTOR_1, ally1, sep = ";", fill = "right")
AC <- AC %>% separate(ASSOC_ACTOR_2, ally2, sep = ";", fill = "right")

numcol <- 40

AC <- AC[,c(8:24,1:7,25:numcol)]

finalAC <- AC[,c(1,10,9,17,18:numcol)]

#Need complete edge list with the relevant edge attributes
#What edge attribute are relevant? 
#For actual conflicts they are all relevant
#For ally relationships only need to know if positive or negative

#Puts the allys into the main dataframe
print(nrow(finalAC))
for (i in c(2:8)){
  print(i)
  tempdata <- AC[,c(1,i,9,17,18:numcol)]
  tempdata <- tempdata[complete.cases(tempdata[,1:2]),]
  colnames(tempdata)[2] <- 'ACTOR2'
  tempdata$RELATION <- "pos"
  finalAC <- rbind(finalAC,tempdata)
  print(nrow(finalAC))
}

for (i in c(11:16)){
  print(i)
  tempdata <- AC[,c(10,i,9,17,18:numcol)]
  tempdata <- tempdata[complete.cases(tempdata[,1:2]),]
  colnames(tempdata)[1] <- 'ACTOR1'
  colnames(tempdata)[2] <- 'ACTOR2'
  tempdata$RELATION <- "pos"
  finalAC <- rbind(finalAC,tempdata)
  print(nrow(finalAC))
}

drops <- c("EVENT_DATE","TIME_PRECISION","EVENT_TYPE","REGION","ADMIN1",
           "ADMIN2","ADMIN3","LATITUDE","LONGITUDE","GEO_PRECISION","SOURCE",
           "SOURCE_SCALE","NOTES","TIMESTAMP")
finalAC <- finalAC[ , !(names(finalAC) %in% drops)]



#Now finalAC should be the complete edgelist
finalGraph <- graph_from_data_frame(finalAC, directed = FALSE)
vcount(finalGraph)
ecount(finalGraph)
edge_attr_names(finalGraph)
vertex_attr_names(finalGraph)

#Checking to see if the graph was made correctly
rand1 <- unique(finalAC[["ACTOR1"]])
rand2 <- unique(finalAC[["ACTOR2"]])
rand3 <- c(rand1,rand2)
length(unique(rand3))

#Number of connected components 
count_components(finalGraph) 

#components(finalGraph)

G2000 <- induced_subgraph(finalGraph, which(E(finalGraph$YEAR == 2000)))

subgraph1 <- subgraph.edges(finalGraph,1:10,1:10)
class(subgraph1)

