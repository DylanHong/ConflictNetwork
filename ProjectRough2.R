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

#Dynamically naming varaibles in R?

#Create all the necessary graphs with the allies
#Then merge all the graphs to get master graph
