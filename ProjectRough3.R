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

#Number of Columns
numcol <- 41

#Rearranging the columns in the dataframe
AC <- AC[,c(8:24,1:7,25:numcol)]
finalAC <- AC[,c(1,10,9,17,18:numcol)]

#Removing all the incomplete edge pairs in the data
finalAC <- finalAC[complete.cases(finalAC[,1:2]),]

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

#Drop columns that will not be used
drops <- c("EVENT_DATE","TIME_PRECISION","EVENT_TYPE","REGION","ADMIN1",
           "ADMIN2","ADMIN3","LATITUDE","LONGITUDE","GEO_PRECISION","SOURCE",
           "SOURCE_SCALE","NOTES","TIMESTAMP")
finalAC <- finalAC[ , !(names(finalAC) %in% drops)]

#Now finalAC should be the complete edgelist
#finalAC

#Get the dataframe sorted by years
dataByYear <- list()
for (i in c(1997:2018)){
  yearTemp <- finalAC[finalAC$YEAR == i,]
  dataByYear[[i-1996]] <- yearTemp
}

#Create a graph of all years
finalGraph <- graph_from_data_frame(finalAC, directed = FALSE)
vcount(finalGraph)
ecount(finalGraph)
edge_attr_names(finalGraph)
vertex_attr_names(finalGraph)

#Create graphs of each year
graphByYear <- list()
i <- 1
for (thisData in dataByYear){
  graphTemp <- graph_from_data_frame(thisData, directed = FALSE)
  print(vcount(graphTemp))
  graphByYear[[i]] <- graphTemp
  i <- i + 1
}

#Checking to see if the full graph was made correctly
rand1 <- unique(finalAC[["ACTOR1"]])
rand2 <- unique(finalAC[["ACTOR2"]])
rand3 <- c(rand1,rand2)
length(unique(rand3))
vcount(finalGraph)
rm(list = c("rand1","rand2","rand3"))

#Number of connected components 
count_components(finalGraph) 
components(finalGraph)$csize

#Get only the largest connected component
#All the other connected components have less than 6 vertices
components <- decompose(finalGraph, min.vertices=6)
length(components)
finalGraph <- components[[1]]

#Graph Description for Full Graph
average.path.length(finalGraph)
mean(degree(finalGraph))
diameter(finalGraph, directed=F, weights=NA)
edge_density(finalGraph, loops=F)
transitivity(finalGraph, type="global")

#Check which vertex has the highest degree
V(finalGraph)$name[degree(finalGraph)==max(degree(finalGraph))]
max(degree(finalGraph))

#Clean the yearly graphs
i <- 1
newGraphByYear <- list()
for (graph in graphByYear){
  count_components(graph) 
  #components(graph)$csize
  components <- decompose(graph, min.vertices=150)
  print(length(components))
  newGraphByYear[[i]] <- components[[1]]
  i <- i + 1
}

#Check to make sure they are all the largest connencted component
for (graph in newGraphByYear){
  print(count_components(graph))
  print(vcount(graph))
  print(V(graph)$name[degree(graph)==max(degree(graph))])
  print(max(degree(graph)))
}

sum(count_triangles(testGraph))
count_triangles(testGraph)
triads <- triangles(testGraph)

triads[3]$RELATION

edge_attr(testGraph, RELATION, index = E(triads[1], triads[2]))

E(testGraph)$RELATION

edgeHere <- get.edge.ids(testGraph, c(triads[1], triads[2]), directed = FALSE, error = FALSE, multi = FALSE)


edge_attr(testGraph, "RELATION", index = 
            get.edge.ids(testGraph, c(triads[1], triads[2]), directed = FALSE))

structural_balance <- function(givenGraph){
  triads <- triangles(givenGraph)
  total <- length(triads)
  threePos <- 0
  twoPos <- 0
  onePos <- 0
  zeroPos <- 0
  for (i in seq(1,(length(triads)-2), by=3)){
    print(i)
    one <- edge_attr(givenGraph, "RELATION", index = 
                       get.edge.ids(givenGraph, c(triads[i], triads[i+1]), directed = FALSE))
    two <- edge_attr(givenGraph, "RELATION", index = 
                       get.edge.ids(givenGraph, c(triads[i+1], triads[i+2]), directed = FALSE))
    three <- edge_attr(givenGraph, "RELATION", index = 
                       get.edge.ids(givenGraph, c(triads[i+2], triads[i+3]), directed = FALSE))
    
    tempSum <- 0
    for (attr in c(one,two,three)){
      if(attr == "pos"){
        tempSum <- tempSum + 1
      }
    }
    
    if (tempSum == 0){
      zeroPos <- zeroPos +1
    }
    if (tempSum == 1){
      onePos <- onePos +1
    }
    if (tempSum == 2){
      twoPos <- twoPos +1
    }
    if (tempSum == 2){
      threePos <- threePos +1
    }
  }
  print(zeroPos)
  print(onePos)
  print(twoPos)
  print(threePos)
}

structural_balance(testGraph)

triads <- triangles(finalGraph)
class(triads)
cliques(finalGraph,min=3,max=3)


testGraph <- newGraphByYear[[22]]

constraint(testGraph)



structural.balance(testGraph)

#Ideas*********************************

#Look at modularity between different types of groups
#Structural Balane
#Burts constraint measure
