rm(list=ls())

#Import necessary libraries
library(readr)
library(igraph)
#install.packages("tidyr")
library(tidyr)
library(dplyr)
#install.packages("RSiena)
library(RSiena)


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

#Create a vertex attribute dataframe
#Note: The intercode can change over time
#Query all the vertex attributes and put into dataframe
#Difficult to do - save to do later if
first <- finalAC[,c(1,3,8)]
second <- finalAC[,c(2,4,8)]
colnames(second)[1] <- 'ACTOR1'
colnames(second)[2] <- 'INTER1'
combined <- rbind(first, second)
combined <- unique(combined)
print(nrow(combined))
length(unique(combined[["ACTOR1"]]))
length(unique(combined[["INTER1"]]))

#testing <- combined_new[combined_new$ACTOR1 == "Rioters (Niger)",]

#Sort the dataframe by date
orderYear <- combined[order(combined$YEAR),]
vattr <- data.frame()

#Get the changing intercodes for each actor
# temp <- list()
# for (index in 1:nrow(orderYear)){ 
#   row = orderYear[index, ]
#   # print(index)
#   if ( !(row[["ACTOR1"]] %in% names(temp))) {
#     temp[[row[["ACTOR1"]]]] <- paste0("", row[["INTER1"]])
#   } else if ( !(grepl(row[["INTER1"]], temp[[row[["ACTOR1"]]]] )) ) {
#     temp[[row[["ACTOR1"]]]] <- paste0(temp[[row[["ACTOR1"]]]], row[["INTER1"]])
#     print(temp[[row[["ACTOR1"]]]])
#   }
# } 


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
#Also look at the highest degree actor and the degree of that actor
for (graph in newGraphByYear){
  print(count_components(graph))
  print(vcount(graph))
  print(V(graph)$name[degree(graph)==max(degree(graph))])
  print(max(degree(graph)))
}

#Function that analyzes triads for structural balance
#Input: graph
structural_balance <- function(givenGraph){
  triads <- triangles(givenGraph)
  total <- length(triads)
  threePos <- 0
  twoPos <- 0
  onePos <- 0
  zeroPos <- 0
  for (i in seq(1,(length(triads)-2), by=3)){
    #print(i)
    one <- edge_attr(givenGraph, "RELATION", index = 
                       get.edge.ids(givenGraph, c(triads[i], triads[i+1]), directed = FALSE))
    two <- edge_attr(givenGraph, "RELATION", index = 
                       get.edge.ids(givenGraph, c(triads[i+1], triads[i+2]), directed = FALSE))
    three <- edge_attr(givenGraph, "RELATION", index = 
                         get.edge.ids(givenGraph, c(triads[i], triads[i+2]), directed = FALSE))
    
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
    if (tempSum == 3){
      threePos <- threePos +1
    }
  }
  print(paste("The total number of triads is", total/3, "There are", zeroPos,
              "triads with all negative,", onePos, "with one positive,", twoPos,
              "with two positive, and", threePos, 
              "with three positive. The percent of structurally balanced triads is", 
              (total-twoPos)/total))
  # print(zeroPos)
  # print(onePos)
  # print(twoPos)
  # print(threePos)
  # print(total)
}

#Test the structural balance function on smaller graph
testGraph <- newGraphByYear[[22]] #2018
structural_balance(testGraph)


############Longitudinal Bullies and Allies##############

#Write the functions to make the relevant matrices
#Years should be indexed to refer to year
create_matrices_2 <- function(year1, year2){
  years <- c(year1,year2)
  
  graph1 <- newGraphByYear[[year1]]
  graph2 <- newGraphByYear[[year2]]

  graphs <- list(graph1,graph2)
  
  names1 <- V(graph1)$name
  names2 <- V(graph2)$name
  
  names <- list(names1,names2)
  
  totalNames <- unique(c(names1,names2))
  print(length(totalNames))
  
  #Loop through and populate the matrix
  #Returns of list of combined matrices
  adjMatrixList <- list()
  index <- 1
  for(year in years){
    adjMatrix <- matrix(data = 0, nrow = length(totalNames), ncol = length(totalNames))
    rownames(adjMatrix) <- totalNames
    colnames(adjMatrix) <- totalNames
    curGraph <- graphs[[index]]
    print(index)
    for (i in c(1:length(totalNames))){
      #print(i)
      for (j in c(1:length(totalNames))){
        xaxis <- totalNames[[i]]
        yaxis <- totalNames[[j]]
        if(xaxis %in% names[[index]] && yaxis %in% names[[index]]){
          if(are_adjacent(curGraph,xaxis,yaxis)){
            edgeID <- get.edge.ids(curGraph, c(xaxis,yaxis), directed = FALSE)
            rel <- edge_attr(curGraph, "RELATION", index = edgeID)
            if(rel == "pos"){
              adjMatrix[i,j] <- 1
            }
            else{
              adjMatrix[i,j] <- -1
            }
          }
        }
      }
    }
    adjMatrixList[[index]] <- adjMatrix
    index <- index + 1
  }
  return(adjMatrixList)
}

#Write the functions to make the relevant matrices
#Years should be indexed to refer to year
create_matrices_3 <- function(year1, year2, year3){
  years <- c(year1,year2,year3)
  
  graph1 <- newGraphByYear[[year1]]
  graph2 <- newGraphByYear[[year2]]
  graph3 <- newGraphByYear[[year3]]
  
  graphs <- list(graph1,graph2,graph3)
  
  names1 <- V(graph1)$name
  names2 <- V(graph2)$name
  names3 <- V(graph3)$name
  
  names <- list(names1,names2,names3)
  
  totalNames <- unique(c(names1,names2,names3))
  print(length(totalNames))
  
  #Loop through and populate the matrix
  #Returns of list of combined matrices
  adjMatrixList <- list()
  index <- 1
  for(year in years){
    adjMatrix <- matrix(data = 0, nrow = length(totalNames), ncol = length(totalNames))
    rownames(adjMatrix) <- totalNames
    colnames(adjMatrix) <- totalNames
    curGraph <- graphs[[index]]
    print(index)
    for (i in c(1:length(totalNames))){
      #print(i)
      for (j in c(1:length(totalNames))){
        xaxis <- totalNames[[i]]
        yaxis <- totalNames[[j]]
        if(xaxis %in% names[[index]] && yaxis %in% names[[index]]){
          if(are_adjacent(curGraph,xaxis,yaxis)){
            edgeID <- get.edge.ids(curGraph, c(xaxis,yaxis), directed = FALSE)
            rel <- edge_attr(curGraph, "RELATION", index = edgeID)
            if(rel == "pos"){
              adjMatrix[i,j] <- 1
            }
            else{
              adjMatrix[i,j] <- -1
            }
          }
        }
      }
    }
    adjMatrixList[[index]] <- adjMatrix
    index <- index + 1
  }
  return(adjMatrixList)
}

#Write the functions to make the relevant matrices
#Years should be indexed to refer to year
create_matrices_4 <- function(year1, year2, year3, year4){
  years <- c(year1,year2,year3,year4)
  
  graph1 <- newGraphByYear[[year1]]
  graph2 <- newGraphByYear[[year2]]
  graph3 <- newGraphByYear[[year3]]
  graph4 <- newGraphByYear[[year4]]
  
  graphs <- list(graph1,graph2,graph3,graph4)
  
  names1 <- V(graph1)$name
  names2 <- V(graph2)$name
  names3 <- V(graph3)$name
  names4 <- V(graph4)$name
  
  names <- list(names1,names2,names3,name4)
  
  totalNames <- unique(c(names1,names2,names3,names4))
  print(length(totalNames))
  
  #Loop through and populate the matrix
  #Returns of list of combined matrices
  adjMatrixList <- list()
  index <- 1
  for(year in years){
    adjMatrix <- matrix(data = 0, nrow = length(totalNames), ncol = length(totalNames))
    rownames(adjMatrix) <- totalNames
    colnames(adjMatrix) <- totalNames
    curGraph <- graphs[[index]]
    print(index)
    for (i in c(1:length(totalNames))){
      #print(i)
      for (j in c(1:length(totalNames))){
        xaxis <- totalNames[[i]]
        yaxis <- totalNames[[j]]
        if(xaxis %in% names[[index]] && yaxis %in% names[[index]]){
          if(are_adjacent(curGraph,xaxis,yaxis)){
            edgeID <- get.edge.ids(curGraph, c(xaxis,yaxis), directed = FALSE)
            rel <- edge_attr(curGraph, "RELATION", index = edgeID)
            if(rel == "pos"){
              adjMatrix[i,j] <- 1
            }
            else{
              adjMatrix[i,j] <- -1
            }
          }
        }
      }
    }
    adjMatrixList[[index]] <- adjMatrix
    index <- index + 1
  }
  return(adjMatrixList)
}

#Estimated run time: 2 minute per year
#list2 <- create_matrices_2(1,2)
#list3 <- create_matrices_3(1,2,3)
#list4 <- create_matrices_4(1,2,3,4)

#Code for basic longitudinal analysis
#
longitudinal_analysis <- function(m1, m2){
  #Make Graph for each time
  #Need a scoring method for each tie
  #Prediction for the next tie
  #Regression based off of what variables?
  
}

#Now that matrices are completed need to implement
#Get function from authors

#Ideas*********************************

#Look at modularity between different types of groups
#Burts constraint measure
#constraint(testGraph)
