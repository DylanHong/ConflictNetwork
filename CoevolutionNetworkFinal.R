rm(list=ls())

#Import necessary libraries
library(readr)
library(igraph)
#install.packages("tidyr")
library(tidyr)
library(dplyr)
#install.packages("RSiena")
library(RSiena)

#Read in the raw CSV
AC <- read_csv("data/ACLED_Africa.csv")

#Quick overview of the data
class(AC)
#summary(AC)

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

#Further clearning of dataframe
df <- finalAC
df <- df[,c(1,2,8,14)]
df <- unique(df)
df <- df[df$ACTOR1 != df$ACTOR2,]

#Querys four specific years of a given dataframe
query_year <- function(df,y1,y2,y3,y4){
  tempdf <- df[df$YEAR == y1 | df$YEAR == y2 | df$YEAR == y3 | df$YEAR == y4,]
  return(tempdf)
}

#Gets a list of all names in a given dataframe
names_list <- function(df){
  names1 <- df$ACTOR1
  names2 <- df$ACTOR2
  names <- list(names1,names2)
  totalNames <- unique(c(names1,names2))
  return(totalNames)
}

#Creates static adjacency matrices given a dataframe and four years
#Returns a list of three lists
#List entry one contains the four ally matrices
#List entry two contains the four conflict matrices
#List entry three contains the number of rows/columns
create_matrices_new <- function(df,y1,y2,y3,y4){
  #List of all the years
  years <- c(y1,y2,y3,y4)
  
  #Make a new dataframe with years of interests and get names
  newdf <- query_year(df,y1,y2,y3,y4)
  namesList <- names_list(newdf)
  
  #Initialize lists to store matrices
  allyMatrixList <- list()
  conflictMatrixList <- list()
  
  #Loop through all the years of interest
  counter <- 1
  for(year in years){
    #Create ally and conflict adjancecy matrices and name the rows and columns
    allyMatrix <- matrix(data = 0, nrow = length(namesList), ncol = length(namesList))
    conflictMatrix <- matrix(data = 0, nrow = length(namesList), ncol = length(namesList))  
    rownames(allyMatrix) <- namesList
    colnames(allyMatrix) <- namesList
    rownames(conflictMatrix) <- namesList
    colnames(conflictMatrix) <- namesList
    
    #Single out the year of interest to fill the two matrices
    tempdf <- newdf[newdf$YEAR == year,]
    
    #Populate the matrices in n time
    #Implicit directionality of conflict and ally
    for (index in 1:nrow(tempdf)) { 
      x <- tempdf[index, ]
      name1 <- x[[1]]
      name2 <- x[[2]]
      if(x[[4]] == "pos"){
        #allyMatrix[name1,name2] <- 1
        allyMatrix[name2,name1] <- 1
      } 
      else{
        conflictMatrix[name1,name2] <- 1
        #conflictMatrix[name2,name1] <- 1
      }
    }
    allyMatrixList[[counter]] <- allyMatrix
    print(counter)
    conflictMatrixList[[counter]] <- conflictMatrix
    print(counter)
    counter <- counter + 1
  }
  rand <- list()
  rand[[1]] <- allyMatrixList
  rand[[2]] <- conflictMatrixList
  rand[[3]] <- length(namesList)
  return(rand)
}

#Create matrices for the given year
masterList <- create_matrices_new(df,2002,2004,2006,2008)

masterAlly <- masterList[[1]]
masterConflict <- masterList[[2]]

#Get individual ally matrices for each year
allyt1 <- masterAlly[[1]]
allyt2 <- masterAlly[[2]]
allyt3 <- masterAlly[[3]]
allyt4 <- masterAlly[[4]]

#Get individual conflict matrices for each year
conflictt1 <- masterConflict[[1]]
conflictt2 <- masterConflict[[2]]
conflictt3 <- masterConflict[[3]]
conflictt4 <- masterConflict[[4]]

#Dimension of the matrices
len <- masterList[[3]]


#Create ally dependent variable
allySiena <- sienaDependent(array(c(allyt1, allyt2, allyt3, allyt4),
                                  dim=c(len, len, 4)))
#Create conflict dependent variable
conflictSiena  <- sienaDependent(array(c(conflictt1 ,conflictt2, conflictt3, conflictt4),
                                       dim=c(len, len, 4)))

#Data set and effects:
coevoData <- sienaDataCreate(conflictSiena, allySiena)

#Get preliminary report of the data
print01Report(coevoData, modelname = "coevoProject")

coevoEffects <- getEffects(coevoData) 

#Add desired effects
coevoEffects <- includeEffects(coevoEffects, transTrip, name = "allySiena")
coevoEffects <- includeEffects(coevoEffects, inPopSqrt, name = "conflictSiena")

#coevoEffects <- includeEffects( coevoEffects, name = "allySiena", from, interaction1 = "conflictSiena" )
#coevoEffects <- includeEffects( coevoEffects, name = "allySiena", to, interaction1 = "conflictSiena" )
#coevoEffects <- includeEffects( coevoEffects, name = "allySiena", sharedIn, interaction1 = "conflictSiena")
#coevoEffects <- includeEffects( coevoEffects, name = "allySiena", cl.XWX, interaction1 = "conflictSiena" )

#Multiplex Triadic Network Effects
coevoEffects <- includeEffects( coevoEffects, name = "conflictSiena", from, interaction1 = "allySiena" )
coevoEffects <- includeEffects( coevoEffects, name = "conflictSiena", to, interaction1 = "allySiena" )
coevoEffects <- includeEffects( coevoEffects, name = "conflictSiena", sharedIn, interaction1 = "allySiena")
coevoEffects <- includeEffects( coevoEffects, name = "conflictSiena", cl.XWX, interaction1 = "allySiena" )

coevoEffects

#Create algorithm
coevoAlgorithm <- sienaAlgorithmCreate(projname = 'coevoProject2', seed =123, n3 = 500)

#Save Model
GroupsModel <- sienaModelCreate(projname = 'coevoProject2')

#Run the Siena Model
coevoModel <- siena07(coevoAlgorithm, data = coevoData, effects = coevoEffects, 
                     useCluster=TRUE, nbrNodes=6)
summary(coevoModel)

#Second Model if needed with specific starting values
#coevoModel2 <- siena07(myCoEvAlgorithm, data = coevoData, effects = coevoEffects, 
#                      prevAns=modelTEST, returnDeps=T, useCluster = TRUE, nbrNodes = 4) 


#Get the pvalues and standard errors for the model
parameter <- coevoModel$effects$effectName
estimate <- coevoModel$theta
st.error <- sqrt(diag(coevoModel$covtheta))
normal.variate <- estimate/st.error

p.value.2sided <- 2*pnorm(abs(normal.variate),lower.tail=FALSE)

data.frame(parameter,
           estimate=round(estimate,3),
           st.error=round(st.error,3),
           normal.variate=round(normal.variate,2),
           p.value=round(p.value.2sided,4)
)

