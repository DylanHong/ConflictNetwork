rm(list=ls())

#Import necessary libraries
library(readr)
library(igraph)
#install.packages("tidyr")
library(tidyr)
library(dplyr)
#remove.packages("RSiena")
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

df <- finalAC
df <- df[,c(1,2,8,14)]
df <- unique(df)
df <- df[df$ACTOR1 != df$ACTOR2,]

query_year <- function(df,y1,y2,y3,y4){
  tempdf <- df[df$YEAR == y1 | df$YEAR == y2 | df$YEAR == y3 | df$YEAR == y4,]
  return(tempdf)
}

names_list <- function(df){
  names1 <- df$ACTOR1
  names2 <- df$ACTOR2
  names <- list(names1,names2)
  totalNames <- unique(c(names1,names2))
  return(totalNames)
}

create_matrices_new <- function(df,y1,y2,y3,y4){
  years <- c(y1,y2,y3,y4)
  
  newdf <- query_year(df,y1,y2,y3,y4)
  namesList <- names_list(newdf)
  
  allyMatrixList <- list()
  conflictMatrixList <- list()
  counter <- 1
  for(year in years){
    allyMatrix <- matrix(data = 0, nrow = length(namesList), ncol = length(namesList))
    conflictMatrix <- matrix(data = 0, nrow = length(namesList), ncol = length(namesList))  
    rownames(allyMatrix) <- namesList
    colnames(allyMatrix) <- namesList
    rownames(conflictMatrix) <- namesList
    colnames(conflictMatrix) <- namesList
    
    tempdf <- newdf[newdf$YEAR == year,]
    
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

masterList <- create_matrices_new(df,2011,2013,2015,2017)

masterAlly <- masterList[[1]]
masterConflict <- masterList[[2]]

allyt1 <- masterAlly[[1]]
allyt2 <- masterAlly[[2]]
allyt3 <- masterAlly[[3]]
allyt4 <- masterAlly[[4]]

conflictt1 <- masterConflict[[1]]
conflictt2 <- masterConflict[[2]]
conflictt3 <- masterConflict[[3]]
conflictt4 <- masterConflict[[4]]

len <- masterList[[3]]

# We do not use the "close friend" relation here, because there are
# very few threshold crossings between values 1 and 2.
# This can be checked by the print01Report for this relation.
#
# We use waves 2-5.
# (Using waves 2-4 leads to a reciprocity effect for the
# friendly/friend threshold with a large standard error;
# there is evidence for reciprocity, but the parameter estimate
# is not very well determined by the data.)

allySiena <- sienaDependent(array(c(allyt1, allyt2, allyt3, allyt4),
        dim=c(len, len, 4)))
conflictSiena  <- sienaDependent(array(c(conflictt1 ,conflictt2, conflictt3, conflictt4),
        dim=c(len, len, 4)))

# Attributes:
# sex          <- coCovar(vdb.attr[,1])
# program      <- coCovar(vdb.attr[,2])
# smoke        <- coCovar(vdb.attr[,3])

# Data set and effects:
vdb.ordered2345 <- sienaDataCreate(conflictSiena, allySiena)
##############
myCoEvolutionEff <- getEffects(vdb.ordered2345) 

# network dynamic (inPopSqrt)
myCoEvolutionEff <- includeEffects(myCoEvolutionEff, transTrip) 
#myCoEvolutionEff <- includeEffects(myCoEvolutionEff, name = "allySiena", egoX, altX, simX, interaction1="sex" )

myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "allySiena", from, interaction1 = "conflictSiena" )
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "allySiena", to, interaction1 = "conflictSiena" )
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "allySiena", sharedIn, interaction1 = "conflictSiena")
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "allySiena", cl.XWX, interaction1 = "conflictSiena" )

myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "conflictSiena", from, interaction1 = "allySiena" )
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "conflictSiena", to, interaction1 = "allySiena" )
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "conflictSiena", sharedIn, interaction1 = "allySiena",type = "dyadic" )
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "conflictSiena", cl.XWX, interaction1 = "allySiena" )

#myCoEvolutionEff <- includeInteraction(myCoEvolutionEff,effFrom, totSim, name = "depressionbeh", interaction1 = c("sex", "friendship" ) )
myCoEvolutionEff

myCoEvAlgorithm <- sienaAlgorithmCreate(projname = 'PleaseForTheLoveOfGodWork', seed =1568,
                                        n3 = 300)
GroupsModel <- sienaModelCreate(projname = 'PleaseForTheLoveOfGodWork')
modelTEST <- siena07(myCoEvAlgorithm, data = vdb.ordered2345, effects = myCoEvolutionEff,
                     useCluster=TRUE, nbrNodes=4)
summary(modelTEST)

modelTEST2 <- siena07(myCoEvAlgorithm, data = vdb.ordered2345, effects = myCoEvolutionEff, 
                      prevAns=modelTEST, returnDeps=T) # specify starting values, return simulated nets (for GOF)
myResults2  # examine results

parameter <- modelTEST$effects$effectName
estimate <- modelTEST $theta
st.error <- sqrt(diag(modelTEST $covtheta))
normal.variate <- estimate/st.error

p.value.2sided <- 2*pnorm(abs(normal.variate),lower.tail=FALSE)

data.frame(parameter,
           estimate=round(estimate,3),
           st.error=round(st.error,3),
           normal.variate=round(normal.variate,2),
           p.value=round(p.value.2sided,4)
)
#################
