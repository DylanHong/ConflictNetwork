#rm(list=ls())

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
names1 <- df$ACTOR1
names2 <- df$ACTOR2
names <- list(names1,names2)
totalNames <- unique(c(names1,names2))

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

newdf <- query_year(df,2000,2004,2008,2012)
namesList <- names_list(newdf)


create_matrices_new <- function(df,y1,y2,y3,y4){
  y1 <- 2000
  y2 <- 2004
  y3 <- 2008
  y4 <- 2012
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
    
    tempdf <- df[df$YEAR == year,]
    
    for (index in 1:nrow(tempdf)) { 
      x = tempdf[index, ]
      if(x[[4]] == "pos"){
        name1 <- x[[1]]
        name2 <- x[[2]]
        allyMatrix[name1,name2] <- 1
        allyMatrix[name2,name1] <- 1
      } 
      else{
        name1 <- x[[1]]
        name2 <- x[[2]]
        conflictMatrix[name1,name2] <- 1
        conflictMatrix[name2,name1] <- 1
      }
    }
    
    
    # apply(tempdf,1,function(x){
    #   if(x[4] == "pos"){
    #     name1 <- x[1]
    #     print(name1)
    #     name2 <- x[[2]]
    #     allyMatrix[name1,name2] <- 1
    #     allyMatrix[name2,name1] <- 1
    #   } 
    #   else{
    #     name1 <- x[[1]]
    #     name2 <- x[[2]]
    #     conflictMatrix[name1,name2] <- 1
    #     conflictMatrix[name2,name1] <- 1
    #   }
    # })
    
    allyMatrixList[[counter]] <- allyMatrix
    print(counter)
    conflictMatrixList[[counter]] <- conflictMatrix
    print(counter)
    counter <- counter + 1
  }
  rand <- list()
  rand[[1]] <- allyMatrixList
  rand[[2]] <- conflictMatrixList
  return(rand)
}

godList <- create_matrices_new(df,2000,2004,2008,2012)

godList1 <- godList[[1]]
godList2 <- godList[[2]]


row_check <- function(mconflict,mally,df1,row){
  apply(df1,1,function(row){
    
  })
}

#Get the dataframe sorted by years
dataByYear <- list()
for (i in c(1997:2018)){
  yearTemp <- finalAC[finalAC$YEAR == i,]
  dataByYear[[i-1996]] <- yearTemp
}

####  Reading Data
''
# Read the van de Bunt data.
# For meaning and codes, consult the Siena website or the Siena manual.
setwd("/Users/junzhao/Downloads/vdBunt_data")
library(RSiena)
vdb.w0 <- as.matrix(read.table("VRND32T0.DAT"))
vdb.w1 <- as.matrix(read.table("VRND32T1.DAT"))
vdb.w2 <- as.matrix(read.table("VRND32T2.DAT"))
vdb.w3 <- as.matrix(read.table("VRND32T3.DAT"))
vdb.w4 <- as.matrix(read.table("VRND32T4.DAT"))
vdb.w5 <- as.matrix(read.table("VRND32T5.DAT"))
vdb.w6 <- as.matrix(read.table("VRND32T6.DAT"))
vdb.attr <- as.matrix(read.table("VARS.DAT"))

# Recode 6 and 9 to missing.

vdb.w0[vdb.w0 %in% c(6,9)] <- NA
vdb.w1[vdb.w1 %in% c(6,9)] <- NA
vdb.w2[vdb.w2 %in% c(6,9)] <- NA
vdb.w3[vdb.w3 %in% c(6,9)] <- NA
vdb.w4[vdb.w4 %in% c(6,9)] <- NA
vdb.w5[vdb.w5 %in% c(6,9)] <- NA
vdb.w6[vdb.w6 %in% c(6,9)] <- NA

# Recode 4 (acquaintance) and 5 (difficult) to no tie

vdb.w0[vdb.w0 %in% c(4,5)] <- 0
vdb.w1[vdb.w1 %in% c(4,5)] <- 0
vdb.w2[vdb.w2 %in% c(4,5)] <- 0
vdb.w3[vdb.w3 %in% c(4,5)] <- 0
vdb.w4[vdb.w4 %in% c(4,5)] <- 0
vdb.w5[vdb.w5 %in% c(4,5)] <- 0
vdb.w6[vdb.w6 %in% c(4,5)] <- 0

# Networks with ordered values for the ties are represented
# in RSiena by using with different dichotomizations of the
# valued networks.
# This leads to multiple dependent networks
# that satisfy order relations.
# The network constructed now has tie values 0, 1, 2, 3.
# The "friendly relation" with values {1, 2, 3}
# is obtained by recoding:

vdb.w0.th3 <- vdb.w0
vdb.w1.th3 <- vdb.w1
vdb.w2.th3 <- vdb.w2
vdb.w3.th3 <- vdb.w3
vdb.w4.th3 <- vdb.w4
vdb.w5.th3 <- vdb.w5
vdb.w6.th3 <- vdb.w6
vdb.w0.th3[vdb.w0 %in% c(1,2,3)] <- 1
vdb.w1.th3[vdb.w1 %in% c(1,2,3)] <- 1
vdb.w2.th3[vdb.w2 %in% c(1,2,3)] <- 1
vdb.w3.th3[vdb.w3 %in% c(1,2,3)] <- 1
vdb.w4.th3[vdb.w4 %in% c(1,2,3)] <- 1
vdb.w5.th3[vdb.w5 %in% c(1,2,3)] <- 1
vdb.w6.th3[vdb.w6 %in% c(1,2,3)] <- 1

# The "friend" relation for values {1, 2} by recoding:

vdb.w0.th2 <- vdb.w0.th3
vdb.w1.th2 <- vdb.w1.th3
vdb.w2.th2 <- vdb.w2.th3
vdb.w3.th2 <- vdb.w3.th3
vdb.w4.th2 <- vdb.w4.th3
vdb.w5.th2 <- vdb.w5.th3
vdb.w6.th2 <- vdb.w6.th3

vdb.w0.th2[vdb.w0 == 3] <- 0
vdb.w1.th2[vdb.w1 == 3] <- 0
vdb.w2.th2[vdb.w2 == 3] <- 0
vdb.w3.th2[vdb.w3 == 3] <- 0
vdb.w4.th2[vdb.w4 == 3] <- 0
vdb.w5.th2[vdb.w5 == 3] <- 0
vdb.w6.th2[vdb.w6 == 3] <- 0

# We do not use the "close friend" relation here, because there are
# very few threshold crossings between values 1 and 2.
# This can be checked by the print01Report for this relation.
#
# We use waves 2-5.
# (Using waves 2-4 leads to a reciprocity effect for the
# friendly/friend threshold with a large standard error;
# there is evidence for reciprocity, but the parameter estimate
# is not very well determined by the data.)

library(RSiena)
friendly2345 <- sienaDependent(
  array(c(vdb.w2.th3, vdb.w3.th3, vdb.w4.th3, vdb.w5.th3),
        dim=c(32, 32, 4)))
friends2345  <- sienaDependent(
  array(c(vdb.w2.th2 ,vdb.w3.th2, vdb.w4.th2, vdb.w5.th2),
        dim=c(32, 32, 4)))

# Attributes:
sex          <- coCovar(vdb.attr[,1])
program      <- coCovar(vdb.attr[,2])
smoke        <- coCovar(vdb.attr[,3])

# Data set and effects:
vdb.ordered2345 <- sienaDataCreate(friendly2345, friends2345,
                                   sex, program, smoke)
##############
myCoEvolutionEff <- getEffects(vdb.ordered2345) 

# network dynamic (inPopSqrt)
myCoEvolutionEff <- includeEffects(myCoEvolutionEff, transTrip,transTies, cycle3) 
myCoEvolutionEff <- includeEffects(myCoEvolutionEff, name = "friendly2345", egoX, altX, simX,
                                   interaction1="sex" )

myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "friendly2345", from, interaction1 = "friends2345" )
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "friendly2345", to, interaction1 = "friends2345" )
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "friendly2345", sharedIn, interaction1 = "friends2345" )
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, name = "friendly2345", cl.XWX, interaction1 = "friends2345" )

#myCoEvolutionEff <- includeInteraction(myCoEvolutionEff,effFrom, totSim, name = "depressionbeh", interaction1 = c("sex", "friendship" ) )
myCoEvolutionEff

myCoEvAlgorithm <- sienaAlgorithmCreate(projname = 'All girls-w35-dep4-7', seed =1568)
GroupsModel <- sienaModelCreate(projname = 'All girls-w35-dep4-7')
modelTEST <- siena07(myCoEvAlgorithm, data = vdb.ordered2345, effects = myCoEvolutionEff)
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
