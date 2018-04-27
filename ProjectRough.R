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

# #Drop the ID columns
# drops <- c("ACTOR1_ID","ACTOR2_ID", "ACTOR_DYAD_ID", "EVENT_ID_CNTY",
#            "EVENT_ID_NO_CNTY")
# AC <- AC[ , !(names(AC) %in% drops)]

#Seperate the allies into multiple columns if there are more than 1
#NA if there is no allies
ally1 <- c("Ally_1a", "Ally_1b", "Ally_1c", "Ally_1d","Ally_1e", "Ally_1f",
           "Ally_1g")
ally2 <- c("Ally_2a", "Ally_2b", "Ally_2c", "Ally_2d","Ally_2e", "Ally_2f")
AC <- AC %>% separate(ASSOC_ACTOR_1, ally1, sep = ";", fill = "right")
AC <- AC %>% separate(ASSOC_ACTOR_2, ally2, sep = ";", fill = "right")

# results <- list()
# lapply(ally1, function(row){
#   print(row)
#   #results[[row]] <- as.vector(unique(AC[row]))
# })
# print(results)


#Get all the unique actors
actor1 <- as.vector(unique(AC$ACTOR1))
actor2 <- as.vector(unique(AC$ACTOR2))

ally1a <- as.vector(unique(AC$Ally_1a))
ally1b <- as.vector(unique(AC$Ally_1b))
ally1c <- as.vector(unique(AC$Ally_1c))
ally1d <- as.vector(unique(AC$Ally_1d))
ally1e <- as.vector(unique(AC$Ally_1e))
ally1f <- as.vector(unique(AC$Ally_1f))
ally1g <- as.vector(unique(AC$Ally_1g))

ally2a <- as.vector(unique(AC$Ally_2a))
ally2b <- as.vector(unique(AC$Ally_2b))
ally2c <- as.vector(unique(AC$Ally_2c))
ally2d <- as.vector(unique(AC$Ally_2d))
ally2e <- as.vector(unique(AC$Ally_2e))
ally2f <- as.vector(unique(AC$Ally_2f))

actortot <- unique(c(actor1,actor2))
actor1tot <- unique(c(ally1a,ally1b,ally1c,ally1d,ally1e,ally1f,ally1g))
actor2tot <- unique(c(ally2a,ally2b,ally2c,ally2d,ally2e,ally2f))

actorfinal <- unique(c(actortot,actor1tot,actor2tot))

length(actorfinal)
sort(actorfinal)

actordict <- c(1:length(actorfinal))
names(actordict) <- actorfinal

names(actorfinal) <- nums

smalltest <- data.frame("actor1" = AC$ACTOR1,
                        "actor2" = AC$ACTOR2,
                        "edge1" = AC$LOCATION,
                        "edge2" = AC$YEAR)

smalltest <- smalltest[complete.cases(smalltest),]

testGraph <- graph_from_data_frame(smalltest, directed = FALSE)

igraph::summary(testGraph)

vcount(testGraph)
ecount(testGraph)
edge_attr_names(testGraph)


x11()
plot(testGraph)
