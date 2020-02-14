# INDOOR POSITION PREDICTION---------------------------------------------------------------------------------------------
# Location prediction Based on Wireless Application Protocol (WAPS) Fingerprinting
# Objective: Predict location (building no, floor, latitude and longitude) of a person based on the connection and
# strength of the signal of the connection to the WASPS.
# LIBRARIES ----

setwd("~/Wi Fi Location")
if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
} else{
  library(pacman)
  rm(list = ls(all = TRUE))
  p_unload(pacman::p_loaded(), character.only = TRUE)
  pacman::p_load(caret,ggplot2,dplyr,lubridate, plotly,readr,rpart,class,randomForest, kernlab, e1071, matlab, plyr)
}

# LOADING DATASETS----
trainingData <- read.csv("trainingData.csv")
validationData <- read.csv("validationData.csv")
trainingData$inTrain <- TRUE
validationData$inTrain <- FALSE
locationData <- rbind(trainingData, validationData) # Building the full Data set. 

#INSPECTING, PREPROCESSING, VISUALIZATIONS ----

# Transform Data Types 
locationData[1:520]<- sapply(locationData[1:520],as.numeric)
locationData [, c("SPACEID","USERID","PHONEID","RELATIVEPOSITION", "BUILDINGID")] <- lapply(locationData [, c("SPACEID","USERID","PHONEID","RELATIVEPOSITION", "BUILDINGID")], factor) # to factors
locationData$TIMESTAMP <- as.POSIXct(as.numeric(locationData$TIMESTAMP),origin  =  "1970-01-01",tz = "GMT")

# Removing duplicated Rows (#637 duplicated)
locationData <- distinct(locationData)

# Get rid of level 0 for building and floor as it migth be misleading and recode the floor to unique
locationData$BUILDINGID <-dplyr::recode(
  locationData$BUILDINGID,
  "0" = "1",
  "1" = "2" ,
  "2" = "3"
)

# Remove near zero variance columns
WAPS_VarTrain <- nearZeroVar(locationData[locationData$inTrain == T, 1:520], saveMetrics = TRUE)
WAPS_VarValid <- nearZeroVar(locationData[locationData$inTrain == F, 1:520], saveMetrics = TRUE)
locationData <- locationData[ - which(WAPS_VarTrain$zeroVar==TRUE | WAPS_VarValid$zeroVar == TRUE)]
