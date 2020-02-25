# INDOOR POSITION PREDICTION---------------------------------------------------------------------------------------------
# Location prediction Based on Wireless Application Protocol (WAPS) Fingerprinting
# Objective: Predict location (building no, floor, latitude and longitude) of a person based on the connection and
# strength of the signal of the connection to the WASPS.
# LIBRARIES ----

if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
} else{
  library(pacman)
  rm(list = ls(all = TRUE))
  p_unload(pacman::p_loaded(), character.only = TRUE)
  pacman::p_load(caret,ggplot2,dplyr,lubridate, plotly,readr,rpart,class,randomForest, kernlab, e1071, matlab, plyr)
}

# Get the location of the current script in order to be perfectly transferable
# The variable "loc" contains full paths to any file with the same name as the
# current script.
# The "iloc", gets the current index inside the vector of potential matches.
# The "myloc" gets the full path of the script.
loc   <- grep("Open and Preprocess.R",list.files(recursive=TRUE),value=TRUE)
iloc  <- which(unlist(gregexpr("/Open and Preprocess.R$",loc)) != -1)
myloc <- paste(getwd(),loc[iloc],sep="/")
# We set the working directory to the path of the script.
setwd(substr(myloc,1,nchar(myloc)-nchar("Open and Preprocess.R")))

# LOADING DATASETS----
trainingData <- read.csv("trainingData.csv")
validationData <- read.csv("validationData.csv")
trainingData$inTrain <- TRUE
validationData$inTrain <- FALSE

#### Ignacio: Ioana, why do you merge both datasets? This shoud be justified.
locationData <- rbind(trainingData, validationData) # Building the full Data set. 

#INSPECTING, PREPROCESSING, VISUALIZATIONS ----

# Transform Data Types 
locationData [, c("SPACEID","USERID","PHONEID","RELATIVEPOSITION", "BUILDINGID","FLOOR")] <- lapply(locationData [, c("SPACEID","USERID","PHONEID","RELATIVEPOSITION", "BUILDINGID","FLOOR")], factor) # to factors
locationData$TIMESTAMP <- as.POSIXct(as.numeric(locationData$TIMESTAMP),origin  =  "1970-01-01",tz = "GMT")

# Removing duplicated Rows (#637 duplicated)
locationData <- distinct(locationData)

# Remove near zero variance columns
WAPS_VarTrain <- nearZeroVar(locationData[locationData$inTrain == T, 1:520], saveMetrics = TRUE)
WAPS_VarValid <- nearZeroVar(locationData[locationData$inTrain == F, 1:520], saveMetrics = TRUE)
locationData <- locationData[ - which(WAPS_VarTrain$zeroVar==TRUE | WAPS_VarValid$zeroVar == TRUE)]
