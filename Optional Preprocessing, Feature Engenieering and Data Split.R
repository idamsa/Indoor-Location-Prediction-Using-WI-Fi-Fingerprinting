source("Open and Preprocess.R")

# OPTIONAL PREPROCESS AND FEATURE ENGENIEERING ---- DATA SPLIT

# 100 is code for the no signal values, we will change them to -110 and then bring them all to positive
# so the values make sense so 0 = no signal and the higher the number the higer the signal

WAPS <-grep("WAP", names(locationData), value = T) #gets all the wap names
locationData[, WAPS] <- sapply(locationData[, WAPS], function(x) ifelse(x == 100, -105, x)) # changes all values of 100(no signal) to 105
locationData[, WAPS] <- locationData[, WAPS] + 105 # flips all waps to positive values

# Filter WAPS that have all 0 signal for the full row and remove them (it means that no user connected to this wap)
# and the ones with near zero variance
locationData <- filter(locationData[which(rowSums(locationData[,WAPS])!=0),])

# Add features highest, lowest  signal column and number of waps connected to 
locationData$HIGHESTSIGNAL <- apply(locationData[1:312], 1, function(x)max(x))
locationData$LOWESTSIGNAL <- apply(locationData[1:312], 1, function(x)min(x[x > 0]))
locationData$NUMBERCONNECTIONS <- apply(locationData[1:312], 1, function(x)sum(x > 0))

# removing the very high and low values of signal 

for (i in 1:nrow(locationData)){
  if ((locationData$HIGHESTSIGNAL[i] < 47) | (locationData$HIGHESTSIGNAL[i] > 100)){
    locationData$HIGHESTSIGNAL[i] <- NA}
}

# Removing rows in which the highest signal is too low or high
locationData <- locationData[complete.cases(locationData), ]

# Drop the columns we won't use for the models and df to clean the enviroment
locationData [, c("SPACEID", "USERID", "PHONEID","RELATIVEPOSITION", "TIMESTAMP", "inTrain")] <- list(NULL)

# DATA SPLIT ----
# We do this again because the initial split was not representative as we so in the plots above
# Also we added the validation dataset in order to better evaluate the models
indicesTraining <-createDataPartition(locationData$BUILDINGID, p = 0.6, list = FALSE)
dfTraining <-locationData[indicesTraining, ] # Training Test 60 %
dfLeftoverTraining <-locationData[-indicesTraining, ]
indicesTest <-createDataPartition(dfLeftoverTraining$BUILDINGID, p = 0.5, list = FALSE)
dfTest <- dfLeftoverTraining[indicesTest, ] # Test Test 20 % of total
dfValidation <-dfLeftoverTraining[-indicesTest, ] # Validation Test 20 % Total

# STANDARDIZING DATA FOR DISTANCE BASED MODELS ----

# Saving the waps in a vector
WAPs<-grep("WAP", names(locationData), value=T)
preprocessParams <- preProcess(locationData[WAPs], method=c("center", "scale")) # calculate the pre-process parameters from the dataset
stand_waps <- predict(preprocessParams, locationData[WAPs]) # transform the waps using the parameters

# complete dataset
stand_dataset <- cbind(stand_waps, BUILDINGID=locationData$BUILDINGID, LONGITUDE=locationData$LONGITUDE, LATITUDE = locationData$LATITUDE,
                       HIGHESTSIGNAL = locationData$HIGHESTSIGNAL, FLOOR = locationData$FLOOR, LOWESTSIGNAL = locationData$LOWESTSIGNAL, NUMBERCONNECTIONS = locationData$NUMBERCONNECTIONS) 


# DATA SPLIT STANDARDIZED DATA ----
indicesTrainingS <-createDataPartition(stand_dataset$BUILDINGID, p = 0.6, list = FALSE)
dfTrainingStand <-stand_dataset[indicesTrainingS, ] # Training Test 60 %
dfLeftoverTrainingS <-stand_dataset[-indicesTrainingS, ]
indicesTestS <-createDataPartition(dfLeftoverTrainingS$BUILDINGID, p = 0.5, list = FALSE)
dfTestStand <- dfLeftoverTrainingS[indicesTestS, ] # Test Test 20 % of total
dfValidationStand <- dfLeftoverTrainingS[-indicesTestS, ] # Validation Test 20 % Total

# clean env
rm(WAPS, trainingData, validationData, WAPS_VarTrain,WAPS_VarValid, i,dfLeftoverTraining,dfLeftoverTrainingS,indicesTest,indicesTestS,indicesTraining,indicesTrainingS,preprocessParams,WAPs,stand_waps)
