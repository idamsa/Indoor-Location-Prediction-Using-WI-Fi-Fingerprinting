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
  pacman::p_load(caret,ggplot2,dplyr,lubridate, plotly,readr,rgl,rpart,class,randomForest, kernlab, e1071)
}

# LOADING DATASETS----
trainingData <- read.csv("trainingData.csv", stringsAsFactors = FALSE)
validationData <- read.csv("validationData.csv", stringsAsFactors = FALSE)
trainingData$inTrain <- TRUE
validationData$inTrain <- FALSE
locationData <- rbind(trainingData, trainingData , stringsAsFactors = FALSE) # Building the full Data set. 

#INSPECTING, PREPROCESSING, VISUALIZATIONS----

# Transform Data Types 
locationData [, c("SPACEID","USERID","PHONEID","RELATIVEPOSITION", "BUILDINGID")] <-
  lapply(locationData [, c("SPACEID","USERID","PHONEID","RELATIVEPOSITION","BUILDINGID")], factor) # to factors
locationData$TIMESTAMP <- as.POSIXct(
  as.numeric(locationData$TIMESTAMP),
  origin  =  "1970-01-01",
  tz = "GMT"
)
locationData$FLOOR <- as.character(locationData$FLOOR) # changeDd to char so we can work on the level names will transform to
# factor after

# Get rid of level 0 for building and floor as it migth be misleading and recode the floor to unique
locationData$BUILDINGID <-
  dplyr::recode(
    locationData$BUILDINGID,
    "0" = "1",
    "1" = "2" ,
    "2" = "3"
  )

# 3D Ploting the building have the same shape as campus techno Universitat Jaume I where the dataset was colected
#https://www.google.es/maps/place/Jaume+I+University/@39.9915504,-0.0682044,516a,35y,32.49h,14.15t/data=!3m1!1e3!4m5!3m4!1s0x0:0x1368bf53b3a7fb3f!8m2!3d39.9945711!4d-0.0689003

plot_ly(
  locationData,
  x = ~ LONGITUDE,
  y = ~ LATITUDE,
  z = ~ FLOOR,
  color = ~ BUILDINGID,
  colors = c("#BF382A", "#0C4B8E", "#33FFAA")
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude"),
      zaxis = list(title = "Floor")
    ),
    annotations = list(
      x = 1,
      y = 1,
      text = "Building Number",
      xref = "paper",
      yref = "paper",
      showarrow = FALSE
    ),
    title = "3D plot of the buildings and floors"
    
  )

# Change names for the floors so they are unique for every building (a bit brute forced? maybe theres a better way but this works)
locationData$FLOOR[locationData$BUILDINGID == 1]  <-
  ifelse(
    locationData$FLOOR[locationData$BUILDINGID == 1] == "0",
    "B1F1",
    ifelse(
      locationData$FLOOR[locationData$BUILDINGID == 1] == "1",
      "B1F2",
      ifelse(locationData$FLOOR[locationData$BUILDINGID == 1] == "2", "B1F3", "B1F4")
    )
  )

locationData$FLOOR[locationData$BUILDINGID == 2]  <-
  ifelse(
    locationData$FLOOR[locationData$BUILDINGID == 2] == "0",
    "B2F1",
    ifelse(
      locationData$FLOOR[locationData$BUILDINGID == 2] == "1",
      "B2F2",
      ifelse(locationData$FLOOR[locationData$BUILDINGID == 2] == "2", "B2F3", "B2F4")
    )
  )

locationData$FLOOR[locationData$BUILDINGID == 3]  <-
  ifelse(
    locationData$FLOOR[locationData$BUILDINGID == 3] == "0",
    "B3F1",
    ifelse(
      locationData$FLOOR[locationData$BUILDINGID == 3] == "1",
      "B3F2",
      ifelse(
        locationData$FLOOR[locationData$BUILDINGID == 3] == "2",
        "B3F3",
        ifelse(locationData$FLOOR[locationData$BUILDINGID == 3] == "3", "B3F4", "B3F5")
      )
    )
  )

# Transform the floor to factor
locationData$FLOOR <-
  as.factor(locationData$FLOOR)

# Removing duplicated Rows (#637 duplicated)
sum(duplicated(locationData))
locationData <- distinct(locationData)

# Checking Missing Values
anyNA(locationData) #No Missing Values

# NUMBER samples/ building, floor plot
# The sets don"t look representative, especially in the first floor for all buildings
# also proportions are not exactly right
# for these reasons I chose to recreate the training testing and validations sets further on
ggplot(locationData, aes(x = BUILDINGID, fill = FLOOR)) +
  geom_bar() +
  facet_grid(FLOOR ~ inTrain, labeller = label_both) +
  labs(title = "Sample Sizes", x = "Building", y = "") +
  coord_flip()

# Dfs for plots ????
# df1 <- locationData %>% dplyr::filter(BUILDINGID == 1)
# df2 <- locationData %>% dplyr::filter(BUILDINGID == 2)
# df3 <- locationData %>% dplyr::filter(BUILDINGID == 3)
# 
# p1 <-plot_ly(df1,
#             x = ~ LONGITUDE,
#             y = ~ LATITUDE,
#             z = ~ FLOOR,
#            color = ~inTrain,
#            colors = c("#BF382A", "#0C4B8E")
#                
# )
# p1 

# 100 is code for the no signal values, we will change them to -110 and then bring them all to positive
# so the values make sense so 0 = no signal and the higher the number the higer the signal
WAPS <-
  grep("WAP", names(locationData), value = T) #gets all the wap names
locationData[, WAPS] <-
  sapply(locationData[, WAPS], function(x)
    ifelse(x == 100, -105, x)) # changes all values of 100(no signal) to 105
locationData[, WAPS] <-
  locationData[, WAPS] + 105 # flips all waps to positive values

# Filter WAPS that have all 0 signal for the full row and remove them (it means that no user connected to this wap)
# and the ones with near zero variance
locationData <- filter(locationData[which(rowSums(locationData[,WAPS])!=0),])  

# Add features highest, lowest  signal column and number of waps connected to
locationData$HIGHESTSIGNAL <- apply(locationData[, 1:465], 1, function(x)
  max(x))
locationData$LOWESTSIGNAL <- apply(locationData[, 1:465], 1, function(x)
  min(x[x > 0]))
locationData$NUMBERCONNECTIONS <- apply(locationData[, 1:465], 1, function(x)
  sum(x > 0))

# Drop the unnecessary columns and df to clean the enviroment
locationData [, c("SPACEID", "USERID", "PHONEID","RELATIVEPOSITION", "TIMESTAMP", "inTrain")] <- list(NULL)
rm(WAPS, trainingData, validationData)
# DATA SPLIT ----
# We do this again because the initial split was not representative as we so in the plots above
# Also we added the validation dataset in order to better evaluate the models
indicesTraining <-
  createDataPartition(locationData$BUILDINGID, p = 0.6, list = FALSE)
dfTraining <-
  locationData[indicesTraining, ] # Training Test 60 %
dfLeftoverTraining <-
  locationData[-indicesTraining, ]
indicesTest <-
  createDataPartition(dfLeftoverTraining$BUILDINGID, p = 0.5, list = FALSE)
dfTest <- dfLeftoverTraining[indicesTest, ] # Test Test 20 % of total
dfValidation <-
  dfLeftoverTraining[-indicesTest, ] # Validation Test 20 % Total

# STANDARDIZING DATA FOR DISTANCE BASED MODELS ----
# Saving the waps in a vector
WAPs<-grep("WAP", names(locationData), value=T)

# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(locationData[WAPs], method=c("center", "scale"))

# transform the waps using the parameters
stand_waps <- predict(preprocessParams, locationData[WAPs])

# complete dataset
stand_dataset <- cbind(stand_waps, BUILDINGID=locationData$BUILDINGID, LONGITUDE=locationData$LONGITUDE, LATITUDE = locationData$LATITUDE,
                       HIGHESTSIGNAL = locationData$HIGHESTSIGNAL, FLOOR = locationData$FLOOR, LOWESTSIGNAL = locationData$LOWESTSIGNAL, NUMBERCONNECTIONS = locationData$NUMBERCONNECTIONS) 

stand_dataset <- filter(stand_dataset[ - which(apply(stand_dataset[WAPs], 2, var) == 0)])

# DATA SPLIT STANDARDIZED DATA ----
indicesTrainingS <-
  createDataPartition(stand_dataset$BUILDINGID, p = 0.6, list = FALSE)
dfTrainingStand <-
  stand_dataset[indicesTrainingS, ] # Training Test 60 %
dfLeftoverTrainingS <-
  stand_dataset[-indicesTrainingS, ]
indicesTestS <-
  createDataPartition(dfLeftoverTrainingS$BUILDINGID, p = 0.5, list = FALSE)
dfTestStand <- dfLeftoverTrainingS[indicesTestS, ] # Test Test 20 % of total
dfValidationStand <-
  dfLeftoverTrainingS[-indicesTestS, ] # Validation Test 20 % Total


# A. MODELS FOR BUILDING ----
set.seed(123)

# 1. KNN BUILDING ----
# We will remove the floor, lat and long from the training as they will be unknown for the actual data
# Check results on test dataset

system.time(knnFitTest <- knn(train = dfTrainingStand[ , -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                              test = dfTestStand[ , -which(names(dfTestStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                              cl = dfTrainingStand$BUILDINGID, k=2
)
)
# user  system elapsed 
# 86.42    0.16   90.21 
print(knnCMT <- confusionMatrix(knnFitTest, dfTestStand$BUILDINGID)) # Confusion Matrix

# Check results on validation dataset
knnFitValid <- knn(train=dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                   test=dfValidationStand[ , -which(names(dfValidationStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                   cl=dfTrainingStand$BUILDINGID, k=2)
print(knnCMV <- confusionMatrix(knnFitValid, dfValidationStand$BUILDINGID)) # Confusion Matrix

# Optimize KNN --- ACC AND KAPPA GO DOWN AFTER K=3 BOTH IN TEST AND VALID
# Decided to keep the nr of neighbours 2 # used this function for the other knn models too
i=1
k.optm=1
for (i in 1:9){
  knn.mod <- knn(train = dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                 test = dfValidationStand[, -which(names(dfValidationStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                 cl = dfTrainingStand$BUILDINGID, k=i)
  k.optm[i] <- 100 * sum(dfValidationStand$BUILDINGID ==  knn.mod)/NROW(dfValidationStand$BUILDINGID)
  k=i
  cat(k,"=",k.optm[i],"
      ")
}

# 2. SVM BUILDING (CHOSEN) ----
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
system.time(svmFit <- train(BUILDINGID ~ ., 
                            data = dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                            method ="svmLinear",
                            trControl = ctrl
)
) 
# user  system elapsed 
# 16.07    1.16   17.98 
# Check results on validation dataset # 99 % acc  kappa 0.9897  
svmTest <- predict(svmFit ,newdata = dfTestStand[, -which(names(dfTestStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))])
print(svmCMT <- confusionMatrix(svmTest, dfTestStand$BUILDINGID)) # Confusion Matrix

# Check results on validation dataset # 99 % acc kappa 0.9897 
svmValid <- predict(svmFit, newdata = dfValidationStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))])
print(svmCMV <- confusionMatrix(svmValid, dfValidationStand$BUILDINGID)) # Confusion Matrix

# Saving Model
saveRDS(svmFit, file = "svmBuilding.rds")

# B. MODELS FOR FLOOR ----

# 1. KNN FLOOR ---- 
# We will remove the lat and long from the training as they will be unknown for the actual data 
# On the blind dataset the building will be added from the previous prediction that has close to 100 % ACC 

# Check results on test dataset Accuracy : 0.9951 ,  Kappa : 0.9936  
set.seed(123)
system.time(knnFitFloorTest <- knn(train = dfTrainingStand[ , -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE"))], 
                                   test  = dfTestStand[ , -which(names(dfTestStand) %in% c("LONGITUDE","LATITUDE"))],
                                   cl    = dfTrainingStand$FLOOR, k=5
)
)
# user  system elapsed 
# 86.35    0.16   89.20 
print(knnCMTFloor <- confusionMatrix(knnFitFloorTest, dfTestStand$FLOOR)) # Confusion Matrix

# Check results on validation dataset   Accuracy : 0.9934 , Kappa : 0.9913    
system.time(knnFitFloorValid <- knn(train = dfTrainingStand[ , -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE"))], 
                                    test  = dfValidationStand[ , -which(names(dfValidationStand) %in% c("LONGITUDE","LATITUDE"))],
                                    cl    = dfTrainingStand$FLOOR, k=5
)
)
# user  system elapsed 
# 82.61    0.14   83.77 
print(knnCMVFloor <- confusionMatrix(knnFitFloorValid, dfValidationStand$FLOOR)) # Confusion Matrix

# 2. SVM FLOOR (CHOSEN) ----
set.seed(123)
system.time(svmFitFloor <- train(FLOOR ~ ., 
                                 data      = dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE"))],
                                 method    = "svmLinear",
                                 trControl = ctrl
)
)
# user  system elapsed 
# 33.89    2.20   37.53 

# Check results on test dataset  Accuracy : 0.9932   ,   Kappa : 0.9926 
svmTestFloor <- predict(svmFitFloor, newdata = dfTestStand[, -which(names(dfTestStand) %in% c("LONGITUDE","LATITUDE"))])
print(svmCMTFloor <- confusionMatrix(svmTestFloor , dfTestStand$FLOOR)) # Confusion Matrix

# Check results on validation dataset Accuracy : 0.9932   ,Kappa : 0.9926   
svmValidFloor <- predict(svmFitFloor ,newdata = dfValidationStand[, -which(names(dfValidationStand) %in% c("LONGITUDE","LATITUDE"))])
print(svmCMVFloor <- confusionMatrix(svmValidFloor, dfValidationStand$FLOOR)) # Confusion Matrix

# Save Model
saveRDS(svmFitFloor, file="svmFloor.rds")

# C. MODELS FOR LATITUDE ----
# When using on the unseen data we will add the building and floor from previous pred and use them too
# for predicting the latitude 
set.seed(123)
ctrlRegression <- trainControl(method        = "repeatedcv",
                               number        = 10,
                               repeats       = 3,
                               allowParallel = TRUE
)

# 1. RANDOM FOREST LATITUDE (CHOSEN) ----
# 
# # bestmtry_rf <- tuneRF(dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE"))],
#                       dfTraining$LATITUDE[, -which(names(dfTraining) %in% c("LONGITUDE"))],
#                       ntreeTry=100,
#                       stepFactor=2,
#                       improve=0.05,
#                       trace=TRUE, 
#                       plot=T)
# 
# #system.time(rfFitLatitude <- randomForest(x = dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE"))],
#                                           y = dfTraining$LATITUDE, 
#                                           ntrees = 100,
#                                           importance = T,
#                                           mtry = 88
# #)
# )
# user  system elapsed 
# 752.86    1.36  771.61 

# Bring model 

rfFitLatitude <- readRDS("rfLatitudeFinal.rds")

# Predict and evaluate on Test
rfTestLatitude <- predict(rfFitLatitude, dfTest[, -which(names(dfTest) %in% c("LONGITUDE"))])
print(postResample_rfTestLatitude <- postResample(rfTestLatitude, dfTest$LATITUDE))

# RMSE  Rsquared       MAE 
# 4.2609324 0.9962189 3.0925014 

# Predict and evaluate on Validation
rfValidLatitude <- predict(rfFitLatitude, dfValidation[, -which(names(dfValidation) %in% c("LONGITUDE"))])
print(postResample_rfValidLatitude <- postResample(rfValidLatitude, dfValidation$LATITUDE))

# RMSE  Rsquared       MAE 
# 4.3369444 0.9959796 3.1199783 

# Save absolute errors
errors_latitude_rfTest <- as.data.frame(dfTest$LATITUDE - rfTestLatitude)
errors_latitude_rfValid <- as.data.frame(dfValidation$LATITUDE - rfValidLatitude)

# Save Model
# saveRDS(rfFitLatitude, file = "rfLatitude.rds")

# 2. KNN LATITUDE ----
system.time(knnFitLatitude <- knnreg(LATITUDE~.,
                                     data = dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE"))])
)

# Check results on test dataset  
knnTestLatitude <- predict(knnFitLatitude, dfTestStand[, -which(names(dfTestStand) %in% c("LONGITUDE"))])
print(postResample_knnTestLatitude <- postResample(knnTestLatitude,dfTestStand$LATITUDE))
# RMSE  Rsquared       MAE 
# 4.5850294 0.9953791 2.3328006 

# Check results on validation dataset        
knnValidLatitude <- predict(knnFitLatitude, dfValidationStand[, -which(names(dfValidationStand) %in% c("LONGITUDE"))])
print(postResample_knnValidLatitude <- (postResample(knnValidLatitude ,dfValidationStand$LATITUDE)))
# RMSE  Rsquared       MAE 
# 5.1371152 0.9942487 2.4594966 

# Save absolute errors
errors_latitude_knnTest <- as.data.frame(dfTestStand$LATITUDE - knnTestLatitude)
errors_latitude_knnValid <- as.data.frame(dfValidationStand$LATITUDE - knnValidLatitude)


# D. MODELS FOR LONGITUDE ----
# THESE MODELS HAVE VERY GOOD PERFORMANCE WHEN USING THE REAL VERIFIED DATA BUT ONCE WE ADD THE PREDICTED VALUES + ERRORS
# THE PERFORMANCE WILL NOT BE AS GOOD
set.seed(123)
# 1. RANDOM FOREST LONGITUDE (CHOSEN)----

# bestmtry_rf <- tuneRF(dfTraining, dfTraining$LONGITUDE, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)
# 
# # mtry OOBError
# # 88    88 5.536374
# # 175  175 5.743512
# # 350  350 7.065408
# 
# system.time(rfFitLongitude <- randomForest(x = dfTraining,
#                                            y = dfTraining$LONGITUDE, 
#                                            ntrees = 100,
#                                            importance = T,
#                                            mtry = 88
# )
# )

# user  system elapsed 
# 684.86    1.73  711.69 

# Bring Model

rfFitLongitude <- readRDS("rfLongitudeFinal.rds")

# Predict and evaluate on Test
rfTestLongitude <- predict(rfFitLongitude, dfTest)
print(postResample_rfTestLongitude <- postResample(rfTestLongitude, dfTest$LONGITUDE))
# RMSE  Rsquared       MAE 
# 5.1949389 0.9982786 3.8511694

# Predict and evaluate on Validation
rfValidLongitude <- predict(rfFitLongitude, dfValidation)
print(postResample_rfValidLongitude <- postResample(rfValidLongitude, dfValidation$LONGITUDE))
# RMSE  Rsquared       MAE 
# 5.0700276 0.9983662 3.7734408 

# Save absolute errors
errors_longitude_rfTest <- as.data.frame(dfTest$LONGITUDE - rfTestLongitude)
errors_longitude_rfValid <- as.data.frame(dfValidation$LONGITUDE - rfValidLongitude)

# Save Model
saveRDS(rfFitLongitude, file = "rfLongitude.rds")

# 2. KNN LONGITUDE  ----
system.time(knnFitLongitude <- knnreg(LONGITUDE~.,
                                      data = dfTrainingStand)
)

# Check results on test dataset  
knnTestLongitude <- predict(knnFitLongitude, dfTestStand)
print(postResample_knnTestLongitude <- postResample(knnTestLongitude,dfTestStand$LONGITUDE))
# RMSE  Rsquared       MAE 
# 3.9097830 0.9990115 1.6463896 
# Check results on validation dataset        
knnValidLongitude <- predict(knnFitLongitude, dfValidationStand)
print(postResample_knnValidLongitude <- postResample(knnValidLongitude ,dfValidationStand$LONGITUDE))
# RMSE  Rsquared       MAE 
# 4.1370858 0.9988972 1.6966097 

# Save absolute errors
errors_longitude_knnTest <- as.data.frame(dfTestStand$LONGITUDE - knnTestLongitude)
errors_longitude_knnValid <- as.data.frame(dfValidationStand$LONGITUDE - knnValidLongitude)

# Save model
saveRDS(knnFitLongitude, file = "knnLongitude.rds")


# ERROR ANALYSIS ----      
# pLOTTING THE ERRORS FOR LONGITUDE Random Forest 
)
plot_ly(dfTest, x = ~dfTest$LONGITUDE, y = ~rfTestLongitude,
        type   = "scatter",
        color  = ~ dfTest$BUILDINGID, 
        colors = c("blue", "pink","green")) %>%
  layout(title = "Errors in Longitude for the Test set Random Forest")

# Longitude errors distribution

plot_ly(alpha = 0.6) %>%
  add_histogram(x = errors_longitude_rfTest$`dfTest$LONGITUDE - rfTestLongitude`, name = "Random Forest Errors") %>%
  add_histogram(x = errors_longitude_knnTest$`dfTestStand$LONGITUDE - knnTestLongitude`, name = "KNN Errors") %>%
  layout(barmode = "overlay",
         title = 'Model Error Distribution Longitude',
         legend = list(x = 0.1, y = 0.9))

# Latitude Errors distribution

plot_ly(alpha = 0.6) %>%
add_histogram(x = errors_latitude_rfTest$`dfTest$LATITUDE - rfTestLatitude`, name = "Random Forest Errors") %>%
  add_histogram(x = errors_latitude_knnTest$`dfTestStand$LATITUDE - knnTestLatitude`, name = "KNN Errors") %>%
  layout(barmode = "overlay",
         title = 'Model Error Distribution Latitude',
         legend = list(x = 0.1, y = 0.9))

# Plot test vs pred longitude latitude

dfTest2 <- dfTest
dfTest2 <- cbind(dfTest2,rfTestLongitude,rfTestLatitude)
dfTest2 <- cbind(dfTest2, diffs$meh)
diffs <- as.data.frame(cbind(dfTest$LONGITUDE,rfTestLongitude))
diffs$same <- diffs$V1 - diffs$rfTestLongitude
diffs$meh <- as.factor(ifelse((diffs$same < 5 & diffs$same > -5) ,"Error +/- 5 m Longitude","Error bigger than 5 m Longitude"))
dfTest2 <- cbind(dfTest2, diffs$meh)
p1 <- plot_ly(
  dfTest2,
  x = ~ rfTestLongitude,
  y = ~ rfTestLatitude,
  z = ~ FLOOR,
  color = dfTest2$`diffs$meh`,
  colors = c("red","blue")
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude"),
      zaxis = list(title = "Floor")
    ),

    title = "3D plot of the buildings and floors"
    
  )

p2 <- plot_ly(
  dfTest2,
  x = ~ LONGITUDE,
  y = ~ LATITUDE,
  z = ~ FLOOR,
  name = "Real",
  color = "purple"
  ) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude"),
      zaxis = list(title = "Floor")
    ),
    annotations = list(
      x = 1,
      y = 1,
      text = "Real Test vs Predicted Test Latitude and Longitude",
      showarrow = FALSE,
      name = "aa"
    ),
    title = "3D plot of the buildings and floors"
    
  )

subplot(p1,p2)
#PIPELINE FOR THE BLIND DATASET ----

