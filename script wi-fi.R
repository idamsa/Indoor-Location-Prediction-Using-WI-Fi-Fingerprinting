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
  pacman::p_load(caret,ggplot2,dplyr,lubridate, plotly,readr,rgl,rpart,class,randomForest)
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
locationData <- filter(locationData[which(rowSums(locationData[,WAPS])!=0),]) 

# Add features highest, lowest  signal column and number of waps connected to
locationData$HIGHESTSIGNAL <- apply(locationData[, 1:521], 1, function(x)
  max(x))
locationData$LOWESTSIGNAL <- apply(locationData[, 1:521], 1, function(x)
  min(x[x > 0]))
locationData$NUMBERCONNECTIONS <- apply(locationData[, 1:521], 1, function(x)
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

# MODELS FOR BUILDING ----
set.seed(123)
# 1. KNN # 100% ON TEST and VALID , kappa 1 and Time is Bearable ~ 1 minute
# We will remove the floor, lat and long from the training as they will be unknown for the actual data
# Check results on test dataset
system.time(knnFitTest <- knn(train = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                              test = dfTest[ , -which(names(dfTest) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                              cl = dfTraining$BUILDINGID, k=3
)
)
# user  system elapsed 
# 86.42    0.16   90.21 
print(knnCMT <- confusionMatrix(knnFitTest, dfTest$BUILDINGID)) # Confusion Matrix

# Check results on validation dataset
knnFitValid <- knn(train=dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                   test=dfValidation[ , -which(names(dfValidation) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                   cl=dfTraining$BUILDINGID, k=3)
print(knnCMV <- confusionMatrix(knnFitValid, dfValidation$BUILDINGID)) # Confusion Matrix

# Optimize KNN --- ACC AND KAPPA GO DOWN AFTER K=3 BOTH IN TEST AND VALID
# Decided to keep the nr of neighbours 3 # used this function for the other knn models too
i=1
k.optm=1
for (i in 1:9){
  knn.mod <- knn(train = dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                 test = dfValidation[, -which(names(dfValidation) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                 cl = dfTraining$BUILDINGID, k=i)
  k.optm[i] <- 100 * sum(dfValidation$BUILDINGID ==  knn.mod)/NROW(dfValidation$BUILDINGID)
  k=i
  cat(k,"=",k.optm[i],"
      ")
}

# 2. SVM # 100 % faster than KNN
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
system.time(svmFit <- train(BUILDINGID ~ ., 
                            data=dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                            method="svmLinear",
                            trControl = ctrl
)
) 
# user  system elapsed 
# 16.07    1.16   17.98 
# Check results on validation dataset # 99 % acc  kappa 0.9897  
svmTest <- predict(svmFit ,newdata = dfTest[, -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))])
print(svmCMT <- confusionMatrix(svmTest, dfTest$BUILDINGID)) # Confusion Matrix

# Check results on validation dataset # 99 % acc kappa 0.9897 
svmValid <- predict(svmFit, newdata = dfValidation[, -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))])
print(svmCMV <- confusionMatrix(svmValid, dfValidation$BUILDINGID)) # Confusion Matrix

# Saving Model
saveRDS(svmFit, file = "svmBuilding.rds")

# MODELS FOR FLOOR ----
# 1. KNN # 100 on test 99 on valid beareable time 
# We will remove the lat and long from the training as they will be unknown for the actual data 
# On the blind dataset the building will be added from the previous prediction that has close to 100 % ACC 

# Check results on test dataset Accuracy : 0.9951 ,  Kappa : 0.9936  
set.seed(123)
system.time(knnFitFloorTest <- knn(train = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE"))], 
                                   test = dfTest[ , -which(names(dfTest) %in% c("LONGITUDE","LATITUDE"))],
                                   cl = dfTraining$FLOOR, k=5
)
)
# user  system elapsed 
# 86.35    0.16   89.20 
print(knnCMTFloor <- confusionMatrix(knnFitFloorTest, dfTest$FLOOR)) # Confusion Matrix

# Check results on validation dataset   Accuracy : 0.9934 , Kappa : 0.9913    
system.time(knnFitFloorValid <- knn(train = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE"))], 
                                    test = dfValidation[ , -which(names(dfValidation) %in% c("LONGITUDE","LATITUDE"))],
                                    cl = dfTraining$FLOOR, k=5
)
)
# user  system elapsed 
# 82.61    0.14   83.77 
print(knnCMVFloor <- confusionMatrix(knnFitFloorValid, dfValidation$FLOOR)) # Confusion Matrix

# 2. SVM #Longer than KNN
set.seed(123)
system.time(svmFitFloor <- train(FLOOR~., 
                                 data=dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE"))],
                                 method="svmLinear",
                                 trControl = ctrl
)
)
# user  system elapsed 
# 33.89    2.20   37.53 

# Check results on test dataset  Accuracy : 0.9872  ,   Kappa : 0.986   
svmTestFloor <- predict(svmFitFloor, newdata = dfTest[, -which(names(dfTest) %in% c("LONGITUDE","LATITUDE"))])
print(svmCMTFloor <- confusionMatrix(svmTestFloor , dfTest$FLOOR)) # Confusion Matrix

# Check results on validation dataset Accuracy : 0.9855   , Kappa : 0.9841 
svmValidFloor <- predict(svmFitFloor ,newdata = dfValidation[, -which(names(dfValidation) %in% c("LONGITUDE","LATITUDE"))])
print(svmCMVFloor <- confusionMatrix(svmValidFloor, dfValidation$FLOOR)) # Confusion Matrix

# Save Model
saveRDS(svmFitFloor, file="svmFloor.rds")

# MODELS FOR LATITUDE ----
# When using on the unseen data we will add the building and floor from previous pred and use them too
# for predicting the latitude 
set.seed(123)
ctrlRegression <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 3
)

# RANDOM FOREST (CHOSEN)
system.time(rfFitLatitude <- randomForest(x = dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE"))],
                                          y = dfTraining$LATITUDE, 
                                          ntrees = 1000,
                                          importance = T,
                                          mtry=16
)
)
# user  system elapsed 
# 752.86    1.36  771.61 

# Predict and evaluate on Test
rfTestLatitude <- predict(rfFitLatitude, dfTest)
print(postResample_rfTestLatitude <- postResample(rfTestLatitude, dfTest$LATITUDE))

# RMSE  Rsquared       MAE 
# 4.8442056 0.9951363 3.4163620 

# Predict and evaluate on Validation
rfValidLatitude <- predict(rfFitLatitude, dfValidation)
print(postResample_rfValidLatitude <- postResample(rfValidLatitude, dfValidation$LATITUDE))

# RMSE Rsquared      MAE 
# 4.816284 0.995293 3.384434 

# Save absolute errors
errors_latitude_rf <- cbind((as.data.frame(dfTest$LATITUDE - rfTestLatitude)),(as.data.frame(dfValidation$LATITUDE - rfValidLatitude)))

# Save Model
saveRDS(rfFitLatitude, file = "rfLatitude.rds")

# SVM
set.seed(123)
system.time(svmFitLatitude <- train(LATITUDE ~ .,
                                    data = dfTraining[,- which(names(dfTraining) %in% c("LONGITUDE"))],
                                    method ="svmLinear",
                                    trControl = ctrlRegression
) 
)

# user  system elapsed 
# 103.47    0.49  106.09 

# Check results on Test dataset  
svmTestLatItude <- predict(svmFitLatitude, newdata = dfTest[, -which(names(dfTraining) %in% c("LONGITUDE"))])
postResample(svmTestLatItude, dfTest$LATITUDE)

# RMSE   Rsquared        MAE 
# 32.2325515  0.7783443 26.3834116 

# Check results on validation dataset 
svmValidLatitude <- predict(svmFitLatitude, newdata = dfValidation[, -which(names(dfTraining) %in% c("LONGITUDE"))])
postResample(svmValidLatitude, dfValidation$LATITUDE)

# RMSE   Rsquared        MAE 
# 32.1597242  0.7829318 26.2658431 

# Save Model
saveRDS(svmFitLatitude, file = "svmLatitude.rds")

# MODELS FOR LONGITUDE ----
# THESE MODELS HAVE VERY GOOD PERFORMANCE WHEN USING THE REAL VERIFIED DATA BUT ONCE WE ADD THE PREDICTED VALUES + ERRORS
# THE PERFORMANCE WILL NOT BE AS GOOD
set.seed(123)
# RANDOM FOREST
system.time(rfFitLongitude <- randomForest(x = dfTraining[ ,-which(names(dfTraining) %in% c("LATITUDE"))],
                                           y = dfTraining$LONGITUDE, 
                                           ntrees = 500,
                                           importance = T,
                                           mtry = 16
)
)

# user  system elapsed 
# 192.12    0.70  212.90

# Predict and evaluate on Test
rfTestLongitude <- predict(rfFitLongitude, dfTest)
print(postResample_rfTestLongitude <- postResample(rfTestLongitude, dfTest$LONGITUDE))
# RMSE Rsquared      MAE 
# 8.306966 0.995620 1.992130 

# Predict and evaluate on Validation
rfValidLongitude <- predict(rfFitLongitude, dfValidation)
print(postResample_rfValidLongitude <- postResample(rfValidLongitude, dfValidation$LONGITUDE))
# RMSE  Rsquared       MAE 
# 7.4855428 0.9964204 1.8185214

# Save absolute errors
errors_latitude_rf <- cbind((as.data.frame(dfTest$LONGITUDE - rfTestLongitude)), (as.data.frame(dfValidation$LONGITUDE - rfValidLongitude)))

# Save Model
saveRDS(rfFitLongitude, file = "rfLongitude.rds")

# SVM
system.time(svmFitLongitude <- train(LONGITUDE~.,
                                     data = dfTraining[, which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","BUILDINGID","FLOOR","HIGHESTSIGNAL","LOWESTSIGNAL","NUMBERCONNECTION"))],
                                     method = "svmLinear",
                                     trControl = ctrlRegression
)
) 
# user  system elapsed 
# 133.39    0.45  134.10 

# Check results on validation dataset  
svmTestLongitude <- predict(svmFitLongitude, newdata = dfTest)
postResample(svmTestLongitude, dfTest$LONGITUDE)
# RMSE   Rsquared        MAE 
# 33.4489216  0.9289858 27.3299243 

# Check results on validation dataset # 99 % acc kappa 0.9897 
svmValidLongitude <- predict(svmFitLongitude ,newdata = dfValidation)
postResample(svmValidLongitude, dfValidation$LONGITUDE)
# RMSE   Rsquared        MAE 
# 33.9755611  0.9262403 27.8674068
# Save Model
saveRDS(svmFitLongitude, file = "svmLongitude.rds")

# pLOTTING THE ERRORS FOR LONGITUDE Random Forest
plot_ly(dfTest, x = ~dfTest$LONGITUDE, y = ~rfTestLongitude,
        type   = "scatter",
        color  = ~ dfTest$BUILDINGID, 
        colors = c("blue", "pink","green")) %>%
  layout(title = "Errors in Longitude for the Test set Random Forest")

#PIPELINE FOR THE BLIND DATASET ----

