### Location prediction Based on Wireless Application Protocol (WAPS)
pacman::p_load(caret,ggplot2,dplyr,lubridate, plotly,readr,rgl,rpart,class,randomForest)

# LOADING DATASETS--------------------------------------------------------------------------
df_location_train <- read.csv("trainingData.csv")
df_location_validation <- read.csv("validationData.csv")
df_location_train$IsTrainSet <- TRUE
df_location_validation$IsTrainSet <- FALSE
df_location_train_and_valid <- rbind(df_location_train,df_location_validation )

### Dataset Info 

# 001 (WAP001): Intensity value for WAP001. Negative integer values from -104 to 0 and +100. Positive value 100 used if WAP001 was not detected.
#....
#Attribute 520 (WAP520): Intensity value for WAP520. Negative integer values from -104 to 0 and +100. Positive Vvalue 100 used if WAP520 was not detected.
#Attribute 521 (Longitude): Longitude. Negative real values from -7695.9387549299299000 to -7299.786516730871000
#Attribute 522 (Latitude): Latitude. Positive real values from 4864745.7450159714 to 4865017.3646842018.
#Attribute 523 (Floor): Altitude in floors inside the building. Integer values from 0 to 4.
#Attribute 524 (BuildingID): ID to identify the building. Measures were taken in three different buildings. Categorical integer values from 0 to 2.
#Attribute 525 (SpaceID): Internal ID number to identify the Space (office, corridor, classroom) where the capture was taken. Categorical integer values.
#Attribute 526 (RelativePosition): Relative position with respect to the Space (1 - Inside, 2 - Outside in Front of the door). Categorical integer values.
#Attribute 527 (UserID): User identifier (see below). Categorical integer values.
#Attribute 528 (PhoneID): Android device identifier (see below). Categorical integer values.
#Attribute 529 (Timestamp): UNIX Time when the capture was taken. Integer value.

#INSPECTING, PREPROCESSING,VISUALIZATIONS-----------------------------------------------------------------
# Inspecting the dataset
str(df_location_train_and_valid [521:529])

# Transform Data Types and levels
df_location_train_and_valid [523:528] <- lapply(df_location_train_and_valid [523:528], factor) 
df_location_train_and_valid $TIMESTAMP <- as.POSIXct(as.numeric(df_location_train_and_valid$TIMESTAMP), origin = '1970-01-01', tz = 'GMT')
df_location_train_and_valid$FLOOR <- recode(df_location_train_and_valid$FLOOR,  "0" = "1", "1"="2" , "2"="3" , "3"="4", "4" = "5")
df_location_train_and_valid$BUILDINGID <- recode(df_location_train_and_valid$BUILDINGID, "0" = "1", "1"="2" , "2"="3")


# Removing duplicated Rows
sum(duplicated(df_location_train_and_valid)) #637 duplicated
sum(duplicated(df_location_validation)) # all the duplicated rows are in test so we will remove them 
df_location_train_and_valid <- distinct(df_location_train_and_valid)

# Checking Missing Values
sum(is.na(df_location_train_and_valid)) #No Missing Values

#  Ploting the building have the same shape as campus techno Universitat Jaume I where the dataset was colected
#https://www.google.es/maps/place/Jaume+I+University/@39.9915504,-0.0682044,516a,35y,32.49h,14.15t/data=!3m1!1e3!4m5!3m4!1s0x0:0x1368bf53b3a7fb3f!8m2!3d39.9945711!4d-0.0689003
#lat and long are in EPSG:3857 WGS 84 / Pseudo-Mercator Coord System 
ggplot(df_location_train_and_valid, aes(LONGITUDE,LATITUDE,colour=BUILDINGID)) +
  geom_density_2d()+
  scale_colour_discrete(drop=TRUE,
                        limits = levels(df_location_train_and_valid$BUILDINGID))+
  labs(title ="Plotted location")

# 3D plot
plot_ly(df_location_train_and_valid, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~BUILDINGID, colors = c('#BF382A', '#0C4B8E',"#33FFAA")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Longitude'),
                      yaxis = list(title = 'Latitude'),
                      zaxis = list(title = 'Floor')),
         annotations = list(x = 1,
                            y = 1,
                            text = 'Building Number',
                            xref = 'paper',
                            yref = 'paper',
                            showarrow = FALSE),
         title="3D plot of the buildings and floors"
         
  )

# NUMBER samples/ building, floor
ggplot(df_location_train_and_valid, aes(x=BUILDINGID, fill=FLOOR)) +
  geom_bar()+
  facet_grid(FLOOR~IsTrainSet,labeller = label_both)+
  labs(title ="Sample Sizes", x = "Building", y = "")


# 100 is the no signal values, we will change them to -110
WAPS <- grep("WAP", names(df_location_train_and_valid), value=T) #gets all the waps
df_location_train_and_valid[,WAPS] <- sapply(df_location_train_and_valid[,WAPS],function(x) ifelse(x==100,-105,x)) # changes all 100 to 105
df_location_train_and_valid[,WAPS] <- df_location_train_and_valid[,WAPS] + 105 # flips all waps to positive values

#Select  WAPS that have all 0 signal for all location and remove them
WAPS_VarTrain <- nearZeroVar(df_location_train_and_valid[df_location_train_and_valid$IsTrainSet==TRUE,WAPS], saveMetrics=TRUE)
WAPS_VarValid <- nearZeroVar(df_location_train_and_valid[df_location_train_and_valid$IsTrainSet==FALSE,WAPS], saveMetrics=TRUE)
df_location_train_and_valid <- df_location_train_and_valid[-which(WAPS_VarTrain$zeroVar==TRUE | 
                                                                    WAPS_VarValid$zeroVar==TRUE)]  

#Remove rows with no variance
# New WAPS
WAPS <- grep("WAP", names(df_location_train_and_valid), value=T) #gets all the waps remaining after removing the 0 ones

# Filter Rows with all Signals = 0
df_location_train_and_valid <- df_location_train_and_valid %>% 
  filter(apply(df_location_train_and_valid[WAPS], 1, function(x)length(unique(x)))>1)

# Add closest wap column
df_location_train_and_valid$CLOSESTWAP = apply(df_location_train_and_valid[,1:312], 1, function(x) names(x)[which.max(x)])
df_location_train_and_valid$CLOSESTWAP <- as.numeric(df_location_train_and_valid$CLOSESTWAP )
# Add highest signal column
df_location_train_and_valid$HIGHESTSIGNAL = apply(df_location_train_and_valid[,1:312], 1, function(x) max(x))


# Drop the unnecessary columns and df
df_location_train_and_valid [ ,c('SPACEID', 'USERID','PHONEID','RELATIVEPOSITION','TIMESTAMP','IsTrainSet')] <- list(NULL)
rm(WAPS,WAPS_VarTrain, WAPS_VarValid)

# Building the sets for TRAIN,TEST AND VALIDATION # We do this because the initial split was not representative
indicesTraining <- createDataPartition(df_location_train_and_valid$BUILDINGID, p = 0.6, list = FALSE)
dfTraining <- df_location_train_and_valid[ indicesTraining,] # Training Test 60 %
dfLeftoverTraining <- df_location_train_and_valid[ -indicesTraining,]
indicesTest <- createDataPartition(dfLeftoverTraining $BUILDINGID, p = 0.5, list = FALSE)
dfTest <- dfLeftoverTraining[indicesTest,] # Test Test 20 % of total
dfValidation <- dfLeftoverTraining[-indicesTest,] # Validation Test 20 % Total

# MODELS FOR BUILDING-----------------------------------------------------------------------------------
set.seed(123)
# 1. KNN # 100% ON TEST and VALID , kappa 1 and Time is Bearable ~ 1 minute
# We will remove the floor, lat and long from the training as they will be unknown for the actual data
# Check results on test dataset
system.time(knnFitTest <- knn(train = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                              test = dfTest[ , -which(names(dfTest) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                              cl = dfTraining$BUILDINGID, k=3))
print(knnCMT <- confusionMatrix(knnFitTest, dfTest$BUILDINGID)) # Confusion Matrix

# Check results on validation dataset
knnFitValid <- knn(train=dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))], test=dfValidation[ , -which(names(dfValidation) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                   cl=dfTraining$BUILDINGID, k=3)
print(knnCMV <- confusionMatrix(knnFitValid, dfValidation$BUILDINGID)) # Confusion Matrix

# Optimize KNN --- ACC AND KAPPA GO DOWN AFTER K=3 BOTH IN TEST AND VALID
# Decided to keep the nr of neighbours 3
i=1
k.optm=1
for (i in 1:7){
  knn.mod <- knn(train = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                 test = dfValidation[ , -which(names(dfValidation) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                 cl = dfTraining$BUILDINGID, k=i)
  k.optm[i] <- 100 * sum(dfValidation$BUILDINGID ==  knn.mod)/NROW(dfValidation$BUILDINGID)
  k=i
  cat(k,'=',k.optm[i],'
      ')
}

# 2. SVM 
set.seed(123)
ctrl <- trainControl(method="cv", number=10)
system.time(svmFit <- train(BUILDINGID~., data=dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                            method='svmLinear',
                            trControl = ctrl)) 

# Check results on validation dataset # 99 % acc  kappa 0.9897  
svmTest <- predict(svmFit ,newdata = dfTest[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))])
print(svmCMT <- confusionMatrix(svmTest , dfTest$BUILDINGID)) # Confusion Matrix

# Check results on validation dataset # 99 % acc kappa 0.9897 
svmValid <- predict(svmFit ,newdata = dfValidation[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))])
print(svmCMV <- confusionMatrix(svmValid, dfValidation$BUILDINGID)) # Confusion Matrix

# Saving Model
saveRDS(svmFit, file="svmBuilding.rds")


# MODELS FOR FLOOR-------------------------------------------------------------------------

# 1. KNN # 100 on test 99 on valid beareable time 
# We will remove the lat and long from the training as they will be unknown for the actual data 
# On the blind dataset the building will be added from the previous prediction that has 100 % ACC 

# Check results on test dataset 100 % KAPPA 1
set.seed(123)
system.time(knnFitFloorTest <- knn(train = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE"))], 
                                   test = dfTest[ , -which(names(dfTest) %in% c("LONGITUDE","LATITUDE"))],
                                   cl = dfTraining$FLOOR, k=5))
print(knnCMTFloor <- confusionMatrix(knnFitFloorTest, dfTest$FLOOR)) # Confusion Matrix

# Check results on validation dataset  99 % KAPPA 0.9987  
system.time(knnFitFloorValid <- knn(train = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE"))], 
                                    test = dfValidation[ , -which(names(dfValidation) %in% c("LONGITUDE","LATITUDE"))],
                                    cl = dfTraining$FLOOR, k=5))
print(knnCMVFloor <- confusionMatrix(knnFitFloorValid , dfValidation$FLOOR)) # Confusion Matrix

# 2. SVM #Longer than KNN
set.seed(123)
svmFitFloor <- train(FLOOR~., data=dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE"))],
                     method='svmLinear',
                     trControl = ctrl)

# Check results on test dataset 98,6 % KAPPA 0.9822  
svmTestFloor <- predict(svmFitFloor ,newdata = dfTest[ , -which(names(dfTest) %in% c("LONGITUDE","LATITUDE"))])
print(svmCMTFloor <- confusionMatrix(svmTestFloor , dfTest$FLOOR)) # Confusion Matrix

# Check results on validation dataset 98,7 % KAPPA 0.9825 
svmValidFloor <- predict(svmFitFloor ,newdata = dfValidation[ , -which(names(dfValidation) %in% c("LONGITUDE","LATITUDE"))])
print(svmCMVFloor <- confusionMatrix(svmValidFloor, dfValidation$FLOOR)) # Confusion Matrix

# Save Model
saveRDS(svmFitFloor, file="svmFloor.rds")

#MODELS FOR LATITUDE-------------------------------------------------------------------------------
set.seed(123)

# RANDOM FOREST
system.time(rfFitLatitude <- randomForest(y = dfTraining$LATITUDE, 
                                          x = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE"))],
                                          ntrees = 2))

# Predict and evaluate on Test
rfTestLatitude <- predict(rfFitLatitude, dfTesting)
postResample_rfTestLatitude <- postResample(rfTestLatitude, dfTesting$LATITUDE)

# Predict and evaluate on Validation
rfValidLatitude <- predict(rfFitLatitude, dfValidation)
postResample_rfValidLatitude <- postResample(rfValidLatitude, dfValidation$LATITUDE)

# Save absolute errors
errors_latitude_rf <- cbind((as.data.frame(dfTesting$LATITUDE - rfTestLatitude)),(as.data.frame(dfValidation$LATITUDE - rfValidLatitude)))

# Save Model
saveRDS(rfFitLatitude, file="rfLatitude.rds")

# SVM
set.seed(123)
system.time(svmFitLatitude <- train(LATITUDE~., data=dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE"))],
                                    method='svmLinear',
                                    trControl = ctrl)) 

# Check results on validation dataset # 99 % acc  kappa 0.9897  
svmTestLatItude <- predict(svmFitLatitude ,newdata = dfTest[ , -which(names(dfTraining) %in% c("LONGITUDE"))])
postResample(svmTestLatItude, dfTest$LATITUDE)

# Check results on validation dataset # 99 % acc kappa 0.9897 
svmValidLatitude <- predict(svmFitLatitude ,newdata = dfValidation[ , -which(names(dfTraining) %in% c("LONGITUDE"))])
postResample(svmvalidLatitude, dfValid$LATITUDE)

# Save Model
saveRDS(svmFitLatitude, file="svmLatitude.rds")

# KNN

# Check results on test dataset 
set.seed(123)
system.time(knnFitLatitudeTest <- knn(train = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE"))], 
                                      test = dfTest[ , -which(names(dfTest) %in% c("LONGITUDE"))],
                                      cl = dfTraining$LATITUDE, k=5))
postResample(knnFitLatitudeTest,dfTest$LATITUDE)

# Check results on validation dataset  99 % KAPPA 0.9987  
system.time(knnFitLatitudeValid <- knn(train = dfTraining[ , -which(names(dfTraining) %in% c("LONGITUDE"))], 
                                       test = dfValidation[ , -which(names(dfValidation) %in% c("LONGITUDE"))],
                                       cl = dfTraining$LATITUDE, k=5))
postResample(knnFitLatitudeValid,dfTest$LATITUDE)

#MODELS FOR LONGITUDE------------------------------------------------------------------------------
set.seed(123)
# RANDOM FOREST
system.time(rfFitLongitude <- randomForest(y = dfTraining$LONGITUDE, 
                                           x = dfTraining[ ,which(names(dfTraining) %in% c("LATITUDE","BUILDINGID","FLOOR","CLOSESTWAP","HIGHESTSIGNAL"))],
                                           ntrees = 500))
                                           

# Predict and evaluate on Test
rfTestLongitude <- predict(rfFitLongitude, dfTest)
print(postResample_rfTestLongitude <- postResample(rfTestLongitude, dfTest$LONGITUDE))

# Predict and evaluate on Validation
rfValidLongitude <- predict(rfFitLongitude, dfValidation)
print(postResample_rfValidLongitude<- postResample(rfValidLongitude, dfValidation$LONGITUDE))

# Save absolute errors
errors_latitude_rf <- cbind((as.data.frame(dfTest$LONGITUDE - rfTestLongitude)),(as.data.frame(dfValidation$LONGITUDE - rfValidLongitude)))

# Save Model
saveRDS(rfFitLongitude, file="rfLongitude.rds")

# SVM
system.time(svmFitLongitude <- train(LONGITUDE~., data=dfTraining,
                                     method='svmLinear',
                                     trControl = ctrl)) 

# Check results on validation dataset # 99 % acc  kappa 0.9897  
svmTestLongitude <- predict(svmFitLongitude ,newdata = dfTest)
postResample(svmTestLongitude, dfTest$LONGITUDE)

# Check results on validation dataset # 99 % acc kappa 0.9897 
svmValidLongitude<- predict(svmFitLongitude ,newdata = dfValidation)
postResample(svmvalidLongitude, dfValid$LONGITUDE)

# Save Model
saveRDS(svmFitLongitude, file="svmLongitude.rds")

# KNN

# Check results on test dataset 100 % KAPPA 1
set.seed(123)
system.time(knnFitLongitudeTest <- knn(train = dfTraining, 
                                       test = dfTest,
                                       cl = dfTraining$LONGITUDE, k=5))
postResample(knnFitLongitudeTest,dfTest$LONGITUDE)

# Check results on validation dataset  99 % KAPPA 0.9987  
system.time(knnFitLongitudeValid <- knn(train = dfTraining, 
                                        test = dfValidation,
                                        cl = dfTraining$LONGITUDE, k=5))
postResample(knnFitLongitudeValid,dfTest$LONGITUDE)

#PIPELINE FOR THE BLIND DATASET--------------------------------------------------------------------

