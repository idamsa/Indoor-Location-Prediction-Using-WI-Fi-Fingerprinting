source("Optional Preprocessing, Feature Engenieering and Data Split.R")

# A. MODELS FOR BUILDING ----
set.seed(123)

# 1. KNN BUILDING ---- 
# We will remove the floor, lat and long from the training as they will be unknown for the actual data
# Check results on test dataset acc: 99.90165 

system.time(knnFitTest <- knn(train = dfTrainingStand[ , -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                              test = dfTestStand[ , -which(names(dfTestStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                              cl = dfTrainingStand$BUILDINGID, k=1
)
)
# user  system elapsed 
# 86.42    0.16   90.21 
print(knnCMT <- confusionMatrix(knnFitTest, dfTestStand$BUILDINGID)) # Confusion Matrix

# Check results on validation dataset Accuracy : 0.999 Kappa : 0.9985  

knnFitValid <- knn(train=dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                   test=dfValidationStand[ , -which(names(dfValidationStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                   cl=dfTrainingStand$BUILDINGID, k=1)
print(knnCMV <- confusionMatrix(knnFitValid, dfValidationStand$BUILDINGID)) # Confusion Matrix

# Optimize KNN --- ACC AND KAPPA GO DOWN AFTER K=1 BOTH IN TEST AND VALID
# Decided to keep the nr of neighbours 1 # used this function for the other knn models too

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
system.time(svmFit <- train(BUILDINGID ~.,
                            data = dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                            method = "svmLinear",
                            trControl = ctrl


)
)

svmFit <- readRDS("svmBuildingFinal.rds")
# Check results on validation dataset # 99 % acc  kappa 0.9897  
svmTest <- predict(svmFit ,newdata = dfTestStand[, -which(names(dfTestStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))])
print(svmCMT <- confusionMatrix(svmTest, dfTestStand$BUILDINGID)) # Confusion Matrix

# Check results on validation dataset # 99 % acc kappa 0.9897 
svmValid <- predict(svmFit, newdata = dfValidationStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))])
print(svmCMV <- confusionMatrix(svmValid, dfValidationStand$BUILDINGID)) # Confusion Matrix

# Saving Model
saveRDS(svmFit, file = "svmBuildingFinal.rds")

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
# # user  system elapsed 
# # 33.89    2.20   37.53 

svmFitFloor <- readRDS("svmFloorFinal.rds")

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

bestmtry_rf <- tuneRF(dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE"))],
                      dfTraining$LATITUDE[, -which(names(dfTraining) %in% c("LONGITUDE"))],
                      ntreeTry=100,
                      stepFactor=2,
                      improve=0.05,
                      trace=TRUE,
                      plot=T)

system.time(rfFitLatitude <- randomForest(x = dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE"))],
                                          y = dfTraining$LATITUDE,
                                          ntrees = 100,
                                          importance = T,
                                          mtry = 88
)
)
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

bestmtry_rf <- tuneRF(dfTraining, dfTraining$LONGITUDE, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)

# mtry OOBError
# 88    88 5.536374
# 175  175 5.743512
# 350  350 7.065408

system.time(rfFitLongitude <- randomForest(x = dfTraining,
                                           y = dfTraining$LONGITUDE,
                                           ntrees = 100,
                                           importance = T,
                                           mtry = 88
)
)

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
