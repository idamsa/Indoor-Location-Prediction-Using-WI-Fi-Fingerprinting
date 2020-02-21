source("Optional Preprocessing, Feature Engenieering and Data Split.R")

# A. MODELS FOR BUILDING ----
set.seed(123)

# 1. KNN BUILDING ---- 
# We will remove the floor, lat and long from the training as they will be unknown for the actual data
# Check results on test dataset acc: 99.90165 
#### Ignacio: Ioana, why you have chosen k=1? Do you have any prior benchmark 
#### which shows you that k=1 is the best option?

system.time(knnFitTest <- knn(train = dfTrainingStand[ , -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                              test = dfTestStand[ , -which(names(dfTestStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                              cl = dfTrainingStand$BUILDINGID, k=1
)
)
# user  system elapsed 
# 20.09    0.00   20.25 
print(knnCMT <- confusionMatrix(knnFitTest, dfTestStand$BUILDINGID)) # Confusion Matrix

# Check results on validation dataset Accuracy : 0.999 Kappa : 0.9985  

knnFitValid <- knn(train=dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                   test=dfValidationStand[ , -which(names(dfValidationStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                   cl=dfTrainingStand$BUILDINGID, k=1)
print(knnCMV <- confusionMatrix(knnFitValid, dfValidationStand$BUILDINGID)) # Confusion Matrix

#### Ignacio: Ioana, here it becomes clear that your initial goal was to benchmark
#### the computational time needed to train a knn model. Here you test different
#### values in order to determine the best option.
# Optimize KNN --- ACC AND KAPPA GO DOWN AFTER K=1 BOTH IN TEST AND VALID
# Decided to keep the nr of neighbours 1 # used this function for the other knn models too

i<-1
k.optm<-1
for (i in 1:9){
  knn.mod <- knn(train = dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))], 
                 test = dfValidationStand[, -which(names(dfValidationStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                 cl = dfTrainingStand$BUILDINGID, k=i)
  k.optm[i] <- 100 * sum(dfValidationStand$BUILDINGID ==  knn.mod)/NROW(dfValidationStand$BUILDINGID)
  k=i
  cat(k,"=",k.optm[i],"")
}

#### Ignacio: Ioana, added a plot to show the performance of the k-nn model as
#### a function of the k value.

k_values <- seq(1:9)
acc <- k.optm
res <-data.frame(k=k_values,accuracy=acc)

ggplot(res,aes(x=k,y=accuracy)) + geom_line(size=1,color="blue") +
  geom_point(size=1.2,color="blue") +
  scale_x_continuous(breaks = c(1:9)) +
  ggtitle("Performance of the k-nn model on the training set")


# 2. SVM BUILDING (CHOSEN) ----
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
system.time(svmFit <- train(BUILDINGID ~.,
                            data = dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE","FLOOR"))],
                            method = "svmLinear",
                            trControl = ctrl,
                            verbose = TRUE


)
)

#### Ignacio: Check if the model exists, otherwise, load it from the folder.
if (!exists("svmFit")) {
  svmFit <- readRDS("svmBuildingFinal.rds")
}

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

print(knnCMTFloor <- confusionMatrix(knnFitFloorTest, dfTestStand$FLOOR)) # Confusion Matrix

# Check results on validation dataset   Accuracy : 0.9934 , Kappa : 0.9913    
system.time(knnFitFloorValid <- knn(train = dfTrainingStand[ , -which(names(dfTrainingStand) %in% c("LONGITUDE","LATITUDE"))], 
                                    test  = dfValidationStand[ , -which(names(dfValidationStand) %in% c("LONGITUDE","LATITUDE"))],
                                    cl    = dfTrainingStand$FLOOR, k=5
)
)
# user  system elapsed 
# 20.19    0.01   20.42  
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

#### Ignacio: Same as before
if (!exists("svmFitFloor")) {
  svmFit <- readRDS("svmFloorFinal.rds")
}

#svmFitFloor <- readRDS("svmFloorFinal.rds")

# Check results on test dataset  Accuracy : 0.9932   ,   Kappa : 0.9926 
svmTestFloor <- predict(svmFitFloor, newdata = dfTestStand[, -which(names(dfTestStand) %in% c("LONGITUDE","LATITUDE"))])
print(svmCMTFloor <- confusionMatrix(svmTestFloor , dfTestStand$FLOOR)) # Confusion Matrix

# Check results on validation dataset Accuracy : 0.9932   ,Kappa : 0.9926   
svmValidFloor <- predict(svmFitFloor ,newdata = dfValidationStand[, -which(names(dfValidationStand) %in% c("LONGITUDE","LATITUDE"))])
print(svmCMVFloor <- confusionMatrix(svmValidFloor, dfValidationStand$FLOOR)) # Confusion Matrix
#plotconfusion(svmCMVFloor)
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
bestmtry_rf_lat <- tuneRF(dfTraining[, -which(names(dfTraining) %in% c("LATITUDE"))],
                      dfTraining$LATITUDE,
                      ntreeTry=100,
                      stepFactor=2,
                      improve=0.05,
                      trace=TRUE,
                      plot=T)

#### Ignacio: How to do this more generic in the next step.
bestmtry_rf_lat <- as.data.frame(bestmtry_rf_lat)

system.time(rfFitLatitude <- randomForest(x = dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE","LATITUDE"))],
                                          y = dfTraining$LATITUDE,
                                          ntrees = 100,
                                          importance = T,
                                          # mtry = 88
                                          mtry = bestmtry_rf_lat[which.min(bestmtry_rf_lat$OOBError),]$mtry
)
)
# user  system elapsed
# 752.86    1.36  771.61

# Bring model 

#### Ignacio: Again, check if the variable exists, then if not, load the search
#### for the model and load it.
rfFitLatitude <- readRDS("rfLatitudeFinal.rds")

# Predict and evaluate on Test
rfTestLatitude <- predict(rfFitLatitude, dfTest[, -which(names(dfTest) %in% c("LONGITUDE"))])
print(postResample_rfTestLatitude <- postResample(rfTestLatitude, dfTest$LATITUDE))

# RMSE Rsquared      MAE 
# 3.815287 0.997192 2.148457 

# Predict and evaluate on Validation
rfValidLatitude <- predict(rfFitLatitude, dfValidation[, -which(names(dfValidation) %in% c("LONGITUDE"))])
print(postResample_rfValidLatitude <- postResample(rfValidLatitude, dfValidation$LATITUDE))

# RMSE  Rsquared       MAE 
# 3.3220601 0.9979198 2.0181775 

# Save absolute errors
errors_latitude_rfTest <- as.data.frame(dfTest$LATITUDE - rfTestLatitude)
errors_latitude_rfValid <- as.data.frame(dfValidation$LATITUDE - rfValidLatitude)

# Save Model
saveRDS(rfFitLatitude, file = "rfLatitude.rds")

# 2. KNN LATITUDE ----
knnFitLatitude <- knnreg(LATITUDE~.,
                        data = dfTrainingStand[, -which(names(dfTrainingStand) %in% c("LONGITUDE"))])


# Check results on test dataset  
system.time(knnTestLatitude <- predict(knnFitLatitude, dfTestStand[, -which(names(dfTestStand) %in% c("LONGITUDE"))]))
print(postResample_knnTestLatitude <- postResample(knnTestLatitude,dfTestStand$LATITUDE))
# RMSE  Rsquared       MAE 
# 5.4615117 0.9941766 2.6570264 

# Check results on validation dataset        
knnValidLatitude <- predict(knnFitLatitude, dfValidationStand[, -which(names(dfValidationStand) %in% c("LONGITUDE"))])
print(postResample_knnValidLatitude <- (postResample(knnValidLatitude ,dfValidationStand$LATITUDE)))
# RMSE Rsquared      MAE 
# 5.505707 0.994196 2.627809 

# Save absolute errors
errors_latitude_knnTest <- as.data.frame(dfTestStand$LATITUDE - knnTestLatitude)
errors_latitude_knnValid <- as.data.frame(dfValidationStand$LATITUDE - knnValidLatitude)

# Save Model
saveRDS(knnFitLatitude, file = "knnFitLatitude.rds")

# D. MODELS FOR LONGITUDE ----
# THESE MODELS HAVE VERY GOOD PERFORMANCE WHEN USING THE REAL VERIFIED DATA BUT ONCE WE ADD THE PREDICTED VALUES + ERRORS
# THE PERFORMANCE WILL NOT BE AS GOOD
set.seed(123)
# 1. RANDOM FOREST LONGITUDE (CHOSEN)----

bestmtry_rf_long <- tuneRF(dfTraining,
                      dfTraining$LONGITUDE, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)

bestmtry_rf_long <- as.data.frame(bestmtry_rf_long)


system.time(rfFitLongitude <- randomForest(x = dfTraining[, -which(names(dfTraining) %in% c("LONGITUDE"))],
                                           y = dfTraining$LONGITUDE,
                                           ntrees = 100,
                                           importance = T,
                                           #mtry = 88
                                           mtry = bestmtry_rf_long[which.min(bestmtry_rf_long$OOBError),]$mtry
)
)

# user  system elapsed
# 684.86    1.73  711.69

# Bring Model

#### Ignacio: Same as before.
if (!exists("rfFitLongitude")) {
  svmFit <- readRDS("rfLongitudeFinal.rds")
}
#rfFitLongitude <- readRDS("rfLongitudeFinal.rds")

# Predict and evaluate on Test
rfTestLongitude <- predict(rfFitLongitude, dfTest)
print(postResample_rfTestLongitude <- postResample(rfTestLongitude, dfTest$LONGITUDE))
# RMSE  Rsquared       MAE 
# 3.0033925 0.9995042 1.6794705 

# Predict and evaluate on Validation
rfValidLongitude <- predict(rfFitLongitude, dfValidation)
print(postResample_rfValidLongitude <- postResample(rfValidLongitude, dfValidation$LONGITUDE))
# RMSE  Rsquared       MAE 
# 3.0586017 0.9994846 1.6571688 

# Save absolute errors
errors_longitude_rfTest <- as.data.frame(dfTest$LONGITUDE - rfTestLongitude)
errors_longitude_rfValid <- as.data.frame(dfValidation$LONGITUDE - rfValidLongitude)

# Save Model
saveRDS(rfFitLongitude, file = "rfLongitude.rds")

# 2. KNN LONGITUDE  ----
knnFitLongitude <- knnreg(LONGITUDE~., data = dfTrainingStand)

# Check results on test dataset  
knnTestLongitude <- predict(knnFitLongitude, dfTestStand)
print(postResample_knnTestLongitude <- postResample(knnTestLongitude,dfTestStand$LONGITUDE))
# RMSE  Rsquared       MAE 
# 4.4904191 0.9988656 1.9491605 

# Check results on validation dataset        
knnValidLongitude <- predict(knnFitLongitude, dfValidationStand)
print(postResample_knnValidLongitude <- postResample(knnValidLongitude ,dfValidationStand$LONGITUDE))
# RMSE  Rsquared       MAE 
# 4.3068451 0.9989439 1.9432282 

# Save absolute errors
errors_longitude_knnTest <- as.data.frame(dfTestStand$LONGITUDE - knnTestLongitude)
errors_longitude_knnValid <- as.data.frame(dfValidationStand$LONGITUDE - knnValidLongitude)

# Save model
saveRDS(knnFitLongitude, file = "knnLongitude.rds")

#### TO DO:
####
#### Plot the where the observations were taken in both datasets. You made this plot
#### Why you didn't include it, even on a separate script?
####
#### Missing the confussion matrix plotting. Where the errors took place?
#### Use an scatter3D plot and use colors to codify if the observation was correct
#### or not. Are the models making mistakes in the same place? 
####

dfValidation$knnFitValid <- ( knnFitValid == dfValidationStand$BUILDINGID ) 
dfValidation$svmValid    <- ( svmValid    == dfValidationStand$BUILDINGID )

plot_ly(dfValidation, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~knnFitValid, colors = c('green', 'red')) %>%
  add_markers() %>%
  layout(title = 'Predicted BUILDINGID from K-NN model',
    scene = list(xaxis = list(title = 'Longitude'),
                      yaxis = list(title = 'Latitude'),
                      zaxis = list(title = 'Floor')))

plot_ly(dfValidation, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~svmValid, colors = c('green', 'red')) %>%
  add_markers() %>%
  layout(title = 'Predicted BUILDINGID from SVM model',
         scene = list(xaxis = list(title = 'Longitude'),
                      yaxis = list(title = 'Latitude'),
                      zaxis = list(title = 'Floor')))

dfValidation$svmValidFloor    <- ( svmValidFloor == dfValidation$FLOOR )
dfValidation$knnFitFloorValid <- ( knnFitFloorValid == dfValidation$FLOOR )

plot_ly(dfValidation, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~knnFitFloorValid, colors = c('green', 'red')) %>%
  add_markers() %>%
  layout(title = 'Predicted FLOOR from K-NN model',
         scene = list(xaxis = list(title = 'Longitude'),
                      yaxis = list(title = 'Latitude'),
                      zaxis = list(title = 'Floor')))

plot_ly(dfValidation, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~svmValidFloor, colors = c('green', 'red')) %>%
  add_markers() %>%
  layout(title = 'Predicted FLOOR from SVM model',
         scene = list(xaxis = list(title = 'Longitude'),
                      yaxis = list(title = 'Latitude'),
                      zaxis = list(title = 'Floor')))

#### The same as well for the location error. Compute the location error by the
#### Euclidian distance for each observation, and then make an scatter plot 
#### of each predicted observation with a color proportional to the error. In
#### addition, to prevent a continuoos scale and making difficult to discriminate
#### over error sizes, you can bin the total error into "reasonable" sizes and
#### color the observations according to those error sizes.
####
#### Also include a comparison plot of the distribution of errors by each model
#### and identify outliyers. It will be great to do it with plotly.