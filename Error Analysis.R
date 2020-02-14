# ERROR ANALYSIS ----      
# pLOTTING THE ERRORS FOR LONGITUDE Random Forest 

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