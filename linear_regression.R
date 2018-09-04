# pollution depending on the flow

linear_regression = function(DF, max_speedlimit = 'None', min_speedlimit = 'None', 
                             min_windspeed = 'None', max_windspeed = 'None') {
  # This function makes the linear regression of the concentrations data depending on the flow. 
  # The purpose is to see if the data can be proportional (y = ax+b with b = 0)
  
  # The function returns results of the linear regression (coefficient, p-value and r squared)
  # The function also plots the result, showing the dataset and the regression results
  
  # Gestion of the limit conditions:
  reg_DF = DF
  if (max_speedlimit != 'None') {
    reg_DF = DF[which(DF$Speed <= max_speedlimit),]
  }
  if (min_windspeed != 'None') {
    reg_DF = DF[which(DF$Speed >= min_windspeed),]
  }
  if (min_speedlimit != 'None') {
    reg_DF = DF[which(DF$Speed >= min_speedlimit),]
  }
  if (max_windspeed != 'None') {
    reg_DF = DF[which(DF$Speed <= max_windspeed),]
  }


  XObs=reg_DF$Flow
  YObs=reg_DF$NOx-reg_DF$NOx_bg
  
  # Linear model : y = ax+b where b = 0
  reg = summary(lm(YObs ~ 0 + XObs))
  
  x11()
  plot(XObs, YObs, ylab = "traffic NOx - background NOx (µg/m3)", xlab = "Flow (veh/h)", 
       main = "Pollution (NOx) depending on the flow", 
       sub = paste("R2 = ", reg$r.squared, "-- y = ", reg$coefficients[1]," *x"))
  lines(XObs, XObs*reg$coefficients[1], col = "red")
  
  # To plot the results 
  summary(lm(YObs ~ 0 + XObs))
  
}

linear_regression(DF, max_speedlimit = 100, min_speedlimit = 50)
linear_regression(DF)
linear_regression(DF, max_windspeed = 30)
linear_regression(DF, min_windspeed = 15)