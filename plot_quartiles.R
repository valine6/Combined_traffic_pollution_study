plot_quartiles = function(DF, percentile = 25, col_var=4, var_name = "NOx") {
  # This function plots the evolution of the median, and two surrounding percentiles.
  # The arguments of the function are DF the dataframe which has the pollution and traffic data,
  # and the percentile that is wanted to be drawn. By default, the algorithm will draw 25th and 75th percentiles.
  # and the col_var which is the number the column that is wanted to be plotted, Default is NOx column.
  # and the var_name is the name of the variable that is studied. Default is NOx.
  
  #install.packages("chron")
  #library("chron")
  
  # The plot is realized over a day
  hours = unique(strftime(DF$Date, format = "%H:%M:%S"))
  
  # The values to plot are in this new Table : 
  variable = data.frame(matrix(nrow = 2*length(hours), ncol = 4))
  colnames(variable) = c("Time", "Median", "first_percentile", "sec_percentile")
  
  # Let's fill the table, while going through the hours vector.
  for (i in 1:length(hours)) {
    variable$Time[i] = hours[i]
    
    variable$Median[i] = median(DF[which(strftime(DF$Date, format = "%H:%M:%S") == hours[i]), col_var], na.rm = TRUE)
    variable$first_percentile[i] = quantile(DF[which(strftime(DF$Date, format = "%H:%M:%S") == hours[i]), col_var], percentile*0.01, na.rm = TRUE)
    variable$sec_percentile[i] = quantile(DF[which(strftime(DF$Date, format = "%H:%M:%S") == hours[i]), col_var], 1-(percentile*0.01), na.rm = TRUE)
    
  }
  
  variable$Time = as.POSIXct(strptime(variable$Time, format = '%H:%M:%S'))
  
  # Now to plot the graph:
  
  x11()
  plot(times(format(variable$Time, "%H:%M:%S")), variable$Median, type = "l", lwd = 2, col = "blue",
       ylim = c(min(variable$first_percentile, na.rm = TRUE), max(variable$sec_percentile, na.rm = TRUE)), ylab = "", xlab = "Time")
  lines(times(format(variable$Time, "%H:%M:%S")), variable$first_percentile, lty = 2, col = "blue")
  lines(times(format(variable$Time, "%H:%M:%S")), variable$sec_percentile, lty = 4, col = "blue")
  legend('topleft', c(paste(percentile, "th percentile"), "Median", paste(100-percentile, "th percentile")), lty = c(2,1,4), col = c("blue", "blue", "blue"))
  title(main = paste("Medians and percentiles of the variable", var_name), ylab = paste(var_name))
  
}

plot_quartiles(DF, 25, 2, "Flow")