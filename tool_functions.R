date_period_selection = function(DF, beginning_date, ending_date){
  # This function returns a new data frame extracted from the first one, containing only the data
  # that are in between the two dates in argument.
  # The format of the 2 dates are POSIXct. In order to create a POSIXct date, use the function
  # as.POSIXct(date, format = ‘%d/%m/%Y %H:%M:%S’)
  
  new_DF = DF[which((DF$Date >= beginning_date) & (DF$Date <= ending_date)),]
  return(new_DF)
}



time_period_selection = function(DF, beginning_hour, ending_hour){
  # This function returns a new data frame extracted from the first one, containing only the data
  # in a specified time period.
  # The format of the 2 hours are integers.
  install.packages("lubridate")
  library("lubridate")
  
  if (beginning_hour < ending_hour) {
    new_DF = DF[which((hour(DF$Date) >= beginning_hour) & (hour(DF$Date) <= ending_hour)),]
  }
  
  if (beginning_hour > ending_hour) {
    new_DF = DF[which((hour(DF$Date) <= beginning_hour) & (hour(DF$Date) >= ending_hour)),]
  }
  
  return(new_DF)

}




