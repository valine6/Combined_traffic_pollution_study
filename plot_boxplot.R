
plot_boxplot = function(DF, period, PH) {
  # This functions draws boxplots of the concentrations depending on the speed, for 
  # a period (for instance 2 months) and at a precise peak hour
  # There is a plot for each class of flow (each 1,000 veh/hour) 
  
  # The variables are: 
  # DF: same dataset with flow, speed, pollution and weather values. 
  # period : the description of the period, it will be used on the plot as a descrition
  # the peak hour chosen. same as earlier, it will be used on the plot as a descrition
  
  library('ggplot2')
  library('gridExtra')
  
  i = 0  # i: flow counter
  flows_list = list()
  
  # The classes of flow are from 0 to 1,000 vehicles, from 1,000 to 2,000, etc...
  # The flows_list contains for each class of flow, the corresponding values from DF
  while (nrow(DF[which(DF$Flow > i*1000),]) != 0 ){
    flows_list[i+1] = list(i=DF[which((DF$Flow > i*1000) & (DF$Flow < ((i+1)*1000))),])
    i=i+1
  }
  
  # creation of the list of plots
  plot_list = list()
  for (j in 0:(length(flows_list)-1)) {
    
    # in order to plot the boxes, the speeds are put in categories of 20 km/h
    flows_list[[j+1]]$Speed_categories = floor(flows_list[[j+1]]$Speed/20)*20+10

    plot_list[[j+1]] = ggplot(flows_list[[j+1]], aes(x=Speed_categories, y=NOx, group=Speed_categories)) + 
      geom_boxplot(width = 10) +
      ggtitle(paste("Flow between ", j*1000, " and", (j+1)*1000, " veh/h")) +
      scale_x_continuous(limits = c(0,100), breaks = seq(10,90,20)) +
      scale_y_continuous(limits = c(0,500)) 
    
  }
  
  # Opening of the plotting window
  x11(width = 16, height = 9)
  
  n <- length(plot_list)
  nCol <- floor(sqrt(n))
  nCol <- 3
  do.call("grid.arrange", c(plot_list, ncol=nCol, 
                            top=paste("Concentrations of NOx depending on the speed, by flow classes : ", period, ", ", PH)))
  
}


# Plots :
plot_boxplot(HPM_octnov, "Octobre et Novembre", "HPM")
plot_boxplot(HCM_octnov, "Octobre et Novembre", "HCM")
plot_boxplot(HPS_octnov, "Octobre et Novembre", "HPS")

plot_boxplot(HPM_maijuin, "May and June", "Morning Peak Hour")
plot_boxplot(HCM_maijuin, "Mai et Juin", "HCM")
plot_boxplot(HPS_maijuin, "Mai et Juin", "HPS")

