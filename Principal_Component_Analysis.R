Principal_Component_Analysis = function(DF) {
  # This function does a PCA of the dataset, using its different variables.
  
  install.packages("FactoMineR")
  library(FactoMineR)
  
  # PCA
  result_pca = PCA(DF[,c(2,3,4,6,7,8,9)], scale.unit = TRUE, graph = TRUE, quanti.sup = c(3,7))
  x11()
  plot.PCA(result_pca, axes = c(1,2), choix ="var")
  x11()
  plot.PCA(result_pca, axes = c(1,2), choix ="ind")
  
  # Description of the dimensions
  dimdesc(result_pca, axes = 1:5)
}