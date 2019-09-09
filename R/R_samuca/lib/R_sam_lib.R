#--- R SAMUCA libraries ----
  
  #--- Installing missing packages
  pkg = c("tictoc", # Track runtime 
          "ggplot2",
          "grid",
          "gridExtra",
          "gtable",
          "R.utils",
          "gdata")# Plot cool charts 
  ipkg = pkg %in% rownames(installed.packages())
  sapply(pkg[!ipkg],function(x) install.packages(x))
  
  #--- Load installed packages
  library(tictoc)
  library(ggplot2)
  library(gridExtra)
  library(gtable)
  library(grid)
  library(R.utils)
  library(gdata)

