#---------------#
#--- Diff ET ---#
#---------------#

if(!exists("laptop")){
  laptop      = T
}

if(laptop){
  
  wd.core     = "C:\\Murilo\\samuca\\R\\R_samuca"
  wd.model    = "C:\\Murilo\\samuca\\model"
  wd.repo     = "C:/Murilo/samuca"
  debug.path  = "C:\\Murilo\\samuca\\samuca_vs_proj\\Debug"
  
}else{
  
  wd.core     = "D:\\Murilo\\samuca\\samuca\\R\\R_samuca"
  wd.model    = "D:\\Murilo\\samuca\\samuca\\model"
  wd.repo = "D:/Murilo/samuca/samuca"
  debug.path  = "D:\\Murilo\\samuca\\samuca\\samuca_vs_proj\\Debug"
}

library(ggplot2)

dif.et = read.csv(paste0(wd.repo,"/doc/excel/diff_et.csv"), as.is = T)

et.df  = rbind(data.frame(et         = dif.et$WithMulch,
                 treatment  = "WM"),
      data.frame(et         = dif.et$WithoutMulch,
                 treatment  = "NM"))
      
ggplot(et.df, aes(x = et)) + 
  geom_density(aes(fill = treatment), alpha = 0.4, size = 0.4) + 
  theme_bw()

ggplot(et.df, aes(x = et)) + 
  geom_histogram(aes(fill = treatment), binwidth = 0.4,alpha = 0.4, size = 0.4) + 
  theme_bw()


hist(dif.et$diff)

bp.dif = boxplot(dif.et$diff)
mean(dif.et$diff)
median(dif.et$diff)

boxplot(et.df$et ~ et.df$treatment)

