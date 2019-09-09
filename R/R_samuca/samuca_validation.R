#--------------------------#
#--- Plot R_SAMUCA_Perf ---#
#--------------------------#

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

wd.out = paste0(wd.core,"\\results_perf\\samuca_paper")

#--- Load Source Files (~/bin) 
invisible(sapply(list.files(path = paste0(wd.core,"\\lib\\"),full.names = T),
                 function(x) source(x)))
library(ggpubr)


#------------------#
#--- Validation ---#
#------------------#
l.id  = c("validation_25_jul")
m.fn  = c("meta_sugar_db_validation.csv")
nl = 8
n.run = 1
nl          = 8 
n.run       = 1
plot.screen = T
use.debug   = T
samuca.exe  = "samuca_vs_proj.exe"
x.breaks = seq(0,480, by = 60)

#--- ggplot options
p.size          = 5
l.size          = 0.5

#--- Color-Blind Friendly palette
cbPalette       = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(cbPalette)= c(   "ATAB",    "COLI",    "OLIM",    "CLER",    "UNIR",    "UNII",   "UNII2")
leg.name.df     = data.frame(M_ID = c(   9,   11,   15,   20,   21,   22,   23),
                             leg  = c(   "ATAB",    "COLI",    "OLIM",    "CLER",    "UNIR",    "UNII",   "UNII2"))

col.treat       = cbPalette
gg.dpi          = 500
gg.h            = 5
gg.w            = 20
x.breaks        = seq(0,480, by = 60)
t = theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.background = NULL,
          #legend.position="none",
          axis.line    = element_line(colour = "black", size = 0.2),
          axis.text    = element_text(size=24, colour = "black"),
          axis.title   = element_text(size=26),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))

for(r in 1:n.run){
  
  #--- SAMUCA R perf
  run.sam.perf = r.samuca.perf.fun(l.id[r],
                                   wd.core,
                                   wd.model,
                                   use.debug,
                                   debug.path,
                                   samuca.exe,
                                   m.fn[r])
  
  #--- Plot plant variables
  gg.das.crop = gg.das(run.sam.perf$perf.dt.plan,
                       run.sam.perf$sim.obs.idx,
                       run.sam.perf$meta,
                       plot.screen,
                       p.size,
                       l.size,
                       col.treat,
                       gg.dpi,
                       20,
                       20,
                       wd.out,
                       l.id[r],
                       T,
                       2,
                       3,
                       F,
                       x.breaks,
                       t)
  
  gg.scatter.crop = gg.scatter(run.sam.perf$perf.dt.plan,
                               run.sam.perf$sim.obs.idx,
                               run.sam.perf$meta,
                               plot.screen,
                               6,
                               l.size,
                               col.treat,
                               gg.dpi,
                               20,
                               20,
                               wd.out,
                               l.id[r],
                               T,
                               2,
                               3,
                               t,
                               T,
                               data.frame(l.sl  = c("swc2","swc3","swc5"),
                                          sl.lab= c("SWC","SWC","SWC")))
  
  
}

