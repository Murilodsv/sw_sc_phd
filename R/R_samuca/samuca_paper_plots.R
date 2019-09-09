#--------------------------#
#--- Plot R_SAMUCA_Perf ---#
#--------------------------#

uni.pc      = T
if(uni.pc){
  wd.core     = "D:\\Murilo\\samuca\\samuca\\R\\R_samuca"
  wd.model    = "D:\\Murilo\\samuca\\samuca\\model"
  debug.path  = "D:\\Murilo\\samuca\\samuca\\samuca_vs_proj\\Debug"
}else{
  wd.core     = "C:\\Murilo\\samuca\\R\\R_samuca"
  wd.model    = "C:\\Murilo\\samuca\\model"
  debug.path  = "C:\\Murilo\\samuca\\samuca_vs_proj\\Debug"
}

wd.out = paste0(wd.core,"\\results_perf\\samuca_paper")

#--- Load Source Files (~/bin) 
invisible(sapply(list.files(path = paste0(wd.core,"\\lib\\"),full.names = T),
                 function(x) source(x)))
library(ggpubr)

#-------------------#
#--- Calibration ---#
#-------------------#

l.id        = c("pira_calibration_tsoil","pira_calibration_tair")
m.fn        = c("meta_sugar_db.csv","meta_sugar_db_airT.csv")
nl          = 8 
n.run       = 1
plot.screen = T
use.debug   = T
samuca.exe  = "samuca_vs_proj.exe"
scatter.plot= T

#--- ggplot options
p.size          = 5
l.size          = 0.5
col.treat       = c("#FF0000", "#04B404")
names(col.treat)= c(     "NM",      "WM")
leg.name.df     = data.frame(M_ID = c(   1,    3,    4,    5,    6,    7,    8),
                               leg  = c("NM", "WM", "NM", "WM", "NM", "WM", "NM"))
gg.dpi          = 500
gg.h            = 5
gg.w            = 20
x.breaks        = seq(0,1400, by = 200)
t = theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.background = NULL,
          legend.position="none",
          axis.line    = element_line(colour = "black", size = 0.2),
          axis.text    = element_text(size=24, colour = "black"),
          axis.title   = element_text(size=26),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))


for(r in 1:n.run){

  #--------------------------#
  #--- SAMUCA performance ---#
  #--------------------------#
  
  run.sam.perf = r.samuca.perf.fun(l.id[r],
                                   wd.core,
                                   wd.model,
                                   use.debug,
                                   debug.path,
                                   samuca.exe,
                                   m.fn[r])
  
  #--------------------#
  #--- Plot Results ---#
  #--------------------#
  
  #--- Crop Variables
  gg.das.crop = gg.das(run.sam.perf$perf.dt.plan,
         run.sam.perf$sim.obs.idx,
         run.sam.perf$meta,
         plot.screen,
         p.size,
         l.size,
         col.treat,
         gg.dpi,
         44,
         20,
         wd.out,
         l.id[r],
         T,
         1,
         9,
         T,
         x.breaks,
         t)
  
  
  #--- Atmospheric Variables
  gg.das.atmo = gg.das(run.sam.perf$perf.dt.atmo,
         run.sam.perf$sim.obs.idx,
         run.sam.perf$meta,
         plot.screen,
         p.size,
         l.size,
         col.treat,
         gg.dpi,
         8,
         8*4,
         wd.out,
         l.id[r],
         F,
         1,
         9,
         T,
         x.breaks,
         t)
  
  
  
  #--- Soil Water Variables only 3 layers
  gg.das.swc = gg.das.sl(run.sam.perf$perf.dt.soil,
            run.sam.perf$sim.obs.idx,
            run.sam.perf$meta,
            plot.screen,
            3,
            l.size,
            col.treat,
            gg.dpi,
            15,
            20,
            wd.out,
            l.id[r],
            T,
            1,
            3,
            T,
            x.breaks,
            t,
            F,
            data.frame(l.sl  = c("swc2","swc3","swc5"),
                       sl.lab= c("SWC 10cm","SWC 30cm","SWC 60cm")))
  
  #--- Soil Water Variables first layer
  gg.das.swc = gg.das.sl(run.sam.perf$perf.dt.soil,
                         run.sam.perf$sim.obs.idx,
                         run.sam.perf$meta,
                         plot.screen,
                         3,
                         l.size,
                         col.treat,
                         gg.dpi,
                         6,
                         24,
                         wd.out,
                         l.id[r],
                         T,
                         1,
                         1,
                         T,
                         x.breaks,
                         t,
                         F,
                         data.frame(l.sl  = c("swc2"),
                                    sl.lab= c("SWC 10cm")))
  
  #--- Soil Water Variables for all the layers
  gg.das.swc = gg.das.sl(run.sam.perf$perf.dt.soil,
                         run.sam.perf$sim.obs.idx,
                         run.sam.perf$meta,
                         plot.screen,
                         3,
                         l.size,
                         col.treat,
                         gg.dpi,
                         30,
                         20,
                         wd.out,
                         l.id[r],
                         T,
                         1,
                         8,
                         T,
                         x.breaks,
                         t,
                         T,
                         data.frame(l.sl  = c("swc1","swc2","swc3","swc4","swc5","swc6","swc7","swc8"),
                                    sl.lab= c("swc1","swc2","swc3","swc4","swc5","swc6","swc7","swc8")))
  
  
  #--- Soil Temperauture Variables only 1 layer
  gg.das.st = gg.das.sl(run.sam.perf$perf.dt.temp,
            run.sam.perf$sim.obs.idx,
            run.sam.perf$meta,
            plot.screen,
            3,
            l.size,
            col.treat,
            gg.dpi,
            6,
            24,
            wd.out,
            l.id[r],
            T,
            1,
            1,
            T,
            x.breaks,
            t,
            F,
            data.frame(l.sl  = c("Ts.1"),
                       sl.lab= c("Soil Temperature")))
  
  
  #--- Soil Temperauture Variables all layers
  gg.das.st = gg.das.sl(run.sam.perf$perf.dt.temp,
                        run.sam.perf$sim.obs.idx,
                        run.sam.perf$meta,
                        plot.screen,
                        3,
                        l.size,
                        col.treat,
                        gg.dpi,
                        40,
                        20,
                        wd.out,
                        l.id[r],
                        T,
                        1,
                        5,
                        T,
                        x.breaks,
                        t,
                        T,
                        data.frame(l.sl  = c("Ts.1","Ts.2","Ts.3","Ts.4","Ts.5"),
                                   sl.lab= c("Ts.1","Ts.2","Ts.3","Ts.4","Ts.5")))
  
  if(scatter.plot){
    
    #---------------------#
    #--- Scatter Plots ---#
    #---------------------#
    
    #--- Crop Variables
    gg.scatter.crop = gg.scatter(run.sam.perf$perf.dt.plan,
                                 run.sam.perf$sim.obs.idx,
                                 run.sam.perf$meta,
                                 plot.screen,
                                 6,
                                 l.size,
                                 col.treat,
                                 gg.dpi,
                                 21,
                                 21,
                                 wd.out,
                                 l.id[r],
                                 T,
                                 3,
                                 3,
                                 t)
    #--- Atmospheric Variables
    gg.scatter.atmo = gg.scatter(run.sam.perf$perf.dt.atmo,
                                 run.sam.perf$sim.obs.idx,
                                 run.sam.perf$meta,
                                 plot.screen,
                                 6,
                                 l.size,
                                 col.treat,
                                 gg.dpi,
                                 12,
                                 12,
                                 wd.out,
                                 l.id[r],
                                 T,
                                 1,
                                 1,
                                 t)
    
    #--- Soil Water Variables only 3 layers
    gg.scatter.swc = gg.scatter(run.sam.perf$perf.dt.soil,
                                run.sam.perf$sim.obs.idx,
                                run.sam.perf$meta,
                                plot.screen,
                                6,
                                l.size,
                                col.treat,
                                gg.dpi,
                                12,
                                12,
                                wd.out,
                                l.id[r],
                                T,
                                1,
                                1,
                                t,
                                F,
                                data.frame(l.sl  = c("swc2","swc3","swc5"),
                                           sl.lab= c("SWC","SWC","SWC")))
    
    #--- Soil Water Variables all layers
    gg.scatter.swc = gg.scatter(run.sam.perf$perf.dt.soil,
                                run.sam.perf$sim.obs.idx,
                                run.sam.perf$meta,
                                plot.screen,
                                6,
                                l.size,
                                col.treat,
                                gg.dpi,
                                12,
                                12,
                                wd.out,
                                l.id[r],
                                T,
                                1,
                                1,
                                t,
                                T,
                                data.frame(l.sl  = c("swc2","swc3","swc5"),
                                           sl.lab= c("SWC","SWC","SWC")))
    
    #--- Soil Temperature all layers
    gg.scatter.stemp = gg.scatter(run.sam.perf$perf.dt.temp,
                                run.sam.perf$sim.obs.idx,
                                run.sam.perf$meta,
                                plot.screen,
                                6,
                                l.size,
                                col.treat,
                                gg.dpi,
                                12,
                                12,
                                wd.out,
                                l.id[r],
                                T,
                                1,
                                1,
                                t,
                                T,
                                data.frame(l.sl  = c("swc2","swc3","swc5"),
                                           sl.lab= c("SWC","SWC","SWC")))
    
    
    #--- Soil Temperature one layer
    gg.scatter.stemp = gg.scatter(run.sam.perf$perf.dt.temp,
                                  run.sam.perf$sim.obs.idx,
                                  run.sam.perf$meta,
                                  plot.screen,
                                  6,
                                  l.size,
                                  col.treat,
                                  gg.dpi,
                                  12,
                                  12,
                                  wd.out,
                                  l.id[r],
                                  T,
                                  1,
                                  1,
                                  t,
                                  F,
                                  data.frame(l.sl  = c("Ts.1"),
                                             sl.lab= c("Soil Temperature")))
    
    
  }
}


#------------------#
#--- Validation ---#
#------------------#
l.id  = c("validation_new_v")
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








