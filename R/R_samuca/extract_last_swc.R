#-------------------------------------------------#
#--- Extract last SWC results from simulations ---#
#-------------------------------------------------#
#--- Necessary to be used as the initial conditions of 2nd run simulations

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

library(agricolae)
library(ggpubr)
library(tictoc)

soil.sim = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/sim_res_all_soil.csv"),as.is = T)

max.dap = max(soil.sim$dap)

sl = paste0("swc",1:8)
last.swc = soil.sim[soil.sim$dap == (max.dap - 3),c("dap","treatment","sim.id","year.run",sl)]
last.swc = last.swc[,2:length(last.swc)]

avg.last.swc = data.frame(treatment = aggregate(swc1 ~ treatment + sim.id, last.swc, mean)[1],
                          sim.id    = aggregate(swc1 ~ treatment + sim.id, last.swc, mean)[2],
                          swc1      = aggregate(swc1 ~ treatment + sim.id, last.swc, mean)[3],
                          swc2      = aggregate(swc2 ~ treatment + sim.id, last.swc, mean)[3],
                          swc3      = aggregate(swc3 ~ treatment + sim.id, last.swc, mean)[3],
                          swc4      = aggregate(swc4 ~ treatment + sim.id, last.swc, mean)[3],
                          swc5      = aggregate(swc5 ~ treatment + sim.id, last.swc, mean)[3],
                          swc6      = aggregate(swc6 ~ treatment + sim.id, last.swc, mean)[3],
                          swc7      = aggregate(swc7 ~ treatment + sim.id, last.swc, mean)[3],
                          swc8      = aggregate(swc8 ~ treatment + sim.id, last.swc, mean)[3])

avg.last.swc$year.run = 1978

all.last.swc = rbind(last.swc, avg.last.swc)

idx.soil.fn = data.frame(sim.id   = paste0("M_ID_",25:28),
                         soil.fn  = paste0("soil_control_",25:28,".csv"))

all.last.swc$soil.fn = as.character(idx.soil.fn$soil.fn[match(all.last.swc$sim.id, idx.soil.fn$sim.id)])

write.csv(all.last.swc, file = paste0(wd.repo,"/R/R_samuca/last_swc.csv"), row.names = F)






