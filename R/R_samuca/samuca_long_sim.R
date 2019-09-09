#-----------------------------#
#--- Long-Term Simulations ---#
#-----------------------------#

#--- Run SAMUCA for the selected combinations of soils, planting dates and sites

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

#--- some additonal package required for libraries
#install.packages("dplyr")
#install.packages("Rcpp")
#install.packages("tibble")

library(agricolae)
library(ggpubr)
library(tictoc)


m.fn   = c("meta_sugar_db_long_simulations_sandy.csv",
           "meta_sugar_db_long_simulations_medium.csv",
           "meta_sugar_db_long_simulations_clay.csv")

nl     = 8
n.meta = length(m.fn)
l.meta.id = c("sandy","loam","clay")
run.id = "msc_new" # soils according to DOI 10.1007/s12355-015-0367-0

planting.scenarios = T
p.scen.df = read.csv(paste0(wd.repo,"/R/R_samuca/planting_scenarios.csv"), as.is = T)
n.pscen   = length(p.scen.df$p.date.scen)

use.last.swc= T # swc conditions will be carried out for subsequent simulations
run.sim     = T
save.sim    = T
read.sim    = F
use.debug   = T
samuca.exe  = "samuca_vs_proj.exe"
last.day.ofset = 5

ini.year = 1979
end.year = 2009
l.year   = seq(ini.year,end.year)

location.names.df = read.csv(paste0(wd.repo,"/R/R_samuca/long_sim_ids.csv"), as.is = T)

treatment.df = data.frame(treat.id   = c("bare", "6_ton_ha", "12_ton_ha", "18_ton_ha"),
                          mulch.biom = c(0,6000,12000,18000),
                          mulch.type = c(0,1,1,1))

#--- Run Long-term simulations
if(run.sim){
  
  for(m in 1:n.meta){
    
    #--- Initialize a new result file per meta
    init.res = T
    
    #--- Read sim control file from meta
    meta.sim    = read.csv(paste0(wd.repo,"/R/R_samuca/",m.fn[m]), as.is = T)
    meta.id     = l.meta.id[m]
    control.fn  = meta.sim$sim_file[meta.sim$run]
    l.soil.fn   = meta.sim$soil_file[meta.sim$run]
    l.treatment = as.character(treatment.df$treat.id)
    l.ctl.id    = paste0("M_ID_",meta.sim$M_ID[meta.sim$run])
    
    for(treatment.run in l.treatment){
      
      for(year.run in l.year){
        
        for(ctl.fn in control.fn){
          
          #--- read control file
          ctl.file = read.csv(paste0(wd.repo,"/R/R_samuca/sim_db/",ctl.fn), as.is = T)
          
          #--- Update Control file with treatment and year
          ctl.file[ctl.file$find == "<s.ini.yr>","rep"] = as.character(year.run)
          ctl.file[ctl.file$find == "<s.end.yr>","rep"] = as.character(year.run+1)
          ctl.file[ctl.file$find == "<p.yr>","rep"]     = as.character(year.run)
          ctl.file[ctl.file$find == "<h.yr>","rep"]     = as.character(year.run+1)
          ctl.file[ctl.file$find == "<r.ini.yr>","rep"] = as.character(year.run)
          ctl.file[ctl.file$find == "<r.type>","rep"]   = as.character(treatment.df$mulch.type[treatment.df$treat.id == treatment.run])
          ctl.file[ctl.file$find == "<r.ini.dw>","rep"] = as.character(treatment.df$mulch.biom[treatment.df$treat.id == treatment.run])
          
          p.scen.f = p.scen.df$p.date.scen == location.names.df$p.date.scen[location.names.df$id == ctl.file$rep[ctl.file$find == "<prj.nm>"]]
          
          #--- Update Control file with doys
          ctl.file[ctl.file$find == "<s.ini.doy>","rep"] = as.character(p.scen.df$s.ini.doy[p.scen.f])  
          ctl.file[ctl.file$find == "<s.end.doy>","rep"] = as.character(p.scen.df$s.end.doy[p.scen.f])
          ctl.file[ctl.file$find == "<p.doy>","rep"]     = as.character(p.scen.df$p.doy[p.scen.f])
          ctl.file[ctl.file$find == "<h.doy>","rep"]     = as.character(p.scen.df$h.doy[p.scen.f])
          ctl.file[ctl.file$find == "<r.ini.doy>","rep"] = as.character(p.scen.df$r.ini.doy[p.scen.f])
          
          #--- write new sim parameters
          write.csv(ctl.file, file = paste0(wd.repo,"/R/R_samuca/sim_db/",ctl.fn), row.names = F)
          
        }
        
        #------------------#
        #--- Run SAMUCA ---#
        #------------------#
        message("---------------------------------------")
        message(paste0("Running SAMUCA for:"))
        message(paste0("Meta ID: ", meta.id))
        message(paste0("Treatment: ", treatment.run))
        message(paste0("Year: ", year.run))
        message("---------------------------------------")
        
        sim.results = r.samuca.sim(wd.core,
                                   wd.model,
                                   use.debug,
                                   debug.path,
                                   samuca.exe,
                                   m.fn[m],
                                   treatment.run,
                                   year.run)
        
        #--- include indentifier
        sim.results$plan.sim$meta.id     = meta.id
        sim.results$atmo.sim$meta.id     = meta.id
        sim.results$soil.sim$meta.id     = meta.id
        sim.results$temp.sim$meta.id     = meta.id
        
        #--- Store and write last swc resuls for next year
        if(use.last.swc){
          
          l.sl = paste0("swc",1:nl)
          
          for(soil.fn in l.soil.fn){
            
            ctl.id = l.ctl.id[l.soil.fn == soil.fn]
            
            max.das = max(sim.results$soil.sim$das[sim.results$soil.sim$sim.id == ctl.id]) - last.day.ofset
            
            #--- extract last year results
            ini.swc  = sim.results$soil.sim[sim.results$soil.sim$das == max.das & 
                                              sim.results$soil.sim$sim.id == ctl.id,l.sl]
            
            #--- read soil control file
            soil.ctl = read.csv(paste0(wd.repo,"/R/R_samuca/sim_db/soil/",soil.fn), as.is = T)
            
            #--- Update Initial Conditions of soil water content for next year
            for(i in 1:nl){
              
              sl = paste0("swc",i)
              rep.sl =  as.numeric(ini.swc[1,i])
              soil.ctl[soil.ctl$find == "<swc_ini>" & soil.ctl$layer == i,"rep"] = as.character(round(rep.sl,3))
            }
            
            #--- write soil initial conditions for next year
            write.csv(soil.ctl, file = paste0(wd.repo,"/R/R_samuca/sim_db/soil/",soil.fn), row.names = F, quote = F)
            
          }
        }
        
        #--- Bind all results to a single df
        if(init.res){
          sim.res.all = sim.results
          init.res    = F
        }else{
          #--- append results
          sim.res.all$plan.sim = rbind(sim.res.all$plan.sim, sim.results$plan.sim)
          sim.res.all$atmo.sim = rbind(sim.res.all$atmo.sim, sim.results$atmo.sim)
          sim.res.all$soil.sim = rbind(sim.res.all$soil.sim, sim.results$soil.sim)
          sim.res.all$temp.sim = rbind(sim.res.all$temp.sim, sim.results$temp.sim)
        }
      }
    }
    
    if(save.sim){
      write.csv(sim.res.all$plan.sim, file = paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/sim_res_all_plan_",meta.id,"_",run.id,".csv"), row.names = F)
      write.csv(sim.res.all$soil.sim, file = paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/sim_res_all_soil_",meta.id,"_",run.id,".csv"), row.names = F)
      write.csv(sim.res.all$atmo.sim, file = paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/sim_res_all_atmo_",meta.id,"_",run.id,".csv"), row.names = F)
      write.csv(sim.res.all$temp.sim, file = paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/sim_res_all_temp_",meta.id,"_",run.id,".csv"), row.names = F)
    }
    
  }
  
  toc()
  
}

