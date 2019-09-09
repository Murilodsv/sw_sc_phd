#-----------------------------#
#--- Long-Term Simulations ---#
#-----------------------------#

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

#install.packages("dplyr")
#install.packages("Rcpp")
#install.packages("tibble")

library(agricolae)
library(ggpubr)
library(tictoc)

#-----------------#
#--- Long-Term ---#
#-----------------#
l.id   = c("long_term")
m.fn   = c("meta_sugar_db_long_simulations_clay_goja_spqu.csv",
           "meta_sugar_db_long_simulations_clay_goja_gocc.csv",
           "meta_sugar_db_long_simulations_clay_goja_goqu.csv")

nl     = 8
n.meta = 3
l.meta.id = c("goja_spqu","goja_gocc","goja_goqu")
run.id = "goja_new_soil"

planting.scenarios = T
p.scen.df = read.csv(paste0(wd.repo,"/R/R_samuca/planting_scenarios.csv"), as.is = T)
n.pscen= length(p.scen.df$p.date.scen)

use.last.swc= T
run.sim     = T
save.sim    = T
read.sim    = F
use.debug   = T
samuca.exe  = "samuca_vs_proj.exe"
ini.year = 1979
l.year   = seq(1979,2009)

location.names.df = data.frame(id   = c(paste0("M_ID_",25:28)),
                               name = c("Jatai","Petrolina","Joao Pessoa", "Piracicaba"))

treatment.df = data.frame(treat.id   = c("bare", "6_ton_ha", "12_ton_ha", "18_ton_ha"),
                          mulch.biom = c(0,6000,12000,18000),
                          mulch.type = c(0,1,1,1))

if(use.last.swc){
  last.swc = read.csv(paste0(wd.repo,"/R/R_samuca/last_swc.csv"), as.is = T)
}

p.scen.df = p.scen.df[p.scen.df$p.date.scen == 7,]

#--- Run Long-term simulations
if(run.sim){
  
  for(p in 1){
    
    p.scen.id = p.scen.df$p.date.scen[p]
    
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
            
            #--- Update Control file with doys
            ctl.file[ctl.file$find == "<s.ini.doy>","rep"] = as.character(p.scen.df$s.ini.doy[p])  
            ctl.file[ctl.file$find == "<s.end.doy>","rep"] = as.character(p.scen.df$s.end.doy[p])
            ctl.file[ctl.file$find == "<p.doy>","rep"]     = as.character(p.scen.df$p.doy[p])
            ctl.file[ctl.file$find == "<h.doy>","rep"]     = as.character(p.scen.df$h.doy[p])
            ctl.file[ctl.file$find == "<r.ini.doy>","rep"] = as.character(p.scen.df$r.ini.doy[p])
            
            #--- write new sim parameters
            write.csv(ctl.file, file = paste0(wd.repo,"/R/R_samuca/sim_db/",ctl.fn), row.names = F)
            
          }
          
          #------------------#
          #--- Run SAMUCA ---#
          #------------------#
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
          sim.results$plan.sim$planting.id = p.scen.id
          
          sim.results$atmo.sim$meta.id     = meta.id
          sim.results$atmo.sim$planting.id = p.scen.id
          
          sim.results$soil.sim$meta.id     = meta.id
          sim.results$soil.sim$planting.id = p.scen.id
          
          sim.results$temp.sim$meta.id     = meta.id
          sim.results$temp.sim$planting.id = p.scen.id
          
          #--- Store last swc resuls for next year
          if(use.last.swc){
            
            l.sl = paste0("swc",1:nl)
            
            for(soil.fn in l.soil.fn){
              
              ctl.id = l.ctl.id[l.soil.fn == soil.fn] 
              
              max.das = max(sim.results$soil.sim$das[sim.results$soil.sim$sim.id == ctl.id]) - 5
              
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
              
              #--- write new sim parameters
              write.csv(soil.ctl, file = paste0(wd.repo,"/R/R_samuca/sim_db/soil/",soil.fn), row.names = F, quote = F)
              
            }
          }
          
          #--- Bind all results to a single df
          if(init.res){
            sim.res.all = sim.results
            init.res    = F
          }else{
            sim.res.all$plan.sim = rbind(sim.res.all$plan.sim, sim.results$plan.sim)
            sim.res.all$atmo.sim = rbind(sim.res.all$atmo.sim, sim.results$atmo.sim)
            sim.res.all$soil.sim = rbind(sim.res.all$soil.sim, sim.results$soil.sim)
            sim.res.all$temp.sim = rbind(sim.res.all$temp.sim, sim.results$temp.sim)
          }
        }
      }
      
      if(save.sim){
        write.csv(sim.res.all$plan.sim, file = paste0(wd.repo,"/R/R_samuca/results_perf/sim_res_all_plan_",meta.id,"_",p.scen.id,"_",run.id,".csv"), row.names = F)
        write.csv(sim.res.all$soil.sim, file = paste0(wd.repo,"/R/R_samuca/results_perf/sim_res_all_soil_",meta.id,"_",p.scen.id,"_",run.id,".csv"), row.names = F)
        write.csv(sim.res.all$atmo.sim, file = paste0(wd.repo,"/R/R_samuca/results_perf/sim_res_all_atmo_",meta.id,"_",p.scen.id,"_",run.id,".csv"), row.names = F)
        write.csv(sim.res.all$temp.sim, file = paste0(wd.repo,"/R/R_samuca/results_perf/sim_res_all_temp_",meta.id,"_",p.scen.id,"_",run.id,".csv"), row.names = F)
      }
      
    }
  }
  
  toc()
  
}

#--- Color-Blind Friendly palette
cbPalette       = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
alpha.ribbon    = 0.2
ltype.ribbon.border = 1
lsize.ribbon.border = 0.1
line.size    = 1
x.breaks = seq(0,365, by = 80)


#--- legend position + idx at right
leg.x.pos.leg.right = 0.82
leg.y.pos.leg.right = 0.80
idx.y.pos.leg.right = 1.00
idx.x.pos.leg.right = 0.07

#--- legend position + idx at left
leg.x.pos.leg.left = 0.08
leg.y.pos.leg.left = 0.90
idx.y.pos.leg.left = 1.00
idx.x.pos.leg.left = 0.93

#-------------#
#--- Plant ---#
#-------------#

analysis.id = "plan"

l.var = c("fw.st", "dw.su","swfacp","till")
l.on.harv.var = c("fw.st","dw.su")
letter.idx = c("(a)","(b)","(c)","(d)")
leg.left = c(T,T,T,T)


add.jitter  = T
add.loess   = T

idx.x.pos.bp = 0.7
font.size.idx = 8
gg.dpi       = 500

same.ylimits.dap = T
sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index.csv"), as.is = T )

file.sim.id = "sim_res_all_plan_"

l.planting.id = 7


for(agg.var in l.var){
  
  message("Creating plants plots for variable: ",agg.var)
  
  if(agg.var == l.var[1]){init.df.summary = T}
  
  y.lab = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
                 " (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
  
  agg.var.units = sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var]
  
  if(leg.left[l.var == agg.var]){
    #--- Left side legend
    leg.x.pos = leg.x.pos.leg.left
    leg.y.pos = leg.y.pos.leg.left
    idx.y.pos = idx.y.pos.leg.left
    idx.x.pos = idx.x.pos.leg.left
  }else{
    #--- Right side legend
    leg.x.pos = leg.x.pos.leg.right
    leg.y.pos = leg.y.pos.leg.right
    idx.y.pos = idx.y.pos.leg.right
    idx.x.pos = idx.x.pos.leg.right
  }
  
  for(p in l.planting.id){
    
    res.fn = paste0(file.sim.id,l.meta.id[1],"_",p,"_",run.id,".csv")
    plan.sim.m.p = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T)
    
    res.fn = paste0(file.sim.id,l.meta.id[2],"_",p,"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    res.fn = paste0(file.sim.id,l.meta.id[3],"_",p,"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    gg.analysis.df = plan.sim.m.p
    
    #--- Plot y-limits
    if(same.ylimits.dap){
      
      y.min = min(plan.sim.m.p[,agg.var])
      y.max = max(plan.sim.m.p[,agg.var])
      
      x.min = min(plan.sim.m.p[,"dap"])
      x.max = max(plan.sim.m.p[,"dap"])
      
    }else{
      
      y.min = min(gg.analysis.df[,agg.var])
      y.max = max(gg.analysis.df[,agg.var])
      x.min = min(gg.analysis.df[,"dap"])
      x.max = max(gg.analysis.df[,"dap"])
      
    }
    
    y.lim = c(y.min,y.max)
    x.lim = c(x.min,x.max)
    
    #--- Treatment legend values
    gg.analysis.df$treatment.orig = gg.analysis.df$treatment
    gg.analysis.df$treatment      = as.factor(treatment.df$mulch.biom[match(gg.analysis.df$treatment,treatment.df$treat.id)] / 1000)
    
    #--- Aggregate data
    df.agg = gg.analysis.df[,c("dap",agg.var,"sim.id","meta.id","treatment","year.run")]
    colnames(df.agg)[2] = "agg.var"
    
    #--- Remove last day to avoid difference in comparison with leap years
    max.dap = max(df.agg$dap)
    df.agg = df.agg[df.agg$dap < max.dap,]
    
    #--- Remove days before planting
    df.agg = df.agg[df.agg$dap > 0,]
    
    df.agg.var = data.frame(dap       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,1],
                            treatment = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,2],
                            site      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,3],
                            soil      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,4],
                            mean      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,5],
                            median    = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, median)[,5],
                            sd        = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, sd)[,5],
                            max       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, max)[,5],
                            min       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, min)[,5])
    
    #--- Limit standard errors to max-min values
    df.agg.var$sd[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)] = max(df.agg$agg.var) - df.agg.var$mean[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)]
    df.agg.var$sd[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] = df.agg.var$mean[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] - min(df.agg$agg.var) 
    #df.agg.var$site = site
    df.agg.var$site = location.names.df$name[match(as.character(df.agg.var$site),as.character(location.names.df$id))]
    df.agg.var$soil = capitalize(df.agg.var$soil)
    
    x.breaks = seq(0,365, by = 100)
    
    gg.var = ggplot(df.agg.var, aes_string(y = "mean", x = "dap", fill = "treatment", colour = "treatment")) + 
      geom_ribbon(aes(ymin = mean-sd, ymax=mean+sd), 
                  alpha = alpha.ribbon,
                  linetype = ltype.ribbon.border,
                  size = lsize.ribbon.border) +
      geom_line(size = line.size) +
      scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_x_continuous(breaks = x.breaks, expand = c(0.01,0.01)) + 
      ylim(y.lim) + 
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.background = NULL,
            #legend.position= c(leg.x.pos,leg.y.pos),
            legend.text  = element_text(size=18),
            legend.title = element_text(size=16),
            axis.line    = element_line(colour = "black", size = 0.2),
            axis.text    = element_text(size=22, colour = "black"),
            axis.title   = element_text(size=22),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
            legend.box.background = element_rect(colour = "black"))
    
    gg.var$labels$x = "Days After Planting (DAP)"
    gg.var$labels$y = y.lab
    gg.var$labels$colour = "Mulch Cover\n(ton/ha)"
    gg.var$labels$fill   = "Mulch Cover\n(ton/ha)"
    
    
    
    gg.var.facet = gg.var + facet_grid(soil ~ site) + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text       = element_text(size = 14)
      )
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_dap_",analysis.id,"_",p,"_",agg.var,"_",run.id,".png"), 
           plot = gg.var.facet, width = 12, height = 8,
           dpi = gg.dpi)
    
    
    #--- Boxplots
    if(agg.var %in% l.on.harv.var){
      
      #--- Extract close to harvest results
      df.agg.end = df.agg[df.agg$dap == (max.dap - 5),]
      df.agg.end$treatment = as.factor(df.agg.end$treatment)
      df.agg.end$site = location.names.df$name[match(as.character(df.agg.end$sim.id),as.character(location.names.df$id))]
      df.agg.end$soil = capitalize(df.agg.end$meta.id)
      
      #--- Boxplots
      agg.var.bp = ggplot(df.agg.end, aes(y = agg.var, x = treatment)) + 
        geom_boxplot(aes(y = agg.var, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
        scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
        scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              panel.background = NULL,
              legend.position="none",
              axis.line    = element_line(colour = "black", size = 0.2),
              axis.text    = element_text(size=24, colour = "black"),
              axis.title   = element_text(size=26),
              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
              plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
      
      agg.var.bp$labels$x = "Mulch Cover Biomass (ton/ha)"
      agg.var.bp$labels$y = y.lab
      
      if(add.jitter){
        #--- Add jitter points
        bp.wid = mean(ggplot_build(agg.var.bp)$data[[1]]$xmax - ggplot_build(agg.var.bp)$data[[1]]$xmin)
        agg.var.bp = agg.var.bp + 
          geom_jitter(aes(group = treatment),
                      size = 1, 
                      shape=16, 
                      alpha = 0.5, position=position_jitter(bp.wid/2))
        
      }
      
      
      agg.var.bp.facet = agg.var.bp + facet_grid(soil ~ site) + 
        theme(
          strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
          strip.text       = element_text(size = 14)
        )
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_",analysis.id,"_",p,"_",agg.var,"_",run.id,".png"), 
             plot = agg.var.bp.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
    }
  }
}

#------------------#
#--- atmosphere ---#
#------------------#

analysis.id = "atmo"

l.var = c("et","et.acc","pet")
l.on.harv.var = c("fw.st","dw.su")
letter.idx = c("(a)","(b)","(c)","(d)")
leg.left = c(F,T,F)


add.jitter  = T
add.loess   = T

idx.x.pos.bp = 0.7
font.size.idx = 8
gg.dpi       = 500

same.ylimits.dap = T
sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index.csv"), as.is = T )

file.sim.id = "sim_res_all_atmo_"

l.planting.id = 7

for(agg.var in l.var){
  
  message("Creating plants plots for variable: ",agg.var)
  
  if(agg.var == l.var[1]){init.df.summary = T}
  
  y.lab = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
                 " (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
  
  agg.var.units = sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var]
  
  if(leg.left[l.var == agg.var]){
    #--- Left side legend
    leg.x.pos = leg.x.pos.leg.left
    leg.y.pos = leg.y.pos.leg.left
    idx.y.pos = idx.y.pos.leg.left
    idx.x.pos = idx.x.pos.leg.left
  }else{
    #--- Right side legend
    leg.x.pos = leg.x.pos.leg.right
    leg.y.pos = leg.y.pos.leg.right
    idx.y.pos = idx.y.pos.leg.right
    idx.x.pos = idx.x.pos.leg.right
  }
  
  for(p in l.planting.id){
    
    res.fn = paste0(file.sim.id,l.meta.id[1],"_",p,"_",run.id,".csv")
    plan.sim.m.p = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T)
    
    res.fn = paste0(file.sim.id,l.meta.id[2],"_",p,"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    res.fn = paste0(file.sim.id,l.meta.id[3],"_",p,"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    gg.analysis.df = plan.sim.m.p
    
    #--- Plot y-limits
    if(same.ylimits.dap){
      
      y.min = min(plan.sim.m.p[,agg.var])
      y.max = max(plan.sim.m.p[,agg.var])
      
      x.min = min(plan.sim.m.p[,"dap"])
      x.max = max(plan.sim.m.p[,"dap"])
      
    }else{
      
      y.min = min(gg.analysis.df[,agg.var])
      y.max = max(gg.analysis.df[,agg.var])
      x.min = min(gg.analysis.df[,"dap"])
      x.max = max(gg.analysis.df[,"dap"])
      
    }
    
    y.lim = c(y.min,y.max)
    x.lim = c(x.min,x.max)
    
    #--- Treatment legend values
    gg.analysis.df$treatment.orig = gg.analysis.df$treatment
    gg.analysis.df$treatment      = as.factor(treatment.df$mulch.biom[match(gg.analysis.df$treatment,treatment.df$treat.id)] / 1000)
    
    #--- Aggregate data
    df.agg = gg.analysis.df[,c("dap",agg.var,"sim.id","meta.id","treatment","year.run")]
    colnames(df.agg)[2] = "agg.var"
    
    #--- Remove last day to avoid difference in comparison with leap years
    max.dap = max(df.agg$dap)
    df.agg = df.agg[df.agg$dap < max.dap,]
    
    #--- Remove days before planting
    df.agg = df.agg[df.agg$dap > 0,]
    
    df.agg.var = data.frame(dap       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,1],
                            treatment = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,2],
                            site      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,3],
                            soil      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,4],
                            mean      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,5],
                            median    = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, median)[,5],
                            sd        = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, sd)[,5],
                            max       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, max)[,5],
                            min       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, min)[,5])
    
    #--- Limit standard errors to max-min values
    df.agg.var$sd[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)] = max(df.agg$agg.var) - df.agg.var$mean[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)]
    df.agg.var$sd[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] = df.agg.var$mean[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] - min(df.agg$agg.var) 
    #df.agg.var$site = site
    df.agg.var$site = location.names.df$name[match(as.character(df.agg.var$site),as.character(location.names.df$id))]
    df.agg.var$soil = capitalize(as.character(df.agg.var$soil))
    
    x.breaks = seq(0,365, by = 100)
    
    gg.var = ggplot(df.agg.var, aes_string(y = "mean", x = "dap", fill = "treatment", colour = "treatment")) + 
      geom_ribbon(aes(ymin = mean-sd, ymax=mean+sd), 
                  alpha = alpha.ribbon,
                  linetype = ltype.ribbon.border,
                  size = lsize.ribbon.border) +
      geom_line(size = line.size) +
      scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_x_continuous(breaks = x.breaks, expand = c(0.01,0.01)) + 
      ylim(y.lim) + 
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.background = NULL,
            #legend.position= c(leg.x.pos,leg.y.pos),
            legend.text  = element_text(size=18),
            legend.title = element_text(size=16),
            axis.line    = element_line(colour = "black", size = 0.2),
            axis.text    = element_text(size=22, colour = "black"),
            axis.title   = element_text(size=22),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
            legend.box.background = element_rect(colour = "black"))
    
    gg.var$labels$x = "Days After Planting (DAP)"
    gg.var$labels$y = y.lab
    gg.var$labels$colour = "Mulch Cover\n(ton/ha)"
    gg.var$labels$fill   = "Mulch Cover\n(ton/ha)"
    
    
    
    gg.var.facet = gg.var + facet_grid(soil ~ site) + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text       = element_text(size = 14)
      )
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_dap_",analysis.id,"_",p,"_",agg.var,"_",run.id,".png"), 
           plot = gg.var.facet, width = 12, height = 8,
           dpi = gg.dpi)
    
    
    #--- Boxplots
    if(agg.var == "et.acc"){
      
      #--- Extract close to harvest results
      df.agg.end = df.agg[df.agg$dap == (max.dap - 5),]
      df.agg.end$treatment = as.factor(df.agg.end$treatment)
      df.agg.end$site = location.names.df$name[match(as.character(df.agg.end$sim.id),as.character(location.names.df$id))]
      df.agg.end$soil = capitalize(df.agg.end$meta.id)
      
      #--- Boxplots
      agg.var.bp = ggplot(df.agg.end, aes(y = agg.var, x = treatment)) + 
        geom_boxplot(aes(y = agg.var, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
        scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
        scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              panel.background = NULL,
              legend.position="none",
              axis.line    = element_line(colour = "black", size = 0.2),
              axis.text    = element_text(size=24, colour = "black"),
              axis.title   = element_text(size=26),
              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
              plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
      
      agg.var.bp$labels$x = "Mulch Cover Biomass (ton/ha)"
      agg.var.bp$labels$y = y.lab
      
      if(add.jitter){
        #--- Add jitter points
        bp.wid = mean(ggplot_build(agg.var.bp)$data[[1]]$xmax - ggplot_build(agg.var.bp)$data[[1]]$xmin)
        agg.var.bp = agg.var.bp + 
          geom_jitter(aes(group = treatment),
                      size = 1, 
                      shape=16, 
                      alpha = 0.5, position=position_jitter(bp.wid/2))
        
      }
      
      
      agg.var.bp.facet = agg.var.bp + facet_grid(soil ~ site) + 
        theme(
          strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
          strip.text       = element_text(size = 14)
        )
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_",analysis.id,"_",p,"_",agg.var,"_",run.id,".png"), 
             plot = agg.var.bp.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
    }
  }
}

#------------------------#
#--- soil temperature ---#
#------------------------#

analysis.id = "temp"

l.var = c("Ts.1","Ts.4","Ts.8")
l.on.harv.var = c("fw.st","dw.su")
letter.idx = c("(a)","(b)","(c)","(d)")
leg.left = c(F,F,F)


add.jitter  = T
add.loess   = T

idx.x.pos.bp = 0.7
font.size.idx = 8
gg.dpi       = 500

same.ylimits.dap = T
sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index.csv"), as.is = T )

file.sim.id = "sim_res_all_temp_"

l.planting.id = 7

for(agg.var in l.var){
  
  message("Creating plants plots for variable: ",agg.var)
  
  if(agg.var == l.var[1]){init.df.summary = T}
  
  y.lab = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
                 " (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
  
  agg.var.units = sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var]
  
  if(leg.left[l.var == agg.var]){
    #--- Left side legend
    leg.x.pos = leg.x.pos.leg.left
    leg.y.pos = leg.y.pos.leg.left
    idx.y.pos = idx.y.pos.leg.left
    idx.x.pos = idx.x.pos.leg.left
  }else{
    #--- Right side legend
    leg.x.pos = leg.x.pos.leg.right
    leg.y.pos = leg.y.pos.leg.right
    idx.y.pos = idx.y.pos.leg.right
    idx.x.pos = idx.x.pos.leg.right
  }
  
  for(p in l.planting.id){
    
    res.fn = paste0(file.sim.id,l.meta.id[1],"_",p,"_",run.id,".csv")
    plan.sim.m.p = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T)
    
    res.fn = paste0(file.sim.id,l.meta.id[2],"_",p,"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    res.fn = paste0(file.sim.id,l.meta.id[3],"_",p,"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    gg.analysis.df = plan.sim.m.p
    
    #--- Plot y-limits
    if(same.ylimits.dap){
      
      y.min = min(plan.sim.m.p[,agg.var])
      y.max = max(plan.sim.m.p[,agg.var])
      
      x.min = min(plan.sim.m.p[,"dap"])
      x.max = max(plan.sim.m.p[,"dap"])
      
    }else{
      
      y.min = min(gg.analysis.df[,agg.var])
      y.max = max(gg.analysis.df[,agg.var])
      x.min = min(gg.analysis.df[,"dap"])
      x.max = max(gg.analysis.df[,"dap"])
      
    }
    
    y.lim = c(y.min,y.max)
    x.lim = c(x.min,x.max)
    
    #--- Treatment legend values
    gg.analysis.df$treatment.orig = gg.analysis.df$treatment
    gg.analysis.df$treatment      = as.factor(treatment.df$mulch.biom[match(gg.analysis.df$treatment,treatment.df$treat.id)] / 1000)
    
    #--- Aggregate data
    df.agg = gg.analysis.df[,c("dap",agg.var,"sim.id","meta.id","treatment","year.run")]
    colnames(df.agg)[2] = "agg.var"
    
    #--- Remove last day to avoid difference in comparison with leap years
    max.dap = max(df.agg$dap)
    df.agg = df.agg[df.agg$dap < max.dap,]
    
    #--- Remove days before planting
    df.agg = df.agg[df.agg$dap > 0,]
    
    df.agg.var = data.frame(dap       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,1],
                            treatment = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,2],
                            site      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,3],
                            soil      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,4],
                            mean      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,5],
                            median    = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, median)[,5],
                            sd        = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, sd)[,5],
                            max       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, max)[,5],
                            min       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, min)[,5])
    
    #--- Limit standard errors to max-min values
    df.agg.var$sd[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)] = max(df.agg$agg.var) - df.agg.var$mean[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)]
    df.agg.var$sd[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] = df.agg.var$mean[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] - min(df.agg$agg.var) 
    #df.agg.var$site = site
    df.agg.var$site = location.names.df$name[match(as.character(df.agg.var$site),as.character(location.names.df$id))]
    df.agg.var$soil = capitalize(as.character(df.agg.var$soil))
    
    x.breaks = seq(0,365, by = 100)
    
    gg.var = ggplot(df.agg.var, aes_string(y = "mean", x = "dap", fill = "treatment", colour = "treatment")) + 
      geom_ribbon(aes(ymin = mean-sd, ymax=mean+sd), 
                  alpha = alpha.ribbon,
                  linetype = ltype.ribbon.border,
                  size = lsize.ribbon.border) +
      geom_line(size = line.size) +
      scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_x_continuous(breaks = x.breaks, expand = c(0.01,0.01)) + 
      ylim(y.lim) + 
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.background = NULL,
            #legend.position= c(leg.x.pos,leg.y.pos),
            legend.text  = element_text(size=18),
            legend.title = element_text(size=16),
            axis.line    = element_line(colour = "black", size = 0.2),
            axis.text    = element_text(size=22, colour = "black"),
            axis.title   = element_text(size=22),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
            legend.box.background = element_rect(colour = "black"))
    
    gg.var$labels$x = "Days After Planting (DAP)"
    gg.var$labels$y = y.lab
    gg.var$labels$colour = "Mulch Cover\n(ton/ha)"
    gg.var$labels$fill   = "Mulch Cover\n(ton/ha)"
    
    
    
    gg.var.facet = gg.var + facet_grid(soil ~ site) + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text       = element_text(size = 14)
      )
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_dap_",analysis.id,"_",p,"_",agg.var,"_",run.id,".png"), 
           plot = gg.var.facet, width = 12, height = 8,
           dpi = gg.dpi)
    
    
    #--- Boxplots
    if(agg.var == "et.acc"){
      
      #--- Extract close to harvest results
      df.agg.end = df.agg[df.agg$dap == (max.dap - 5),]
      df.agg.end$treatment = as.factor(df.agg.end$treatment)
      df.agg.end$site = location.names.df$name[match(as.character(df.agg.end$sim.id),as.character(location.names.df$id))]
      df.agg.end$soil = capitalize(df.agg.end$meta.id)
      
      #--- Boxplots
      agg.var.bp = ggplot(df.agg.end, aes(y = agg.var, x = treatment)) + 
        geom_boxplot(aes(y = agg.var, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
        scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
        scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              panel.background = NULL,
              legend.position="none",
              axis.line    = element_line(colour = "black", size = 0.2),
              axis.text    = element_text(size=24, colour = "black"),
              axis.title   = element_text(size=26),
              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
              plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
      
      agg.var.bp$labels$x = "Mulch Cover Biomass (ton/ha)"
      agg.var.bp$labels$y = y.lab
      
      if(add.jitter){
        #--- Add jitter points
        bp.wid = mean(ggplot_build(agg.var.bp)$data[[1]]$xmax - ggplot_build(agg.var.bp)$data[[1]]$xmin)
        agg.var.bp = agg.var.bp + 
          geom_jitter(aes(group = treatment),
                      size = 1, 
                      shape=16, 
                      alpha = 0.5, position=position_jitter(bp.wid/2))
        
      }
      
      
      agg.var.bp.facet = agg.var.bp + facet_grid(soil ~ site) + 
        theme(
          strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
          strip.text       = element_text(size = 14)
        )
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_",analysis.id,"_",p,"_",agg.var,"_",run.id,".png"), 
             plot = agg.var.bp.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
    }
  }
}


#------------------#
#--- soil water ---#
#------------------#

analysis.id = "soil"

l.var = c("swc1","swc2","swc3","swc4","swc5")
l.on.harv.var = c("fw.st","dw.su")
letter.idx = c("(a)","(b)","(c)","(d)")
leg.left = c(F,F,F,F,F)


add.jitter  = T
add.loess   = T

idx.x.pos.bp = 0.7
font.size.idx = 8
gg.dpi       = 500

same.ylimits.dap = T
sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index.csv"), as.is = T )

file.sim.id = "sim_res_all_soil_"

l.planting.id = 7

for(agg.var in l.var){
  
  message("Creating plants plots for variable: ",agg.var)
  
  if(agg.var == l.var[1]){init.df.summary = T}
  
  y.lab = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
                 " (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
  
  agg.var.units = sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var]
  
  if(leg.left[l.var == agg.var]){
    #--- Left side legend
    leg.x.pos = leg.x.pos.leg.left
    leg.y.pos = leg.y.pos.leg.left
    idx.y.pos = idx.y.pos.leg.left
    idx.x.pos = idx.x.pos.leg.left
  }else{
    #--- Right side legend
    leg.x.pos = leg.x.pos.leg.right
    leg.y.pos = leg.y.pos.leg.right
    idx.y.pos = idx.y.pos.leg.right
    idx.x.pos = idx.x.pos.leg.right
  }
  
  for(p in l.planting.id){
    
    res.fn = paste0(file.sim.id,l.meta.id[1],"_",p,"_",run.id,".csv")
    plan.sim.m.p = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T)
    
    res.fn = paste0(file.sim.id,l.meta.id[2],"_",p,"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    res.fn = paste0(file.sim.id,l.meta.id[3],"_",p,"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    gg.analysis.df = plan.sim.m.p
    
    #--- Plot y-limits
    if(same.ylimits.dap){
      
      y.min = min(plan.sim.m.p[,agg.var])
      y.max = max(plan.sim.m.p[,agg.var])
      
      x.min = min(plan.sim.m.p[,"dap"])
      x.max = max(plan.sim.m.p[,"dap"])
      
    }else{
      
      y.min = min(gg.analysis.df[,agg.var])
      y.max = max(gg.analysis.df[,agg.var])
      x.min = min(gg.analysis.df[,"dap"])
      x.max = max(gg.analysis.df[,"dap"])
      
    }
    
    y.lim = c(y.min,y.max)
    x.lim = c(x.min,x.max)
    
    #--- Treatment legend values
    gg.analysis.df$treatment.orig = gg.analysis.df$treatment
    gg.analysis.df$treatment      = as.factor(treatment.df$mulch.biom[match(gg.analysis.df$treatment,treatment.df$treat.id)] / 1000)
    
    #--- Aggregate data
    df.agg = gg.analysis.df[,c("dap",agg.var,"sim.id","meta.id","treatment","year.run")]
    colnames(df.agg)[2] = "agg.var"
    
    #--- Remove last day to avoid difference in comparison with leap years
    max.dap = max(df.agg$dap)
    df.agg = df.agg[df.agg$dap < max.dap,]
    
    #--- Remove days before planting
    df.agg = df.agg[df.agg$dap > 0,]
    
    df.agg.var = data.frame(dap       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,1],
                            treatment = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,2],
                            site      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,3],
                            soil      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,4],
                            mean      = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, mean)[,5],
                            median    = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, median)[,5],
                            sd        = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, sd)[,5],
                            max       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, max)[,5],
                            min       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, min)[,5])
    
    #--- Limit standard errors to max-min values
    df.agg.var$sd[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)] = max(df.agg$agg.var) - df.agg.var$mean[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)]
    df.agg.var$sd[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] = df.agg.var$mean[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] - min(df.agg$agg.var) 
    #df.agg.var$site = site
    df.agg.var$site = location.names.df$name[match(as.character(df.agg.var$site),as.character(location.names.df$id))]
    df.agg.var$soil = capitalize(as.character(df.agg.var$soil))
    
    x.breaks = seq(0,365, by = 100)
    
    gg.var = ggplot(df.agg.var, aes_string(y = "mean", x = "dap", fill = "treatment", colour = "treatment")) + 
      geom_ribbon(aes(ymin = mean-sd, ymax=mean+sd), 
                  alpha = alpha.ribbon,
                  linetype = ltype.ribbon.border,
                  size = lsize.ribbon.border) +
      geom_line(size = line.size) +
      scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_x_continuous(breaks = x.breaks, expand = c(0.01,0.01)) + 
      ylim(y.lim) + 
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.background = NULL,
            #legend.position= c(leg.x.pos,leg.y.pos),
            legend.text  = element_text(size=18),
            legend.title = element_text(size=16),
            axis.line    = element_line(colour = "black", size = 0.2),
            axis.text    = element_text(size=22, colour = "black"),
            axis.title   = element_text(size=22),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
            legend.box.background = element_rect(colour = "black"))
    
    gg.var$labels$x = "Days After Planting (DAP)"
    gg.var$labels$y = y.lab
    gg.var$labels$colour = "Mulch Cover\n(ton/ha)"
    gg.var$labels$fill   = "Mulch Cover\n(ton/ha)"
    
    
    
    gg.var.facet = gg.var + facet_grid(soil ~ site) + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text       = element_text(size = 14)
      )
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_dap_",analysis.id,"_",p,"_",agg.var,"_",run.id,".png"), 
           plot = gg.var.facet, width = 12, height = 8,
           dpi = gg.dpi)
    
    
    #--- Boxplots
    if(agg.var == "et.acc"){
      
      #--- Extract close to harvest results
      df.agg.end = df.agg[df.agg$dap == (max.dap - 5),]
      df.agg.end$treatment = as.factor(df.agg.end$treatment)
      df.agg.end$site = location.names.df$name[match(as.character(df.agg.end$sim.id),as.character(location.names.df$id))]
      df.agg.end$soil = capitalize(df.agg.end$meta.id)
      
      #--- Boxplots
      agg.var.bp = ggplot(df.agg.end, aes(y = agg.var, x = treatment)) + 
        geom_boxplot(aes(y = agg.var, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
        scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
        scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              panel.background = NULL,
              legend.position="none",
              axis.line    = element_line(colour = "black", size = 0.2),
              axis.text    = element_text(size=24, colour = "black"),
              axis.title   = element_text(size=26),
              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
              plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
      
      agg.var.bp$labels$x = "Mulch Cover Biomass (ton/ha)"
      agg.var.bp$labels$y = y.lab
      
      if(add.jitter){
        #--- Add jitter points
        bp.wid = mean(ggplot_build(agg.var.bp)$data[[1]]$xmax - ggplot_build(agg.var.bp)$data[[1]]$xmin)
        agg.var.bp = agg.var.bp + 
          geom_jitter(aes(group = treatment),
                      size = 1, 
                      shape=16, 
                      alpha = 0.5, position=position_jitter(bp.wid/2))
        
      }
      
      
      agg.var.bp.facet = agg.var.bp + facet_grid(soil ~ site) + 
        theme(
          strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
          strip.text       = element_text(size = 14)
        )
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_",analysis.id,"_",p,"_",agg.var,"_",run.id,".png"), 
             plot = agg.var.bp.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
    }
  }
}




# if(read.sim){
#   plan.sim = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/sim_res_all_plan.csv"),as.is = T)
#   atmo.sim = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/sim_res_all_atmo.csv"),as.is = T)
#   soil.sim = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/sim_res_all_soil.csv"),as.is = T)
#   temp.sim = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/sim_res_all_temp.csv"),as.is = T)
#   
#   sim.res.all = list(plan.sim = plan.sim,
#                      atmo.sim = atmo.sim,
#                      soil.sim = soil.sim,
#                      temp.sim = temp.sim)
#   
#   rm(plan.sim)
#   rm(atmo.sim)
#   rm(soil.sim)
#   rm(temp.sim)
# }

# 
# #--- 
# plan.sim = read.csv(paste0("C:/Users/muril/Downloads/drive-download-20190709T163356Z-001/sim_res_all_plan_clay_12__all_soils_all_dates.csv"),as.is = T)
# 
# l.planting.id = seq(1,12)
# 
# plan.sim$planting.id
# 
# head(plan.sim)
# 
# 
# plan.sim$
# 
# 
# #--- Color-Blind Friendly palette
# cbPalette       = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# alpha.ribbon    = 0.2
# ltype.ribbon.border = 1
# lsize.ribbon.border = 0.1
# line.size    = 1
# x.breaks = seq(0,365, by = 60)
# 
# 
# #--- legend position + idx at right
# leg.x.pos.leg.right = 0.82
# leg.y.pos.leg.right = 0.80
# idx.y.pos.leg.right = 1.00
# idx.x.pos.leg.right = 0.07
# 
# #--- legend position + idx at left
# leg.x.pos.leg.left = 0.16
# leg.y.pos.leg.left = 0.80
# idx.y.pos.leg.left = 1.00
# idx.x.pos.leg.left = 0.93
# 
# # #--- Implement this code for diff
# # head(sim.res.all$plan.sim)
# # 
# # plan.bare  = sim.res.all$plan.sim[sim.res.all$plan.sim$treatment == "bare",]
# # plan.mulch = sim.res.all$plan.sim[sim.res.all$plan.sim$treatment != "bare",]
# # merg.col = c("das","sim.id","year.run")
# # var.merg = "fw.st"
# # sfm.merge = merge(plan.mulch[,c(merg.col,var.merg,"treatment")], plan.bare[,c(merg.col,var.merg)], by = merg.col)
# # sfm.merge$relsfm = (sfm.merge$fw.st.x / sfm.merge$fw.st.y - 1) * 100
# # sfm.merge$dsfm = sfm.merge$fw.st.x - sfm.merge$fw.st.y
# # max.das = max(sfm.merge$das)
# # 
# # sfm.merge.harv = sfm.merge[sfm.merge$das == max.das - 15,]
# # 
# # site = "M_ID_27"
# # boxplot(sfm.merge.harv$dsfm[sfm.merge.harv$sim.id == site] ~ 
# #           sfm.merge.harv$treatment[sfm.merge.harv$sim.id == site],
# #         col = "orange")
# 
# 
# 
# 
# #-------------#
# #--- Plant ---#
# #-------------#
# 
# l.site = unique(sim.res.all$plan.sim$sim.id)
# analysis.id = "plan"
# 
# #l.var = c("fw.st", "dw.st" ,"dw.su","pol")
# l.var = c("fw.st", "dw.su")
# l.on.harv.var = c("fw.st","dw.su")
# letter.idx = c("(a)","(b)","(c)","(d)")
# leg.left = c(T,T)
# 
# 
# add.jitter  = T
# add.loess   = T
# 
# idx.x.pos.bp = 0.7
# font.size.idx = 8
# gg.dpi       = 500
# 
# same.ylimits.dap = T
# sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index.csv"), as.is = T )
# 
# for(agg.var in l.var){
#   
#   message("Creating plants plots for variable: ",agg.var)
#   
#   if(agg.var == l.var[1]){init.df.summary = T}
#   
#   y.lab = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
#                  " (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
#   
#   agg.var.units = sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var]
#   
#   if(leg.left[l.var == agg.var]){
#     #--- Left side legend
#     leg.x.pos = leg.x.pos.leg.left
#     leg.y.pos = leg.y.pos.leg.left
#     idx.y.pos = idx.y.pos.leg.left
#     idx.x.pos = idx.x.pos.leg.left
#   }else{
#     #--- Right side legend
#     leg.x.pos = leg.x.pos.leg.right
#     leg.y.pos = leg.y.pos.leg.right
#     idx.y.pos = idx.y.pos.leg.right
#     idx.x.pos = idx.x.pos.leg.right
#   }
#   
#   gg.n  = 1
#   for(site in l.site){
#     
#     #--- Pass to template df
#     gg.analysis.df = sim.res.all$plan.sim[sim.res.all$plan.sim$sim.id == site,]
#     
#     #--- Plot y-limits
#     if(same.ylimits.dap){
#       
#       y.min = min(sim.res.all$plan.sim[,agg.var])
#       y.max = max(sim.res.all$plan.sim[,agg.var])
#       
#       x.min = min(sim.res.all$plan.sim[,"dap"])
#       x.max = max(sim.res.all$plan.sim[,"dap"])
#       
#     }else{
#       
#       y.min = min(gg.analysis.df[,agg.var])
#       y.max = max(gg.analysis.df[,agg.var])
#       x.min = min(gg.analysis.df[,"dap"])
#       x.max = max(gg.analysis.df[,"dap"])
#       
#     }
#     
#     y.lim = c(y.min,y.max)
#     x.lim = c(x.min,x.max)
#     
#     #--- Treatment legend values
#     gg.analysis.df$treatment.orig = gg.analysis.df$treatment
#     gg.analysis.df$treatment      = as.factor(treatment.df$mulch.biom[match(gg.analysis.df$treatment,treatment.df$treat.id)] / 1000)
#     
#     #--- Aggregate data
#     df.agg = gg.analysis.df[,c("dap",agg.var,"sim.id","treatment","year.run")]
#     colnames(df.agg)[2] = "agg.var"
#     
#     #--- Remove last day to avoid difference in comparison with leap years
#     max.dap = max(df.agg$dap)
#     df.agg = df.agg[df.agg$dap < max.dap,]
#     
#     #--- Remove days before planting
#     df.agg = df.agg[df.agg$dap > 0,]
#     
#     df.agg.var = data.frame(dap       = aggregate(agg.var ~ dap + treatment, df.agg, mean)[,1],
#                             treatment = aggregate(agg.var ~ dap + treatment, df.agg, mean)[,2],
#                             mean      = aggregate(agg.var ~ dap + treatment, df.agg, mean)[,3],
#                             median    = aggregate(agg.var ~ dap + treatment, df.agg, median)[,3],
#                             sd        = aggregate(agg.var ~ dap + treatment, df.agg, sd)[,3],
#                             max       = aggregate(agg.var ~ dap + treatment, df.agg, max)[,3],
#                             min       = aggregate(agg.var ~ dap + treatment, df.agg, min)[,3])
#     
#     #--- Limit standard errors to max-min values
#     df.agg.var$sd[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)] = max(df.agg$agg.var) - df.agg.var$mean[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)]
#     df.agg.var$sd[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] = df.agg.var$mean[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] - min(df.agg$agg.var) 
#     df.agg.var$site = site
#     
#     gg.var = ggplot(df.agg.var, aes_string(y = "mean", x = "dap", fill = "treatment", colour = "treatment")) + 
#       geom_ribbon(aes(ymin = mean-sd, ymax=mean+sd), 
#                   alpha = alpha.ribbon,
#                   linetype = ltype.ribbon.border,
#                   size = lsize.ribbon.border) +
#       geom_line(size = line.size) +
#       scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
#       scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
#       scale_x_continuous(breaks = x.breaks, expand = c(0.01,0.01)) + 
#       ylim(y.lim) + 
#       theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
#             panel.background = NULL,
#             legend.position= c(leg.x.pos,leg.y.pos),
#             legend.text  = element_text(size=16),
#             legend.title = element_text(size=14),
#             axis.line    = element_line(colour = "black", size = 0.2),
#             axis.text    = element_text(size=22, colour = "black"),
#             axis.title   = element_text(size=22),
#             axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
#             axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)),
#             plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
#             legend.box.background = element_rect(colour = "black"))
#     
#     gg.var$labels$x = "Days After Planting (DAP)"
#     gg.var$labels$y = y.lab
#     gg.var$labels$colour = "Mulch Cover\n(ton/ha)"
#     gg.var$labels$fill   = "Mulch Cover\n(ton/ha)"
#     
#     #--- Add markers
#     y.range = y.lim[2] - y.lim[1]
#     x.range = x.lim[2] - x.lim[1]
#     
#     pos.fig.id  = c(x.lim[1] + x.range * idx.x.pos, y.lim[1] + y.range * idx.y.pos)
#     gg.var      = gg.var + geom_text(x=pos.fig.id[1], y=pos.fig.id[2], label= letter.idx[gg.n],
#                                             parse = F,
#                                             color = "black",
#                                             size = font.size.idx)
#     
#     #--- On-Harvesting Results
#     if(agg.var %in% l.on.harv.var){
#       
#       #--- Extract close to harvest results
#       df.agg.end = df.agg[df.agg$dap == (max.dap - 1),]
#       df.agg.end$treatment = as.factor(df.agg.end$treatment)
#       
#       #--- Boxplots
#       agg.var.bp = ggplot(df.agg.end, aes(y = agg.var, x = treatment)) + 
#         geom_boxplot(aes(y = agg.var, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5, size =1) + 
#         scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
#         scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
#         theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
#               panel.background = NULL,
#               legend.position="none",
#               axis.line    = element_line(colour = "black", size = 0.2),
#               axis.text    = element_text(size=24, colour = "black"),
#               axis.title   = element_text(size=26),
#               axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#               axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
#               plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
#       
#       agg.var.bp$labels$x = "Mulch Cover Biomass (ton/ha)"
#       agg.var.bp$labels$y = y.lab
#       
#       if(add.jitter){
#         #--- Add jitter points
#         bp.wid = mean(ggplot_build(agg.var.bp)$data[[1]]$xmax - ggplot_build(agg.var.bp)$data[[1]]$xmin)
#         agg.var.bp = agg.var.bp + 
#           geom_jitter(aes(group = treatment),
#                       size = 1, 
#                       shape=16, 
#                       alpha = 0.5, position=position_jitter(bp.wid/2))
#         
#       }
#       
#       #--- Add markers
#       y.range = max(agg.var.bp$data$agg.var) - min(agg.var.bp$data$agg.var)
#       
#       pos.fig.id  = c(idx.x.pos.bp, min(agg.var.bp$data$agg.var) + y.range * idx.y.pos)
#       agg.var.bp  = agg.var.bp + geom_text(x= pos.fig.id[1], y=pos.fig.id[2], label= letter.idx[gg.n],
#                                        parse = F,
#                                        color = "black",
#                                        size = font.size.idx)
#       
#       #--- Descriptive-stat
#       summary.stat.end = data.frame(var.sam   = agg.var,
#                                     sim.id    = aggregate(agg.var ~ treatment + sim.id, df.agg.end, mean)[,2],
#                                     treatment = aggregate(agg.var ~ treatment + sim.id, df.agg.end, mean)[,1],
#                                     mean      = aggregate(agg.var ~ treatment + sim.id, df.agg.end, mean)[,3],
#                                     sd        = aggregate(agg.var ~ treatment + sim.id, df.agg.end, sd)[,3],
#                                     median    = aggregate(agg.var ~ treatment + sim.id, df.agg.end, median)[,3],
#                                     min       = aggregate(agg.var ~ treatment + sim.id, df.agg.end, min)[,3],
#                                     max       = aggregate(agg.var ~ treatment + sim.id, df.agg.end, max)[,3],
#                                     count     = aggregate(agg.var ~ treatment + sim.id, df.agg.end, length)[,3])
#       
#       #--- Coefficient of variation (%)
#       summary.stat.end$cv = summary.stat.end$sd / summary.stat.end$mean * 100
#       
#       #--- Variance test
#       res.aov       = aov(agg.var ~ treatment, data = df.agg.end)
#       p.value       = summary(res.aov)[[1]][["Pr(>F)"]][1]
#       
#       #--- Normality Test
#       aov.residuals = residuals(object = res.aov)
#       norm.test     = shapiro.test(x = aov.residuals)
#       p.value.norm  = norm.test$p.value
#       k.test        = kruskal.test(agg.var ~ treatment, data = df.agg.end)
#       
#       #--- Tukey groups and kruskal
#       hsd.group = HSD.test(res.aov, "treatment", group=TRUE)
#       
#       #--- Bind tests results
#       aov.norm.df = data.frame(aov.pvalue = p.value,
#                                norm.pvalue= p.value.norm,
#                                k.test     = k.test$p.value)
#       aov.norm.df$type = agg.var
#       aov.norm.df$unit = agg.var.units
#       aov.norm.df$site = site
#       
#       #--- Bind groups results
#       tukey.groups = hsd.group$groups
#       colnames(tukey.groups) = c("mean","groups")
#       tukey.groups$type = agg.var
#       tukey.groups$unit = agg.var.units
#       tukey.groups$site = site
#       tukey.groups$treatment = summary.stat.end$treatment[match(summary.stat.end$mean, tukey.groups$mean)]
#       
#     }
#     
#     
#     if(init.df.summary){
#       summary.stat.end.all = summary.stat.end
#       init.df.summary = F
#     }else{
#       summary.stat.end.all = rbind(summary.stat.end.all,summary.stat.end)
#     }
#     
#     
#     #--- Bind plots and statistics
#     if(site == l.site[1]){
#       
#       #--- ggplots
#       list.gg.dap = list(gg.var)
#       list.gg.bp  = list(agg.var.bp)
#       
#       names(list.gg.dap) = paste0(site,"_",agg.var)
#       names(list.gg.bp)  = paste0(site,"_",agg.var)
#       
#       #--- stat results
#       aov.norm.df.all  = aov.norm.df
#       tukey.groups.all = tukey.groups
#       
#     }else{
#       #--- ggplots
#       list.gg.dap[[gg.n]] = gg.var
#       list.gg.bp[[gg.n]]  = agg.var.bp
#       
#       names(list.gg.dap)[length(list.gg.dap)] = paste0(site,"_",agg.var)
#       names(list.gg.bp)[length(list.gg.bp)]   = paste0(site,"_",agg.var)
#       
#       #--- stat results
#       aov.norm.df.all  = rbind(aov.norm.df.all, aov.norm.df)
#       tukey.groups.all = rbind(tukey.groups.all,tukey.groups)
#     }
#     
#     gg.n  =  gg.n + 1
#     
#     #--- Arrange plots
#     if(site == l.site[length(l.site)]){
#       gg.arrange.plan.dap = ggarrange(plotlist = list.gg.dap)
#       gg.arrange.plan.bp  = ggarrange(plotlist = list.gg.bp)
#       
#       #--- Save ggs
#       ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sims_dap_",analysis.id,"_",agg.var,".png"), 
#              plot = gg.arrange.plan.dap, width = 12, height = 12,
#              dpi = gg.dpi)
#       
#       #--- Save ggs
#       ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sims_bp_",analysis.id,"_",agg.var,".png"), 
#              plot = gg.arrange.plan.bp, width = 12, height = 12,
#              dpi = gg.dpi)
#       
#       #--- Write stat results
#       write.csv(aov.norm.df.all, file = paste0(wd.repo,"/R/R_samuca/results_perf/p_values_treatments_",analysis.id,"_",agg.var,".csv"), row.names = F)
#       
#       #--- Write stat results
#       write.csv(tukey.groups.all, file = paste0(wd.repo,"/R/R_samuca/results_perf/t_groups_treatments_",analysis.id,"_",agg.var,".csv"), row.names = F)
#       
#     }
#   }
# }
# 
# 
# #--- Write summary stat results
# write.csv(summary.stat.end.all, file = paste0(wd.repo,"/R/R_samuca/results_perf/summary_stats_",analysis.id,".csv"), row.names = F)
# 
# summary.stat.end.all.plan = summary.stat.end.all
# 
# l.var.cv = c("fw.st","dw.su")
# 
# summary.df.gg = summary.stat.end.all[summary.stat.end.all$var.sam %in% l.var.cv,]
# summary.df.gg$treatment = as.numeric(levels(summary.df.gg$treatment))[summary.df.gg$treatment] 
# 
# cv.gg = ggplot(summary.df.gg, aes(y = cv, x = treatment)) + 
#   geom_point(size = 4, aes(shape = var.sam, colour = sim.id)) + 
#   geom_smooth(data = summary.df.gg, aes(y = cv, x = treatment, colour = sim.id),method='lm',formula=y~x, size = 0.5, alpha = 0.1) +
#   scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
#   scale_x_continuous(breaks = seq(0,18,by = 6), expand = c(0.01,0.01), limits = c(-2,20)) + 
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
#         panel.background = NULL,
#         legend.position=c(),
#         axis.line    = element_line(colour = "black", size = 0.2),
#         axis.text    = element_text(size=24, colour = "black"),
#         axis.title   = element_text(size=26),
#         axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#         axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
#         plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
# 
# cv.gg$labels$y = "CV (%)"
# cv.gg$labels$x = "Mulch Cover (ton/ha)"
# cv.gg$labels$shape  = "Location"
# cv.gg$labels$colour = "Variable"
# 
# #-----------------#
# #--- Water-use ---#
# #-----------------#
# 
# l.site = unique(sim.res.all$plan.sim$sim.id)
# analysis.id = "atmo"
# 
# l.var = c("et","et.acc","pet")
# letter.idx = c("(a)","(b)","(c)","(d)")
# leg.left = c(F,T,F)
# 
# idx.x.pos.bp = 0.7
# font.size.idx = 8
# gg.dpi       = 500
# 
# same.ylimits.dap = T
# sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index.csv"), as.is = T )
# 
# for(agg.var in l.var){
#   
#   if(agg.var == l.var[1]){init.df.summary = T}
#   y.lab = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
#                  " (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
#   
#   agg.var.units = sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var]
#   
#   if(leg.left[l.var == agg.var]){
#     #--- Left side legend
#     leg.x.pos = leg.x.pos.leg.left
#     leg.y.pos = leg.y.pos.leg.left
#     idx.y.pos = idx.y.pos.leg.left
#     idx.x.pos = idx.x.pos.leg.left
#   }else{
#     #--- Right side legend
#     leg.x.pos = leg.x.pos.leg.right
#     leg.y.pos = leg.y.pos.leg.right
#     idx.y.pos = idx.y.pos.leg.right
#     idx.x.pos = idx.x.pos.leg.right
#   }
#   
#   
#   gg.n  = 1
#   for(site in l.site){
#     
#     #--- Pass to template df
#     gg.analysis.df = sim.res.all$atmo.sim[sim.res.all$atmo.sim$sim.id == site,]
#     
#     #--- Plot y-limits
#     if(same.ylimits.dap){
#       
#       y.min = min(sim.res.all$atmo.sim[,agg.var])
#       y.max = max(sim.res.all$atmo.sim[,agg.var])
#       
#       x.min = min(sim.res.all$atmo.sim[,"dap"])
#       x.max = max(sim.res.all$atmo.sim[,"dap"])
#       
#     }else{
#       
#       y.min = min(gg.analysis.df[,agg.var])
#       y.max = max(gg.analysis.df[,agg.var])
#       x.min = min(gg.analysis.df[,"dap"])
#       x.max = max(gg.analysis.df[,"dap"])
#       
#     }
#     
#     y.lim = c(y.min,y.max)
#     x.lim = c(x.min,x.max)
#     
#     #--- Treatment legend values
#     gg.analysis.df$treatment.orig = gg.analysis.df$treatment
#     gg.analysis.df$treatment      = as.factor(treatment.df$mulch.biom[match(gg.analysis.df$treatment,treatment.df$treat.id)] / 1000)
#     
#     #--- Aggregate data
#     df.agg = gg.analysis.df[,c("dap",agg.var,"sim.id","treatment","year.run")]
#     colnames(df.agg)[2] = "agg.var"
#     
#     #--- Remove last day to avoid difference in comparison with leap years
#     max.dap = max(df.agg$dap)
#     df.agg = df.agg[df.agg$dap < max.dap,]
#     
#     #--- Remove days before planting
#     df.agg = df.agg[df.agg$dap > 0,]
#     
#     df.agg.var = data.frame(dap       = aggregate(agg.var ~ dap + treatment, df.agg, mean)[,1],
#                             treatment = aggregate(agg.var ~ dap + treatment, df.agg, mean)[,2],
#                             mean      = aggregate(agg.var ~ dap + treatment, df.agg, mean)[,3],
#                             median    = aggregate(agg.var ~ dap + treatment, df.agg, median)[,3],
#                             sd        = aggregate(agg.var ~ dap + treatment, df.agg, sd)[,3],
#                             max       = aggregate(agg.var ~ dap + treatment, df.agg, max)[,3],
#                             min       = aggregate(agg.var ~ dap + treatment, df.agg, min)[,3])
#     
#     #--- Limit standard errors to max-min values
#     df.agg.var$sd[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)] = max(df.agg$agg.var) - df.agg.var$mean[df.agg.var$sd + df.agg.var$mean > max(df.agg$agg.var)]
#     df.agg.var$sd[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] = df.agg.var$mean[df.agg.var$mean - df.agg.var$sd < min(df.agg$agg.var)] - min(df.agg$agg.var) 
#     df.agg.var$site = site
#     
#     gg.var = ggplot(df.agg.var, aes_string(y = "mean", x = "dap", fill = "treatment", colour = "treatment")) + 
#       geom_ribbon(aes(ymin = mean-sd, ymax=mean+sd), 
#                   alpha = alpha.ribbon,
#                   linetype = ltype.ribbon.border,
#                   size = lsize.ribbon.border) +
#       geom_line(size = line.size) +
#       scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
#       scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
#       scale_x_continuous(breaks = x.breaks, expand = c(0.01,0.01)) + 
#       ylim(y.lim) + 
#       theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
#             panel.background = NULL,
#             legend.position= c(leg.x.pos,leg.y.pos),
#             legend.text  = element_text(size=16),
#             legend.title = element_text(size=14),
#             axis.line    = element_line(colour = "black", size = 0.2),
#             axis.text    = element_text(size=22, colour = "black"),
#             axis.title   = element_text(size=22),
#             axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
#             axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)),
#             plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
#             legend.box.background = element_rect(colour = "black"))
#     
#     gg.var$labels$x = "Days After Planting (DAP)"
#     gg.var$labels$y = y.lab
#     gg.var$labels$colour = "Mulch Cover\n(ton/ha)"
#     gg.var$labels$fill   = "Mulch Cover\n(ton/ha)"
#     
#     #--- Add markers
#     y.range = y.lim[2] - y.lim[1]
#     x.range = x.lim[2] - x.lim[1]
#     
#     pos.fig.id  = c(x.lim[1] + x.range * idx.x.pos, y.lim[1] + y.range * idx.y.pos)
#     gg.var      = gg.var + geom_text(x=pos.fig.id[1], y=pos.fig.id[2], label= letter.idx[gg.n],
#                                      parse = F,
#                                      color = "black",
#                                      size = font.size.idx)
#     
#     #--- Total ET
#     if(agg.var == "et.acc"){
#       
#       #--- Extract close to harvest results
#       df.agg.end = df.agg[df.agg$dap == (max.dap - 1),]
#       df.agg.end$treatment = as.factor(df.agg.end$treatment)
#       
#       #--- Boxplots
#       agg.var.bp = ggplot(df.agg.end, aes(y = agg.var, x = treatment, fill = treatment)) + 
#         geom_boxplot(outlier.shape=21, outlier.size = 2.5, size =1) + 
#         scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
#         theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
#               panel.background = NULL,
#               legend.position="none",
#               axis.line    = element_line(colour = "black", size = 0.2),
#               axis.text    = element_text(size=24, colour = "black"),
#               axis.title   = element_text(size=26),
#               axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#               axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
#               plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
#       
#       agg.var.bp$labels$x = "Mulch Cover Biomass (ton/ha)"
#       agg.var.bp$labels$y = "Total Evapotranspiration (mm)"
#       
#       if(add.jitter){
#         #--- Add jitter points
#         bp.wid = mean(ggplot_build(agg.var.bp)$data[[1]]$xmax - ggplot_build(agg.var.bp)$data[[1]]$xmin)
#         agg.var.bp = agg.var.bp + 
#           geom_jitter(aes(group = treatment),
#                       size = 1, 
#                       shape=16, 
#                       alpha = 0.5, position=position_jitter(bp.wid/2))
#         
#       }
#       
#       
#       #--- Add markers
#       y.range = max(agg.var.bp$data$agg.var) - min(agg.var.bp$data$agg.var)
#       
#       pos.fig.id  = c(idx.x.pos.bp, min(agg.var.bp$data$agg.var) + y.range * idx.y.pos)
#       agg.var.bp  = agg.var.bp + geom_text(x= pos.fig.id[1], y=pos.fig.id[2], label= letter.idx[gg.n],
#                                            parse = F,
#                                            color = "black",
#                                            size = font.size.idx)
#       
#       #--- Descriptive-stat
#       summary.stat.end = data.frame(var.sam   = agg.var,
#                                     sim.id    = aggregate(agg.var ~ treatment + sim.id, df.agg.end, mean)[,2],
#                                     treatment = aggregate(agg.var ~ treatment + sim.id, df.agg.end, mean)[,1],
#                                     mean      = aggregate(agg.var ~ treatment + sim.id, df.agg.end, mean)[,3],
#                                     sd        = aggregate(agg.var ~ treatment + sim.id, df.agg.end, sd)[,3],
#                                     median    = aggregate(agg.var ~ treatment + sim.id, df.agg.end, median)[,3],
#                                     min       = aggregate(agg.var ~ treatment + sim.id, df.agg.end, min)[,3],
#                                     max       = aggregate(agg.var ~ treatment + sim.id, df.agg.end, max)[,3],
#                                     count     = aggregate(agg.var ~ treatment + sim.id, df.agg.end, length)[,3])
#       
#       #--- Coefficient of variation (%)
#       summary.stat.end$cv = summary.stat.end$sd / summary.stat.end$mean * 100
#       
#       #--- Variance test
#       res.aov       = aov(agg.var ~ treatment, data = df.agg.end)
#       p.value       = summary(res.aov)[[1]][["Pr(>F)"]][1]
#       
#       #--- Normality Test
#       aov.residuals = residuals(object = res.aov)
#       norm.test     = shapiro.test(x = aov.residuals)
#       p.value.norm  = norm.test$p.value
#       k.test        = kruskal.test(agg.var ~ treatment, data = df.agg.end)
#       
#       #--- Tukey groups and kruskal
#       hsd.group = HSD.test(res.aov, "treatment", group=TRUE)
#       
#       #--- Bind tests results
#       aov.norm.df = data.frame(aov.pvalue = p.value,
#                                norm.pvalue= p.value.norm,
#                                k.test     = k.test$p.value)
#       aov.norm.df$type = agg.var
#       aov.norm.df$unit = agg.var.units
#       aov.norm.df$site = site
#       
#       #--- Bind groups results
#       tukey.groups = hsd.group$groups
#       colnames(tukey.groups) = c("mean","groups")
#       tukey.groups$type = agg.var
#       tukey.groups$unit = agg.var.units
#       tukey.groups$site = site
#       tukey.groups$treatment = summary.stat.end$treatment[match(summary.stat.end$mean, tukey.groups$mean)]
#       
#     }
#     
#     if(init.df.summary){
#       summary.stat.end.all = summary.stat.end
#       init.df.summary = F
#     }else{
#       summary.stat.end.all = rbind(summary.stat.end.all,summary.stat.end)
#     }
#     
#     #--- Bind plots and statistics
#     if(site == l.site[1]){
#       
#       #--- ggplots
#       list.gg.dap = list(gg.var)
#       list.gg.bp  = list(agg.var.bp)
#       
#       names(list.gg.dap) = paste0(site,"_",agg.var)
#       names(list.gg.bp)  = paste0(site,"_",agg.var)
#       
#       #--- stat results
#       aov.norm.df.all  = aov.norm.df
#       tukey.groups.all = tukey.groups
#       
#      
#     }else{
#       #--- ggplots
#       list.gg.dap[[gg.n]] = gg.var
#       list.gg.bp[[gg.n]]  = agg.var.bp
#       
#       names(list.gg.dap)[length(list.gg.dap)] = paste0(site,"_",agg.var)
#       names(list.gg.bp)[length(list.gg.bp)]   = paste0(site,"_",agg.var)
#       
#       #--- stat results
#       aov.norm.df.all  = rbind(aov.norm.df.all, aov.norm.df)
#       tukey.groups.all = rbind(tukey.groups.all,tukey.groups)
#       
#     }
#     
#     gg.n  =  gg.n + 1
#     
#     #--- Arrange plots
#     if(site == l.site[length(l.site)]){
#       gg.arrange.plan.dap = ggarrange(plotlist = list.gg.dap)
#       gg.arrange.plan.bp  = ggarrange(plotlist = list.gg.bp)
#       
#       #--- Save ggs
#       ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sims_dap_",analysis.id,"_",agg.var,".png"), 
#              plot = gg.arrange.plan.dap, width = 12, height = 12,
#              dpi = gg.dpi)
#       
#       if(agg.var == "et.acc"){
#         #--- Save ggs
#         ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sims_bp_",analysis.id,"_",agg.var,".png"), 
#                plot = gg.arrange.plan.bp, width = 12, height = 12,
#                dpi = gg.dpi)
#         
#         #--- Write stat results
#         write.csv(aov.norm.df.all, file = paste0(wd.repo,"/R/R_samuca/results_perf/p_values_treatments_",analysis.id,"_",agg.var,".csv"), row.names = F)
#         
#         #--- Write stat results
#         write.csv(tukey.groups.all, file = paste0(wd.repo,"/R/R_samuca/results_perf/t_groups_treatments_",analysis.id,"_",agg.var,".csv"), row.names = F)
#         
#       }
#       
#     }
#   }
# }
# 
# #--- Write summary stat results
# write.csv(summary.stat.end.all, file = paste0(wd.repo,"/R/R_samuca/results_perf/summary_stats_",analysis.id,".csv"), row.names = F)
# 
# summary.stat.end.all.et = summary.stat.end.all
# 
# summary.stat.end.all = rbind(summary.stat.end.all.plan,summary.stat.end.all.et)
# 
# l.var.cv = c("et.acc","fw.st","dw.su")
# 
# summary.df.gg = summary.stat.end.all[summary.stat.end.all$var.sam %in% l.var.cv,]
# summary.df.gg$treatment = as.numeric(levels(summary.df.gg$treatment))[summary.df.gg$treatment] 
# 
# cv.gg = ggplot(summary.df.gg, aes(y = cv, x = treatment)) + 
#   geom_point(size = 4, aes(shape = var.sam, colour = sim.id)) + 
#   geom_smooth(data = summary.df.gg, aes(y = cv, x = treatment, colour = sim.id),method='lm',formula=y~x, size = 0.5, alpha = 0.1) +
#   scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
#   scale_x_continuous(breaks = seq(0,18,by = 6), expand = c(0.01,0.01), limits = c(-2,20)) + 
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
#         panel.background = NULL,
#         legend.position=c(),
#         axis.line    = element_line(colour = "black", size = 0.2),
#         axis.text    = element_text(size=24, colour = "black"),
#         axis.title   = element_text(size=26),
#         axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#         axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
#         plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
# 
# cv.gg$labels$y = "CV (%)"
# cv.gg$labels$x = "Mulch Cover (ton/ha)"
# cv.gg$labels$shape  = "Location"
# cv.gg$labels$colour = "Variable"
# 
# cv.gg



