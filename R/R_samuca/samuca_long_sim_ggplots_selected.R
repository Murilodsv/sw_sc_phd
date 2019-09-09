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
#install.packages("plotly")

library(agricolae)
library(ggpubr)
library(tictoc)
library(reshape2)
library(plotly)

#-----------------#
#--- Long-Term ---#
#-----------------#
l.id   = c("long_term")
m.fn   = c("meta_sugar_db_long_simulations_sandy.csv",
           "meta_sugar_db_long_simulations_medium.csv",
           "meta_sugar_db_long_simulations_clay.csv")
nl     = 8
n.meta = 3
l.meta.id = c("sandy","medium","clay")
run.id = "all_combinations"
gg.id  = "msc"

planting.scenarios = T
p.scen.df = read.csv(paste0(wd.repo,"/R/R_samuca/planting_scenarios.csv"), as.is = T)
n.pscen= length(p.scen.df$p.date.scen)

use.last.swc= F
run.sim     = T
save.sim    = T
read.sim    = F
use.debug   = T
samuca.exe  = "samuca_vs_proj.exe"
ini.year = 1979
l.year   = seq(1979,2009)

location.names.df = data.frame(id   = c(paste0("M_ID_",25:28)),
                               name = c("Jatai","Petrolina","Recife", "Piracicaba"))

treatment.df = data.frame(treat.id   = c("bare", "6_ton_ha", "12_ton_ha", "18_ton_ha"),
                          mulch.biom = c(0,6000,12000,18000),
                          mulch.type = c(0,1,1,1))

if(use.last.swc){
  last.swc = read.csv(paste0(wd.repo,"/R/R_samuca/last_swc.csv"), as.is = T)
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

l.var = c("fw.st", "dw.su")
l.on.harv.var = c("fw.st","dw.su")
letter.idx = c("(a)","(b)","(c)","(d)")
leg.left = c(T,T)


add.jitter  = T
add.loess   = T

idx.x.pos.bp = 0.7
font.size.idx = 8
gg.dpi       = 500

same.ylimits.dap = T
sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index.csv"), as.is = T )

file.sim.id = "sim_res_all_plan_"

#--- Selected months
s.months = data.frame(site = c("Jatai","Petrolina","Recife","Piracicaba"),
                      p.mo = c(7,7,2,7),
                      sim.id = c(paste0("M_ID_",25:28)))

#--- gather data from corresponding selected months
ini.df = T
for(p.mo in unique(s.months$p.mo)){
  
  res.fn = paste0(file.sim.id,l.meta.id[1],"_",p.mo,"_",run.id,".csv")
  plan.sim.m.p = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T)
  
  res.fn = paste0(file.sim.id,l.meta.id[2],"_",p.mo,"_",run.id,".csv")
  plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
  
  res.fn = paste0(file.sim.id,l.meta.id[3],"_",p.mo,"_",run.id,".csv")
  plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
  
  sim.id.fil = as.character(s.months$sim.id[s.months$p.mo == p.mo])
  
  plan.sim.m.p = plan.sim.m.p[plan.sim.m.p$sim.id %in% sim.id.fil,]
  
  if(ini.df){
    gg.analysis.df = plan.sim.m.p
    ini.df = F
  }else{
    gg.analysis.df = rbind(gg.analysis.df,plan.sim.m.p)
  }
}

#--- Treatment legend values
gg.analysis.df$treatment.orig = gg.analysis.df$treatment
gg.analysis.df$treatment      = as.factor(treatment.df$mulch.biom[match(gg.analysis.df$treatment,treatment.df$treat.id)] / 1000)


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
  
    #--- Plot y-limits
    if(same.ylimits.dap){
      
      y.min = min(gg.analysis.df[,agg.var])
      y.max = max(gg.analysis.df[,agg.var])
      
      x.min = min(gg.analysis.df[,"dap"])
      x.max = max(gg.analysis.df[,"dap"])
      
    }else{
      
      y.min = min(gg.analysis.df[,agg.var])
      y.max = max(gg.analysis.df[,agg.var])
      x.min = min(gg.analysis.df[,"dap"])
      x.max = max(gg.analysis.df[,"dap"])
      
    }
    
    y.lim = c(y.min,y.max)
    x.lim = c(x.min,x.max)
    
    #--- Aggregate data
    df.agg = gg.analysis.df[,c("dap","das",agg.var,"sim.id","meta.id","treatment","year.run")]
    colnames(df.agg)[3] = "agg.var"
    
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
                            min       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, min)[,5],
                            count     = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, length)[,5])
    
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
    
    
    gg.var.facet = gg.var + facet_grid(soil ~ site, scales = "free") + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text       = element_text(size = 14)
      )
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_dap_",analysis.id,"_",agg.var,"_",gg.id,".png"), 
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
      
      
      agg.var.bp.facet = agg.var.bp + facet_grid(soil ~ site, scales = "free") + 
        theme(
          strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
          strip.text       = element_text(size = 14)
        )
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_",analysis.id,"_",agg.var,"_",gg.id,".png"), 
             plot = agg.var.bp.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
      #---------------------------#
      #--- Relative difference ---#
      #---------------------------#
      
      df.agg$num.treat = as.numeric(levels(df.agg$treatment))[df.agg$treatment]
      
      #--- Separate bare and mulch treatments
      df.bare  = df.agg[df.agg$num.treat == 0 & df.agg$dap == (max.dap - 5),]
      df.mulch = df.agg[df.agg$num.treat != 0 & df.agg$dap == (max.dap - 5),]
      
      #--- Merge to dap ~ site ~ soil ~ year.run
      merg.col = c("dap","sim.id","meta.id","year.run")
      df.merge = merge(df.mulch[,c(merg.col,"agg.var","treatment")], 
                       df.bare[,c(merg.col,"agg.var")], by = merg.col)
      
      #--- Compute relative and absolute differences (%)
      df.merge$drel = (df.merge$agg.var.x / df.merge$agg.var.y - 1) * 100
      df.merge$dabs = df.merge$agg.var.x - df.merge$agg.var.y
      
      df.merge.harv = df.merge
      
      df.merge.harv$treatment = as.factor(df.merge.harv$treatment)
      df.merge.harv$site = location.names.df$name[match(as.character(df.merge.harv$sim.id),as.character(location.names.df$id))]
      df.merge.harv$soil = capitalize(df.merge.harv$meta.id)
      
      #--- Boxplots for Relative difference
      agg.var.bp.drel = ggplot(df.merge.harv, aes(y = drel, x = treatment)) + 
        geom_boxplot(aes(y = drel, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
        geom_hline(yintercept = 0, linetype = 3, colour = "red", size = 0.5) +
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
      
      agg.var.bp.drel$labels$x = "Mulch Cover Biomass (ton/ha)"
      agg.var.bp.drel$labels$y = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
                                        " Difference (%)")
      
      if(add.jitter){
        #--- Add jitter points
        bp.wid = mean(ggplot_build(agg.var.bp.drel)$data[[1]]$xmax - ggplot_build(agg.var.bp.drel)$data[[1]]$xmin)
        agg.var.bp.drel = agg.var.bp.drel + 
          geom_jitter(aes(group = treatment),
                      size = 0.5, 
                      shape=16, 
                      alpha = 0.25, position=position_jitter(bp.wid/2))
        
      }
      
      
      agg.var.bp.drel.facet = agg.var.bp.drel + facet_grid(soil ~ site, scales = "free") + 
        theme(
          strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
          strip.text       = element_text(size = 14)
        )
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_drel_",analysis.id,"_",agg.var,"_",gg.id,".png"), 
             plot = agg.var.bp.drel.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
      
      #--- Boxplots for Absolute difference
      agg.var.bp.dabs = ggplot(df.merge.harv, aes(y = dabs, x = treatment)) + 
        geom_boxplot(aes(y = dabs, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
        geom_hline(yintercept = 0, linetype = 3, colour = "red", size = 0.5) +
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
      
      agg.var.bp.dabs$labels$x = "Mulch Cover Biomass (ton/ha)"
      agg.var.bp.dabs$labels$y = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
                                        " Difference (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
      
      if(add.jitter){
        #--- Add jitter points
        bp.wid = mean(ggplot_build(agg.var.bp.dabs)$data[[1]]$xmax - ggplot_build(agg.var.bp.dabs)$data[[1]]$xmin)
        agg.var.bp.dabs = agg.var.bp.dabs + 
          geom_jitter(aes(group = treatment),
                      size = 0.5, 
                      shape=16, 
                      alpha = 0.25, position=position_jitter(bp.wid/2))
        
      }
      
      
      agg.var.bp.dabs.facet = agg.var.bp.dabs + facet_grid(soil ~ site, scales = "free") + 
        theme(
          strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
          strip.text       = element_text(size = 14)
        )
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_dabs_",analysis.id,"_",agg.var,"_",gg.id,".png"), 
             plot = agg.var.bp.dabs.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
      
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

#--- Selected months
s.months = data.frame(site = c("Jatai","Petrolina","Recife","Piracicaba"),
                      p.mo = c(7,7,2,7),
                      sim.id = c(paste0("M_ID_",25:28)))

#--- gather data from corresponding selected months
ini.df = T
for(p.mo in unique(s.months$p.mo)){
  
  res.fn = paste0(file.sim.id,l.meta.id[1],"_",p.mo,"_",run.id,".csv")
  plan.sim.m.p = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T)
  
  res.fn = paste0(file.sim.id,l.meta.id[2],"_",p.mo,"_",run.id,".csv")
  plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
  
  res.fn = paste0(file.sim.id,l.meta.id[3],"_",p.mo,"_",run.id,".csv")
  plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
  
  sim.id.fil = as.character(s.months$sim.id[s.months$p.mo == p.mo])
  
  plan.sim.m.p = plan.sim.m.p[plan.sim.m.p$sim.id %in% sim.id.fil,]
  
  if(ini.df){
    gg.analysis.df = plan.sim.m.p
    ini.df = F
  }else{
    gg.analysis.df = rbind(gg.analysis.df,plan.sim.m.p)
  }
}

#--- Treatment legend values
gg.analysis.df$treatment.orig = gg.analysis.df$treatment
gg.analysis.df$treatment      = as.factor(treatment.df$mulch.biom[match(gg.analysis.df$treatment,treatment.df$treat.id)] / 1000)


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
  
  #--- Plot y-limits
  if(same.ylimits.dap){
    
    y.min = min(gg.analysis.df[,agg.var])
    y.max = max(gg.analysis.df[,agg.var])
    
    x.min = min(gg.analysis.df[,"dap"])
    x.max = max(gg.analysis.df[,"dap"])
    
  }else{
    
    y.min = min(gg.analysis.df[,agg.var])
    y.max = max(gg.analysis.df[,agg.var])
    x.min = min(gg.analysis.df[,"dap"])
    x.max = max(gg.analysis.df[,"dap"])
    
  }
  
  y.lim = c(y.min,y.max)
  x.lim = c(x.min,x.max)
  
  #--- Aggregate data
  df.agg = gg.analysis.df[,c("dap","das",agg.var,"sim.id","meta.id","treatment","year.run")]
  colnames(df.agg)[3] = "agg.var"
  
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
                          min       = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, min)[,5],
                          count     = aggregate(agg.var ~ dap + treatment + sim.id + meta.id, df.agg, length)[,5])
  
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
  
  
  gg.var.facet = gg.var + facet_grid(soil ~ site, scales = "free") + 
    theme(
      strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
      strip.text       = element_text(size = 14)
    )
  
  ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_dap_",analysis.id,"_",agg.var,"_",gg.id,".png"), 
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
    
    
    agg.var.bp.facet = agg.var.bp + facet_grid(soil ~ site, scales = "free") + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text       = element_text(size = 14)
      )
    
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_",analysis.id,"_",agg.var,"_",gg.id,".png"), 
           plot = agg.var.bp.facet, width = 12, height = 8,
           dpi = gg.dpi)
    
    #---------------------------#
    #--- Relative difference ---#
    #---------------------------#
    
    df.agg$num.treat = as.numeric(levels(df.agg$treatment))[df.agg$treatment]
    
    #--- Separate bare and mulch treatments
    df.bare  = df.agg[df.agg$num.treat == 0 & df.agg$dap == (max.dap - 5),]
    df.mulch = df.agg[df.agg$num.treat != 0 & df.agg$dap == (max.dap - 5),]
    
    #--- Merge to dap ~ site ~ soil ~ year.run
    merg.col = c("dap","sim.id","meta.id","year.run")
    df.merge = merge(df.mulch[,c(merg.col,"agg.var","treatment")], 
                     df.bare[,c(merg.col,"agg.var")], by = merg.col)
    
    #--- Compute relative and absolute differences (%)
    df.merge$drel = (df.merge$agg.var.x / df.merge$agg.var.y - 1) * 100
    df.merge$dabs = df.merge$agg.var.x - df.merge$agg.var.y
    
    df.merge.harv = df.merge
    
    df.merge.harv$treatment = as.factor(df.merge.harv$treatment)
    df.merge.harv$site = location.names.df$name[match(as.character(df.merge.harv$sim.id),as.character(location.names.df$id))]
    df.merge.harv$soil = capitalize(df.merge.harv$meta.id)
    
    #--- Boxplots for Relative difference
    agg.var.bp.drel = ggplot(df.merge.harv, aes(y = drel, x = treatment)) + 
      geom_boxplot(aes(y = drel, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
      geom_hline(yintercept = 0, linetype = 3, colour = "red", size = 0.5) +
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
    
    agg.var.bp.drel$labels$x = "Mulch Cover Biomass (ton/ha)"
    agg.var.bp.drel$labels$y = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
                                      " Difference (%)")
    
    if(add.jitter){
      #--- Add jitter points
      bp.wid = mean(ggplot_build(agg.var.bp.drel)$data[[1]]$xmax - ggplot_build(agg.var.bp.drel)$data[[1]]$xmin)
      agg.var.bp.drel = agg.var.bp.drel + 
        geom_jitter(aes(group = treatment),
                    size = 0.5, 
                    shape=16, 
                    alpha = 0.25, position=position_jitter(bp.wid/2))
      
    }
    
    
    agg.var.bp.drel.facet = agg.var.bp.drel + facet_grid(soil ~ site, scales = "free") + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text       = element_text(size = 14)
      )
    
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_drel_",analysis.id,"_",agg.var,"_",gg.id,".png"), 
           plot = agg.var.bp.drel.facet, width = 12, height = 8,
           dpi = gg.dpi)
    
    
    #--- Boxplots for Absolute difference
    agg.var.bp.dabs = ggplot(df.merge.harv, aes(y = dabs, x = treatment)) + 
      geom_boxplot(aes(y = dabs, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
      geom_hline(yintercept = 0, linetype = 3, colour = "red", size = 0.5) +
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
    
    agg.var.bp.dabs$labels$x = "Mulch Cover Biomass (ton/ha)"
    agg.var.bp.dabs$labels$y = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
                                      " Difference (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
    
    if(add.jitter){
      #--- Add jitter points
      bp.wid = mean(ggplot_build(agg.var.bp.dabs)$data[[1]]$xmax - ggplot_build(agg.var.bp.dabs)$data[[1]]$xmin)
      agg.var.bp.dabs = agg.var.bp.dabs + 
        geom_jitter(aes(group = treatment),
                    size = 0.5, 
                    shape=16, 
                    alpha = 0.25, position=position_jitter(bp.wid/2))
      
    }
    
    
    agg.var.bp.dabs.facet = agg.var.bp.dabs + facet_grid(soil ~ site, scales = "free") + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text       = element_text(size = 14)
      )
    
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_dabs_",analysis.id,"_",agg.var,"_",gg.id,".png"), 
           plot = agg.var.bp.dabs.facet, width = 12, height = 8,
           dpi = gg.dpi)
    
    
  }
}
