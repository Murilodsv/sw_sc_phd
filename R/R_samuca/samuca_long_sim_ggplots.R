#--------------------------------------------#
#--- Generate Long-Term Simulations plots ---#
#--------------------------------------------#

#--- Run SAMUCA for all selected combinations of soils, planting dates and sites

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


m.fn   = c("meta_sugar_db_long_simulations_sandy.csv",
           "meta_sugar_db_long_simulations_medium.csv",
           "meta_sugar_db_long_simulations_clay.csv")

nl     = 8
n.meta = length(m.fn)
l.meta.id = c("sandy","loam","clay")
run.id = "msc_new" # soils according to DOI 10.1007/s12355-015-0367-0

#--- Color-Blind Friendly palette
cbPalette       = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
alpha.ribbon    = 0.2
ltype.ribbon.border = 1
lsize.ribbon.border = 0.1
line.size    = 0.75
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

last.day.ofset = 5

location.names.df = read.csv(paste0(wd.repo,"/R/R_samuca/long_sim_ids.csv"), as.is = T)
sim.leg.index     = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index_final_plots.csv"), as.is = T)

treatment.df = data.frame(treat.id   = c("bare", "6_ton_ha", "12_ton_ha", "18_ton_ha"),
                          mulch.biom = c(0,6000,12000,18000),
                          mulch.type = c(0,1,1,1))

#--- ggplots and statistics function
create.ggplots = function(analysis.id,
                          l.var,
                          l.on.harv.var,
                          letter.idx,
                          leg.left,
                          add.jitter,
                          add.loess,
                          idx.x.pos.bp,
                          font.size.idx,
                          gg.dpi,
                          same.ylimits.dap,
                          sim.leg.index,
                          last.day.ofset){
  
  for(agg.var in l.var){
    
    file.sim.id = paste0("sim_res_all_",analysis.id,"_")
    
    message("Generating plots for: ",agg.var)
    
    if(agg.var == l.var[1]){init.df.summary = T}
    
    y.lab = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == agg.var],
                   " (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
    
    agg.var.units = paste0("(",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == agg.var],")")
    
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
    
    #--- Append all soils together
    res.fn = paste0(file.sim.id,l.meta.id[1],"_",run.id,".csv")
    plan.sim.m.p = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T)
    
    res.fn = paste0(file.sim.id,l.meta.id[2],"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    res.fn = paste0(file.sim.id,l.meta.id[3],"_",run.id,".csv")
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
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_dap_",analysis.id,"_",agg.var,"_",run.id,".png"), 
           plot = gg.var.facet, width = 12, height = 8,
           dpi = gg.dpi)
    
    #--- Boxplots
    if(agg.var %in% l.on.harv.var){
      
      #--- Extract close to harvest results
      df.agg.end = df.agg[df.agg$dap == (max.dap - last.day.ofset),]
      df.agg.end$treatment = as.factor(df.agg.end$treatment)
      df.agg.end$site = location.names.df$name[match(as.character(df.agg.end$sim.id),as.character(location.names.df$id))]
      df.agg.end$soil = capitalize(df.agg.end$meta.id)
      
      message(paste0("Computing Statistical tests for: ",agg.var))
      
      init.stat = T
      for(site in unique(df.agg.end$site)){
        for(soil in unique(df.agg.end$soil)){
          
          df.agg.end.stat = df.agg.end[df.agg.end$site == site & df.agg.end$soil == soil,]
          
          agg.stat = data.frame(sim.id    = aggregate(agg.var ~ sim.id +  meta.id + treatment, df.agg.end.stat, mean)[,1],
                                meta.id   = aggregate(agg.var ~ sim.id +  meta.id + treatment, df.agg.end.stat, mean)[,2],
                                treatment = aggregate(agg.var ~ sim.id +  meta.id + treatment, df.agg.end.stat, mean)[,3],
                                mean      = aggregate(agg.var ~ sim.id +  meta.id + treatment, df.agg.end.stat, mean)[,4],
                                sd        = aggregate(agg.var ~ sim.id +  meta.id + treatment, df.agg.end.stat, sd)[,4])
          
          agg.stat$cv = agg.stat$sd / agg.stat$mean
          
          #--- order 
          agg.stat = agg.stat[order(agg.stat$mean, decreasing = T),]
          
          #--- Variance test
          res.aov       = aov(agg.var ~ treatment, data = df.agg.end.stat)
          p.value       = summary(res.aov)[[1]][["Pr(>F)"]][1]
          
          #--- Normality Test
          aov.residuals = residuals(object = res.aov)
          norm.test     = shapiro.test(x = aov.residuals)
          p.value.norm  = norm.test$p.value
          k.test        = kruskal.test(agg.var ~ treatment, data = df.agg.end.stat)
          
          #--- Tukey groups and kruskal
          hsd.group = HSD.test(res.aov, "treatment", group=TRUE)
          
          #--- Bind tests results
          aov.norm.df = data.frame(aov.pvalue = p.value,
                                   norm.pvalue= p.value.norm,
                                   k.test     = k.test$p.value)
          aov.norm.df$type = agg.var
          aov.norm.df$unit = agg.var.units
          aov.norm.df$site = site
          aov.norm.df$soil = soil
          
          #--- Bind groups results
          tukey.groups = hsd.group$groups
          colnames(tukey.groups) = c("mean","groups")
          tukey.groups$type = agg.var
          tukey.groups$unit = agg.var.units
          tukey.groups$site = site
          tukey.groups$soil = soil
          tukey.groups = cbind(tukey.groups,agg.stat[,c("treatment","sd","cv")])
          
          if(init.stat){
            
            aov.norm.df.b  = aov.norm.df
            tukey.groups.b = tukey.groups
            
            init.stat = F
          }else{
            
            aov.norm.df.b  = rbind(aov.norm.df.b,  aov.norm.df)
            tukey.groups.b = rbind(tukey.groups.b, tukey.groups)
          }
        }
      }
      
      write.csv(aov.norm.df.b, file = paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_aov_norm_",analysis.id,"_",agg.var,"_",run.id,".csv"),
                row.names = F)
      
      write.csv(tukey.groups.b, file = paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_tukey_groups_",analysis.id,"_",agg.var,"_",run.id,".csv"),
                row.names = F)
      
      message(paste0("Generating Boxplots for: ",agg.var))
      
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
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_",analysis.id,"_",agg.var,"_",run.id,".png"), 
             plot = agg.var.bp.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
      
      #---------------------------#
      #--- Relative difference ---#
      #---------------------------#
      
      df.agg$num.treat = as.numeric(levels(df.agg$treatment))[df.agg$treatment]
      
      #--- Separate bare and mulch treatments
      df.bare  = df.agg[df.agg$num.treat == 0 & df.agg$dap == (max.dap - last.day.ofset),]
      df.mulch = df.agg[df.agg$num.treat != 0 & df.agg$dap == (max.dap - last.day.ofset),]
      
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
        scale_fill_manual(values= cbPalette[2:length(treatment.df$treat.id)]) +
        scale_colour_manual(values= cbPalette[2:length(treatment.df$treat.id)]) +
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
                      size = 0.75, 
                      shape=16, 
                      alpha = 0.35, position=position_jitter(bp.wid/2))
        
      }
      
      
      agg.var.bp.drel.facet = agg.var.bp.drel + facet_grid(soil ~ site, scales = "free") + 
        theme(
          strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
          strip.text       = element_text(size = 14)
        )
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_drel_",analysis.id,"_",agg.var,"_",run.id,".png"), 
             plot = agg.var.bp.drel.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
      
      #--- Boxplots for Absolute difference
      agg.var.bp.dabs = ggplot(df.merge.harv, aes(y = dabs, x = treatment)) + 
        geom_boxplot(aes(y = dabs, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
        geom_hline(yintercept = 0, linetype = 3, colour = "red", size = 0.5) +
        scale_fill_manual(values= cbPalette[2:length(treatment.df$treat.id)]) +
        scale_colour_manual(values= cbPalette[2:length(treatment.df$treat.id)]) +
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
                      size = 0.75, 
                      shape=16, 
                      alpha = 0.35, position=position_jitter(bp.wid/2))
        
      }
      
      
      agg.var.bp.dabs.facet = agg.var.bp.dabs + facet_grid(soil ~ site, scales = "free") + 
        theme(
          strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
          strip.text       = element_text(size = 14)
        )
      
      
      ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_dabs_",analysis.id,"_",agg.var,"_",run.id,".png"), 
             plot = agg.var.bp.dabs.facet, width = 12, height = 8,
             dpi = gg.dpi)
      
      
    }
  }
}

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
sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index_final_plots.csv"), as.is = T )

create.ggplots(analysis.id,
               l.var,
               l.on.harv.var,
               letter.idx,
               leg.left,
               add.jitter,
               add.loess,
               idx.x.pos.bp,
               font.size.idx,
               gg.dpi,
               same.ylimits.dap,
               sim.leg.index,
               last.day.ofset)

#------------------#
#--- atmosphere ---#
#------------------#

analysis.id = "atmo"

l.var = c("et","et.acc","tra.dep")
l.on.harv.var = c("et.acc")
letter.idx = c("(a)","(b)","(c)","(d)")
leg.left = c(F,T,F)


add.jitter  = T
add.loess   = T

idx.x.pos.bp = 0.7
font.size.idx = 8
gg.dpi       = 500

same.ylimits.dap = T
sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index_final_plots.csv"), as.is = T )

create.ggplots(analysis.id,
               l.var,
               l.on.harv.var,
               letter.idx,
               leg.left,
               add.jitter,
               add.loess,
               idx.x.pos.bp,
               font.size.idx,
               gg.dpi,
               same.ylimits.dap,
               sim.leg.index,
               last.day.ofset)


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
sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index_final_plots.csv"), as.is = T )

create.ggplots(analysis.id,
               l.var,
               l.on.harv.var,
               letter.idx,
               leg.left,
               add.jitter,
               add.loess,
               idx.x.pos.bp,
               font.size.idx,
               gg.dpi,
               same.ylimits.dap,
               sim.leg.index,
               last.day.ofset)

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
sim.leg.index = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index_final_plots.csv"), as.is = T )

create.ggplots(analysis.id,
               l.var,
               l.on.harv.var,
               letter.idx,
               leg.left,
               add.jitter,
               add.loess,
               idx.x.pos.bp,
               font.size.idx,
               gg.dpi,
               same.ylimits.dap,
               sim.leg.index,
               last.day.ofset)
