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

last.day.ofset = 5

#--- select filter for soil for each location
location.names.df = read.csv(paste0(wd.repo,"/R/R_samuca/long_sim_ids.csv"), as.is = T)
sim.leg.index     = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index_final_plots.csv"), as.is = T)
filter.soil       = T
add.jitter        = T

analysis.id.df = data.frame(analysis.id = c("plan" , "atmo"),
                            l.var       = c("fw.st", "et.acc"))

l.idx = c("dap","das","sim.id","treatment","year.run","meta.id")


  #--- combine files
  init.res = T
  
  for(f in analysis.id.df$analysis.id){
    
    init.res.var = T
    
    file.sim.id = paste0("sim_res_all_",f,"_")
    
    #--- Append all soils together
    res.fn = paste0(file.sim.id,l.meta.id[1],"_",run.id,".csv")
    plan.sim.m.p = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T)
    
    res.fn = paste0(file.sim.id,l.meta.id[2],"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    res.fn = paste0(file.sim.id,l.meta.id[3],"_",run.id,".csv")
    plan.sim.m.p = rbind(plan.sim.m.p, read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/long_sim_results/",res.fn),as.is = T))
    
    gg.analysis.df = plan.sim.m.p
    
    #--- filter soil per site
    if(filter.soil){
      
      message(paste0("Filtering soil for ",f))
      #--- filter to corresponding soil type
      for(site in location.names.df$id){
        
        gg.analysis.df[gg.analysis.df$sim.id == site,] = gg.analysis.df[gg.analysis.df$sim.id == site & 
                                                                          gg.analysis.df$meta.id == location.names.df$meta.id[location.names.df$id == site],] 
        
        message(unique(gg.analysis.df$meta.id[gg.analysis.df$sim.id == site]))
        
      }
    }
    
    #--- bind results per variable
    for(res.var in analysis.id.df$l.var[analysis.id.df$analysis.id == f]){
      
      gg.analysis.df.var = gg.analysis.df[,c(l.idx,res.var)]
      colnames(gg.analysis.df.var)[length(gg.analysis.df.var)] = "var.res"
      
      y.lab = paste0(sim.leg.index$label_var[sim.leg.index$samuca_sim_Rcode == res.var],
                     " (",sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == res.var],")")
      
      agg.var.units = sim.leg.index$units[sim.leg.index$samuca_sim_Rcode == res.var]
      
      
      gg.analysis.df.var$var.name   = res.var
      gg.analysis.df.var$var.leg    = y.lab
      gg.analysis.df.var$var.units  = agg.var.units
      
      if(init.res.var){
        gg.analysis.df.var.all = gg.analysis.df.var
        init.res.var = F
      }else{
        gg.analysis.df.var.all = rbind(gg.analysis.df.var.all, gg.analysis.df.var)
      }
      
    }
    
    #--- bind results per f
    if(init.res){
      
      gg.analysis.df.all = gg.analysis.df.var.all
      init.res = F
      
    }else{
      gg.analysis.df.all = rbind(gg.analysis.df.all,gg.analysis.df.var.all)
    }
    
  }
  
  gg.analysis.df = gg.analysis.df.all
  gg.analysis.df = gg.analysis.df[!duplicated(gg.analysis.df),]
  
  #--- Treatment legend values
  gg.analysis.df$treatment.orig = gg.analysis.df$treatment
  gg.analysis.df$treatment      = as.factor(treatment.df$mulch.biom[match(gg.analysis.df$treatment,treatment.df$treat.id)] / 1000)
  
  #--- Aggregate data
  df.agg = gg.analysis.df
  
  #--- Remove last day to avoid difference in comparison with leap years
  max.dap = max(df.agg$dap)
  df.agg = df.agg[df.agg$dap < max.dap,]
  
  #--- Remove days before planting
  df.agg = df.agg[df.agg$dap > 0,]
  
  df.agg.var = data.frame(dap       = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg,   mean)[,1],
                          treatment = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg,   mean)[,2],
                          site      = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg,   mean)[,3],
                          soil      = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg,   mean)[,4],
                          var.name  = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg,   mean)[,5],
                          mean      = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg,   mean)[,6],
                          median    = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg, median)[,6],
                          sd        = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg,     sd)[,6],
                          max       = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg,    max)[,6],
                          min       = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg,    min)[,6],
                          count     = aggregate(var.res ~ dap + treatment + sim.id + meta.id + var.name, df.agg, length)[,6])
  
  #--- Limit standard errors to max-min values
  
  df.agg.var$sd[df.agg.var$mean + df.agg.var$sd > df.agg.var$max] = df.agg.var$max[df.agg.var$mean + df.agg.var$sd > df.agg.var$max] - df.agg.var$mean[df.agg.var$mean + df.agg.var$sd > df.agg.var$max]
  df.agg.var$sd[df.agg.var$mean - df.agg.var$sd < df.agg.var$min] = df.agg.var$mean[df.agg.var$mean - df.agg.var$sd < df.agg.var$min] - df.agg.var$min[df.agg.var$mean - df.agg.var$sd < df.agg.var$min]
  df.agg.var$site = location.names.df$name[match(as.character(df.agg.var$site),as.character(location.names.df$id))]
  df.agg.var$soil = capitalize(df.agg.var$soil)
  df.agg.var$var.leg = df.agg$var.leg[match(as.character(df.agg.var$var.name),as.character(df.agg$var.name))]
  
  x.breaks = seq(0,365, by = 100)
  
  #--- Separete by variable
    gg.var = ggplot(df.agg.var[,], aes_string(y = "mean", x = "dap", fill = "treatment", colour = "treatment")) + 
      geom_ribbon(aes(ymin = mean-sd, ymax=mean+sd), 
                  alpha = alpha.ribbon,
                  linetype = ltype.ribbon.border,
                  size = lsize.ribbon.border) +
      geom_line(size = line.size) +
      scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) + 
      scale_x_continuous(breaks = x.breaks, expand = c(0.01,0.01)) + 
      #ylim(y.lim) + 
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.background = NULL,
            legend.position= c(0.855,-0.14),
            legend.direction = "horizontal",
            legend.text  = element_text(size=14),
            legend.title = element_text(size=14),
            axis.line    = element_line(colour = "black", size = 0.2),
            axis.text    = element_text(size=24, colour = "black"),
            axis.title   = element_text(size=26),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
            legend.box.background = element_rect(colour = "black"))
    
    gg.var$labels$x = "Days After Planting (DAP)"
    gg.var$labels$y = NULL
    gg.var$labels$colour = "Mulch Cover\n(ton/ha)"
    gg.var$labels$fill   = "Mulch Cover\n(ton/ha)"
    
    gg.var.facet = gg.var + facet_grid(var.leg~site, scales = "free",switch="y"
    ) + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text.x     = element_text(size = 20),
        strip.text.y     = element_text(size = 21),
        strip.placement = "left",
        strip.background.y = element_blank()
        
      )
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_paper_dap_",run.id,".png"), 
           plot = gg.var.facet, width = 14, height = 7,
           dpi = gg.dpi)
    
  #--- Boxplots
    
    #--- Extract close to harvest results
    df.agg.end = df.agg[df.agg$dap == (max.dap - last.day.ofset),]
    df.agg.end$treatment = as.factor(df.agg.end$treatment)
    df.agg.end$site = location.names.df$name[match(as.character(df.agg.end$sim.id),as.character(location.names.df$id))]
    df.agg.end$soil = capitalize(df.agg.end$meta.id)
    df.agg.end$var.leg = df.agg$var.leg[match(as.character(df.agg.end$var.name),as.character(df.agg$var.name))]
    
    
    
    
    #--- Boxplots
    agg.var.bp = ggplot(df.agg.end, aes(y = var.res, x = treatment)) + 
      geom_boxplot(aes(y = var.res, x = treatment, fill = treatment),outlier.shape=21, outlier.size = 2.5,outlier.alpha = 0, size =0.5) + 
      scale_fill_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
      scale_colour_manual(values= cbPalette[1:length(treatment.df$treat.id)]) +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.background = NULL,
            legend.position="none",
            axis.line    = element_line(colour = "black", size = 0.2),
            axis.text    = element_text(size=20, colour = "black"),
            axis.title   = element_text(size=20),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
    
    agg.var.bp$labels$x = "Mulch Cover Biomass (ton/ha)"
    agg.var.bp$labels$y = NULL
    
    
    if(add.jitter){
      #--- Add jitter points
      bp.wid = mean(ggplot_build(agg.var.bp)$data[[1]]$xmax - ggplot_build(agg.var.bp)$data[[1]]$xmin)
      agg.var.bp = agg.var.bp + 
        geom_jitter(aes(group = treatment),
                    size = 1, 
                    shape=16, 
                    alpha = 0.5, position=position_jitter(bp.wid/2))
      
    }
    
    
    agg.var.bp.facet = agg.var.bp + facet_grid(var.leg~site, scales = "free",switch="y") + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text.x     = element_text(size = 20),
        strip.text.y     = element_text(size = 21),
        strip.placement = "left",
        strip.background.y = element_blank()
      )
    
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_paper_bp_",run.id,".png"), 
           plot = agg.var.bp.facet, width = 14, height = 7,
           dpi = gg.dpi)
    
    
    #---------------------------#
    #--- Relative difference ---#
    #---------------------------#
    
    df.agg$num.treat = as.numeric(levels(df.agg$treatment))[df.agg$treatment]
    
    #--- Separate bare and mulch treatments
    df.bare  = df.agg[df.agg$num.treat == 0 & df.agg$dap == (max.dap - last.day.ofset),]
    df.mulch = df.agg[df.agg$num.treat != 0 & df.agg$dap == (max.dap - last.day.ofset),]
    
    #--- Merge to dap ~ site ~ soil ~ year.run
    merg.col = c("dap","var.name","sim.id","meta.id","year.run")
    df.merge = merge(df.mulch[,c(merg.col,"var.res","treatment")], 
                     df.bare[,c(merg.col,"var.res")], by = merg.col)
    
    
    #--- Compute relative and absolute differences (%)
    df.merge$drel = (df.merge$var.res.x / df.merge$var.res.y - 1) * 100
    df.merge$dabs = df.merge$var.res.x - df.merge$var.res.y
    
    df.merge.harv = df.merge
    
    df.merge.harv$treatment = as.factor(df.merge.harv$treatment)
    df.merge.harv$site = location.names.df$name[match(as.character(df.merge.harv$sim.id),as.character(location.names.df$id))]
    df.merge.harv$soil = capitalize(df.merge.harv$meta.id)
    df.merge.harv$var.leg = df.agg$var.leg[match(as.character(df.merge.harv$var.name),as.character(df.agg$var.name))]
    
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
            axis.text    = element_text(size=20, colour = "black"),
            axis.title   = element_text(size=20),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
    
    agg.var.bp.drel$labels$x = "Mulch Cover Biomass (ton/ha)"
    agg.var.bp.drel$labels$y = NULL
    agg.var.bp.drel$labels$yintercept = NULL
    
    if(add.jitter){
      #--- Add jitter points
      bp.wid = mean(ggplot_build(agg.var.bp.drel)$data[[1]]$xmax - ggplot_build(agg.var.bp.drel)$data[[1]]$xmin)
      agg.var.bp.drel = agg.var.bp.drel + 
        geom_jitter(aes(group = treatment),
                    size = 1, 
                    shape=16, 
                    alpha = 0.5, position=position_jitter(bp.wid/2))
      
    }
    
    
    agg.var.bp.drel.facet = agg.var.bp.drel + facet_grid(var.leg~site, scales = "free",switch="y") + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text.x     = element_text(size = 20),
        strip.text.y     = element_text(size = 20),
        strip.placement = "left",
        strip.background.y = element_blank()
      )
    
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_paper_drel_",run.id,".png"), 
           plot = agg.var.bp.drel.facet, width = 14, height = 7,
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
            legend.direction = "horizontal",
            axis.line    = element_line(colour = "black", size = 0.2),
            axis.text    = element_text(size=24, colour = "black"),
            axis.title   = element_text(size=26),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
            legend.box.background = element_rect(colour = "black"))
    
    agg.var.bp.dabs$labels$x = "Mulch Cover Biomass (ton/ha)"
    agg.var.bp.dabs$labels$y = NULL
    agg.var.bp.dabs$labels$yintercept = NULL
    agg.var.bp.dabs$labels$fill = "Mulch Cover\n(ton/ha)"
    
    if(add.jitter){
      #--- Add jitter points
      bp.wid = mean(ggplot_build(agg.var.bp.dabs)$data[[1]]$xmax - ggplot_build(agg.var.bp.dabs)$data[[1]]$xmin)
      agg.var.bp.dabs = agg.var.bp.dabs + 
        geom_jitter(aes(group = treatment),
                    size = 1, 
                    shape=16, 
                    alpha = 0.5, position=position_jitter(bp.wid/2))
      
    }
    
    agg.var.bp.dabs.facet = agg.var.bp.dabs + facet_grid(var.leg~site, scales = "free",switch="y") + 
      theme(
        strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
        strip.text.x     = element_text(size = 20),
        strip.text.y     = element_text(size = 21),
        strip.placement = "left",
        strip.background.y = element_blank()
      )
    
    
    ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/long_term_sim/long_term_sims_bp_paper_dabs_",run.id,".png"), 
           plot = agg.var.bp.dabs.facet, width = 14, height = 7,
           dpi = gg.dpi)
    
    




