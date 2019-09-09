#--- R SAMUCA GGPLOT SCATTER
#--- MDSV - mAY/2019

gg.scatter = function(perf.dt.plot,
                      sim.obs.idx,
                      meta,
                      plot.screen,
                      p.size,
                      l.size,
                      col.treat,
                      gg.dpi,
                      gg.h,
                      gg.w,
                      wd.out,
                      analysis.id,
                      all.together,
                      n.col,
                      n.row,
                      t,
                      all.sl,
                      sl.lab.df){
  
  
  if(missing(all.sl)){all.sl = T}
  if(is.null(perf.dt.plot)){return(NULL)}
  
  l.plot.var   = unique(perf.dt.plot$meas_ID)
  
  if(missing(t)){t = theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                           panel.background = NULL,
                           #legend.position="none",
                           axis.line    = element_line(colour = "black", size = 0.2),
                           axis.text    = element_text(size=24, colour = "black"),
                           axis.title   = element_text(size=26),
                           axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                           axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))}
  
  var.i = 1
  for(var in l.plot.var){
    
    perf.dt.var = perf.dt.plot[perf.dt.plot$meas_ID == var,]
    y.lab       = paste0("Simulated ", sim.obs.idx$label_var[sim.obs.idx$obs_db_code == var]," (",
                         sim.obs.idx$units[sim.obs.idx$obs_db_code == var],")")
    
    x.lab       = paste0("Observed ", sim.obs.idx$label_var[sim.obs.idx$obs_db_code == var]," (",
                         sim.obs.idx$units[sim.obs.idx$obs_db_code == var],")")
    
    
    var.class   = paste0(sim.obs.idx$class[sim.obs.idx$obs_db_code == var],
                         "_",
                         sim.obs.idx$subclass[sim.obs.idx$obs_db_code == var])
   
    y.lim = c(min(perf.dt.var$val),max(perf.dt.var$val))
    x.lim = c(min(perf.dt.var$val),max(perf.dt.var$val))
    
    perf.dt.var$leg = as.character(leg.name.df$leg[match(perf.dt.var$M_ID, leg.name.df$M_ID)])
    
    perf.dt.var = perf.dt.var[!perf.dt.var$M_ID == 0,]
    
    sim.perf.dt.var = perf.dt.var[perf.dt.var$type == "sim",]
    obs.perf.dt.var = perf.dt.var[perf.dt.var$type == "obs",]
    
    colnames(sim.perf.dt.var)[colnames(sim.perf.dt.var) == "val"] = "sim"
    colnames(obs.perf.dt.var)[colnames(obs.perf.dt.var) == "val"] = "obs"
    
    if(any("meas_ID1" %in% colnames(obs.perf.dt.var))){
      #--- there are more than one sub type
      sim.obs.perf.dt = merge(sim.perf.dt.var,obs.perf.dt.var, by = c("year","doy","das","M_ID","meas_ID","meas_ID1","leg"))  
    }else{
      #--- no subtypes
      sim.obs.perf.dt = merge(sim.perf.dt.var,obs.perf.dt.var, by = c("year","doy","das","M_ID","meas_ID","leg"))  
    }
    
    
    
    if(all.sl){
      
      if(any("meas_ID1" %in% colnames(obs.perf.dt.var))){
        var.class = paste0(var.class,"_all_layers")  
      }
      
    }else{
      #--- if missing use all layers
      if(missing(sl.lab.df)){
        l.sl = unique(sim.obs.perf.dt$meas_ID1)
      }else{
        l.sl = sl.lab.df$l.sl
      }
      
      var.class = paste0(var.class,"_layers_")
      
      for(sl in l.sl){
        var.class = paste0(var.class,sl)  
      }
      
      #--- Only defined layers
      sim.obs.perf.dt = sim.obs.perf.dt[sim.obs.perf.dt$meas_ID1 %in% l.sl,]
    }
    
    #--- Create ggplot base
    gg.var = ggplot(sim.obs.perf.dt, aes_string(y = "sim", x = "obs")) + 
      geom_abline(intercept = 0,slope =  1, linetype = 3, color = "red", size = 1) +
      geom_smooth(method='lm',formula=y~x, colour = "black", size = 0.5, alpha = 0.1) +
      ylab(y.lab) + xlab(x.lab) +
      geom_point(data = sim.obs.perf.dt, 
                 aes_string(y = "sim", x = "obs", fill = "leg"), 
                 color = "black", shape = 21, size = p.size, stroke = 1.2) + 
      scale_fill_manual(values= col.treat) + t
      
    
    gg.limits = c(min(c(ggplot_build(gg.var)$data[[2]]$ymin,
                        gg.var$data$sim,
                        gg.var$data$obs)), 
                  max(c(ggplot_build(gg.var)$data[[2]]$ymax,
                        gg.var$data$sim,
                        gg.var$data$obs)))
    
    gg.var = gg.var + ylim(gg.limits) + xlim(gg.limits)
    
    #--- remove legend title
    gg.var$labels$colour = ""
    gg.var$labels$fill = ""
    
    if(var == l.plot.var[1]){
      
      #--- Initialize gg list
      l.gg.list         = list(gg.var)
      names(l.gg.list)  = var
      
    }else{
      #--- Append to gg list
      l.gg.list[[var.i]]       = gg.var
      names(l.gg.list)[var.i]  = var
    }
    
    
    if(plot.screen){gg.var}
    
    
    if(all.together){
      
      if(var == l.plot.var[length(l.plot.var)]){
        
        message(paste0("Saving ggplot for ",var.class, " variables."))
        
        #--- Arrange all plot together
        gg.var = ggarrange(plotlist = l.gg.list,
                           ncol = n.col, nrow = n.row)
        
        #--- save it
        ggsave(plot     = gg.var,
               filename = paste0(wd.out,"\\",var.class,"_scatter_ggplot_",analysis.id,".png"),
               dpi = gg.dpi,
               height = gg.h, width = gg.w)
        
      }
      
    }else{
      message(paste0("Saving ggplot for ",var))
      ggsave(plot     = gg.var,
             filename = paste0(wd.out,"\\",var,"_scatter_ggplot_",analysis.id,".png"),
             dpi = gg.dpi,
             height = gg.h, width = gg.w)
      
    }
    
    var.i = var.i +  1
    
  }
  
  #--- Return all gg as list
  return(l.gg.list)
  
  
}
