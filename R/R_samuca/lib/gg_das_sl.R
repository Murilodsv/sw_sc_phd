#--- R SAMUCA GGPLOT ~ DAS
#--- MDSV - Feb/2019

gg.das.sl = function(perf.dt.plot,
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
                  is.seq,
                  x.breaks,
                  t,
                  all.sl,
                  sl.lab.df){
  
  
  #----------------------------------------------#
  #---------------- ggplot ~ das ----------------#
  #----------------------------------------------#
  
  # perf.dt.plot,         data.frame with sim and obs values
  # sim.obs.idx,          data.frame with index of names labels and units for each variable (R_samuca_perf)
  # meta,                 data.frame with meta information to run R_samuca_perf
  # plot.screen,          flagg for ploting on the screen
  # p.size,               size of points in ggplot
  # col.treat,            col of groups in ggplot
  # gg.dpi,               image resolution
  # gg.h,                 image height
  # gg.w,                 image width
  # wd.out                output path
  # analysis.id           analysis id name
  #----------------------------------------------#
  
  if(is.null(perf.dt.plot)){return(NULL)}
  
  l.plot.var   = unique(perf.dt.plot$meas_ID)
  
  if(missing(t)){t = theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                           panel.background = NULL,
                           legend.position="none",
                           axis.line    = element_line(colour = "black", size = 0.2),
                           axis.text    = element_text(size=24, colour = "black"),
                           axis.title   = element_text(size=26),
                           axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                           axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))}
  
  for(var in l.plot.var){
    
    perf.dt.var = perf.dt.plot[perf.dt.plot$meas_ID == var,]
    y.units     = sim.obs.idx$units[sim.obs.idx$obs_db_code == var]
    y.lab       = paste0(sim.obs.idx$label_var[sim.obs.idx$obs_db_code == var]," (",
                         y.units,")")
    
    var.class   = paste0(sim.obs.idx$class[sim.obs.idx$obs_db_code == var],
                         "_",
                         sim.obs.idx$subclass[sim.obs.idx$obs_db_code == var])
    
    
    if(is.seq){
      
      #--- Use Days after simulation started as x-axis
      x.lab       = "Days After Simulation Started (DAS)"
      x.val.nm    = "das" 
      
    }else{
      
      x.lab       = "Days After Planting (DAP)"
      x.val.nm    = "dap" 
    }
    
    ylim = c(min(perf.dt.var$val),max(perf.dt.var$val))
    xlim = c(0,max(perf.dt.var[,x.val.nm]))
    
    perf.dt.var$leg = as.character(leg.name.df$leg[match(perf.dt.var$M_ID, leg.name.df$M_ID)])
    
    perf.dt.var = perf.dt.var[!perf.dt.var$M_ID == 0,]
    
    #--- store all values in this df
    perf.dt.var.all = perf.dt.var
    
    #--- aggregate for all layers
    perf.dt.var = aggregate(val ~ year + doy + das + dap + meas_ID + type + seq + Field_ID + seq_ID + M_ID + leg,
                            data = perf.dt.var,
                            mean)
    
    #--- Create ggplot base
    gg.var = ggplot(perf.dt.var, aes_string(y = "val", x = x.val.nm, colour = "leg", fill = "leg")) + 
      scale_color_manual(values= col.treat) + 
      scale_fill_manual(values= col.treat) + 
      scale_x_continuous(limits = xlim, breaks = x.breaks, expand = c(0.01,0.01)) + 
      scale_y_continuous(limits = ylim, expand = c(0.02,0.02))
    
    l.id = unique(perf.dt.var$M_ID)
    for(i in l.id){
      
      p.yr   = meta$planting_yr[meta$M_ID == i]
      p.doy  = meta$planting_doy[meta$M_ID == i]
      
      h.yr  = meta$harvesting_yr[meta$M_ID == i]  
      h.doy = meta$harvesting_doy[meta$M_ID == i]
      
      perf.dt.var.i = perf.dt.var[perf.dt.var$M_ID == i,]
      
      perf.dt.var.i = perf.dt.var.i[perf.dt.var.i$year >= p.yr & perf.dt.var.i$year <= h.yr,]
      perf.dt.var.i = perf.dt.var.i[!(perf.dt.var.i$year == h.yr & perf.dt.var.i$doy >=  h.doy) ,]
      
      gg.var = gg.var + geom_line( data = perf.dt.var.i[perf.dt.var.i$type == "sim",], size = l.size)
      
      x.end = max(perf.dt.var.i[,x.val.nm])
      x.ini = min(perf.dt.var.i[,x.val.nm])
      
      if(is.seq){
      gg.var = gg.var + geom_vline(xintercept = x.end, color = "grey", linetype = "dotted")
      if(i == l.id[1]){gg.var = gg.var + geom_vline(xintercept = x.ini, color = "grey", linetype = "dotted")}
      }
      
    }
    
   
    gg.var = gg.var + geom_point(data = perf.dt.var[perf.dt.var$type == "obs",], 
                                 size = p.size,
                                 color= "black",
                                 shape= 21,
                                 stroke= 1)
    
    
    gg.var = gg.var + ylab(y.lab) + xlab(x.lab) + t
      
    #--- remove legend title
    gg.var$labels$colour = ""
    
    if(plot.screen){gg.var}
    
    message(paste0("Saving ggplot for ",var))
    ggsave(plot     = gg.var,
           filename = paste0(wd.out,"\\",var,"_ggplot_",analysis.id,".png"),
           dpi = gg.dpi,
           height = gg.h / n.row, width = gg.w / n.col)
    
    #-------------------------------#
    #--- Run for each soil layer ---#
    #-------------------------------#
    
    if(all.sl){
      
      l.sl = unique(perf.dt.var.all$meas_ID1)  
      var.class = paste0(var.class,"_all_layers")
    }else{
      #--- if missing use all layers
      if(missing(sl.lab.df)){
        l.sl = unique(perf.dt.var.all$meas_ID1)
      }else{
        l.sl = sl.lab.df$l.sl
      }
      
      var.class = paste0(var.class,"_layers_")
      
      for(sl in l.sl){
        var.class = paste0(var.class,sl)  
      }
    }
    
    if(missing(sl.lab.df)){
      sl.lab = l.sl
    }else{
      sl.lab = sl.lab.df$sl.lab
    }
    
    sl.i = 1
    for(sl in l.sl){
      
      #--- Separate by layer data
      perf.dt.var = perf.dt.var.all[perf.dt.var.all$meas_ID1 == sl,]
      y.lab       = paste0(sl.lab[sl.i]," (",
                           y.units,")")
      
      #--- Create ggplot base
      gg.var = ggplot(perf.dt.var, aes_string(y = "val", x = x.val.nm, colour = "leg", fill = "leg")) + 
        scale_color_manual(values= col.treat) + 
        scale_fill_manual(values= col.treat) + 
        scale_x_continuous(limits = xlim, breaks = x.breaks, expand = c(0.01,0.01)) + 
        scale_y_continuous(limits = ylim, expand = c(0.02,0.02))
      
      l.id = unique(perf.dt.var$M_ID)
      for(i in l.id){
        
        p.yr   = meta$planting_yr[meta$M_ID == i]
        p.doy  = meta$planting_doy[meta$M_ID == i]
        
        h.yr  = meta$harvesting_yr[meta$M_ID == i]  
        h.doy = meta$harvesting_doy[meta$M_ID == i]
        
        perf.dt.var.i = perf.dt.var[perf.dt.var$M_ID == i,]
        
        perf.dt.var.i = perf.dt.var.i[perf.dt.var.i$year >= p.yr & perf.dt.var.i$year <= h.yr,]
        perf.dt.var.i = perf.dt.var.i[!(perf.dt.var.i$year == h.yr & perf.dt.var.i$doy >=  h.doy) ,]
        
        gg.var = gg.var + geom_line( data = perf.dt.var.i[perf.dt.var.i$type == "sim",], size = l.size)
        
        x.end = max(perf.dt.var.i[,x.val.nm])
        x.ini = min(perf.dt.var.i[,x.val.nm])
        
        if(is.seq){
          gg.var = gg.var + geom_vline(xintercept = x.end, color = "grey", linetype = "dotted")
          if(i == l.id[1]){gg.var = gg.var + geom_vline(xintercept = x.ini, color = "grey", linetype = "dotted")}
        }
        
        
      }
      
      gg.var = gg.var + geom_point(data = perf.dt.var[perf.dt.var$type == "obs",], 
                                   size = p.size,
                                   color= "black",
                                   shape= 21,
                                   stroke= 1)
      
      gg.var = gg.var + ylab(y.lab) + xlab(x.lab) + t
      
      #--- remove legend title
      gg.var$labels$colour = ""
      
      if(sl == l.sl[1]){
        
        #--- Initialize gg list
        l.gg.list.sl         = list(gg.var)
        names(l.gg.list.sl)  = paste0(var,"_",sl)
        
      }else{
        #--- Append to gg list
        l.gg.list.sl[[sl.i]]           = gg.var
        names(l.gg.list.sl)[sl.i]      = paste0(var,"_",sl)
      }
      
      
      if(all.together){
        
        if(sl == l.sl[length(l.sl)]){
          
          message(paste0("Saving ggplot for ",var.class, " variables."))
          
          #--- Arrange all plot together
          gg.var = ggarrange(plotlist = l.gg.list.sl,
                             ncol = n.col, nrow = n.row)
          #--- save it
          ggsave(plot     = gg.var,
                 filename = paste0(wd.out,"\\",var.class,"_ggplot_",analysis.id,".png"),
                 dpi = gg.dpi,
                 height = gg.h, width = gg.w)
          
        }
        
      }else{
        message(paste0("Saving ggplot for ",sl))
        ggsave(plot     = gg.var,
               filename = paste0(wd.out,"\\",sl,"_ggplot_",analysis.id,".png"),
               dpi = gg.dpi,
               height = gg.h, width = gg.w)
        
      }
      
      sl.i = sl.i +  1
      
    }
  }
  
  return(l.gg.list.sl)
  
}
