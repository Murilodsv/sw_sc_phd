#--- R SAMUCA GGPLOT ~ DAS
#--- MDSV - Feb/2019

gg.das = function(perf.dt.plot,
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
                  t){
  
  
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
  #----------------------------------------------#
  
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
    y.lab       = paste0(sim.obs.idx$label_var[sim.obs.idx$obs_db_code == var]," (",
                         sim.obs.idx$units[sim.obs.idx$obs_db_code == var],")")
    
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
        #--- include vertical lines
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
    gg.var$labels$fill   = ""
    
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
               filename = paste0(wd.out,"\\",var.class,"_ggplot_",analysis.id,".png"),
               dpi = gg.dpi,
               height = gg.h, width = gg.w)
        
      }
      
    }else{
      message(paste0("Saving ggplot for ",var))
      ggsave(plot     = gg.var,
             filename = paste0(wd.out,"\\",var,"_ggplot_",analysis.id,".png"),
             dpi = gg.dpi,
             height = gg.h, width = gg.w)
      
    }
    
    var.i = var.i +  1
    
  }
  
  #--- Return all gg as list
  return(l.gg.list)
  
}
