#--- R SAMUCA
#--- MDSV - Feb/2019

perf.var.temp = function(perf.dt,
                         meta,
                         sim.obs.idx,
                         save.plots,
                         wd.core,
                         analysis.id,
                         var.plot){
  
  #---------------------------------------------------------------
  #----------------- Compute model performance -------------------
  #---------------------------------------------------------------
  #
  #--- Input Parameters:
  #   perf.dt         observed and simualted values [data.frame]
  #   meta            meta data meta.csv [data.frame]
  #   sim.obs.idx     Meta Indexes of simulated x observed data exchange [data.frame]
  #   wd.core         Root path to save outputs
  #   save.plots      Logical for saving plots or not (T/F)
  #   analysis.id     Overall analysis name
  #
  #--- Outputs:
  #   ggplots of "scatter plot sim x obs for each variable in perf.dt" saved as png (when save.plots == T)
  #   perf.dt as a data.frame with IDs and corresponding simulated and observed values
  #
  #--- Required functions:
  #   miss.wrn()      Check input parameters
  #   add.id.seq()    Associate IDs
  #---------------------------------------------------------------
  
  #--- Check input data
  fnm = "perf.var.temp"
  
  miss.wrn(perf.dt   		,fnm)
  miss.wrn(meta			    ,fnm)
  miss.wrn(sim.obs.idx	,fnm)
  miss.wrn(save.plots		,fnm)
  miss.wrn(wd.core		  ,fnm)
  miss.wrn(analysis.id	,fnm)
  miss.wrn(var.plot	,fnm)
  
  #--- flag to initiate
  init = T
  
  for(var in unique(perf.dt$meas_ID)){
    
    message("Computing Statistical Indexes for Variable: ",var)
    perf.var = merge(perf.dt[perf.dt$meas_ID == var & perf.dt$type == "obs",c("das","val","M_ID","meas_ID1")],
                     perf.dt[perf.dt$meas_ID == var & perf.dt$type == "sim",c("das","val","M_ID","meas_ID1")],
                     by = c("M_ID","das","meas_ID1"))
    
    perf.var =   merge(perf.var,meta[,c("M_ID","site","Name_ID")],
                       by = "M_ID")
    
    colnames(perf.var) = c("M_ID","das","meas_ID1","obs","sim","Name","Site")
    
    #--- Statistical indexes
    perf.var.res = mperf(perf.var$sim,perf.var$obs,vnam = var,dchart = F)
    
    #--- Scatter plot
    y.lab = paste0("Simulated ", sim.obs.idx$label_var[sim.obs.idx$obs_db_code == var], " (",sim.obs.idx$units[sim.obs.idx$obs_db_code == var],")")
    x.lab = paste0("Observed ", sim.obs.idx$label_var[sim.obs.idx$obs_db_code == var], " (",sim.obs.idx$units[sim.obs.idx$obs_db_code == var],")")
    ll = c(min(c(perf.var$obs,perf.var$sim)),max(c(perf.var$obs,perf.var$sim)))
    
    Legend = perf.var[,var.plot]
    gg.perf = ggplot(perf.var,aes(x = obs, y = sim, shape = Legend)) + 
      scale_shape_manual(values=1:length(unique(Legend))) + 
      geom_point(size = 5) + 
      ylab(y.lab) + xlab(x.lab) +
      scale_x_continuous(limits = ll) + scale_y_continuous(limits = ll) +
      geom_abline(intercept = perf.var.res$a,slope =  perf.var.res$b, linetype = 1, color = "black", size = 0.5) +
      geom_abline(intercept = 0,slope =  1, linetype = 3, color = "red", size = 1) +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.background = NULL,
            axis.line = element_line(colour = "black", size = 0.2),
            axis.text=element_text(size=24, colour = "black"),
            axis.title=element_text(size=26),
            legend.position=c(.91,.15),
            legend.background = element_rect(fill="white",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            legend.text=   element_text(size=14),
            legend.title = element_text(size = 16),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))
    
    #--- Add statistical indexes table
    xfac   = 0.15
    xinner = -0.6
    
    x.min = (ll[2] - ll[1]) * (1-xfac) - (ll[2] - ll[1]) * xinner
    x.max = x.min + (ll[2] - ll[1]) * (xfac)
    
    yfac   = 0.15
    yinner = 0
    
    y.min = (ll[2] - ll[1]) * (1-yfac) - (ll[2] - ll[1]) * yinner
    y.max = y.min + (ll[2] - ll[1]) * (yfac)
    
    mytheme <- gridExtra::ttheme_minimal(
      core = list(fg_params=list(cex = 1.5)),
      colhead = list(fg_params=list(cex = 1.5)),
      rowhead = list(fg_params=list(cex = 1.5)))
    
    
    tab.perf = tableGrob(data.frame(Index = c("RMSE","R2","d"),
                                    Value = round(c(perf.var.res$rmse,perf.var.res$r2,perf.var.res$d),2)), rows=NULL,theme=mytheme)
    
    tab.perf = gtable_add_grob(tab.perf,
                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
                               t = 1, b = nrow(tab.perf), l = 1, r = ncol(tab.perf))
    tab.perf = gtable_add_grob(tab.perf,
                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
                               t = 2, b = nrow(tab.perf), l = 1, r = ncol(tab.perf))
    
    
    gg.perf = gg.perf + annotation_custom(tab.perf,xmin=x.min, xmax=x.max, ymin=y.min, ymax=y.max)
    
    #--- Save ggplot to .png file?
    if(save.plots){
      ggsave(plot     = gg.perf,
             filename = paste0(wd.core,"\\results_perf\\","Perf_",analysis.id,"_",var,".png"),dpi = 400,
             height = 10, width = 10)
      
    }
    
    if(init){
      perf = perf.var.res
      init = F
    }else{
      perf = rbind(perf,perf.var.res)
    }
    
    #--- Run for each soil layer
    for(sl in unique(perf.dt$meas_ID1)){
      
      message("Computing Statistical Indexes for Variable: ",var, " layer ", sl)
      perf.var = merge(perf.dt[perf.dt$meas_ID == var & perf.dt$meas_ID1 == sl & perf.dt$type == "obs",c("das","val","M_ID","meas_ID1")],
                       perf.dt[perf.dt$meas_ID == var & perf.dt$meas_ID1 == sl & perf.dt$type == "sim",c("das","val","M_ID","meas_ID1")],
                       by = c("M_ID","das","meas_ID1"))
      
      perf.var =   merge(perf.var,meta[,c("M_ID","site","Name_ID")],
                         by = "M_ID")
      
      colnames(perf.var) = c("M_ID","das","meas_ID1","obs","sim","Name","Site")
      
      #--- Statistical indexes
      perf.var.res = mperf(perf.var$sim,perf.var$obs,vnam = sl,dchart = F)
      
      #--- Scatter plot
      y.lab = paste0("Simulated ", sl, " (",sim.obs.idx$units[sim.obs.idx$obs_db_code == var],")")
      x.lab = paste0("Observed ", sl, " (",sim.obs.idx$units[sim.obs.idx$obs_db_code == var],")")
      ll = ll # same as for all data to keep same range within axis
      
      #--- check if mperf coul generate a/b coeffs
      if(is.null(perf.var.res$a)){perf.var.res$a = NA; perf.var.res$b = NA}
      
      Legend = perf.var[,var.plot]
      gg.perf = ggplot(perf.var,aes(x = obs, y = sim, shape = Legend)) + 
        scale_shape_manual(values=1:length(unique(Legend))) + 
        geom_point(size = 5) + 
        ylab(y.lab) + xlab(x.lab) +
        scale_x_continuous(limits = ll) + scale_y_continuous(limits = ll) +
        geom_abline(intercept = perf.var.res$a,slope =  perf.var.res$b, linetype = 1, color = "black", size = 0.5) +
        geom_abline(intercept = 0,slope =  1, linetype = 3, color = "red", size = 1) +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              panel.background = NULL,
              axis.line = element_line(colour = "black", size = 0.2),
              axis.text=element_text(size=24, colour = "black"),
              axis.title=element_text(size=26),
              legend.position=c(.91,.15),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              legend.text=   element_text(size=14),
              legend.title = element_text(size = 16),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))
      
      #--- Add statistical indexes table
      xfac   = 0.15
      xinner = -0.6
      
      x.min = (ll[2] - ll[1]) * (1-xfac) - (ll[2] - ll[1]) * xinner
      x.max = x.min + (ll[2] - ll[1]) * (xfac)
      
      yfac   = 0.15
      yinner = 0
      
      y.min = (ll[2] - ll[1]) * (1-yfac) - (ll[2] - ll[1]) * yinner
      y.max = y.min + (ll[2] - ll[1]) * (yfac)
      
      mytheme <- gridExtra::ttheme_minimal(
        core = list(fg_params=list(cex = 1.5)),
        colhead = list(fg_params=list(cex = 1.5)),
        rowhead = list(fg_params=list(cex = 1.5)))
      
      
      tab.perf = tableGrob(data.frame(Index = c("RMSE","R2","d"),
                                      Value = round(c(perf.var.res$rmse,perf.var.res$r2,perf.var.res$d),2)), rows=NULL,theme=mytheme)
      
      tab.perf = gtable_add_grob(tab.perf,
                                 grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
                                 t = 1, b = nrow(tab.perf), l = 1, r = ncol(tab.perf))
      tab.perf = gtable_add_grob(tab.perf,
                                 grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
                                 t = 2, b = nrow(tab.perf), l = 1, r = ncol(tab.perf))
      
      
      gg.perf = gg.perf + annotation_custom(tab.perf,xmin=x.min, xmax=x.max, ymin=y.min, ymax=y.max)
      
      #--- Save ggplot to .png file?
      if(save.plots){
        ggsave(plot     = gg.perf,
               filename = paste0(wd.core,"\\results_perf\\","Perf_",analysis.id,"_",sl,".png"),dpi = 400,
               height = 10, width = 10)
        
      }
      
      if(init){
        perf = perf.var.res
        init = F
      }else{
        perf = rbind(perf,perf.var.res)
      }
    } 
    
  }
  return(perf)
  
}
