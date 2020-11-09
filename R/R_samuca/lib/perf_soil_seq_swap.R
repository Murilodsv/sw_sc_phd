#--- R SAMUCA
#--- MDSV - Jan/2019

perf.soil.seq.swap = function(obs.f,
                         sim.ctl,
                         soil.ctl,
                         sim.obs.idx,
                         soil.sim,
                         field.id,
                         seque.id,
                         meta.perf,
                         sim.id,
                         wd.core,
                         save.plots,
                         analysis.id){
  
  #---------------------------------------------------------------
  #----------------- Compute model performance -------------------
  #--- Compute SAMUCA performance in simulate soil  variables ----
  #--- SEQUENTIAL MODE: Only works for sequential simulations, 
  #--- e.g. same treatment/field for more than one season
  #---------------------------------------------------------------
  #
  #--- Input Parameters:
  #   obs.f           observed values [data.frame]
  #   sim.ctl         sim control file [data.frame]
  #   sim.obs.idx     Meta Indexes of simulated x observed data exchange [data.frame]
  #   plan.sim        simulated values [data.frame]
  #   field.id        Corresponding field id -> meta.csv
  #   seque.id        Corresponding sequence id -> meta.csv
  #   meta.perf       sequence meta
  #   sim.id          Simulation ID
  #   wd.core         Root path to save outputs
  #   save.plots      Logical for saving plots or not (T/F)
  #   analysis.id     Overall analysis name
  #
  #--- Outputs:
  #   ggplots of "simulated and observed data ~ time" saved as png (when save.plots == T)
  #   perf.dt as a data.frame with IDs and corresponding simulated and observed values
  #   
  #--- Required functions:
  #   miss.wrn()      Check input parameters
  #   add.id.seq()    Associate IDs
  #---------------------------------------------------------------
  
  #--- Check input data
  fnm = "perf.soil.seq"
  
  miss.wrn(obs.f       ,fnm)
  miss.wrn(sim.ctl     ,fnm)
  miss.wrn(sim.obs.idx ,fnm)
  miss.wrn(soil.sim    ,fnm)
  miss.wrn(field.id    ,fnm)
  miss.wrn(seque.id    ,fnm)
  miss.wrn(meta.perf   ,fnm)
  miss.wrn(sim.id      ,fnm)
  miss.wrn(wd.core     ,fnm)
  miss.wrn(save.plots  ,fnm)
  miss.wrn(analysis.id ,fnm)
  
  #--- initializae flag
  init = T
  
  #--- compute DAS (Days After Simulation Starts)
  m.date    = as.POSIXct(paste0("01/01/",obs.f$year), format="%m/%d/%Y", tz = "GMT") + (obs.f$doy - 1) * 24 * 60 * 60
  s.date    = as.POSIXct(gsub("_","-",sim.ctl$rep[sim.ctl$find == "<date_inisim>"]), format="%d-%b-%Y",  tz = "GMT")
  obs.f$das = as.numeric(m.date - s.date)
  
  #--- filter only data that SAMUCA simulates
  obs.s.df = sim.obs.idx[sim.obs.idx$obs_db_code %in% unique(obs.f$meas_ID) & sim.obs.idx$samuca_sim_Rcode != "",]
  obs.f    = obs.f[obs.f$meas_ID %in% obs.s.df$obs_db_code,]
  
  #--- Number of variables to compare with
  n.obs.var = length(obs.s.df$obs_db_code)
  
  #--- Soil Layers
  soil.dp.ly        = soil.ctl[soil.ctl$find == "<dp>",c("rep","layer")]
  soil.dp.ly$sim.nm = paste0("swc",soil.dp.ly$layer)
  
  #--- Read Simulated and Observed Layers Association File
  if(length(unique(meta.perf$soil_compare_file)) == 1){
    sim.obs.slayers.idx.fn = unique(meta.perf$soil_compare_file)
    sim.obs.slayers.idx = read.csv(paste0(wd.core,"\\sim_db\\soil\\",sim.obs.slayers.idx.fn), as.is = T)
  }else{
    stop(paste0("More than one soil layer association file for a sequential Run. Check Meta file collumn: 'soil_compare_file' for Field_ID: ",field.id," seq_ID: ",seque.id))
  }
  
  #--- Compare simulated vs observed for each variable in this site
  for(o in 1:n.obs.var){
    
    #--- set ids
    o.id    = obs.s.df$obs_db_code[o]
    s.id    = obs.s.df$samuca_sim_Rcode[o]
    
    #--- select only unique layers flagged for performance analysis 
    n.obs.ly = unique(sim.obs.slayers.idx$depth[sim.obs.slayers.idx$run_perf])
    sim.obs.slayers.perf = sim.obs.slayers.idx[sim.obs.slayers.idx$depth %in% n.obs.ly,]
    
    #--- check whether sim file match with observed file association (has all layers?)
    sim.obs.slayers.perf = sim.obs.slayers.perf[sim.obs.slayers.perf$depth %in% unique(obs.f$depth),]
    #sim.obs.slayers.perf = sim.obs.slayers.perf[sim.obs.slayers.perf$samuca_soil_code_var %in% soil.dp.ly$sim.nm,]
    
    #--- observed depths
    soil.dp.vec = sim.obs.slayers.perf$depth * -1
    
    match_soil_layer_by_mid = F
    if(match_soil_layer_by_mid){
      
      #--- get approximate depths on simulations
      #--- This will filter out all simulated data does not match with observations
      sim.df.all = 
        get_data_dsoil(soil.out   = sim.df.all,
                       d.nm       = 'mid',
                       dsoil.vec  = soil.dp.vec,
                       exact.match= F,
                       max.dif    = 10)
      
    }else{
      
      #--------------------------------------------------------------#
      #--- get match observations that are within the soil layers ---#
      #--------------------------------------------------------------#
      
      ini_df_sl_obs = T
      for(d in soil.dp.vec){
        
        #--- filter 
        f = (d <= sim.df.all$bottom) & (d > sim.df.all$upper)
        f.df = sim.df.all[f,]
        
        #--- tag with the required depth
        f.df$depth_req = d
        
        if(ini_df_sl_obs){
          sim.df.all.f  = f.df
          ini_df_sl_obs = F
        }else{
          sim.df.all.f = rbind(sim.df.all.f,
                               f.df)
        }
      }
      
      sim.df.all = sim.df.all.f
    }
    
    #--- Extract for each soil layer depth
    for(l in sim.obs.slayers.perf$depth){
      
      #--- soil
      #--- extract simulated data
      sim.df = sim.df.all[sim.df.all$depth_req == (-l),]
      
      sim.df  = sim.df[,c("year","doy","das","dap",'wcontent')]
      colnames(sim.df) = c("year","doy","das","dap","val")
      
      #--- get approximate depths simulations
      
      #--- Indexers
      sim.df$meas_ID = o.id # Indexing to the observed data
      sim.df$meas_ID1= paste0(s.id,' (', l,' cm)')
      sim.df$type    = "sim"
      sim.df$seq     = T
      sim.df$Field_ID= field.id
      sim.df$seq_ID  = seque.id
      
      #--- Sequential run IDs are initiated as zero (before planting period)
      #--- any data within planting-harvesting year will be associated to its meta ID from meta file[csv]
      #--- i.e. the remaining zeros are the intervals between or out of planting harvesting dates informed in meta!
      sim.df$M_ID    = 0
      sim.df$M_ID    = add.id.seq(meta.perf[meta.perf$Field_ID == field.id & meta.perf$seq_ID == seque.id,],sim.df)
      
      #--- extract observed data
      #--- soil depth of this layer associated to the observed db
      dp.obs = l
      
      obs.df.sl = obs.f[obs.f$depth == dp.obs,]
      
      if(any(obs.df.sl$type == "avg")){
        
        obs.df  = obs.df.sl[obs.df.sl$meas_ID == o.id & obs.df.sl$type == "avg",c("year","doy","das","dap","value")]
        colnames(obs.df) = c("year","doy","das","dap","val")
        
      }else{
        message(paste0("None avg values found in observed data base for ",o.id, " meas_ID1 ",l," performance analysis."))
        message("The avg will be computed based on 'msr' values.")
        
        obs.f.agg = aggregate(value ~ M_ID + meas_ID + type + year + doy + dap + das, data = obs.df.sl[obs.df.sl$type == "msr",],
                              mean)
        obs.f.agg$type = "avg"
        
        colnames(obs.f.agg) = c(colnames(obs.f.agg)[1:(length(colnames(obs.f.agg))-1)],"val")
        obs.df = obs.f.agg[,c("year","doy","das","dap","val")]
      }
      
      #--- Indexers
      obs.df$meas_ID = o.id # Indexing to the observed data
      obs.df$meas_ID1= paste0(s.id,' (', l,' cm)')
      obs.df$type    = "obs"
      obs.df$seq     = T
      obs.df$Field_ID= field.id
      obs.df$seq_ID  = seque.id
      
      #--- Sequential run IDs are initiated as zero 
      #--- any data within planting-harvesting year will be associated to its meta ID from meta file[csv]
      #--- Note that remaining zeros are the intervals between or out of planting harvesting dates informed in meta!
      obs.df$M_ID    = 0 
      obs.df$M_ID    = add.id.seq(meta.perf[meta.perf$Field_ID == field.id & meta.perf$seq_ID == seque.id,],obs.df)
      
      #--- plots Axis
      y.lab = paste0(obs.s.df$label_var[o], " (",obs.s.df$units[o],")") # Name + Units
      x.lab = "Days After Simulation Started (DAS)"
      
      #--- plot limits
      if(any(obs.f$meas_ID == o.id & obs.f$type == "std")){
        
        #--- with standard error bars
        sd = obs.f[obs.f$depth %in% sim.obs.slayers.perf$depth & obs.f$type == "std",c("year","doy","das","dap","value")]
        colnames(sd) =  c("year","doy","das","dap","std")
        
        #--- average duplicate values
        if(length(sd$std) > length(obs.df$year)){
          message(paste0("More Standard Error Data provided than Observations for variable ",o.id," on simulation ID: ",sim.id,". Duplicated values will be averaged."))
          sd = aggregate(std ~ year + doy, data = sd,mean)
        }
        
        sd.df = merge(obs.df,sd, by = c("year","doy"), all.x = T)
        sd.df$std[is.na(sd.df$std)] = 0
        
        sd = sd.df$std
        
        #--- limit to positive values only
        sd[obs.f$value[obs.f$depth %in% sim.obs.slayers.perf$depth]-sd < 0] = 0
        
        ylim  = c(min(c(sim.df$val, obs.f$value[obs.f$depth %in% sim.obs.slayers.perf$depth]-sd)),
                  max(c(sim.df$val, obs.f$value[obs.f$depth %in% sim.obs.slayers.perf$depth]+sd)))    
      }else{
        
        
        max.y = max(c(max(sim.df.all$wcontent),max(obs.f$value[obs.f$depth %in% sim.obs.slayers.perf$depth])))
        min.y = min(c(min(sim.df.all$wcontent),min(obs.f$value[obs.f$depth %in% sim.obs.slayers.perf$depth])))
        ylim  = c(min.y,max.y)
      }
      
      xlim  = c(min(sim.df$das),max(sim.df$das))
      x.breaks = seq(xlim[1],xlim[2], by = 300)
      
      #--- Plot (Sim + Obs) ~ DAS
      dap.sim = ggplot(obs.df,aes(x = das, y = val)) + 
        geom_point(size = 5) + ylab(y.lab) + xlab(x.lab) +
        scale_x_continuous(limits = xlim, breaks = x.breaks) + scale_y_continuous(limits = ylim) +
        geom_line(data = data.frame(das = sim.df$das, val = sim.df$val))+
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              panel.background = NULL,
              axis.line    = element_line(colour = "black", size = 0.2),
              axis.text    = element_text(size=24, colour = "black"),
              axis.title   = element_text(size=26),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))
      
      
      #--- Add Standard error bars?
      if(any(obs.f$meas_ID == o.id & obs.f$type == "std")){
        
        #--- Add Standard error bars
        dap.sim = dap.sim + geom_errorbar(aes(ymin=obs.df$val-sd, ymax=obs.df$val+sd), width=10)
        
      }
      
      #--- Add Statistical indexes?
      if(length(obs.df$val) > 1){
        
        #--- Compute performance
        o.perf = mperf(sim.df$val[sim.df$das %in% obs.df$das],obs.df$val, vnam = "EVAL", dchart = F)
        
        #--- locate on ggplot
        xfac   = -0.10
        xinner = 0.08
        
        x.min = (xlim[2] - xlim[1]) * (1-xfac) - (xlim[2] - xlim[1]) * xinner
        x.max = x.min + (xlim[2] - xlim[1]) * (xfac)
        
        yfac   = 0.15
        yinner = 0.65
        
        y.min = (ylim[2] - ylim[1]) * (1-yfac) - (ylim[2] - ylim[1]) * yinner
        y.max = y.min + (ylim[2] - ylim[1]) * (yfac)
        
        mytheme <- gridExtra::ttheme_minimal(
          core = list(fg_params=list(cex = 1.5)),
          colhead = list(fg_params=list(cex = 1.5)),
          rowhead = list(fg_params=list(cex = 1.5)))
        
        
        tab.perf = tableGrob(data.frame(Index = c("RMSE","R2","d"),
                                        Value = round(c(o.perf$rmse,o.perf$r2,o.perf$d),2)), rows=NULL,theme=mytheme)
        
        tab.perf = gtable_add_grob(tab.perf,
                                   grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
                                   t = 1, b = nrow(tab.perf), l = 1, r = ncol(tab.perf))
        tab.perf = gtable_add_grob(tab.perf,
                                   grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
                                   t = 2, b = nrow(tab.perf), l = 1, r = ncol(tab.perf))
        
        
        dap.sim = dap.sim + annotation_custom(tab.perf,xmin=x.min, xmax=x.max, ymin=y.min, ymax=y.max)
        
      }
      
      #--- create results_perf if not there yet
      dir.create(paste0(wd.core,"\\results_perf"), showWarnings = F)
      
      #--- Save ggplot to .png file?
      if(save.plots){
        ggsave(plot     = dap.sim,
               filename = paste0(wd.core,"\\results_perf\\","Perf_",analysis.id,"_",o.id,"_MID_",sim.id,"_subID_",l,".png"),dpi = 400,
               height = 10, width = 20)
      }
      
      if(init){
        #--- Create performance df
        perf.dt = rbind(sim.df,obs.df)
        init = F
      }else{
        #--- Append to performance df
        perf.dt = rbind(perf.dt,sim.df,obs.df)
      }
      
    } # end layer
    
  } # end variable
  
  return(perf.dt)
}
