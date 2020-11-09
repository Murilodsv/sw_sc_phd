#--- R SAMUCA PERF WRAP
#--- MDSV - Feb/2019

r.samuca.perf.fun.swap = function(analysis.id,
                             wd.core,
                             wd.model,
                             use.debug,
                             debug.path,
                             samuca.exe,
                             obs.meta.fn,
                             standalone.swb){

#------------------------------------------------------------------
#------------------ Running SAMUCA Platform -----------------------
#------------------------------------------------------------------
#--- Performance Analysis:
#---
#--- 1) Read Simulation Setup ("meta_sugar_db.csv")
#--- 2) Run for each site and compare with observed data ("sugarcane_data.csv")
#--- 3) Plot charts and save results

#--- Additional infos:
#--- Simulation control files are created "on-the-fly" for each site
#--- Templates are located in ~wd.core/templates/
#--- Auxiliary functions ~lib/ are needed
#------------------------------------------------------------------

#--- Running Parameters
fl.plan.soil= T
fl.perf.soil= T
fl.perf.temp= T
fl.perf.atmo= T

fl.msg = T

save.plots  = T
save.sim.obs= T
var.plot    = "Site"

#obs.meta.fn         = "meta_sugar_db.csv"
obs.db.plan.fn      = "sugarcane_data.csv"
obs.db.soil.fn      = "soil_data.csv"
obs.db.stemp.fn     = "stemp_data.csv"
obs.db.atmo.fn      = "et.csv"
sim.obs.idx.fn      = "sim_obs_index.csv" # links observed to simulated data

#--- Load Source Files (~/bin) 
invisible(sapply(list.files(path = paste0(wd.core,"\\lib\\"),full.names = T),
                 function(x) source(x)))

#--- Read Simulations Files 
meta      = read.csv(paste0(wd.core,"\\",obs.meta.fn),
                     as.is = T)

#--- Read index simulations & Observations variable names
sim.obs.idx = read.csv(paste0(wd.core,"\\",sim.obs.idx.fn),
                       as.is = T)

#--- Read plant observations
obs.data.plan = read.csv(paste0(wd.core,"\\obs_db\\plant\\",obs.db.plan.fn),
                         as.is = T)

#--- Read Soil Observations
if(fl.perf.soil){
  obs.data.soil = read.csv(paste0(wd.core,"\\obs_db\\soil\\",obs.db.soil.fn),
                           as.is = T)
}

#--- Read Soil Temperature Observations
if(fl.perf.temp){
  obs.data.temp = read.csv(paste0(wd.core,"\\obs_db\\soil\\",obs.db.stemp.fn),
                           as.is = T)
}


#--- Read Atmosphere Observations
if(fl.perf.atmo){
  obs.data.atmo = read.csv(paste0(wd.core,"\\obs_db\\atmo\\",obs.db.atmo.fn),
                           as.is = T)
}

#--- Update model.exe
if(use.debug){
  file.copy(paste0(debug.path,"\\",samuca.exe),
            wd.model, overwrite = T)
}

#--- initialize simulated vs observed df
init = T

init.plan = T
init.soil = T
init.temp = T
init.atmo = T

#--- time-track
tic(paste0("Performance of ",analysis.id))

#------------------------------------------------
#----------- Sequential Simulations -------------
#------------------------------------------------

#--- Meta for performance (tagged for performance and only sequential runs)
meta.perf = meta[meta$performance & meta$seq,]

#--- list of sequential sites for analysis
fields.seq = unique(meta.perf$Field_ID)

#--- Number of sequential sites for analysis
n.sites.seq = length(meta.perf$Field_ID)

#--- Run Performance for each site with sequential run
for(f in fields.seq){
  
  if(n.sites.seq == 0){break} #--- Do not run the loop if there is nothing to run
  
  f.seq = unique(meta.perf$seq_ID[meta.perf$Field_ID == f])
  
  #--- Run for each sequential within site "f"
  for(s in f.seq){
    
    #--- Simulation info
    sim.ctl.fn  = as.character(meta.perf$sim_file[meta.perf$Field_ID == f & meta.perf$seq_ID == s])
    soil.ctl.fn = as.character(meta.perf$soil_file[meta.perf$Field_ID == f & meta.perf$seq_ID == s])
    crp.ctl.fn  = as.character(meta.perf$crop_file[meta.perf$Field_ID == f & meta.perf$seq_ID == s])
    met.ctl.fn  = as.character(meta.perf$met_file[meta.perf$Field_ID == f & meta.perf$seq_ID == s])
    met.dt.fn   = as.character(meta.perf$met_data[meta.perf$Field_ID == f & meta.perf$seq_ID == s])
    irri.ctl.fn = as.character(meta.perf$irrig_control[meta.perf$Field_ID == f & meta.perf$seq_ID == s])
    mana.ctl.fn = as.character(meta.perf$mana_control[meta.perf$Field_ID == f & meta.perf$seq_ID == s])
    
    sim.id = paste0("SEQ_F",f,"_S",s)
    
    #--- check if control filename for this sequential run are unique
    #--- They must be!
    if(!any(c(length(unique(sim.ctl.fn))    == 1,
              length(unique(soil.ctl.fn))   == 1,
              length(unique(crp.ctl.fn))    == 1,
              length(unique(met.ctl.fn))    == 1,
              length(unique(met.dt.fn))     == 1,
              length(unique(irri.ctl.fn))   == 1,
              length(unique(mana.ctl.fn))   == 1))){
      stop(paste0("Non unique control files provided in meta_sugar_db.csv for the sequential run ",sim.id))
      
    }else{
      sim.ctl.fn  = sim.ctl.fn[1]
      soil.ctl.fn = soil.ctl.fn[1]
      crp.ctl.fn  = crp.ctl.fn[1]
      met.ctl.fn  = met.ctl.fn[1]
      met.dt.fn   = met.dt.fn[1]
      irri.ctl.fn = irri.ctl.fn[1]
      mana.ctl.fn = mana.ctl.fn[1]
    }
    
    #--- Run SWAP-SAMUCA
    swap.samuca.res = 
      run.swap.samuca(SC.set.ctrl.fn = paste0(wd.core,"\\sim_db\\",sim.ctl.fn),
                      SC.set.irri.fn = paste0(wd.core,"\\sim_db\\management\\",irri.ctl.fn),
                      SC.set.mana.fn = paste0(wd.core,"\\sim_db\\management\\",mana.ctl.fn),
                      SC.set.crop.fn = paste0(wd.core,"\\sim_db\\crop\\",crp.ctl.fn),
                      SC.set.mete.fn = paste0(wd.core,"\\sim_db\\weather\\",met.ctl.fn),
                      met.dt.fn      = paste0(wd.core,"\\sim_db\\weather\\",met.dt.fn),
                      samuca.exe     = samuca.exe,
                      sim.id         = sim.id,
                      SC.outpath     = wd.model,
                      wd.core        = wd.core,
                      wd.model       = wd.model)

    #--- Read control files
    sim.ctl    = read.csv(paste0(wd.core,"\\sim_db\\",sim.ctl.fn), as.is = T)
    soil.ctl   = read.csv(paste0(wd.core,"\\sim_db\\soil\\",soil.ctl.fn), as.is = T)
    crp.ctl    = read.csv(paste0(wd.core,"\\sim_db\\crop\\",crp.ctl.fn), as.is = T)
    met.ctl    = read.csv(paste0(wd.core,"\\sim_db\\weather\\",met.ctl.fn), as.is = T)
    
    m.id       = meta.perf$M_ID[meta.perf$Field_ID == f & meta.perf$seq_ID == s]
    nl         = length(unique(soil.ctl$layer)) - 1 # number of soil layers (Note: -1 is the header)
    
    #--- Check M_ID
    if(sim.ctl$rep[sim.ctl$find == "<prj.nm>"] != sim.id){stop(paste0("Project name (<prj.nm>) provided in sim_control_",m.id,".csv differs from obs.meta.fn file"))}
  

    # Read Outputs ------------------------------------------------------------

    plan.sim    = swap.samuca.res$plan
    soil.sim    = swap.samuca.res$soil
    atmo.sim    = swap.samuca.res$incr
    temp.sim    = swap.samuca.res$soil
    
    #--- broadcast dap to other simulation results
    soil.sim = 
    merge(soil.sim, 
          plan.sim[,c('das','dap')],
          by = c('das'),
          all.x = T)
    
    temp.sim = 
      merge(temp.sim, 
            plan.sim[,c('das','dap')],
            by = c('das'),
            all.x = T)
    
    atmo.sim = 
      merge(atmo.sim, 
            plan.sim[,c('das','dap')],
            by = c('das'),
            all.x = T)
    
    atmo.sim$et.act.acc = 0
    for(das in atmo.sim$das){
      mask = (atmo.sim$das == das)
      if(atmo.sim$dap[mask] == 1 | is.na(atmo.sim$dap[mask])){
        atmo.sim$et.act.acc[mask] = 0
      }else{
        last_das = (atmo.sim$das == (das-1))
        atmo.sim$et.act.acc[mask] = atmo.sim$et.act.acc[last_das] + atmo.sim$et.act[mask] 
      }
    }
    
    # Compare with observed data ----------------------------------------------
    
    #--- Plant outputs performance
    has.plan.dt = F
    if(fl.plan.soil){
      
      if(any(obs.data.plan$M_ID %in% m.id)){
        
        if(fl.msg){message(paste0("Extracting simulated and observerd values for plant variables in sequential: ",s))}
        
        has.plan.dt = T
        
        #--- Select only the corresponding data
        obs.f    = obs.data.plan[obs.data.plan$M_ID %in% m.id,]
        
        #--- Compute Plant Performance for this sequential simulation
        perf.plan.seq.df =  perf.plan.seq.swap(obs.f,
                                          sim.ctl,
                                          sim.obs.idx,
                                          plan.sim,
                                          field.id = f,
                                          seque.id = s,
                                          meta.perf,
                                          sim.id,
                                          wd.core,
                                          save.plots,
                                          analysis.id)
        
        #--- Bind results in a single df
        if(init.plan){
          #--- Create performance df
          perf.dt.plan  = perf.plan.seq.df
          init.plan     = F
        }else{
          perf.dt.plan  = rbind(perf.dt.plan,perf.plan.seq.df)
        }
      }
    }
    
    #--- Soil outputs performance
    has.soil.dt = F 
    if(fl.perf.soil){
      
      if(any(obs.data.soil$M_ID %in% m.id)){
        
        if(fl.msg){message(paste0("Extracting simulated and observerd values for soil variables in sequential: ",s))}
        
        has.soil.dt = T
        obs.f = obs.data.soil[obs.data.soil$M_ID %in% m.id,]
        #soil.dp.ly = soil.ctl[soil.ctl$find == "<dp>",c("rep","layer")]
        
        perf.soil.seq.df = perf.soil.seq.swap(obs.f,
                                         sim.ctl,
                                         soil.ctl,
                                         sim.obs.idx,
                                         soil.sim,
                                         field.id = f,
                                         seque.id = s,
                                         meta.perf,
                                         sim.id,
                                         wd.core,
                                         save.plots,
                                         analysis.id)
        #--- Bind results in a single df
        if(init.soil){
          #--- Create performance df
          perf.dt.soil  = perf.soil.seq.df
          init.soil     = F
        }else{
          perf.dt.soil  = rbind(perf.dt.soil,perf.soil.seq.df)
        }
      }
    }
    
    #--- Soil Temperature outputs performance
    has.temp.dt = F 
    if(fl.perf.temp){
      
      if(any(obs.data.temp$M_ID %in% m.id)){
        
        if(fl.msg){message(paste0("Extracting simulated and observerd values for temperature variables in sequential: ",s))}
        
        has.temp.dt = T
        
        obs.f = obs.data.temp[obs.data.temp$M_ID %in% m.id,]
        #soil.dp.ly = soil.ctl[soil.ctl$find == "<dp>",c("rep","layer")]
        
        perf.temp.seq.df = perf.temp.seq.swap(obs.f,
                                         sim.ctl,
                                         soil.ctl,
                                         sim.obs.idx,
                                         temp.sim,
                                         field.id = f,
                                         seque.id = s,
                                         meta.perf,
                                         sim.id,
                                         wd.core,
                                         save.plots,
                                         analysis.id)
        
        #--- Bind results in a single df
        if(init.temp){
          #--- Create performance df
          perf.dt.temp  = perf.temp.seq.df
          init.temp     = F
        }else{
          perf.dt.temp  = rbind(perf.dt.temp,perf.temp.seq.df)
        }
      }
    }
    
    #--- Atmosphere outputs performance
    has.atmo.dt = F
    if(fl.perf.atmo){
      
      if(any(obs.data.atmo$M_ID %in% m.id)){
        
        if(fl.msg){message(paste0("Extracting simulated and observerd values for ET variables in sequential: ",s))}
        
        has.atmo.dt = T
        
        obs.f = obs.data.atmo[obs.data.atmo$M_ID %in% m.id,]
        
        perf.atmo.seq.df =  perf.atmo.seq.swap(obs.f,
                                          sim.ctl,
                                          sim.obs.idx,
                                          atmo.sim,
                                          field.id = f,
                                          seque.id = s,
                                          meta.perf,
                                          sim.id,
                                          wd.core,
                                          save.plots,
                                          analysis.id)
        
        #--- Bind results in a single df
        if(init.atmo){
          #--- Create performance df
          perf.dt.atmo  = perf.atmo.seq.df
          init.atmo     = F
        }else{
          perf.dt.atmo  = rbind(perf.dt.atmo,perf.atmo.seq.df)
        }
      }
    }
    
  } #--- End of sequence "s"
} #--- End of field "f"


#------------------------------------------------
#----------- Non-Sequential Simulations ---------
#------------------------------------------------

#--- Meta for performance of non-sequential sites
meta.perf = meta[meta$performance & !meta$seq,]

#--- Number of non-sequential sites for analysis
n.sites.nonseq = length(meta.perf$sim_file)

#--- Check M_IDs
if(length(unique(meta.perf$M_ID)) > length(meta.perf$M_ID)){
  z = meta.perf$M_ID
  stop(paste0("Duplicated M_IDs for non-sequential run: ",paste(z[duplicated(z)],collapse = ",")))
}

#--- Run Performance for each non-sequential site
for(s in 1:n.sites.nonseq){
  
  if(n.sites.nonseq == 0){break} #--- Do not run the loop if there is nothing to run
  
  #--- Simulation info
  sim.ctl.fn  = as.character(meta.perf$sim_file[s])
  soil.ctl.fn = as.character(meta.perf$soil_file[s])
  crp.ctl.fn  = as.character(meta.perf$crop_file[s])
  met.ctl.fn  = as.character(meta.perf$met_file[s])
  met.dt.fn   = as.character(meta.perf$met_data[s])
  
  #--- Read instruction to create simulation control file.ctl
  sim.ctl    = read.csv(paste0(wd.core,"\\sim_db\\",sim.ctl.fn), as.is = T)
  soil.ctl   = read.csv(paste0(wd.core,"\\sim_db\\soil\\",soil.ctl.fn), as.is = T)
  crp.ctl    = read.csv(paste0(wd.core,"\\sim_db\\crop\\",crp.ctl.fn), as.is = T)
  met.ctl    = read.csv(paste0(wd.core,"\\sim_db\\weather\\",met.ctl.fn), as.is = T)
  
  m.id       = meta.perf$M_ID[s]
  sim.id     = paste0("M_ID_",m.id)
  nl         = length(unique(soil.ctl$layer)) - 1 # number of soil layers (Note: -1 is the header)
  
  #--- Check M_ID
  if(sim.ctl$rep[sim.ctl$find == "<prj.nm>"] != sim.id){stop(paste0("Project name (<prj.nm>) provided in sim_control_",m.id,".csv differs from obs.meta.fn file"))}
  
  # Run SAMUCA --------------------------------------------------------------
  run.samuca(wd.core,
             wd.model,
             samuca.exe,
             sim.ctl.fn,
             soil.ctl.fn,
             met.ctl.fn,
             met.dt.fn,
             crp.ctl.fn,
             sim.id,
             F)
  
  # Read Outputs ------------------------------------------------------------
  plan.out.fn = paste0(wd.model,"\\Output\\Plant_",sim.id,".out")
  atmo.out.fn = paste0(wd.model,"\\Output\\Atm_",sim.id,".out")
  soil.out.fn = paste0(wd.model,"\\Output\\Soil_",sim.id,".out")
  temp.out.fn = paste0(wd.model,"\\Output\\Soil_Temperature_",sim.id,".out")
  
  plan.sim    = read.plan.out(plan.out.fn)
  atmo.sim    = read.atmo.out(atmo.out.fn)
  soil.sim    = read.soil.out(soil.out.fn, nl)
  temp.sim    = read.temp.out(temp.out.fn, nl)
  
  # Compare with observed data ----------------------------------------------
  
  #--- Plant outputs performance
  has.plan.dt = F
  if(fl.plan.soil){
    
    if(any(obs.data.plan$M_ID %in% m.id)){
      
      has.plan.dt = T
      
      #--- Select the M_ID data only
      obs.f    = obs.data.plan[obs.data.plan$M_ID == m.id,]
      
      #--- Plant outputs performance
      perf.plan.df =  perf.plan(obs.f,
                                sim.ctl,
                                sim.obs.idx,
                                plan.sim,
                                m.id,
                                meta.perf,
                                sim.id,
                                wd.core,
                                save.plots,
                                analysis.id)
      
      #--- Bind results in a single df
      if(init.plan){
        #--- Create performance df
        perf.dt.plan  = perf.plan.df
        init.plan     = F
      }else{
        perf.dt.plan  = rbind(perf.dt.plan,perf.plan.df)
      }
    }
  }
  
  #--- Soil outputs performance
  has.soil.dt = F
  if(fl.perf.soil){
    
    if(any(obs.data.soil$M_ID %in% m.id)){
      
      #--- There is observed soil data
      has.soil.dt = T
      
      obs.f = obs.data.soil[obs.data.soil$M_ID %in% m.id,]
      
      soil.dp.ly = soil.ctl[soil.ctl$find == "<dp>",c("rep","layer")]
      
      message("Warning: Soil performance function for non-sequential was not developed yet, using sequential function may lead to unexpected error...")
      perf.soil.seq.df = perf.soil.seq(obs.f,
                                       sim.ctl,
                                       soil.ctl,
                                       sim.obs.idx,
                                       soil.sim,
                                       f,
                                       s,
                                       meta.perf,
                                       sim.id,
                                       wd.core,
                                       save.plots,
                                       analysis.id)
      
      #--- Bind results in a single df
      if(init.soil){
        #--- Create performance df
        perf.dt.soil  = perf.soil.seq.df
        init.soil     = F
      }else{
        perf.dt.soil  = rbind(perf.dt.soil,perf.soil.seq.df)
      }
    }else{
      message(paste0("No observations to compare with soil simulations in ID: ", m.id))
    }
  }
  
  #--- Soil Temperature outputs performance
  has.temp.dt = F 
  if(fl.perf.temp){
    
    if(any(obs.data.temp$M_ID %in% m.id)){
      
      has.temp.dt = T
      
      obs.f = obs.data.temp[obs.data.temp$M_ID %in% m.id,]
      
      soil.dp.ly = soil.ctl[soil.ctl$find == "<dp>",c("rep","layer")]
      message("Warning: Soil temperature performance function for non-sequential simulations was not developed yet, using sequential function may lead to unexpected error...")
      perf.temp.seq.df = perf.temp.seq(obs.f,
                                       sim.ctl,
                                       soil.ctl,
                                       sim.obs.idx,
                                       temp.sim,
                                       f,
                                       s,
                                       meta.perf,
                                       sim.id,
                                       wd.core,
                                       save.plots,
                                       analysis.id)
      
      #--- Bind results in a single df
      if(init.temp){
        #--- Create performance df
        perf.dt.temp  = perf.temp.seq.df
        init.temp     = F
      }else{
        perf.dt.temp  = rbind(perf.dt.temp,perf.temp.seq.df)
      }
    }else{
      message(paste0("No observations to compare with soil temperature simulations in ID: ", m.id))
    }
  }
  
  #--- Atmosphere outputs performance
  has.atmo.dt = F
  if(fl.perf.atmo){
    
    if(any(obs.data.atmo$M_ID %in% m.id)){
      
      has.atmo.dt = T
      
      obs.f = obs.data.atmo[obs.data.atmo$M_ID %in% m.id,]
      
      message("Warning: Atmosphere performance function for non-sequential simulations was not developed yet, using sequential function may lead to unexpected error...")
      perf.atmo.seq.df =  perf.atmo.seq(obs.f,
                                        sim.ctl,
                                        sim.obs.idx,
                                        atmo.sim,
                                        f,
                                        s,
                                        meta.perf,
                                        sim.id,
                                        wd.core,
                                        save.plots,
                                        analysis.id)
      
      #--- Bind results in a single df
      if(init.atmo){
        #--- Create performance df
        perf.dt.atmo  = perf.atmo.seq.df
        init.atmo     = F
      }else{
        perf.dt.atmo  = rbind(perf.dt.atmo,perf.atmo.seq.df)
      }
    }else{
      message(paste0("No observations to compare with atmospheric simulations in ID: ", m.id))
    }
  }
  
}

if(save.sim.obs){
  
  #--- Simualted vs Measured df for latter analyis
  if(!init.plan){write.csv(perf.dt.plan, file = paste0(wd.core,"\\results_perf\\","Perf_Data_Plan_",analysis.id,".csv"), row.names = F)}
  if(!init.soil){write.csv(perf.dt.soil, file = paste0(wd.core,"\\results_perf\\","Perf_Data_Soil_",analysis.id,".csv"), row.names = F)}
  if(!init.temp){write.csv(perf.dt.temp, file = paste0(wd.core,"\\results_perf\\","Perf_Data_Temp_",analysis.id,".csv"), row.names = F)}
  if(!init.atmo){write.csv(perf.dt.atmo, file = paste0(wd.core,"\\results_perf\\","Perf_Data_Atmo_",analysis.id,".csv"), row.names = F)}
}


#------------------------------------------------
#----------- Overall Variables Performance ------
#------------------------------------------------


if(!init.plan){
  #--- Plants Variables performance
perf.var.plan.results = perf.var.plan(perf.dt.plan,
                                      meta,
                                      sim.obs.idx,
                                      save.plots,
                                      wd.core,
                                      analysis.id,
                                      var.plot)
}

if(!init.soil){
#--- Soil Variables performance
perf.var.soil.results = perf.var.soil(perf.dt.soil,
                                      meta,
                                      sim.obs.idx,
                                      save.plots,
                                      wd.core,
                                      analysis.id,
                                      var.plot)
}


if(!init.temp){
#--- Temp Variables performance
perf.var.temp.results = perf.var.temp(perf.dt.temp,
                                      meta,
                                      sim.obs.idx,
                                      save.plots,
                                      wd.core,
                                      analysis.id,
                                      var.plot)
}

if(!init.atmo){
#--- Atmo Variables performance
perf.var.atmo.results = perf.var.atmo(perf.dt.atmo,
                                      meta,
                                      sim.obs.idx,
                                      save.plots,
                                      wd.core,
                                      analysis.id,
                                      var.plot)
}

#--- save performance results
if(!init.plan){write.csv(perf.var.plan.results, file = paste0(wd.core,"\\results_perf\\","Perf_Plan_",analysis.id,".csv"), row.names = F)}
if(!init.soil){write.csv(perf.var.soil.results, file = paste0(wd.core,"\\results_perf\\","Perf_Soil_",analysis.id,".csv"), row.names = F)}
if(!init.temp){write.csv(perf.var.temp.results, file = paste0(wd.core,"\\results_perf\\","Perf_Temp_",analysis.id,".csv"), row.names = F)}
if(!init.atmo){write.csv(perf.var.atmo.results, file = paste0(wd.core,"\\results_perf\\","Perf_Atmo_",analysis.id,".csv"), row.names = F)}

#--- Create list of reults

l.results = list(meta = meta,
                 sim.obs.idx = sim.obs.idx)

#--- Append plant results
if(!init.plan){
  l.results[[length(l.results)+1]]    = perf.var.plan.results
  names(l.results)[length(l.results)] = "perf.var.plan.results"
  
  l.results[[length(l.results)+1]]    = perf.dt.plan
  names(l.results)[length(l.results)] = "perf.dt.plan"
  
}

if(!init.soil){
  
  l.results[[length(l.results)+1]]    = perf.var.soil.results
  names(l.results)[length(l.results)] = "perf.var.soil.results"
  
  l.results[[length(l.results)+1]]    = perf.dt.soil
  names(l.results)[length(l.results)] = "perf.dt.soil"
  
  
}


if(!init.temp){
  
  l.results[[length(l.results)+1]]    = perf.var.temp.results
  names(l.results)[length(l.results)] = "perf.var.temp.results"
  
  l.results[[length(l.results)+1]]    = perf.dt.temp
  names(l.results)[length(l.results)] = "perf.dt.temp"
  
  
}


if(!init.atmo){
  
  l.results[[length(l.results)+1]]    = perf.var.atmo.results
  names(l.results)[length(l.results)] = "perf.var.atmo.results"
  
  l.results[[length(l.results)+1]]    = perf.dt.atmo
  names(l.results)[length(l.results)] = "perf.dt.atmo"
  
}

return(l.results)

}
