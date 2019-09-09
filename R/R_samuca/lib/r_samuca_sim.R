#--- R SAMUCA SIMULATIONS
#--- MDSV - Jun/2019

r.samuca.sim = function(wd.core,
                        wd.model,
                        use.debug,
                        debug.path,
                        samuca.exe,
                        obs.meta.fn,
                        treatment,
                        year.run){
  
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
  
  fl.msg = T
  
  save.plots  = T
  save.sim.obs= T
  var.plot    = "Site"
  
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
  
  #--- Update model.exe
  if(use.debug){
    file.copy(paste0(debug.path,"\\",samuca.exe),
              wd.model, overwrite = T)
  }
  
  #--- initialize simulated vs observed df
  init = T
  init.sim = T
  
  init.plan = T
  init.soil = T
  init.temp = T
  init.atmo = T
  
  message(paste0("#--- Running of:"))
  message(paste0("Treatment: ", treatment," Year: ", year.run))
  
  #--- time-track
  tic(paste0("Running of treatment ",treatment," for year ", year.run))
  
  #------------------------------------------------
  #----------- Sequential Simulations -------------
  #------------------------------------------------
  
  #--- Meta for performance (tagged for performance and only sequential runs)
  meta.perf = meta[meta$run & meta$seq,]
  
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
      
      sim.id = paste0("SEQ_F",f,"_S",s)
      
      #--- check if control filename for this sequential run are unique
      #--- They must be!
      if(!any(c(length(unique(sim.ctl.fn))  == 1,
                length(unique(soil.ctl.fn)) == 1,
                length(unique(crp.ctl.fn))  == 1,
                length(unique(met.ctl.fn))  == 1,
                length(unique(met.dt.fn))   == 1))){
        stop(paste0("Non unique control files provided in meta_sugar_db.csv for the sequential run ",sim.id))
        
      }else{
        sim.ctl.fn  = sim.ctl.fn[1]
        soil.ctl.fn = soil.ctl.fn[1]
        crp.ctl.fn  = crp.ctl.fn[1]
        met.ctl.fn  = met.ctl.fn[1]
        met.dt.fn   = met.dt.fn[1]
      }
      
      #--- Read instruction to create simulation control file.ctl
      sim.ctl    = read.csv(paste0(wd.core,"\\sim_db\\",sim.ctl.fn), as.is = T)
      soil.ctl   = read.csv(paste0(wd.core,"\\sim_db\\soil\\",soil.ctl.fn), as.is = T)
      crp.ctl    = read.csv(paste0(wd.core,"\\sim_db\\crop\\",crp.ctl.fn), as.is = T)
      met.ctl    = read.csv(paste0(wd.core,"\\sim_db\\weather\\",met.ctl.fn), as.is = T)
      
      m.id       = meta.perf$M_ID[meta.perf$Field_ID == f & meta.perf$seq_ID == s]
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
                 T)
      
      # Read Outputs ------------------------------------------------------------
      plan.out.fn = paste0(wd.model,"\\Output\\Plant_",sim.id,".out")
      atmo.out.fn = paste0(wd.model,"\\Output\\Atm_",sim.id,".out")
      soil.out.fn = paste0(wd.model,"\\Output\\Soil_",sim.id,".out")
      temp.out.fn = paste0(wd.model,"\\Output\\Soil_Temperature_",sim.id,".out")
      
      plan.sim    = read.plan.out(plan.out.fn)
      atmo.sim    = read.atmo.out(atmo.out.fn)
      soil.sim    = read.soil.out(soil.out.fn, nl)
      temp.sim    = read.temp.out(temp.out.fn, nl)
      
    } #--- End of sequence "s"
  } #--- End of field "f"
  
  
  #------------------------------------------------
  #----------- Non-Sequential Simulations ---------
  #------------------------------------------------
  
  #--- Meta for performance of non-sequential sites
  meta.perf = meta[meta$run & !meta$seq,]
  
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
    
    #--- Site ID from meta-file
    plan.sim$sim.id = sim.id
    atmo.sim$sim.id = sim.id
    soil.sim$sim.id = sim.id
    temp.sim$sim.id = sim.id
    
    #--- Treatment ID
    plan.sim$treatment = treatment
    atmo.sim$treatment = treatment
    soil.sim$treatment = treatment
    temp.sim$treatment = treatment
    
    #--- Year Run
    plan.sim$year.run  = year.run
    atmo.sim$year.run  = year.run
    soil.sim$year.run  = year.run
    temp.sim$year.run  = year.run
    
    
    if(init.sim){
      
      plan.sim.all = plan.sim
      atmo.sim.all = atmo.sim
      soil.sim.all = soil.sim
      temp.sim.all = temp.sim
      
      init.sim = F
      
    }else{
      
      plan.sim.all = rbind(plan.sim.all, plan.sim)
      atmo.sim.all = rbind(atmo.sim.all, atmo.sim)
      soil.sim.all = rbind(soil.sim.all, soil.sim)
      temp.sim.all = rbind(temp.sim.all, temp.sim)
      
    }
  }
  
  
  l.results = list(plan.sim = plan.sim.all,
                   atmo.sim = atmo.sim.all,
                   soil.sim = soil.sim.all,
                   temp.sim = temp.sim.all)
  
  
  
  return(l.results)
  
}
