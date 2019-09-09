#--- R SAMUCA
#--- MDSV - Dec/2018

run.samuca = function(wd.core,
                      wd.model,
                      samuca.exe,
                      sim.ctl.fn,
                      soil.ctl.fn,
                      met.ctl.fn,
                      met.dt.fn,
                      crp.ctl.fn,
                      sim.id,
                      fl.seq){
  
  #--------------------------------------------------------#
  #----------------- RUN SAMUCA MODEL ---------------------#
  #--------------------------------------------------------#
  #   sim.ctl     Instruction to create simulation control file.ctl (dataframe - sim_control.csv)
  #   wd.core     Working directory of R core Scripts
  #   wd.model    Working directory of SAMUCA model root
  #   samuca.exe  SAMUCA model executable filename
  #   sim.ctl.fn  sim.ctl filename
  #   sim.id      Simulation ID for ListControl call
  #   fl.seq      Flag to for sequential run (T/F)
  #--------------------------------------------------------#
  
  #--- Check inputs
  miss.var.msg = function(var){stop(paste0("Variable ",var," is missing in function run.samuca"))}
  
  if(missing(wd.core))    {miss.var.msg("wd.core")}
  if(missing(wd.model))   {miss.var.msg("wd.model")}
  if(missing(samuca.exe)) {miss.var.msg("samuca.exe")}
  if(missing(sim.ctl.fn)) {miss.var.msg("sim.ctl.fn")}
  if(missing(soil.ctl.fn)){miss.var.msg("soil.ctl.fn")}
  if(missing(met.ctl.fn)) {miss.var.msg("met.ctl.fn")}
  if(missing(crp.ctl.fn)) {miss.var.msg("crp.ctl.fn")}
  if(missing(sim.id))     {miss.var.msg("sim.id")}
  if(missing(fl.seq))     {miss.var.msg("fl.seq")}
  
  #--- Read Control File and extract simulation infos
  sim.ctl  = read.csv(paste0(wd.core,"\\sim_db\\",sim.ctl.fn), as.is = T)
  soil.ctl = read.csv(paste0(wd.core,"\\sim_db\\soil\\",soil.ctl.fn), as.is = T)
  crp.ctl  = read.csv(paste0(wd.core,"\\sim_db\\crop\\",crp.ctl.fn), as.is = T)
  met.ctl  = read.csv(paste0(wd.core,"\\sim_db\\weather\\",met.ctl.fn), as.is = T)
  
  #--- Check IDs consistency
  if(sim.ctl$rep[sim.ctl$find == "<soil.id>"] != soil.ctl$rep[soil.ctl$find == "<soil.id>"]){stop(paste0("Soil ID provided in soil_control.csv of ",sim.id," differs from sim_control.csv"))}
  #if(sim.ctl$rep[sim.ctl$find == "<cv.id>"]   != crp.ctl$rep[crp.ctl$find   == "<cv.id>"])  {stop(paste0("Crop ID provided in crop_control.csv of ",sim.id," differs from sim_control.csv"))}
  if(sim.ctl$rep[sim.ctl$find == "<met.fn>"]  != met.ctl$rep[met.ctl$find   == "<prj.nm>"]){stop(paste0("Meteorological Filename provided in met_control.csv of ",sim.id," differs from sim_control.csv"))}
  
  #--- Check filenames consistency
  if(sim.ctl$rep[sim.ctl$find == "<soil.fn>"] != soil.ctl$rep[soil.ctl$find == "<prj.nm>"]){stop(paste0("Soil Filename provided in sim_control.csv of ",sim.id," differs from meta file"))}
  
  SC.template.fn = paste0(wd.core,"\\templates\\crp_template.crp")
  SC.set.fn = paste0(wd.core,"\\sim_db\\crop\\",crp.ctl.fn)
  SC.outpath = paste0(wd.model,"\\Crop\\")
  SC.outfn = sim.ctl$rep[sim.ctl$find == "<crop.nm>"]
  
  #--- Create Crop File
  SimCrop(SC.template.fn,
          SC.set.fn,
          SC.outpath,
          SC.outfn)
  message(paste0("File ",sim.ctl$rep[sim.ctl$find == "<crop.nm>"],".crp Created."))
  
  #--- Create Soil File
  SimSoil(paste0(wd.core,"\\templates\\spd_template.spd"),
          paste0(wd.core,"\\sim_db\\soil\\",soil.ctl.fn),
          paste0(wd.model,"\\Soil\\"),
          sim.id)
  message(paste0("File ",sim.id,".spd Created."))
  
  #--- Create Meteorological File
  SimMet(paste0(wd.core,"\\templates\\met_template.met"),
         paste0(wd.core,"\\sim_db\\weather\\",met.ctl.fn),
         met.dt.fn,
         paste0(wd.model,"\\Weather\\"),
         sim.id)
  message(paste0("File ",sim.id," Created."))
  
  if(fl.seq){
    
    #--- Create Simulation Control File for sequential run
    SimControl.seq(paste0(wd.core,"\\templates\\ctl_template.ctl"),
                   paste0(wd.core,"\\sim_db\\",sim.ctl.fn),
                   paste0(wd.model,"\\Control\\"),
                   sim.id)
    
  }else{
  
    #--- Create Simulation Control File for non-sequential run
    SimControl(paste0(wd.core,"\\templates\\ctl_template.ctl"),
               paste0(wd.core,"\\sim_db\\",sim.ctl.fn),
               paste0(wd.model,"\\Control\\"),
               sim.id)
    
  }
  
  message(paste0("File ",sim.id,".ctl Created."))
  
  #--- Create ListControl.sam
  ListControl(1,sim.id,paste0(wd.model,"\\Control\\"))
  message(paste0("List File Created."))
  
  #--- Run the model
  setwd(wd.model)
  
  message(paste0("Running SAMUCA (",samuca.exe,")"))
  system(samuca.exe)
  
}
