#------------------------------------------------------------------
#------------------ Running SAMUCA Platform -----------------------
#------------------------------------------------------------------


#--- Running Parameters

sim.id      = "_Debug_test"
sim.ctl     = "R_debug"
nl          = 4
samuca.exe  = "samuca_vs_proj_2010.exe"
use.debug   = F
read.obs    = T
uni.pc      = T
run.perf    = T

if(uni.pc){

  wd.core     = "D:\\Murilo\\samuca\\samuca\\R\\R_samuca"
  wd.model    = "D:\\Murilo\\samuca\\samuca\\model"
  debug.path  = "D:\\Murilo\\samuca\\samuca\\samuca_vs_proj_2010\\Debug"
  obs.db.nm   = "D:\\Murilo\\samuca\\samuca\\exp_db\\sugarcane\\areao\\sugarcane_data.csv"
  obs.meta.nm = "D:\\Murilo\\samuca\\samuca\\exp_db\\sugarcane\\areao\\meta_sugar_db.csv"
  
}else{
  wd.core     = "C:\\Murilo\\samuca\\R\\R_samuca"
  wd.model    = "C:\\Murilo\\samuca\\model"
  debug.path  = "C:\\Murilo\\samuca\\samuca_vs_proj_2010\\Debug"
  obs.db.nm   = "C:\\Murilo\\samuca\\exp_db\\sugarcane\\areao\\sugarcane_data.csv"
  obs.meta.nm = "C:\\Murilo\\samuca\\exp_db\\sugarcane\\areao\\meta_sugar_db.csv"
  
}


# Code --------------------------------------------------------------------
source(paste0(wd.core,"\\R_sam_lib.R"))
source(paste0(wd.core,"\\R_samuca_fun.R"))
source(paste0(wd.core,"\\R_sam_simfiles.R"))


# Run SAMUCA --------------------------------------------------------------

#--- Create Simulation Control File
SimControl(paste0(wd.core,"\\templates\\ctl_template.ctl"),
           paste0(wd.core,"\\sim_control.csv"),
           paste0(wd.model,"\\Control\\"),
           sim.ctl)

#--- Create ListControl.sam
ListControl(1,sim.ctl,paste0(wd.model,"\\Control\\"))

#--- Update model.exe
if(use.debug){
  file.copy(paste0(debug.path,"\\",samuca.exe),
            wd.model, overwrite = T)
}

#--- Run the model
setwd(wd.model)
system(samuca.exe)


# Read Outputs ------------------------------------------------------------
plan.out.fn   = paste0(wd.model,"\\Output\\Plant",sim.id,".out")
atmo.out.fn   = paste0(wd.model,"\\Output\\Atm",sim.id,".out")
soil.out.fn   = paste0(wd.model,"\\Output\\Soil",sim.id,".out")
temp.out.fn   = paste0(wd.model,"\\Output\\Soil_Temperature",sim.id,".out")

#--- Read SAMUCA outputs
plan.sim = read.plan.out(plan.out.fn)
atmo.sim = read.atmo.out(atmo.out.fn)
soil.sim = read.soil.out(soil.out.fn, nl)
temp.sim = read.temp.out(temp.out.fn, nl)

# Compare with observed data ----------------------------------------------

if(run.perf){
  
  #--- read observations
  obs.data = read.csv(obs.db.nm)
  obs.meta = read.csv(obs.meta.nm)

  
  #--- CLER - M_ID = 20
  #--- obs.meta[,c("M_ID","site")]
  id = 20
  
  #--- filter observed data
  obs.f    = obs.data[obs.data$M_ID == id,]
  
  obs = obs.f$value[obs.f$meas_ID == "SMDMD"]
  sim = plan.sim$dw.st[plan.sim$dap %in% obs.f$dap[obs.f$meas_ID == "SMDMD"]]
  
  plot(plan.sim$dw.st ~ plan.sim$dap, type = "l")
  points(obs.f$value[obs.f$meas_ID == "SMDMD"] ~ obs.f$dap[obs.f$meas_ID == "SMDMD"])
  
  mperf(sim,obs,vnam = "Samuca")
  
}


