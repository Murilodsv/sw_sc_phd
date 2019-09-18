#---------------------------------------#
#--- Optimize SWAP-SAMUCA parameters ---#
#---------------------------------------#

#--- Get paths
get.rp.root = function(){
  
  r.wd = dirname(rstudioapi::getSourceEditorContext()$path)
  
  git.not.found = T
  l.f = strsplit(r.wd,"/")[[1]]
  i.path = length(l.f)
  
  while(git.not.found){
    wd = paste(l.f[1:i.path], collapse = "/")
    
    if(length(list.files(wd, all.files = T,pattern = ".git")) > 0){
      git.not.found = F
      return(wd)
    }else{
      if(i.path == 1){
        
        git.not.found = F
        message("No Git File Found in Path Tree:")
        message(r.wd)
        return(r.wd)
        
      }else{
        
        i.path = i.path - 1
        
      }
    }
  }
}
wd.repo   = get.rp.root()
wd.rsam   = paste0(wd.repo,"/R/R_samuca")
wd.lib    = paste0(wd.repo,"/R/R_samuca/lib")
wd.model  = paste0(wd.repo,"/model")
wd.debug  = paste0(wd.repo,"/Debug")

#--- Load Source Files (~/bin) 
invisible(sapply(list.files(path = wd.lib,full.names = T),
                 function(x) source(x)))
use.debug  = T
samuca.exe = "swap_samuca_v1.exe"
if(use.debug){
  #--- Update .exe
  file.copy(paste0(wd.debug,"/",samuca.exe),
            wd.model, overwrite = T)
}

#------------------------#
#--- GLOBAL VARIABLES ---#
#------------------------#
sim.id          = "SEQ_F1_S1"
target.sim.df   = "plan"
target.var      = c("swface","swfacp")
obs.data        = read.csv(paste0(wd.rsam,"/opt/target_data.csv")) 
p.df            = read.csv(paste0(wd.rsam,"/opt/par_df.csv"), as.is = T)
index.obj       = "rmse"
penalty.fac     = 1000
plot.perf       = T
save.surf       = T
opt.df.fn       = paste0(wd.rsam,"/opt/opt_iteration.csv")
surf.df.fn      = paste0(wd.rsam,"/opt/par_surf.csv")
log.file        = paste0(wd.rsam,"/log_",sim.id,".txt")
SC.outpath      = wd.model
SC.set.ctrl.fn  = paste0(wd.rsam,"/sim_db/sim_control_",sim.id,"_SWAP.csv")
SC.set.mana.fn  = paste0(wd.rsam,"/sim_db/management/mana_control_",sim.id,".csv")
SC.set.mete.fn  = paste0(wd.rsam,"/sim_db/weather/met_control_",sim.id,".csv")
SC.set.irri.fn  = paste0(wd.rsam,"/sim_db/management/irri_control_",sim.id,".csv")
met.dt.fn       = paste0(wd.rsam,"/sim_db/weather/met_data_f1.csv")

#--- Initialize iteration
opt.df    = data.frame(it = 0,
                     ini.run = T,
                     objective = 0,
                     sim.id  = sim.id)
write.csv(opt.df, file = opt.df.fn, row.names = F)

#--- Initial conditions
new.par = p.df$value

#--- Optimize with "Nelder-Mead"
opt.res = optim(new.par,opt.swap.samuca, method = "Nelder-Mead")

