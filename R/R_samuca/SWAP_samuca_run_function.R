

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

#--- GLOBAL VARIABLES
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
sim.id          = "SEQ_F1_S1"
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

#--- New parameter: for testing...
new.par = p.df$value

opt.swap.samuca(new.par * rnorm(1,1,0.05))

opt.swap.samuca = function(new.par){
  
  #-----------------------------------------------------------#
  #------------- Optimize SWAP-SAMUCA parameters -------------#
  #-----------------------------------------------------------#
  
  #--- Goals:
  #---  (a) update target parameters
  #---  (b) validate new set of parameters 
  #---  (c) generate control files with new parameters
  #---  (d) Run SWAP-SAMUCA
  #---  (e) Retrieve target variables
  #---  (f) Compare with observations (objective function)
  
  #--- To run this function global variables must be declared:
  #--- opt.df.fn        - iteration control csv file
  #--- surf.df.fn       - objective response surface filename
  #--- log.file         - warning log file
  #--- sim.id           - simulation ID
  #--- SC.outpath       - SWAP-SAMUCA model root
  #--- SC.set.ctrl.fn   - SWAP-SAMUCA ctrl file
  #--- SC.set.mana.fn   - SWAP-SAMUCA management file
  #--- SC.set.mete.fn   - SWAP-SAMUCA meteorological file
  #--- SC.set.irri.fn   - SWAP-SAMUCA irrigation file
  #--- met.dt.fn        - SWAP-SAMUCA meteorological data file
  #--- penalty.fac      - objective fun penalty when out of bounds
  #--- index.obj        - Index used as objective function (from mperf())
  #--- target.sim.df    - Name of list element where target sim results are
  #--- target.var       - vector with name of target variables used
  #--- obs.data         - data.frame with observed data to optimize
  #--- p.df             - data.frame with parameters control
  #--- plot.perf        - flag to plot progress of objective
  #--- save.surf        - flag to save objective response surface
  
  #--- Note: to initizalize save the iteration control file for it = 0
  #--- example:
  # opt.df  = data.frame(it = 1,
  #                      ini.run = T,
  #                      objective = 0,
  #                      sim.id  = sim.id)
  
  #--- Functions needed:
  #--- check.cond()
  #--- check.rang()
  #--- mperf()
  #--- run.swap.samuca()
  #-----------------------------------------------------------#
  
  #--- Read iteration control
  opt.df  = read.csv(opt.df.fn, as.is = T)
  it      = opt.df$it[length(opt.df$it)]
  ini.run = opt.df$ini.run[length(opt.df$it)]
  
  if(!ini.run){
    last.objective.fun = opt.df$objective[length(opt.df$it)] 
  }
  
  #--------------------------------#
  #--- Update set of parameters ---#
  #--------------------------------#
  p.df$value = new.par  
  
  if(ini.run){
    
    #--- Initializa log file
    w.log(log.file = log.file,
          ini.log = T)
    
    w.log(w.msg = paste0("Running ",sim.id),log.file = log.file,
          h.msg = F)
  }
  
  #--- Check parameters
  valid.par.cond = check.cond(p.df)
  valid.par.rang = check.rang(p.df)
  
  if(any(!valid.par.rang,!valid.par.cond)){
    
    #-----------------------------------------------#
    #--- Parameters are out bounds or conditions ---#
    #-----------------------------------------------#
    
    #--- compute penalty
    if(exists("last.objective.fun")){
      penalty = last.objective.fun * penalty.fac
    }else{
      message("Warning: Error in parameter set at first step.")
      message(paste0("Warning: Penalty of ",penalty.fac," was employed."))
      penalty = penalty.fac
    }
    
    #--- Penalize objective
    objective.fun = penalty
    
  }else{
    
    #-------------------------#
    #--- Parameters are OK ---#
    #-------------------------#
    
    #--- Update control files
    l.par.files = unique(p.df$file)
    for(f in l.par.files){
      
      #--- open and replace new parameter values
      rep.par = read.csv(f,as.is = T)
      rep.par$rep[rep.par$find %in% p.df$find[p.df$file == f]] = 
        as.character(p.df$value[p.df$find %in% p.df$find[p.df$file == f]])
      
      #--- rewrite parameters file
      write.csv(rep.par, file = f,row.names = F)
    }
    
    #--- Run the model
    sim.results.l = run.swap.samuca(SC.set.ctrl.fn,
                                  SC.set.irri.fn,
                                  SC.set.mana.fn,
                                  SC.set.mete.fn,
                                  met.dt.fn,
                                  samuca.exe,
                                  sim.id,
                                  SC.outpath)
    
    #--- extract target results
    for(v in target.var){
      sim.results = sim.results.l[[target.sim.df]]
      target.res.df.v = sim.results[,c("year","doy",v)]
      colnames(target.res.df.v) = c("year","doy","sim")
      target.res.df.v$sim.id = sim.id
      target.res.df.v$var = v
      
      if(v == target.var[1]){
        target.res.df = target.res.df.v
      }else{
        target.res.df = rbind(target.res.df,target.res.df.v)
      }
      rm(target.res.df.v)
    }
    
    #--- merge with observed data
    comp.df = obs.data
    colnames(comp.df)[colnames(comp.df) == "value"] = "obs"
    sim.obs = merge(comp.df,target.res.df, by = c("year","doy","var"))
    
    #--- Compute objective function
    perf = mperf(sim = sim.obs$sim,
                 obs = sim.obs$obs,
                 vnam = "opt",
                 dchart = F)
    
    #--- extract objective index value
    objective.fun = perf[,index.obj]
    
    #--- Update iteration control and flags
    it = it + 1
    if(ini.run){ini.run = F}
    
    opt.df = rbind(opt.df,data.frame(it = it,
                                     ini.run = ini.run,
                                     objective = objective.fun,
                                     sim.id  = sim.id))
    write.csv(opt.df, file = opt.df.fn, row.names = F)
    
    if(plot.perf){
      
      plot(ggplot(opt.df, aes(x = it, y = objective)) + 
             geom_line(colour = "blue") +
             ylab("Objective Fucntion") + 
             xlab("Iteration") +
             theme_bw())
      
    }
    
    if(save.surf){
      
      surf.df = data.frame(it  = it,
                           objective = objective.fun,
                           sim.id = sim.id,
                           t(new.par))
      p.names = p.df$find
      p.names = gsub("<","",p.names)
      p.names = gsub(">","",p.names)
      colnames(surf.df) = c("it","objective","sim.id",p.names)
      
      if(it == 1){
        write.csv(surf.df, file = surf.df.fn,
                  row.names = F)
      }else{
        
        surf.file = read.csv(file = surf.df.fn,
                               as.is = T)
        
        surf.file = rbind(surf.file, surf.df)
        write.csv(surf.file, file = surf.df.fn,
                  row.names = F)
        
      }
    }
  }
  
  return(objective.fun)
}
