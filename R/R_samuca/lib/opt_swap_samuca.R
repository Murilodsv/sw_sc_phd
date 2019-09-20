#--- optimize parameters of SWAP-SAMUCA
#--- MDSV Sep/2019

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
  #--- index.obj.cor    - Correction factor to objective function
  #--- penalty.fac      - objective fun penalty when out of bounds
  #--- index.obj        - Index used as objective function (from mperf())
  #--- target.sim.df    - Name of list element where target sim results are
  #--- target.var       - vector with name of target variables used
  #--- obs.data         - data.frame with observed data to optimize
  #--- p.df             - data.frame with parameters control
  #--- plot.perf        - flag to plot progress of objective
  #--- save.surf        - flag to save objective response surface
  #--- get.obj.fun      - flag to return objective function or model results
  
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
  
  #--- Unscaled parameters
  p.df$value = p.df$min + new.par * (p.df$max - p.df$min)  
  
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
    if(penalize.obj){
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
      #--- return NA
      objective.fun = NA
    }
    
  }else{
    
    #-------------------------#
    #--- Parameters are OK ---#
    #-------------------------#
    message("Iteration Parameters:")
    print(p.df[,c("find","value")])
    
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
                                    SC.set.crop.fn,
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
    
    #--- Correction factor
    #--- In case of otmizing 0-1 indexes (e.g r2, NSE, d) set index.obj.cor = 1
    objective.fun = index.obj.cor - objective.fun
    
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
             geom_line(colour = "blue",
                       size = 1.5) +
             ylab("Objective Fucntion") + 
             xlab("Iteration") +
             theme_bw())
      
    }
    
    if(save.surf){
      
      surf.df = data.frame(it  = it,
                           objective = objective.fun,
                           sim.id = sim.id,
                           t(p.df$value))
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
  
  if(get.obj.fun){
    #--- return objective result
    return(objective.fun)
  }else{
    #--- return simulation results for comparison
    return(list(sim.results = sim.results.l,
                sim.obs = sim.obs))
  }
  
}
