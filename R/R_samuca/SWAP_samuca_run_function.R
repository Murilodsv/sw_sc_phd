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

SC.outpath      = wd.model
SC.set.ctrl.fn  = paste0(wd.rsam,"/sim_db/sim_control_",sim.id,"_SWAP.csv")
SC.set.mana.fn  = paste0(wd.rsam,"/sim_db/management/mana_control_",sim.id,".csv")
SC.set.mete.fn  = paste0(wd.rsam,"/sim_db/weather/met_control_",sim.id,".csv")
SC.set.irri.fn     = paste0(wd.rsam,"/sim_db/management/irri_control_",sim.id,".csv")
met.dt.fn       = paste0(wd.rsam,"/sim_db/weather/met_data_f1.csv")
use.debug = T
if(use.debug){
  #--- Update .exe
  file.copy(paste0(wd.debug,"/",samuca.exe),
            wd.model, overwrite = T)
}

sim.id = "SEQ_F1_S1"


results = run.swap.samuca(SC.set.ctrl.fn,
                          SC.set.irri.fn,
                          SC.set.mana.fn,
                          SC.set.mete.fn,
                          met.dt.fn,
                          samuca.exe,
                          sim.id,
                          SC.outpath)


#--- plant
id.vec   = c("das","year","doy","sim.id")
plan.var = c("dw.st","dw.su","fw.st","pol","lai","till","p.ht","n.gl","swface","swfacp")

plan.df = melt(plan.out,
               id.vars = colnames(plan.out)[colnames(plan.out) %in% id.vec],
               measure.vars = plan.var)

#--- atmosphere
id.vec   = c("das","year","doy","sim.id")
incr.var = c("Tpot","Tact","Epot","Eact","et.pot", "et.act")

incr.df = melt(incr.out,
               id.vars = colnames(incr.out)[colnames(incr.out) %in% id.vec],
               measure.vars = incr.var)

#--- soil

soil.dp.vec = c(-10,-14.0,-31.5,-62,-90,-130)

soil.df           = soil.out
soil.df$value     = soil.df$wcontent
soil.df$variable  = as.factor(soil.df$depth)

soil.df = soil.df[soil.df$depth %in% soil.dp.vec,]

#--- temperature
temp.df           = soil.out
temp.df$value     = temp.df$temp
temp.df$variable  = as.factor(temp.df$depth)

temp.df = temp.df[temp.df$depth %in% soil.dp.vec,]

#--- ggplot function
gg.fun = function(df){
  
  gg.obj = 
    ggplot(df, aes(x = das, y = value)) + 
    geom_line(colour = "blue") + 
    facet_wrap(variable~., 
               scales = "free") + 
    theme_bw()
  
  return(gg.obj)
  
}

#--- list dataframe
l.df = list(plan.df,
            incr.df,
            soil.df,
            temp.df)

#--- run all ggplots
ggplots = lapply(l.df, gg.fun)


#--- check results
ggplots[[1]]  # plant
ggplots[[2]]  # atmo
ggplots[[3]]  # soil water
ggplots[[4]]  # soil temperature