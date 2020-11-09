#------------------------------#
#--- RUN + Plot SWAP-SAMUCA ---#
#------------------------------#

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
soil.dp.vec = c(-10,-30,-60,-90,-120)



#-----------------------#
#--- Run SWAP-SAMUCA ---#
#-----------------------#

#--- Create swap.swp
SC.set.fn      = paste0(wd.rsam,"/sim_db/sim_control_seq_f1_s1_SWAP.csv")
SC.template.fn = paste0(wd.rsam,"/templates/swp_template.swp")
SC.irrig.fn    = paste0(wd.rsam,"/sim_db/management/irri_control_seq_f1_s1.csv")
SC.outpath     = wd.model
SC.outfn       = "Swap"

SC.set.fn.ctl = SC.set.fn

SimControl.SWAP(SC.template.fn,
                SC.set.fn,
                SC.outpath,
                SC.outfn,
                SC.irrig.fn)

#--- Create Management
SC.set.fn      = paste0(wd.rsam,"/sim_db/management/mana_control_seq_f1_s1.csv")
SC.template.fn = paste0(wd.rsam,"/templates/mng_template.mng")
SC.outpath     = wd.model
SC.outfn       = "Samuca"

SC.set.fn.mana = SC.set.fn

SimMana(SC.template.fn,
        SC.set.fn,
        SC.outpath,
        SC.outfn)

#--- Create Meteorological
SC.set.fn = paste0(wd.rsam,"/sim_db/weather/met_control_seq_f1_s1.csv")
met.dt.fn = paste0(wd.rsam,"/sim_db/weather/met_data_f1.csv")
SC.outpath = wd.model
SC.outfn   = "SEQ_F1_S1"

SC.set.fn.met = SC.set.fn

SimMet.SWAP(SC.set.fn,
            met.dt.fn,
            SC.outpath,
            SC.outfn)


if(use.debug){
  #--- Update .exe
  file.copy(paste0(wd.debug,"/",samuca.exe),
            wd.model, overwrite = T)
}
setwd(wd.model)
system(samuca.exe)

#--- Output names
plan.out.fn = paste0(wd.model,"/Plant_",SC.outfn,".out")
atmo.out.fn = paste0(wd.model,"/",SC.outfn,".wba")
soil.out.fn = paste0(wd.model,"/",SC.outfn,".vap")
stre.out.fn = paste0(wd.model,"/",SC.outfn,".str")
incr.out.fn = paste0(wd.model,"/",SC.outfn,".inc")

#--- Read outputs
plan.out = read.plan.SWAP.out(plan.out.fn)
atmo.out = read.atmo.SWAP.out(atmo.out.fn)
soil.out = read.soil.SWAP.out(soil.out.fn)
stre.out = read.stre.SWAP.out(stre.out.fn)
incr.out = read.incr.SWAP.out(incr.out.fn)

#--- Evapotranspiration
incr.out$et.pot = incr.out$Tpot + incr.out$Epot
incr.out$et.act = incr.out$Tact + incr.out$Eact


SC.set.fn.crop = paste0(wd.rsam,"/sim_db/crop/crop_control_seq_f1_s1.csv")
sim.id = 'SEQ_F1_S1'


# run.swap.samuca(SC.set.ctrl.fn = SC.set.fn.ctl,
#                 SC.set.irri.fn = SC.irrig.fn,
#                 SC.set.mana.fn = SC.set.fn.mana,
#                 SC.set.crop.fn = SC.set.fn.crop,
#                 SC.set.mete.fn = SC.set.fn.met,
#                 met.dt.fn = met.dt.fn,
#                 samuca.exe,
#                 sim.id,
#                 SC.outpath)

#---------------------------------#
#--- Prepare data for the plot ---#
#---------------------------------#

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
soil.df = 
  get_data_dsoil(soil.out,
                 d.nm       = 'depth',
                 dsoil.vec  = soil.dp.vec,
                 exact.match= F,
                 max.dif    = 10)


soil.df$value     = soil.df$wcontent
soil.df$variable  = as.factor(soil.df$depth)

#--- temperature
temp.df = 
  get_data_dsoil(soil.out,
                 d.nm       = 'depth',
                 dsoil.vec  = soil.dp.vec,
                 exact.match= F,
                 max.dif    = 10)

temp.df$value     = temp.df$temp
temp.df$variable  = as.factor(temp.df$depth)

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

