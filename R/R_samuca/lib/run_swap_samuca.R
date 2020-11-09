#--- R SWAP-SAMUCA SIMULATIONS
#--- MDSV - Sep/2019

run.swap.samuca = function(SC.set.ctrl.fn,
                           SC.set.irri.fn,
                           SC.set.mana.fn,
                           SC.set.crop.fn,
                           SC.set.mete.fn,
                           met.dt.fn,
                           samuca.exe,
                           sim.id,
                           SC.outpath,
                           wd.core,
                           wd.model){
  
  #--- Create swap.swp
  SC.template.fn = paste0(wd.core,"/templates/swp_template.swp")
  SC.outfn       = "Swap"
  
  SimControl.SWAP(SC.template.fn = SC.template.fn,
                  SC.set.fn      = SC.set.ctrl.fn,
                  SC.outpath     = SC.outpath,
                  SC.outfn       = SC.outfn,
                  SC.irrig.fn    = SC.set.irri.fn)

  message(paste0("File Swap.swp created for: ",sim.id))
  
  #--- Create Management
  SC.template.fn = paste0(wd.core,"/templates/mng_template.mng")
  SC.outfn       = "Samuca"
  
  SimMana(SC.template.fn,
          SC.set.mana.fn,
          SC.outpath,
          SC.outfn)
  
  message(paste0("File Samuca.mng created for: ",sim.id))
  
  #--- Create Crop Parameters file
  SC.template.fn = paste0(wd.core,"/templates/crp_template.par")
  SC.outfn       = "Samuca"
  
  SimCrop.swap(SC.template.fn,
               SC.set.crop.fn,
               SC.outpath,
               SC.outfn)
  
  message(paste0("File Samuca.par created for: ",sim.id))
  
  #--- Create Meteorological files
  SC.outfn   = sim.id
  
  SimMet.SWAP(SC.set.fn = SC.set.mete.fn,
              met.dt.fn,
              SC.outpath,
              SC.outfn)
  
  message(paste0("Meteorolgical Files created for: ",sim.id))
  
  #-----------------------#
  #--- Run SWAP-SAMUCA ---#
  #-----------------------#
  
  message(paste0("Running SWAP-SAMUCA: ",sim.id))
  
  orig.wd = getwd()
  setwd(wd.model)
  
  #--- Run SWAP-SAMUCA
  system(samuca.exe)
  
  #--- Check if simulations were successfull
  if(!file.exists('swap.ok')){
    stop('Something went wrong in SWAP simulations.\n --- Simulation Aborted ---')
  }
  
  setwd(orig.wd)
  
  #--------------------#
  #--- Read outputs ---#
  #--------------------#
  
  
  
  #--- Output names
  plan.out.fn = paste0(wd.model,"/Plant_",SC.outfn,".out")
  atmo.out.fn = paste0(wd.model,"/",SC.outfn,".wba")
  soil.out.fn = paste0(wd.model,"/",SC.outfn,".vap")
  stre.out.fn = paste0(wd.model,"/",SC.outfn,".str")
  incr.out.fn = paste0(wd.model,"/",SC.outfn,".inc")
  dsoi.out.fn = paste0(wd.model,"/Detailed_Soil_",SC.outfn,".out")
  
  #--- Read outputs
  plan.out = read.plan.SWAP.out(plan.out.fn)
  atmo.out = read.atmo.SWAP.out(atmo.out.fn)
  soil.out = read.soil.SWAP.out(soil.out.fn)
  stre.out = read.stre.SWAP.out(stre.out.fn)
  incr.out = read.incr.SWAP.out(incr.out.fn)
  dsoi.out = read.dsoi.SWAP.out(dsoi.out.fn)
  
  #--- Evapotranspiration
  incr.out$et.pot = (incr.out$Tpot + incr.out$Epot) * 10 # mm/day
  incr.out$et.act = (incr.out$Tact + incr.out$Eact) * 10 # mm/day
  
  return(list(plan = plan.out,
              atmo = atmo.out,
              soil = soil.out,
              stre = stre.out,
              incr = incr.out,
              dsoi = dsoi.out))
  
}



