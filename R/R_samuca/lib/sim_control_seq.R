#--- R SAMUCA
#--- MDSV - Jan/2019

SimControl.seq  = function(SC.template.fn,
                       SC.set.fn,
                       SC.outpath,
                       SC.outfn){
  
  #--------------------------------------------------------#
  #------ Create Simulation Control File for SAMUCA -------#
  #--------------------------------------------------------#
  #   Same as SimControl but only for sequential run
  #
  #   SC.template.fn  Control File Template "Path\\name.ctl"
  #   C.set.fn        Control File Settings "Path\\file.csv"
  #   SC.outpath      Output Control File Path
  #   SC.outfn        Output Control Filename
  #
  #   -Settings.csv
  #   File .csv must contain 7 columns:
  #   find      target string on template
  #   rep       replacement value
  #   justify   justify format (r = right, l = left, c = centre)
  #   width     character size of replecement
  #   type      type of value (c = character, i = integer, r = real) Note: Logical = character
  #   digits    maximun number of digits including character (More info in: ?format())
  #   nsmall    minimun number of digits
  #--------------------------------------------------------#
  
  if(missing(SC.template.fn)){stop("Argument SC.template.fn is missing for SimControl function")}
  if(missing(SC.set.fn)){stop("Argument SC.set.fn is missing for SimControl function")}
  if(missing(SC.outpath)){stop("Argument SC.outpath is missing for SimControl function")}
  if(missing(SC.outfn)){stop("Argument SC.outfn is missing for SimControl function")}
  
  #--- Read Template file
  SC.template = readLines(SC.template.fn)
  
  #--- Separate sections (needed for sequential simulations)
  #--- Note that soil and weather files do not change on a sequential run
  sec1 = SC.template[1:39]
  
  planting.seq = SC.template[40]    #Planting
  
  sec2 = SC.template[41:43]
  harvesting.seq = SC.template[44]  #Harvesting
  
  sec3 = SC.template[45:53]
  residue.seq    = SC.template[54]  #Residue Cover
  
  sec4 = SC.template[55:86]
  
  #--- Read Template Settings file (csv)
  SC.set      = read.csv(SC.set.fn, as.is = T)
  
  #--- Replacement on sections without sequence info
  sec1.rep =  rep.fun(rep = sec1,
                      set = SC.set[SC.set$seq == 0,])

  sec2.rep =  rep.fun(rep = sec2,
                      set = SC.set[SC.set$seq == 0,])
  
  sec3.rep =  rep.fun(rep = sec3,
                      set = SC.set[SC.set$seq == 0,])
  
  sec4.rep =  rep.fun(rep = sec4,
                      set = SC.set[SC.set$seq == 0,])
  
  #--- Replacement for sections with sequential info
  seq.list = unique(SC.set$seq[SC.set$seq > 0])
  
  for(s in seq.list){
    
    p.rep.s = rep.fun(rep = planting.seq,
                      set = SC.set[SC.set$seq == s,])
    
    h.rep.s = rep.fun(rep = harvesting.seq,
                      set = SC.set[SC.set$seq == s,])
    
    r.rep.s = rep.fun(rep = residue.seq,
                      set = SC.set[SC.set$seq == s,])
    
    if(s == seq.list[1]){
      #--- initialize
      planting.rep    = p.rep.s
      harvesting.rep  = h.rep.s
      residue.rep     = r.rep.s
    }else{
      #--- append
      planting.rep    = c(planting.rep,p.rep.s)
      harvesting.rep  = c(harvesting.rep,h.rep.s)
      residue.rep     = c(residue.rep,r.rep.s)
    }
  }
  
  #--- mount all sections
  rep = c(sec1.rep,
          planting.rep,
          sec2.rep,
          harvesting.rep,
          sec3.rep,
          residue.rep,
          sec4.rep)
  
  # write Simulation Control File -------------------------------------------
  write(rep,file = paste0(SC.outpath,"\\",SC.outfn,".ctl"))
  
}