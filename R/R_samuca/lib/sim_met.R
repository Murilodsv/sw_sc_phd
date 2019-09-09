#--- R SAMUCA
#--- MDSV - Dec/2018

SimMet  = function(SC.template.fn,
                    SC.set.fn,
                    met.dt.fn,
                    SC.outpath,
                    SC.outfn){
  
  #--------------------------------------------------------#
  #------ Create Meteorological File for SAMUCA -----------#
  #--------------------------------------------------------#
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
  
  #--- Set Column width and SAMUCA's meteorological variables and order
  col.wid       = 7 # column width
  sam.met.order = c("year","doy","tmin","tmax","rain","srad","rh","wind","irr")
  
  #--- Read Template file
  SC.template = readLines(SC.template.fn)
  
  #--- Read Template Settings file (csv)
  SC.set      = read.csv(SC.set.fn, as.is = T)
  
  #--- Read meteorological data
  met.dt      = read.csv(paste0(wd.core,"\\sim_db\\weather\\",met.dt.fn), as.is = T)
  
  #--- SAMUCA met data and ordering
  met.dt.sam  = met.dt[,sam.met.order]
  
  # Replacement -------------------------------------------------------------
  
  rep = SC.template
  
  #--- Character Values
  rep.set     = SC.set[SC.set$type == "c",]
  rep.set$rep = as.character(rep.set$rep)
  
  for(f in rep.set$find){
    
    r = format(rep.set$rep[rep.set$find == f],
               justify = rep.set$justify[rep.set$find == f],
               width   = rep.set$width[rep.set$find == f],
               digits  = rep.set$digits[rep.set$find == f],
               nsmall  = rep.set$nsmall[rep.set$find == f])
    
    rep = gsub(f,r,rep)
  }
  
  #--- Integer values
  rep.set = SC.set[SC.set$type == "i",]
  rep.set$rep = as.integer(rep.set$rep)
  
  for(f in rep.set$find){
    
    r = format(rep.set$rep[rep.set$find == f],
               justify = rep.set$justify[rep.set$find == f],
               width   = rep.set$width[rep.set$find == f],
               digits  = rep.set$digits[rep.set$find == f],
               nsmall  = rep.set$nsmall[rep.set$find == f])
    
    rep = gsub(f,r,rep)
  }
  
  #--- Real Values
  rep.set = SC.set[SC.set$type == "r",]
  rep.set$rep = as.numeric(rep.set$rep)
  
  for(f in rep.set$find){
    
    r = format(rep.set$rep[rep.set$find == f],
               justify = rep.set$justify[rep.set$find == f],
               width   = rep.set$width[rep.set$find == f],
               digits  = rep.set$digits[rep.set$find == f],
               nsmall  = rep.set$nsmall[rep.set$find == f])
    
    rep = gsub(f,r,rep)
  }
  
  
  # write Simulation Control File -------------------------------------------
  write(rep,file = paste0(SC.outpath,"\\",SC.outfn,".met"))
  
  write.fwf(met.dt.sam,
            file = paste0(SC.outpath,"\\",SC.outfn,".met"),
            width = rep(col.wid,length(met.dt.sam)),
            append = T,
            colnames = F)
  
}
