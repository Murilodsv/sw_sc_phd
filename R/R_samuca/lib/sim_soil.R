#--- R SAMUCA
#--- MDSV - Dec/2018

SimSoil     = function(SC.template.fn,
                       SC.set.fn,
                       SC.outpath,
                       SC.outfn){

  #--------------------------------------------------------#
  #------ Create Soil File for SAMUCA Simulations ---------#
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
  #   layer     soil layer ID (0 = profile header, 1 = layer1, 2 = layer2 ...)
  #--------------------------------------------------------#
  
  #--- Read Template file
  SC.template = readLines(SC.template.fn)
  
  #--- Layered template
  lay.temp = SC.template[length(SC.template)]
  prf.temp = SC.template[1:(length(SC.template)-1)]
  
  #--- Read Template Settings file (csv)
  SC.set      = read.csv(SC.set.fn, as.is = T)
  

# Replacement of profile description --------------------------------------
  
  #--- initialize
  rep = prf.temp
  
  #--- Character Values. Note Layer == 0 is profile description
  rep.set     = SC.set[SC.set$type == "c" & SC.set$layer == 0,]
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
  rep.set = SC.set[SC.set$type == "i" & SC.set$layer == 0,]
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
  rep.set = SC.set[SC.set$type == "r" & SC.set$layer == 0,]
  rep.set$rep = as.numeric(rep.set$rep)
  
  for(f in rep.set$find){
    
    r = format(rep.set$rep[rep.set$find == f],
               justify = rep.set$justify[rep.set$find == f],
               width   = rep.set$width[rep.set$find == f],
               digits  = rep.set$digits[rep.set$find == f],
               nsmall  = rep.set$nsmall[rep.set$find == f])
    
    rep = gsub(f,r,rep)
  }
  

# Replacement of layer data -----------------------------------------------

  l.layers = unique(SC.set$layer)[unique(SC.set$layer)>0]
  
  for(l in l.layers){
    
    rep.l = lay.temp
    
    #--- Character Values. Note Layer == 0 is profile description
    rep.set     = SC.set[SC.set$type == "c" & SC.set$layer == l,]
    rep.set$rep = as.character(rep.set$rep)
    
    for(f in rep.set$find){
      
      r = format(rep.set$rep[rep.set$find == f],
                 justify = rep.set$justify[rep.set$find == f],
                 width   = rep.set$width[rep.set$find == f],
                 digits  = rep.set$digits[rep.set$find == f],
                 nsmall  = rep.set$nsmall[rep.set$find == f])
      
      rep.l = gsub(f,r,rep.l)
    }
    
    #--- Integer values
    rep.set = SC.set[SC.set$type == "i" & SC.set$layer == l,]
    rep.set$rep = as.integer(rep.set$rep)
    
    for(f in rep.set$find){
      
      r = format(rep.set$rep[rep.set$find == f],
                 justify = rep.set$justify[rep.set$find == f],
                 width   = rep.set$width[rep.set$find == f],
                 digits  = rep.set$digits[rep.set$find == f],
                 nsmall  = rep.set$nsmall[rep.set$find == f])
      
      rep.l = gsub(f,r,rep.l)
    }
    
    #--- Real Values
    rep.set = SC.set[SC.set$type == "r" & SC.set$layer == l,]
    rep.set$rep = as.numeric(rep.set$rep)
    
    for(f in rep.set$find){
      
      r = format(rep.set$rep[rep.set$find == f],
                 justify = rep.set$justify[rep.set$find == f],
                 width   = rep.set$width[rep.set$find == f],
                 digits  = rep.set$digits[rep.set$find == f],
                 nsmall  = rep.set$nsmall[rep.set$find == f])
      
      rep.l = gsub(f,r,rep.l)
    }
    
    #--- Append to Soil profile
    rep = c(rep,rep.l)
    
  }
  
  # write Soil Control File -------------------------------------------
  write(rep,file = paste0(SC.outpath,"\\",SC.outfn,".spd"))
  
}
