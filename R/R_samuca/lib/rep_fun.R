#--- R SAMUCA
#--- MDSV - Jan/2019

rep.fun = function(rep,set){
  
  #--------------------------------------------------------------------------
  #--- Function to replace the template values to simulation control values
  #
  #     rep     is the fragment where the replacement will take place
  #     set     is the settings for the replacement and corresponding values (this is a data.frame - csv file)
  #     "c"     is for character
  #     "i"     is for integer
  #     "r"     is for real
  #--------------------------------------------------------------------------
  
  if(missing(rep)){stop("Missing replace template (rep) in rep.fun()")}
  if(missing(set)){stop("Missing replace settings (SC.set) in rep.fun()")}
  
  SC.set = set
  
  
  
  #--- Character Values
  rep.set     = set[set$type == "c",]
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
  rep.set = set[set$type == "i",]
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
  rep.set = set[set$type == "r",]
  rep.set$rep = as.numeric(rep.set$rep)
  
  for(f in rep.set$find){
    
    r = format(rep.set$rep[rep.set$find == f],
               justify = rep.set$justify[rep.set$find == f],
               width   = rep.set$width[rep.set$find == f],
               digits  = rep.set$digits[rep.set$find == f],
               nsmall  = rep.set$nsmall[rep.set$find == f])
    
    rep = gsub(f,r,rep)
  }
  
  return(rep)
  
}
