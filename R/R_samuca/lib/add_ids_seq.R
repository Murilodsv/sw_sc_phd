#--- R SAMUCA
#--- MDSV - Jan/2019

add.id.seq = function(meta.perf, df){
  
  #--------------------------------------------------------------------------
  #---------------- Add meta IDs to sequential runs ------------------------
  #   meta.perf     data-frame with corresponding sequential run info
  #   df            data-frame with simulations or observation to include ids
  #--------------------------------------------------------------------------
  
  last.year = max(meta.perf$harvesting_yr)
  last.doy  = meta.perf$harvesting_doy[meta.perf$harvesting_yr == last.year]
  
  for(i in 1:length(meta.perf$M_ID)){
    
    if(length(last.doy) > 1 ){
      stop("Warning: Harvesting date is repeated for same sequential run. Please provide a unique Field x Sequence x Treatment running set in meta file [csv]")
    }
    
    p.yr = meta.perf$planting_yr[i]
    p.doy= meta.perf$planting_doy[i]
    h.yr = meta.perf$harvesting_yr[i]
    h.doy = meta.perf$harvesting_doy[i]
    id    = meta.perf$M_ID[i]
    
    df[df$year == p.yr & df$doy >= p.doy,"M_ID"] = id
    df[df$year == h.yr & df$doy < h.doy,"M_ID"]  = id
    
    if(h.yr == last.year & h.doy == last.doy){
      df[df$year == last.year & df$doy == last.doy, "M_ID"] = id  
    }
  }
  
  return(df$M_ID)
  
}
