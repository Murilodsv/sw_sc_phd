#--- get_data_dsoil
#--- MDSV - Jun/2020

get_data_dsoil = function(soil.out,
                          d.nm,
                          dsoil.vec,
                          exact.match,
                          max.dif){
  
  #---------------------------------------------------#
  #---------------- get_data_dsoil -------------------#
  #---------------------------------------------------#
  #--- Goal: Retrieve output data from corresponding depths
  #---
  #--- Parameters:
  #---  soil.out      # Output file with data for all soil compartments
  #---  d.nm          # Col name of soil.out of compartments depths 
  #---  dsoil.vec     # Vector with required depths
  #---  exact.match   # Flag to indicate whether should be an exact match of depths
  #---  max.dif       # Max depth diference for retrieving (in case of exact.match = F)
  #---------------------------------------------------#
  
  if(missing(exact.match)){exact.match   = F}
  if(missing(d.nm)){       d.nm          = 'depth'}
  if(missing(max.dif)){    max.dif       = 10}
  
  if(exact.match){max.dif = 0}
  
  init_df = T
  for(d in dsoil.vec){
    
    #--- get absolute difference
    dif_d = abs(d - unique(soil.out[,d.nm]))
    
    if(min(dif_d) <= max.dif){
      
      #--- get the lower absolute difference
      best_match = unique(soil.out[,d.nm])[dif_d == min(dif_d)][1] # the [1] is added in case of a draw, forcing to get only 1 value
      
      #--- get best match data
      res_d = soil.out[soil.out[,d.nm] == best_match,]
      
      #--- provide required depth
      res_d$depth_req = d
      
      if(init_df){
        res = res_d
        init_df = F
      }else{
        res = rbind(res, 
                    res_d)
      }
      
    }else{
      message(paste0('Warning: The soil depth ',d,
                     ' required from simulations deviates ',min(dif_d),
                     ' (e.g. > max.dif = ',max.dif,') from simulated value and can not be compared.'))
    }
  }
  
  if(init_df){
    message(paste0('Warning: None soil values were retrieve from simulation output.\n ---> Please review file, required depths or max limit for comparison (max.dif = ',max.dif,').'))
    return(NULL)
  }else{
    return(res)
  }
  
}