#--- water stress factor sugarcane

#--- parameters

#--- crop extension
pfac_ext_ini = 0.90
pfac_ext_end = 0.50
sens_ext_shp = 5

#--- crop photosynthesis
pfac_pho_ini = 0.70
pfac_pho_end = 0.20
sens_pho_shp = 5

#--- ratio between actual and potential transpiration
r_tra = seq(0,1, by = 0.01)


adj_shp = T
adj_tol = 0.01
shp_ext = sens_ext_shp
while(adj_shp){
   
  swsfac_ext = AScurv(0,
                      x = r_tra,
                      min = 0,
                      max = 1,
                      shp = shp_ext,
                      mid = mean(c(pfac_ext_ini,pfac_ext_end)),
                      asy = 1)
  
  ini_sws = swsfac_ext[r_tra ==r_tra[which.min(abs(r_tra - pfac_ext_ini))]]
  end_sws = swsfac_ext[r_tra ==r_tra[which.min(abs(r_tra - pfac_ext_end))]]

  if(abs(ini_sws - 1) >= adj_tol | abs(end_sws - 0) >= adj_tol){
    shp_ext = shp_ext + 1
  }else{
    adj_shp = F
  }
}

adj_shp = T
adj_tol = 0.01
shp_pho = sens_pho_shp
while(adj_shp){
  swsfac_pho = AScurv(0,
                      x = r_tra,
                      min = 0,
                      max = 1,
                      shp = shp_pho,
                      mid = mean(c(pfac_pho_ini,pfac_pho_end)),
                      asy = 1)
  
  ini_sws = swsfac_pho[r_tra ==r_tra[which.min(abs(r_tra - pfac_pho_ini))]]
  end_sws = swsfac_pho[r_tra ==r_tra[which.min(abs(r_tra - pfac_pho_end))]]

  if(abs(ini_sws - 1) >= adj_tol | abs(end_sws - 0) >= adj_tol){
    shp_pho = shp_pho + 1
  }else{
    adj_shp = F
  }
}


plot(swsfac_ext~r_tra, type = "l", ylim = c(0,1.1))
lines(swsfac_pho~r_tra, type = "l", col = "red")



#--- as function

AScurv =  function(df,x,min,max,shp,mid,asy){
#--- Growth Function with asymetry (optional)
#--- Asymptote Curve and its first derivative (df=1)
#--- Author: Murilo S. Vianna
#--- Feb-2018
#--- Why using this function to describe growth? Refer to -> doi: 10.1093/aob/mcg029

# integer df    ! Equation form: 0 = integral, 1 = 1st derivative
# real  x       ! X Value
# real  min     ! Minimum Value
# real  max     ! Maximum Value
# real  shp     ! Function Shape
# real  mid     ! X Value in which y = (max - min) / 2.
# real  asy     ! Asymetry

  if(df == 0){
    #--- Integral form
    return(max + ((min - max) / ((1. + (x/mid)^shp)^asy)))
  }else{
    #--- 1st derivative
    return( -asy * (min-max) * ((1.+(x/mid)^shp)^(-asy-1.)) * (shp*(x/mid)^(shp-1.)) * (1./mid))
  }
  
}




