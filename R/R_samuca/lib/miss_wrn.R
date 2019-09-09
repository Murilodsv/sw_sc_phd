#--- R SAMUCA
#--- MDSV - Jan/2019

#--- Functions
miss.wrn  = function(var,fnm) {
  #--- var      variable
  #--- fnm      function name
  
  if(missing(fnm)){
    fnm = ""
  }else{
    fnm = paste0("In function ",fnm,":")
  }
  
  if(missing(var)){
    stop(paste0(fnm," the input variable '",deparse(substitute(var)),"' is missing."), call. = F)
  }
}