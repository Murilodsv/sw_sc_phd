#--- Check parameters range 
#--- Used for parameter optmization
#--- MDSV Sep/2019

check.rang = function(p.df){
  invalid.value = F
  if(any(p.df$value > p.df$max | p.df$value < p.df$min)){
    invalid.value = T
    
    invalid.finds = p.df$find[p.df$value > p.df$max | p.df$value < p.df$min]
    
    w.msg = "One or more parameters value(s) are out of max-min bounds:"
    message(w.msg)
    tryCatch(w.log(w.msg = w.msg, log.file = log.file, ini.log = F),error = "Error in writing log msg")
    w.msg = print(p.df[p.df$find %in% invalid.finds,c("find","value","min","max")])
    tryCatch(w.log(w.msg = w.msg, log.file = log.file, ini.log = F, h.msg = F),error = "Error in writing log msg")
    
  }
  
  return(!invalid.value)
}
