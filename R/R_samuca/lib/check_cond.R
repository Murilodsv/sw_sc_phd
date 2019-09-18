#--- Check conditions in parameters file
#--- Used for parameter optmization
#--- MDSV Sep/2019

check.cond = function(p.df){
  
  #----------------------------------#
  #--- Validate new parameter set ---#
  #----------------------------------#
  sep = " "
  cond.v = sapply(p.df$find[!is.na(p.df$cond)], 
                  function(x) strsplit(p.df$cond[p.df$find == x], split = sep)[[1]][2])
  v1.v = sapply(p.df$find[!is.na(p.df$cond)], 
                function(x) strsplit(p.df$cond[p.df$find == x], split = sep)[[1]][1])
  v2.v = sapply(p.df$find[!is.na(p.df$cond)], 
                function(x) strsplit(p.df$cond[p.df$find == x], split = sep)[[1]][3])
  
  chk.df = data.frame(cond = cond.v,
                      v1.v = v1.v,
                      v2.v = v2.v,
                      stringsAsFactors = F)
  
  l.cond = c("<","<=","==",">=",">","!=")
  invalid.value = F
  for(c in l.cond){
    if(any(chk.df$cond == c)){
      chk.df.c = chk.df[chk.df$cond == c,]
      
      v1.values = p.df$value[p.df$find %in% chk.df.c$v1.v]
      v2.values = p.df$value[p.df$find %in% chk.df.c$v2.v]
      
      exp = paste("v1.values",c,"v2.values")
      
      if(any(!eval(parse(text=exp)))){
        
        #--- Invalid value found!
        invalid.value = T
        invalid.finds = c(chk.df.c$v1.v[!eval(parse(text=exp))],chk.df.c$v2.v[!eval(parse(text=exp))])
        
        w.msg = "One or more parameters value(s) do not match requirements provided in 'cond':"
        message(w.msg)
        tryCatch(w.log(w.msg = w.msg, log.file = log.file, ini.log = F),error = "Error in writing log msg")
        w.msg = print(p.df[p.df$find %in% invalid.finds,c("find","value","min","max","cond")])
        tryCatch(w.log(w.msg = w.msg, log.file = log.file, ini.log = F, h.msg = F),error = "Error in writing log msg")
      }
    }
  }
  
  return(!invalid.value)
}