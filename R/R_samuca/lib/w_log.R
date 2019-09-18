#--- Write log file
#--- MDSV Sep/2019

w.log = function(w.msg, log.file, ini.log, h.msg){
  
  # w.msg       = message in string or data.frame
  # log.file    = path and filename for writing logs
  # ini.log     = flag indicating to initialize log file
  # h.msg       = flag to add time.Sys() as header of current  msg
  
  if(missing(ini.log)){ini.log = F}
  if(missing(w.msg)){w.msg = ""}
  if(missing(log.file)){stop("Please provide log.file path and name.")}
  if(missing(h.msg)){h.msg = T}
  
  if(ini.log){
    write.table(data.frame(message = "Initialized at",
                           time = Sys.time()),
                file = log.file,
                append = F,
                row.names = F,
                col.names = F,
                quote = F)
    
    write.table(" ",
                file = log.file,
                append = T,
                row.names = F,
                col.names = F,
                quote = F)
  }else{
    
    if(h.msg){
      write.table(data.frame(message = "Log Message at",
                             time = Sys.time()),
                  file = log.file,
                  append = T,row.names = F,
                  col.names = F,
                  quote = F)
    }
    
    if(is.data.frame(w.msg)){
      
      write.table(t(colnames(w.msg)),
                  file = log.file,
                  append = T,row.names = F,
                  col.names = F,
                  quote = F)
      
    }
    
    write.table(w.msg,
                file = log.file,
                append = T,row.names = F,
                col.names = F,
                quote = F)
    
    write.table(" ",
                file = log.file,
                append = T,
                row.names = F,
                col.names = F,
                quote = F)
    
  }
  
}