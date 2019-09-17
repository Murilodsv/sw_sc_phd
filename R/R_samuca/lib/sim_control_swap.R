#--- R SAMUCA
#--- MDSV - Jan/2019

SimControl.SWAP  = function(SC.template.fn,
                           SC.set.fn,
                           SC.outpath,
                           SC.outfn,
                           SC.irrig.fn){
  
  #--------------------------------------------------------#
  #------ Create Simulation Control File for SAMUCA -------#
  #--------------------------------------------------------#
  #   Same as SimControl but only for sequential run
  #
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
  
  #--- Read Template file
  SC.template = readLines(SC.template.fn)
  
  #--- Read Template Settings file (csv)
  SC.set      = read.csv(SC.set.fn, as.is = T)
  
  #--- Read Irrigation applications files
  SC.irrig    = read.csv(SC.irrig.fn, as.is = T)
  
  #--- Read Irrigation meta info
  SC.irrig.meta = read.csv(gsub("irri_control_","irri_meta_",SC.irrig.fn), as.is = T)
  
  #--- Melt irrigation df
  SC.irrig.set = melt.df(SC.irrig)
  
  #--- use only col.id and char.val
  SC.irrig.set = SC.irrig.set[,c("col.id","char.val")]
  
  #--- add "<>" to finders
  SC.irrig.set$col.id = paste0("<",SC.irrig.set$col.id,">")
  
  #--- bind meta information
  SC.irrig.set = cbind(SC.irrig.set, 
                       SC.irrig.meta[match(SC.irrig.set$col.id,paste0("<",SC.irrig.meta$find,">")),
                                     c("justify","width","type","digits","nsmall")])
  #--- Add seq, seq_id, and section
  SC.irrig.set$seq = rep(1:length(SC.irrig[,1]),
                         length(unique(SC.irrig.set$col.id)))
  SC.irrig.set$seq_id = "cs_p2"
  SC.irrig.set$section_sam = "CROP"
  
  #--- rename colnames to cope with SC.set
  colnames(SC.irrig.set)[1:2] = c("find","rep")
  
  #--- append to the rest of control file
  SC.set = rbind(SC.set,SC.irrig.set)
  
  #--- replace "_" by "-" in SWAP dates
  SC.set$rep[grep("date", SC.set$find)] = gsub("_","-",SC.set$rep[grep("date", SC.set$find)])
  
  #--- Replacement on sections without sequence info
  rep.temp =  rep.fun(rep = SC.template,
                      set = SC.set[SC.set$seq == 0,])
  
  #--- find sequential or tabulated inputs
  SC.set.seq  = SC.set[!is.na(SC.set$seq_id),]
  
  l.seq.id = unique(SC.set.seq$seq_id)
  
  for(s in l.seq.id){
    
    #--- get one var id
    line.var = SC.set.seq$find[SC.set.seq$seq_id == s][1]
    
    #--- find the row in template to be replicated
    ln.rep   = grep(line.var, rep.temp)
    l.rep    = rep.temp[ln.rep]
    
    l.seq.id.s = unique(SC.set.seq$seq[SC.set.seq$seq_id == s])
    
    #--- order
    l.seq.id.s = l.seq.id.s[order(l.seq.id.s)]
    
    for(l in l.seq.id.s){
      
      #--- update line number
      ln.rep   = grep(line.var, rep.temp)
      
      #--- replace in line number
      rep.temp[ln.rep] = rep.fun(rep = l.rep,
                                 set = SC.set.seq[SC.set.seq$seq_id == s & SC.set.seq$seq == l,])
      
      if(l!=l.seq.id.s[length(l.seq.id.s)]){
        #--- insert new
        rep.temp = insert(rep.temp,ln.rep+1,l.rep)
      }
    }
  }
  
  #--- time this file was created
  rep.temp = gsub("<time_now>",paste0(Sys.time()),rep.temp)
  
  # write Simulation Control File -------------------------------------------
  write(rep.temp,file = paste0(SC.outpath,"\\",SC.outfn,".swp"))
  
}


melt.df = function(df){
  
  #--- convert a df in two collum:
  #--- col1 = colname
  #--- col2 = value
  
  if(missing(df)){stop("Missing data.frame in melt.df function.")}
  if(!is.data.frame(df)){stop("Please provide a data.frame type.")}
  
  l.col = colnames(df)
  
  for(c in l.col){
    
    c.id      = rep(c,length(df[,1]))
    c.value   = df[,c]
    c.class   = rep(class(c.value),length(df[,1]))
    
    df.c = data.frame(col.id = c.id, 
                      char.val = as.character(c.value),
                      clas.val = c.class,
                      stringsAsFactors = F)
    
    if(c == l.col[1]){
      df.m = df.c
    }else{
      df.m = rbind(df.m,df.c)
    }
  }
  
  return(df.m)
}

