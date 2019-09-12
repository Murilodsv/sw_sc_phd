#--- R SAMUCA
#--- MDSV - Jan/2019

wd = "D:/Murilo/sw_sc/R/R_samuca/"

SC.set.fn      = paste0(wd,"/sim_db/sim_control_seq_f1_s1_SWAP.csv")
SC.template.fn = paste0(wd,"/templates/swp_template.swp")
SC.outpath     = wd
SC.outfn       = "debug_template_swap"

SimControl.SWAP  = function(SC.template.fn,
                           SC.set.fn,
                           SC.outpath,
                           SC.outfn){
  
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
  
  # write Simulation Control File -------------------------------------------
  write(rep.temp,file = paste0(SC.outpath,"\\",SC.outfn,".swp"))
  
}