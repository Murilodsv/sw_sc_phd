#--- R SAMUCA
#--- MDSV - Dec/2018

ListControl = function(seq,sim.fn,LC.path){

  #--------------------------------------------------------#
  #---- Create ListControl File for SAMUCA Simulations ----#
  #--------------------------------------------------------#
  #   seq       Simulation Sequence
  #   sim.fn    Simulation Control Filename
  #   LC.path   Directory for created ListControl.sam
  #--------------------------------------------------------#
  
  if(missing(seq)){stop("Parameters missing for function ListControl")}
  if(missing(sim.fn)){stop("Parameters missing for function ListControl")}
  if(missing(LC.path)){stop("Parameters missing for function ListControl")}
  
  #--- Constants
  LC.template = c(
  "*Simulation             Control File",
  " <seq>             <sim.file>",
  "")
  
  #--- Format inputs
  seq.fmt     = format(seq   , width = 10,justify = "right")
  sim.fn.fmt  = format(sim.fn, justify = "left")
  
  #--- Replace with inputs
  rep = gsub("<seq>"     ,seq.fmt    ,LC.template)
  rep = gsub("<sim.file>",sim.fn.fmt ,rep)
  
  #--- Write formated file
  write(rep,file = paste0(LC.path,"\\ListControl.sam"))
  
}




