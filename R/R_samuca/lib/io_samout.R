#--- Functions to Read SAMUCA outputs

# Read SAMUCA Outputs ------------------------------------------------------
read.plan.out = function(fn){
  
  #--------------------------
  #--- Read Plant outputs ---
  #--------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
    if(missing(fn)){
    stop("No filename passed to function read.plant.out")
  }

  #--- Read by lines
  p.out = readLines(fn)
  
  #--- Only numeric lines
  p.out.num = suppressWarnings(p.out[!is.na(as.numeric(substr(p.out,1,3)))])
  
  #--- Read as table
  p.out.df = read.table(text = p.out.num,
                        col.names = c("seq",
                                      "pl.type",
                                      "year",
                                      "doy",
                                      "das",
                                      "dap",
                                      "diac",
                                      "dw.tp",
                                      "dw.st",
                                      "dw.lv",
                                      "dw.rt",
                                      "fw.st",
                                      "dw.su",
                                      "pol",
                                      "lai",
                                      "till",
                                      "p.ht",
                                      "n.gl",
                                      "swface",
                                      "swfacp",
                                      "c.status",
                                      "c.stage",
                                      "sim.id"))
  
  return(p.out.df)
  
}

read.atmo.out = function(fn){
  
  #-------------------------------
  #--- Read Atmosphere outputs ---
  #-------------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
  if(missing(fn)){
    stop("No filename passed to function read.plant.out")
  }
  
  #--- Read as table
  out.df = read.table(file = fn,
                      skip = 7,
                      col.names = c("year",
                                      "doy",
                                      "das",
                                      "dap",
                                      "ret",
                                      "pet",
                                      "peva",
                                      "ptra",
                                      "prwu",
                                      "et",
                                      "eva",
                                      "tra",
                                      "arwu",
                                      "es",
                                      "esm",
                                      "muwat",
                                      "muwat.abs",
                                      "etp.acc",
                                      "et.acc",
                                    "eva.acc",
                                    "tra.acc",
                                    "tra.dep"))
  
  
  return(out.df)
  
}

read.soil.out = function(fn,nl){
  
  #-------------------------
  #--- Read Soil outputs ---
  #-------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
  if(missing(fn)){stop("No filename passed to function read.plant.out")}
  if(missing(nl)){stop("Number of soil layers (nl) not provided to function read.soil.out")}
  
  #--- colnames
  c.names = col.names = c("year",
                          "doy",
                          "das",
                          "dap",
                          "rain",
                          "irrig",
                          "runoff",
                          "drain",
                          paste0("swc",seq(1,nl)),
                          "rel.watavl",
                          "rel.watavl.rd",
                          "prwu",
                          "ptra",
                          "tra",
                          "eva",
                          "et",
                          "dw.rt",
                          paste0("rld",seq(1,nl)),
                          "rt.dp"
                          )
  
  #--- Read as table
  out.df = read.table(file = fn,
                      skip = 7,
                      col.names = c.names)
  
  return(out.df)
  
}

read.soil.out.standalone = function(fn,nl){
  
  #-------------------------
  #--- Read Soil outputs ---
  #-------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
  if(missing(fn)){stop("No filename passed to function read.plant.out")}
  if(missing(nl)){stop("Number of soil layers (nl) not provided to function read.soil.out")}
  
  #--- colnames
  c.names = col.names = c("year",
                          "doy",
                          "das",
                          "dap",
                          "rain",
                          "irrig",
                          paste0("swc",seq(1,nl)),
                          paste0("rld",seq(1,nl))
                          
  )
  
  #--- Read as table
  out.df = read.table(file = fn,
                      skip = 7,
                      col.names = c.names)
  
  return(out.df)
  
}

read.temp.out = function(fn,nl){
  
  #-------------------------
  #--- Read Soil Temperature
  #-------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
  if(missing(fn)){stop("No filename passed to function read.plant.out")}
  if(missing(nl)){stop("Number of soil layers (nl) not provided to function read.soil.out")}
  
  #--- colnames
  c.names = col.names = c("year",
                          "doy",
                          "das",
                          "dap",
                          "Tair",
                          "tetop",
                          "mulch",
                          paste0("Ts.",seq(1,nl)),
                          "tebot",
                          "G",
                          "hmulch",
                          "heacap_top",
                          "heacon_top")
  
  #--- Read as table
  out.df = read.csv(file = fn,
                      skip = 6,
                      col.names = c.names)
  
  return(out.df)
  
}
