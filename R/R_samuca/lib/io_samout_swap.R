#--- Functions to Read SWAP-SAMUCA outputs

# Read SAMUCA Outputs ------------------------------------------------------
read.plan.SWAP.out = function(fn){
  
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

read.atmo.SWAP.out = function(fn){
  
  #-------------------------------
  #--- Read Atmosphere outputs ---
  #-------------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
  if(missing(fn)){
    stop("No filename passed to function read.plant.out")
  }
  
  #--- Read as table
  out.df = read.csv(file = fn,
                    skip = 6,
                    as.is = T)
  
  #--- Convert date to R dates and retrieve year vector
  out.df$Date = as.Date(out.df$Date, format = "%d-%B-%Y")
  out.df$year = format(out.df$Date, "%Y")
  
  #--- use same col index names
  colnames(out.df)[1:3] = c("date",
                "doy",
                "das")
  
  return(out.df)
  
}

read.incr.SWAP.out = function(fn){
  
  #------------------------------
  #--- Read Increment outputs ---
  #------------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
  if(missing(fn)){
    stop("No filename passed to function read.plant.out")
  }
  
  #--- Read as table
  out.df = read.csv(file = fn,
                    skip = 6,
                    as.is = T)
  
  #--- Convert date to R dates and retrieve year vector
  out.df$Date = as.Date(out.df$Date, format = "%d-%B-%Y")
  out.df$year = format(out.df$Date, "%Y")
  
  #--- use same col index names
  colnames(out.df)[1:3] = c("date",
                            "doy",
                            "das")
  
  return(out.df)
  
}

read.soil.SWAP.out = function(fn){
  
  #-------------------------
  #--- Read Soil outputs ---
  #-------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
  if(missing(fn)){stop("No filename passed to function read.plant.out")}
  
  #--- Read as table
  out.df = read.csv(file = fn,
                      skip = 11,
                      as.is = T)
  
  #--- Convert date to R dates and retrieve year vector
  out.df$date = as.Date(out.df$date, format = "%d-%B-%Y")
  out.df$year = format(out.df$date, "%Y")
  
  #--- use same col index names
  colnames(out.df)[colnames(out.df) %in% c("day","dcum")] = c("doy","das")
  
  return(out.df)
  
}

read.stre.SWAP.out = function(fn){
  
  #-------------------------
  #--- Read Soil outputs ---
  #-------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
  if(missing(fn)){stop("No filename passed to function read.plant.out")}
  
  #--- Read as table
  out.df = read.csv(file = fn,
                    skip = 6,
                    as.is = T)
  
  #--- Convert date to R dates and retrieve year vector
  out.df$Date = as.Date(out.df$Date, format = "%d-%B-%Y")
  out.df$year = format(out.df$Date, "%Y")
  
  #--- use same col index names
  #--- Convert date to R dates and retrieve year vector
  out.df$Date = as.Date(out.df$Date, format = "%d-%B-%Y")
  out.df$year = format(out.df$Date, "%Y")
  
  #--- use same col index names
  colnames(out.df)[1:3] = c("date",
                            "doy",
                            "das")
  
  return(out.df)
  
}


read.dsoi.SWAP.out = function(fn){
  
  #-------------------------
  #--- Read Soil outputs ---
  #-------------------------
  # fn        Path\\filename (Plant.out)
  #--------------------------
  
  if(missing(fn)){stop("No filename passed to function read.plant.out")}
  
  #--- Read as table
  out.df = read.csv(file = fn,
                    as.is = T)
  
  colnames(out.df) = c("seqnow","year","doy","das","dap","rd","rld","dp","slthickness","sl")
  
  return(out.df)
  
}
