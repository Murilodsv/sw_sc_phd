#--- Debug Samuca

wd          = "D:/Murilo/samuca/swap/sw_sc"
deb_fn      = "Debug_SWAP-SAMUCA_PIRA.out"

setwd(wd)
deb = read.table(deb_fn, col.names = c("year","doy","das","dap","gdd","qrot","ritchie","ptra",paste0(rep("cumdens",202),seq(1,202))))


plot(deb$qrot~deb$das, type = "l", ylim = c(0,2))
lines(deb$ritchie~deb$das, type = "l", col = "red")

plot(deb$qrot~deb$ritchie)

mperf(sim = deb$qrot, obs = deb$ritchie, vnam = "RWU")
xl = 1
yl = 2
for(i in 1:100){
  
  x = deb[,paste0("cumdens",xl)]
  y = deb[,paste0("cumdens",yl)]
  
  
  if(i == 1){
    y_df = data.frame(das   = deb$das,
                    cum_y = y,
                    cum_x = x)
  }else{
    y_df = rbind(y_df,data.frame(das   = deb$das,
                                 cum_y = y,
                                 cum_x = x))
  }
  
  xl = xl + 2
  yl = yl + 2
  
}


plot(y_df$cum_y~y_df$cum_x, type)
