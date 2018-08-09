#--- swap-samuca functions

#--- Function to index das on other DBs
inx = function(df){
  df = merge(indexc,df,by = c("year","doy"))
  df = df[order(df$das),]#--- sort by das
}

#--- create charts
fdrpl = function(x){
  
  plot(fdr[,x]~fdr$das,
       col  = "black",
       xlab = "",
       ylab = "SWC (cm3 cm-3)",
       xlim = c(50,1500),
       ylim = c(0.15,0.45),
       cex.lab = 1.,
       cex.axis= 1.,
       main =NULL)
  
  lines(l$wcontent[l$fdr==x]~l$das[l$fdr==x], col = "grey")
  
}

#--- plot detailed data
pl_detdata = function(obs,sim,yl,dxlab,i,n){
  
  if(obs$dat[1] == -99){
    #--- no observed data
    pylim = c(min(sim$dat),max(sim$dat)*1.04)
    pxlim = c(min(sim$das),max(sim$das))
    
    if(dxlab){
      
      li = sapply(unique(sim$ctype), 
                  function(x) lines(sim$dat[sim$ctype==x]~sim$das[sim$ctype==x],
                                    ylab = yl,
                                    xlab = "DAS",
                                    ylim = pylim,
                                    xlim = pxlim,
                                    yaxs="i",
                                    type = "l",
                                    col = alpha(rgb(0,0,0.3), 1 - i/n)))
    }else{
      
      li = sapply(unique(sim$ctype), 
                  function(x) lines(sim$dat[sim$ctype==x]~sim$das[sim$ctype==x],
                                    ylab = yl,
                                    xlab = "",
                                    ylim = pylim,
                                    xlim = pxlim,
                                    yaxs="i",
                                    xaxt='n',
                                    type = "l",
                                    col = alpha(rgb(0,0,0.3), 1 - i/n)))
    }
    
  }else{
    #--- observed data
    pylim = c(min(sim$dat,obs$dat),max(sim$dat,obs$dat)*1.04)
    pxlim = c(min(sim$das,obs$das),max(sim$das,obs$das))
    
    if(dxlab){
      
      plot(obs$dat~obs$das,
           ylab = yl,
           xlab = "DAS",
           ylim = pylim,
           xlim = pxlim,
           yaxs="i")
    }else{
      
      plot(obs$dat~obs$das,
           ylab = yl,
           xlab = "",
           ylim = pylim,
           xlim = pxlim,
           yaxs="i",
           xaxt='n')
    }
    
    li = sapply(unique(sim$ctype), function(x) lines(sim$dat[sim$ctype==x]~sim$das[sim$ctype==x]))
    
  }
}


#--- Compute statistical indexes of performance
#--- Performance function
mperf = function(sim,obs,vnam){
  
  # sim  - simulated values [R]
  # obs  - observed values [R]
  # vnam - Name of variable as string for chart axis  [S]
  
  #--- Statistical indexes
  fit   = lm(sim~obs)
  bias  = (1/length(obs)) * sum(sim-obs)
  mse   = (1/length(obs)) * sum((sim-obs)^2)
  rmse  = sqrt(mse)
  mae   = (1/length(obs)) * sum(abs(sim-obs))
  rrmse = rmse / mean(obs)
  rmae  = (1/length(obs[obs>0])) * sum(abs(sim[obs>0]-obs[obs>0])/abs(obs[obs>0]))
  ef    = 1 - (sum((sim-obs)^2) / sum((obs-mean(obs))^2))
  r     = sum((obs-mean(obs))*(sim-mean(sim)))/sqrt(sum((obs-mean(obs))^2)*sum((sim-mean(sim))^2))
  r2    = r^2
  d     = 1 - (sum((sim-obs)^2) / sum((abs(sim-mean(obs))+abs(obs-mean(obs)))^2))
  a     = summary(fit)$coefficients["(Intercept)","Estimate"]
  b     = summary(fit)$coefficients["obs","Estimate"]
  
  #--- Chart Sim ~ Obs
  varlab = vnam 
  
  mindt = min(obs,sim)
  maxdt = max(obs,sim)
  #--- Ploting limits 
  pllim = c(mindt-0.1*(maxdt-mindt),maxdt+0.1*(maxdt-mindt))
  xx = seq(min(obs),max(obs),length = (max(obs)-min(obs))*1000)
  z = summary(fit)
  
  plot(sim~obs,
       ylab = paste("Sim - ",varlab,sep = ""),
       xlab = paste("Obs - ",varlab,sep = ""),
       ylim = pllim,
       xlim = pllim)
  
  lines(xx, predict(fit, data.frame(obs=xx)),
        col = "black",
        lty = 1,
        lwd = 1.5)
  
  l11 = seq(pllim[1]-0.5*(maxdt-mindt), pllim[2] + 0.5 * (maxdt-mindt),length = 1000)
  
  lines(l11*1~l11,
        col = "red",
        lty = 2,
        lwd = 1.5)
  
  perf = data.frame(bias,
                    mse,
                    rmse,
                    mae,
                    rrmse,
                    rmae,
                    ef,
                    r,
                    r2,
                    d,
                    a,
                    b)
  
}

pl_bio = function(obs,sim,yl,dxlab){
  
  pylim = c(min(sim$dat,obs$dat),max(sim$dat,obs$dat)*1.04)
  pxlim = c(min(sim$das,obs$das),max(sim$das,obs$das))
  
  if(dxlab){
    
    plot(obs$dat~obs$das,
         ylab = yl,
         xlab = "DAS",
         ylim = pylim,
         xlim = pxlim,
         yaxs="i")
  }else{
    
    plot(obs$dat~obs$das,
         ylab = yl,
         xlab = "",
         ylim = pylim,
         xlim = pxlim,
         yaxs="i",
         xaxt='n')
  }
  
  li = sapply(unique(sim$ctype), function(x) lines(sim$dat[sim$ctype==x]~sim$das[sim$ctype==x]))
  
}