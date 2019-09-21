#---------------------------------------#
#--- Optimize SWAP-SAMUCA parameters ---#
#---------------------------------------#

#--- Get paths
get.rp.root = function(){
  
  r.wd = dirname(rstudioapi::getSourceEditorContext()$path)
  
  git.not.found = T
  l.f = strsplit(r.wd,"/")[[1]]
  i.path = length(l.f)
  
  while(git.not.found){
    wd = paste(l.f[1:i.path], collapse = "/")
    
    if(length(list.files(wd, all.files = T,pattern = ".git")) > 0){
      git.not.found = F
      return(wd)
    }else{
      if(i.path == 1){
        
        git.not.found = F
        message("No Git File Found in Path Tree:")
        message(r.wd)
        return(r.wd)
        
      }else{
        
        i.path = i.path - 1
        
      }
    }
  }
}
wd.repo   = get.rp.root()
wd.rsam   = paste0(wd.repo,"/R/R_samuca")
wd.lib    = paste0(wd.repo,"/R/R_samuca/lib")
wd.model  = paste0(wd.repo,"/model")
wd.debug  = paste0(wd.repo,"/Debug")

#--- Load Source Files (~/bin) 
invisible(sapply(list.files(path = wd.lib,full.names = T),
                 function(x) source(x)))
use.debug  = F
samuca.exe = "swap_samuca_v1.exe"
if(use.debug){
  #--- Update .exe
  file.copy(paste0(wd.debug,"/",samuca.exe),
            wd.model, overwrite = T)
}

#------------------------#
#--- GLOBAL VARIABLES ---#
#------------------------#
sim.id          = "SEQ_F1_S2"
target.ctrl     = read.csv(paste0(wd.rsam,"/opt/target_control.csv"), as.is = T)
obs.data        = read.csv(paste0(wd.rsam,"/opt/target_data.csv"), as.is = T) 
p.df.opt        = read.csv(paste0(wd.rsam,"/opt/par_df.csv"), as.is = T)
index.obj       = "ef"
index.obj.cor   = 1
penalize.obj    = F
penalty.fac     = 2
plot.perf       = T
save.surf       = T
opt.df.fn       = paste0(wd.rsam,"/opt/opt_iteration.csv")
surf.df.fn      = paste0(wd.rsam,"/opt/par_surf.csv")
log.file        = paste0(wd.rsam,"/log_",sim.id,".txt")
SC.outpath      = wd.model
SC.set.ctrl.fn  = paste0(wd.rsam,"/sim_db/sim_control_",sim.id,"_SWAP.csv")
SC.set.mana.fn  = paste0(wd.rsam,"/sim_db/management/mana_control_",sim.id,".csv")
SC.set.crop.fn  = paste0(wd.rsam,"/sim_db/crop/crop_control_",sim.id,".csv")
SC.set.mete.fn  = paste0(wd.rsam,"/sim_db/weather/met_control_",sim.id,".csv")
SC.set.irri.fn  = paste0(wd.rsam,"/sim_db/management/irri_control_",sim.id,".csv")
met.dt.fn       = paste0(wd.rsam,"/sim_db/weather/met_data_f1.csv")

#--- Initialize iteration
opt.df    = data.frame(it = 0,
                     ini.run = T,
                     objective = 0,
                     sim.id  = sim.id)
write.csv(opt.df, file = opt.df.fn, row.names = F)

#--- Only parameters set to for optimization
p.df = p.df.opt[p.df.opt$opt,]

#--- Initial conditions
ini.par.scaled = (p.df$value - p.df$min) / (p.df$max - p.df$min)

#---------------------------#
#--- Optimize parameters ---#
#---------------------------#

# Choose methods for optimization and/or add limits ?optim()
# Note as we scaled parameters limits should be 0-1

#--- Search for an approximation of global minimum valley
get.obj.fun = T
opt.surf = optim(ini.par.scaled,
                 opt.swap.samuca,
                 gr = NULL,
                 method = "SANN",
                 control = list(maxit = 5000, temp = 1))

#--- Improve optimization
opt.res = optim(opt.surf$par,
                 opt.swap.samuca,
                 gr = NULL,
                 method = "Nelder-Mead")

#--- Optimized parameters scaled
opt.par.scaled = opt.res$par

#--- Run with best set of parameters
get.obj.fun = F
opt.sim.res = opt.swap.samuca(opt.par.scaled)

#--- Unscale best set of parameters
opt.par = p.df$min + opt.par.scaled * (p.df$max - p.df$min)

opt.par.df = data.frame(par.name = p.df$find,
                        par.opt  = opt.par)

write.csv(opt.par.df, file = paste0(wd.rsam,"/opt/opt_result_",sim.id,".csv"),
          row.names = F)

#--- Scatter plot
axis.lim = c(min(opt.sim.res$sim.obs$obs,opt.sim.res$sim.obs$sim) * (1 - 0.1),
  max(opt.sim.res$sim.obs$obs,opt.sim.res$sim.obs$sim) * (1 + 0.1))
gg.opt.sct = ggplot(opt.sim.res$sim.obs, aes(x = obs, y = sim, fill = var)) + 
  ylab("Simulated")+xlab("Observed")+
  geom_point(size = 2,
             shape = 21,
             colour = "black") + 
  geom_abline(slope = 1,
              intercept = 0,
              colour = "red",
              linetype = 2) + 
  geom_smooth(method='lm') + 
  scale_x_continuous(limits=axis.lim) +
  scale_y_continuous(limits=axis.lim) +
  theme_bw() + facet_wrap(.~var, scales = "free")

#--- time-course
tc.df = melt(opt.sim.res$sim.obs, id.vars = c("year","doy","das","dap","var"))

gg.opt.tc = ggplot(tc.df, aes(x = das, y = as.numeric(value), fill = var, colour = var)) + 
  geom_line(data = tc.df[tc.df$variable == "sim",]) + 
  geom_point(data = tc.df[tc.df$variable == "obs",],
             shape = 21,
             colour = "black",
             size =2) + 
  scale_y_continuous(limits=axis.lim) + 
  ylab("Variables") + xlab("Time-Course") + 
  theme_bw() + facet_wrap(.~var, scales = "free")


#--- Exploring results
get.obj.fun = F

#--- running with different set of parameters to force water stress
opt.sim.res = opt.swap.samuca(rep(0.5,length(p.df$find)))

#--- Scatter plot
axis.lim = c(min(opt.sim.res$sim.obs$obs,opt.sim.res$sim.obs$sim) * (1 - 0.1),
             max(opt.sim.res$sim.obs$obs,opt.sim.res$sim.obs$sim) * (1 + 0.1))
gg.opt.sct = ggplot(opt.sim.res$sim.obs, aes(x = obs, y = sim, fill = var)) + 
  ylab("Simulated")+xlab("Observed")+
  geom_point(size = 2,
             shape = 21,
             colour = "black") + 
  geom_abline(slope = 1,
              intercept = 0,
              colour = "red",
              linetype = 2) + 
  geom_smooth(method='lm') + 
  scale_x_continuous(limits=axis.lim) +
  scale_y_continuous(limits=axis.lim) +
  theme_bw() + facet_wrap(.~var, scales = "free")

gg.opt.sct

#--- time-course
tc.df = melt(opt.sim.res$sim.obs, id.vars = c("year","doy","das","dap","var","sub_var"))

#--- convert sub_var to unique var
tc.df$var[!is.na(tc.df$sub_var)] = paste0(tc.df$var[!is.na(tc.df$sub_var)],"_",tc.df$sub_var[!is.na(tc.df$sub_var)])

gg.opt.tc = ggplot(tc.df, aes(x = das, y = as.numeric(value), fill = var, colour = var)) + 
  geom_line(data = tc.df[tc.df$variable == "sim",]) + 
  geom_point(data = tc.df[tc.df$variable == "obs",],
             shape = 21,
             colour = "black",
             size =2) + 
  ylab("Variables") + xlab("Time-Course") + 
  theme_bw() + facet_wrap(var~., scales = "free",
                          nrow = 3)

gg.opt.tc

#--- pressure head
h.df = opt.sim.res$sim.results$soil
h.df = h.df[h.df$depth %in% c(-5,-15,-30,-60),]

ggplot(h.df, aes(y = phead, x = das)) + 
  geom_line() + facet_wrap(.~depth, scales = "free") + 
  scale_y_continuous() + theme_bw()

#--- transpiration
et.df = opt.sim.res$sim.results$incr
et.df = melt(et.df[,c("das","Tact","Tpot")], id.vars = c("das"))

ggplot(et.df, aes(x = das, y = value, colour = variable)) + 
  geom_line() + 
  theme_bw()

#--- Stress
str.red.df = opt.sim.res$sim.results$stre
str.red.df = melt(str.red.df, id.vars = c("date","year","doy","das"))

str.v = c("Tredwet","Treddry","Tredsol","Tredfrs")

ggplot(str.red.df[str.red.df$variable %in% str.v,], aes(x = das, y = value, colour = variable)) + 
  geom_line() + 
  theme_bw() + facet_wrap(.~variable)
