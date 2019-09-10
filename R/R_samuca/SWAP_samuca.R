#--------------------------#
#--- Plot R_SAMUCA_Perf ---#
#--------------------------#

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
wd.lib    = paste0(wd.repo,"/R/R/samuca/lib")
wd.model  = paste0(wd.repo,"/model")

#--- Load Source Files (~/bin) 
invisible(sapply(list.files(path = wd.lib,full.names = T),
                 function(x) source(x)))

plan.out.fn = paste0(wd.model,"/Plant_SWAP-SAMUCA_PIRA.out")
atmo.out.fn = paste0(wd.model,"/result.wba")
soil.out.fn = paste0(wd.model,"/result.vap")
stre.out.fn = paste0(wd.model,"/result.str")
incr.out.fn = paste0(wd.model,"/result.inc")

plan.out = read.plan.SWAP.out(plan.out.fn)
atmo.out = read.atmo.SWAP.out(atmo.out.fn)
soil.out = read.soil.SWAP.out(soil.out.fn)
stre.out = read.stre.SWAP.out(stre.out.fn)
incr.out = read.incr.SWAP.out(incr.out.fn)

incr.out$et.pot = incr.out$Tpot + incr.out$Epot
incr.out$et.act = incr.out$Tact + incr.out$Eact

library(ggplot2)


ggplot(soil.out[soil.out$depth %in% c(-10,-16.5,-31.5),], aes(x = das, y = wcontent, colour = as.factor(depth))) + 
  geom_line() + theme_classic()

ggplot(soil.out[soil.out$depth %in% c(-10,-16.5,-31.5),], aes(x = das, y = temp, colour = as.factor(depth))) + 
  geom_line() + theme_classic()

ggplot(atmo.out, aes(x = das, y = Tpot)) + 
  geom_line() + theme_classic()

ggplot(incr.out, aes(x = das, y = Eact)) + 
  geom_line() + theme_classic()

ggplot(incr.out, aes(x = das, y = et.pot)) + 
  geom_line() + theme_classic()

ggplot(incr.out, aes(x = das, y = et.act)) + 
  geom_line() + theme_classic()


fn = paste0(wd.model, "/result.inc")


#------------------#
#--- Validation ---#
#------------------#
l.id  = c("validation_25_jul")
m.fn  = c("meta_sugar_db_validation.csv")
nl = 8
n.run = 1
nl          = 8 
n.run       = 1
plot.screen = T
use.debug   = T
samuca.exe  = "samuca_vs_proj.exe"
x.breaks = seq(0,480, by = 60)

#--- ggplot options
p.size          = 5
l.size          = 0.5

#--- Color-Blind Friendly palette
cbPalette       = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(cbPalette)= c(   "ATAB",    "COLI",    "OLIM",    "CLER",    "UNIR",    "UNII",   "UNII2")
leg.name.df     = data.frame(M_ID = c(   9,   11,   15,   20,   21,   22,   23),
                             leg  = c(   "ATAB",    "COLI",    "OLIM",    "CLER",    "UNIR",    "UNII",   "UNII2"))

col.treat       = cbPalette
gg.dpi          = 500
gg.h            = 5
gg.w            = 20
x.breaks        = seq(0,480, by = 60)
t = theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.background = NULL,
          #legend.position="none",
          axis.line    = element_line(colour = "black", size = 0.2),
          axis.text    = element_text(size=24, colour = "black"),
          axis.title   = element_text(size=26),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))

for(r in 1:n.run){
  
  #--- SAMUCA R perf
  run.sam.perf = r.samuca.perf.fun(l.id[r],
                                   wd.core,
                                   wd.model,
                                   use.debug,
                                   debug.path,
                                   samuca.exe,
                                   m.fn[r])
  
  #--- Plot plant variables
  gg.das.crop = gg.das(run.sam.perf$perf.dt.plan,
                       run.sam.perf$sim.obs.idx,
                       run.sam.perf$meta,
                       plot.screen,
                       p.size,
                       l.size,
                       col.treat,
                       gg.dpi,
                       20,
                       20,
                       wd.out,
                       l.id[r],
                       T,
                       2,
                       3,
                       F,
                       x.breaks,
                       t)
  
  gg.scatter.crop = gg.scatter(run.sam.perf$perf.dt.plan,
                               run.sam.perf$sim.obs.idx,
                               run.sam.perf$meta,
                               plot.screen,
                               6,
                               l.size,
                               col.treat,
                               gg.dpi,
                               20,
                               20,
                               wd.out,
                               l.id[r],
                               T,
                               2,
                               3,
                               t,
                               T,
                               data.frame(l.sl  = c("swc2","swc3","swc5"),
                                          sl.lab= c("SWC","SWC","SWC")))
  
  
}

