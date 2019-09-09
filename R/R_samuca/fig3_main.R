#--------------------------#
#--- SAMUCA PAPER PLOTS ---#
#--------------------------#

if(!exists("laptop")){
  laptop      = T
}

if(laptop){
  
  wd.core     = "C:\\Murilo\\samuca\\R\\R_samuca"
  wd.model    = "C:\\Murilo\\samuca\\model"
  wd.repo     = "C:/Murilo/samuca"
  debug.path  = "C:\\Murilo\\samuca\\samuca_vs_proj\\Debug"
  
}else{
  
  wd.core     = "D:\\Murilo\\samuca\\samuca\\R\\R_samuca"
  wd.model    = "D:\\Murilo\\samuca\\samuca\\model"
  wd.repo = "D:/Murilo/samuca/samuca"
  debug.path  = "D:\\Murilo\\samuca\\samuca\\samuca_vs_proj\\Debug"
}

wd.out = paste0(wd.core,"\\results_perf\\samuca_paper")

#--- Load Source Files (~/bin) 
invisible(sapply(list.files(path = paste0(wd.core,"\\lib\\"),full.names = T),
                 function(x) source(x)))
library(ggpubr)

#----------------#
#--- Figure 3 ---#
#----------------#

l.id        = c("pira_calibration_WM_NM")
m.fn        = c("meta_sugar_db.csv")
nl          = 8 
n.run       = 1
plot.screen = T
use.debug   = T
samuca.exe  = "samuca_vs_proj.exe"
scatter.plot= T
is.seq      = T

#--- ggplot options
p.size          = 2.5
l.size          = 0.4
col.treat       = c("#FF0000", "#04B404")
names(col.treat)= c(     "NM",      "WM")
leg.name.df     = data.frame(M_ID = c(   1,    3,    4,    5,    6,    7,    8),
                             leg  = c("NM", "WM", "NM", "WM", "NM", "WM", "NM"))
gg.dpi          = 500
gg.h            = 5
gg.w            = 20
x.breaks        = seq(0,1400, by = 200)
t = theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.background = NULL,
          legend.position="none",
          axis.line    = element_line(colour = "black", size = 0.2),
          axis.text    = element_text(size=24, colour = "black"),
          axis.title   = element_text(size=26),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))

#--- Read metafile
meta = read.csv(paste0(wd.repo,"/R/R_samuca/",m.fn[1]), as.is = T)

#--- Read var.names indexes
sim.leg.index     = read.csv(paste0(wd.repo,"/R/R_samuca/sim_obs_index_final_plots.csv"), as.is = T)

#--- Read calibration outputs
temp.calib.out = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/Perf_Data_Temp_pira_calibration_WM_NM.csv"), as.is = T)
soil.calib.out = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/Perf_Data_Soil_pira_calibration_WM_NM.csv"), as.is = T)
atmo.calib.out = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/Perf_Data_Atmo_pira_calibration_WM_NM.csv"), as.is = T)
plan.calib.out = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/Perf_Data_Plan_pira_calibration_WM_NM.csv"), as.is = T)

#--- Variables to be used
soil.var       = "swc2"           # Top soil moisture (10 cm)
temp.var       = "Ts.1"           # Top soil temperature (10 cm)
atmo.var       = c("et","et_acc") # Evapotranspiration daily + totals
col.ret        = c("year","doy","das","dap","val","type","seq","Field_ID","seq_ID","M_ID")
var.name.v     = c(soil.var, temp.var,atmo.var)

var.names.df   = data.frame(var.name  = var.name.v,
                            label.var = sim.leg.index$label_var[match(var.name.v,sim.leg.index$samuca_sim_Rcode)],
                            units.var = sim.leg.index$units[match(var.name.v,sim.leg.index$samuca_sim_Rcode)],
                            var.name.type = c("swc","stemp","et","et_acc"), stringsAsFactors = F)

#--- bind all variables for ggplot
gg.df = rbind(temp.calib.out[temp.calib.out$meas_ID1 %in% temp.var,c(col.ret,"meas_ID")],
              atmo.calib.out[atmo.calib.out$meas_ID  %in% atmo.var,c(col.ret,"meas_ID")],
              soil.calib.out[soil.calib.out$meas_ID1 %in% soil.var,c(col.ret,"meas_ID")])

#--- Remove ID = 0
gg.df = gg.df[gg.df$M_ID != 0,]

#--- Add treatment names
gg.df$treatment = leg.name.df$leg[match(gg.df$M_ID,leg.name.df$M_ID)]

#--- Add var names
gg.df$var.labels = paste0(
                          "(",
                          var.names.df$units.var[match(gg.df$meas_ID,var.names.df$var.name.type)],
                          ")")

#--- Order facets
gg.df$var.labels_f = factor(gg.df$var.labels, levels = c("(cm3/cm3)","(mm/day)","(mm)","(°C)"))

#--- create ggplot base
gg.fig3 = ggplot(gg.df, aes(y = val, x = das, fill = treatment, colour = treatment))

xlim.fig = c(0,1350)
x.breaks = seq(0,1350, by = 120)

#--- Add fixed colors and axis setups
gg.fig3 = gg.fig3 + 
  scale_color_manual(values = col.treat) + 
  scale_fill_manual(values  = col.treat) + 
  scale_x_continuous(limits = xlim.fig, breaks = x.breaks, expand = c(0.01,0.01))

#--- Add sequential runs simulations
#--- This loop is necessary to avoid steep changes in seasons transitions
l.id = unique(gg.df$M_ID)
for(i in l.id){
  
  p.yr   = meta$planting_yr[meta$M_ID == i]
  p.doy  = meta$planting_doy[meta$M_ID == i]
  
  h.yr  = meta$harvesting_yr[meta$M_ID == i]  
  h.doy = meta$harvesting_doy[meta$M_ID == i]
  
  gg.df.i = gg.df[gg.df$M_ID == i,]
  
  gg.df.i = gg.df.i[gg.df.i$year >= p.yr & gg.df.i$year <= h.yr,]
  gg.df.i = gg.df.i[!(gg.df.i$year == h.yr & gg.df.i$doy >=  h.doy) ,]
  
  gg.fig3 = gg.fig3 + geom_line(data = gg.df.i[gg.df.i$type == "sim",], size = l.size)
  
  x.end = max(gg.df.i[,"das"])
  x.ini = min(gg.df.i[,"das"])
  
  if(is.seq){
    #--- include vertical lines to mark harvest
    gg.fig3 = gg.fig3 + geom_vline(xintercept = x.end, color = "grey", linetype = "dotted")
    if(i == l.id[1]){gg.fig3 = gg.fig3 + geom_vline(xintercept = x.ini, color = "grey", linetype = "dotted")}
    
  }
}

#--- Add limits
ylimits = data.frame(das = c(10,10,
                             10,10,
                             10,10,
                             10,10),
                     val = c(0.24,0.36,
                             0,8,
                             0,1000,
                             19,43),
                     var.labels_f = factor(c(rep("(cm3/cm3)",2),rep("(mm/day)",2),rep("(mm)",2),rep("(°C)",2)),levels = c("(cm3/cm3)","(mm/day)","(mm)","(°C)")),
                     treatment = "WM")

gg.fig3 = gg.fig3 + geom_point(data = ylimits, 
                               size = 0,
                               alpha = 0)

#--- Add observations
gg.fig3 = gg.fig3 + geom_point(data = gg.df[gg.df$type == "obs",], 
                             size = p.size,
                             color= "black",
                             shape= 21,
                             stroke= 0.25) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = NULL,
        legend.position="none",
        legend.direction = "horizontal",
        axis.line    = element_line(colour = "black", size = 0.2),
        axis.text    = element_text(size=18, colour = "black"),
        axis.title   = element_text(size=18),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.box.background = element_rect(colour = "black"))

#--- Add text annotations
das.offset = 5
seasons.annotate = data.frame(das = c(10 + das.offset,374 + das.offset,647 + das.offset,975 + das.offset),
                              val = 17,
                              lab = as.character(c("Plant-Cane","1st Ratoon","2nd Ratoon", "3rd Ratoon")),
                              var.labels_f = factor("(°C)",levels = c("(cm3/cm3)","(mm/day)","(mm)","(°C)")),
                              treatment = "WM")






gg.fig3 = gg.fig3 + geom_text(data = seasons.annotate, 
                    aes(y = val, x = das, label = lab), 
                    colour = "black",
                    size   = 2.5,
                    hjust  = 0) 

gg.fig3$labels$x = "Days After Simulation Started (DAS)"
gg.fig3$labels$y = NULL

#--- Wrap to faceted gg
gg.fig3 = gg.fig3 + facet_grid(var.labels_f~., scales = "free", switch="y") + 
  theme(
    strip.background = element_rect(color="black", fill="grey", size=0.5, linetype="solid"),
    strip.text.x     = element_text(size = 14),
    strip.text.y     = element_text(size = 18),
    strip.placement = "left",
    strip.background.y = element_blank()
  )

ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/paper_figures/fig3_main.png"), 
       plot = gg.fig3, width = 13, height = 10,
       dpi = gg.dpi)




