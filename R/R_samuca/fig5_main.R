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
#--- Figure 4 ---#
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

#--- Color-Blind Friendly palette
cbPalette       = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(cbPalette)= c(   "ATAB",    "COLI",    "OLIM",    "CLER",    "UNIR",    "UNII",   "UNII2")
leg.name.df     = data.frame(M_ID = c(   9,   11,   15,   20,   21,   22,   23),
                             leg  = c(   "ATAB",    "COLI",    "OLIM",    "CLER",    "UNIR",    "UNII",   "UNII2"))
col.treat       = cbPalette

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
plan.valid.out = read.csv(paste0(wd.repo,"/R/R_samuca/results_perf/Perf_Data_Plan_validation_25_jul.csv"), as.is = T)

#--- Variables to be used
plan.var       = c("stalk_fw","stalk_dw","suc_stk_fresh","lai_dev_gleaf","till","stalk_h")
col.ret        = c("year","doy","das","dap","val","type","seq","Field_ID","seq_ID","M_ID")
var.name.v     = plan.var

var.names.df   = data.frame(var.name  = var.name.v,
                            label.var = sim.leg.index$label_var[match(var.name.v,sim.leg.index$obs_db_code)],
                            units.var = sim.leg.index$units[match(var.name.v,sim.leg.index$obs_db_code)],
                            var.name.type = "plan", stringsAsFactors = F)

#--- bind all variables for ggplot
gg.df = plan.valid.out[plan.valid.out$meas_ID %in% plan.var,c(col.ret,"meas_ID")]

#--- Remove ID = 0
gg.df = gg.df[gg.df$M_ID != 0,]

#--- Add treatment names
gg.df$treatment = leg.name.df$leg[match(gg.df$M_ID,leg.name.df$M_ID)]

#--- Add var names
gg.df$var.labels = paste0(var.names.df$label.var[match(gg.df$meas_ID,var.names.df$var.name)],
                          " (",
                          var.names.df$units.var[match(gg.df$meas_ID,var.names.df$var.name)],
                          ")")

#--- Order facets
gg.df$var.labels_f = factor(gg.df$var.labels, levels = c("Dry Cane (ton[DW]/ha)","Fresh Cane (ton[FW]/ha)","POL (%[stalk fresh])","LAI (m2/m2)","Tiller Population (#/m2)","Stalk Height (m)"))

#--- create ggplot base
gg.fig = ggplot(gg.df, aes(y = val, x = dap, fill = treatment, colour = treatment, group = var.labels_f))

#--- Add fixed colors and axis setups
gg.fig = gg.fig + 
  scale_color_manual(values = col.treat) + 
  scale_fill_manual(values  = col.treat)

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
  
  gg.fig = gg.fig + geom_line(data = gg.df.i[gg.df.i$type == "sim",], size = l.size)
  
  x.end = max(gg.df.i[,"dap"])
  x.ini = min(gg.df.i[,"dap"])
  
}

#--- Add limits
# ylimits = data.frame(dap = c(10,10,
#                              10,10,
#                              10,10,
#                              10,10),
#                      val = c(0.24,0.36,
#                              0,8,
#                              0,1000,
#                              19,43),
#                      var.labels_f = factor(c(rep("(cm3/cm3)",2),rep("(mm/day)",2),rep("(mm)",2),rep("(°C)",2)),levels = c("(cm3/cm3)","(mm/day)","(mm)","(°C)")),
#                      treatment = "WM")
# 
# gg.fig = gg.fig + geom_point(data = ylimits, 
#                                size = 0,
#                                alpha = 0)

#--- Add observations
gg.fig = gg.fig + geom_point(data = gg.df[gg.df$type == "obs",], 
                             size = p.size,
                             color= "black",
                             shape= 21,
                             stroke= 0.25) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = NULL,
        legend.position=c(0.85,-0.09),
        legend.direction = "horizontal",
        legend.text  = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.line    = element_line(colour = "black", size = 0.2),
        axis.text    = element_text(size=14, colour = "black"),
        axis.title   = element_text(size=14),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
        plot.margin = unit(c(0.5,0.5,1.0,0.5), "cm"),
        legend.box.background = element_rect(colour = "black"))


gg.fig$labels$x = "Days After Planting (DAP)"
gg.fig$labels$y = NULL
gg.fig$labels$fill = NULL
gg.fig$labels$colour = NULL


#--- Wrap to faceted gg
gg.fig = gg.fig + facet_wrap(~ var.labels_f, scales = "free", ncol = 2) + 
  theme(
    strip.background = element_rect(color="black", fill="lightgrey", size=0.5, linetype="solid"),
    strip.text.x     = element_text(size = 14),
    strip.text.y     = element_text(size = 18),
    strip.placement = "left",
    strip.background.y = element_blank()
  )

ggsave(paste0(wd.repo,"/R/R_samuca/results_perf/samuca_paper/paper_figures/fig5_main.png"), 
       plot = gg.fig, width = 8, height = 8,
       dpi = gg.dpi)

