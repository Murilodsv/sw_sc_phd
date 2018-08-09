#--- SWAP-Samuca Assessment and Calibration
#--- This script is intended to:
#--- 1) Read and build-up charts of SWAP-Samuca model for assessment and calibration
#--- 2) Be used as function to automate calibration procedure as described in my Murilo Vianna phd Thesis (2017) 

#--- Packages required
pkg = c("lubridate",
        "plyr",
        "scales")

#--- Installing missing packages
ipkg = pkg %in% rownames(installed.packages())
sapply(pkg[!ipkg],function(x) install.packages(x))

library(lubridate) #--- compute doy from date
library(plyr)
library(scales)

#--- parameters

wd          = "D:/Murilo/samuca/swap/sw_sc"
run_model   = T
model_fn    = "swap_samuca_v1.exe"
swap_prj    = "SWAP-SAMUCA_PIRA"
soil_depth  = c(-10,-19.5,-28.5,-58.5) # simulated soil depth compartments to retrive data (see on swap.swp)

#--- Read Measured Data
#--- Note data must have "year" and "doy" collumns!!!!!!!
fdr = read.csv(file = paste0(wd,"/data/SOIL_FDR_SP_DATA.csv"))
et  = read.csv(file = paste0(wd,"/data/bowen.csv"))
bio = read.csv(file = paste0(wd,"/data/biometrics.csv"))



#------------------------- start running ---------------
#--- Outputs Directory
setwd(wd)

#--- load functions
source(paste0(wd,"/R/swap_samuca_f.R"))

#--- Read model parameters
par = read.table(file = "Samuca.par",skip = 4)
colnames(par) = c("value","parname","type","class")

#--- run swap-samuca
if(run_model){
  #--- make sure to use the last compiled version
  file.copy(from = paste0(wd,"/Debug/",model_fn), to = wd, overwrite = T)
  
  #--- Run SWAP_SAMUCA
  system(model_fn)
}

#--- Read Outputs
#--- Crop Default
plant_lines = readLines(paste0("Plant_",swap_prj,".OUT"))       #Read files lines
plant_numlines = plant_lines[substr(plant_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
plant = read.table(text = plant_numlines)                   #Read numeric lines as data.frame
colnames(plant) = c("year","doy","das","dap","gdd","dw","reserv","rw","lw","tp","sw","sucw","fibw","tch","pol","adryw","lai","till","h","devgl","itn","swface","swfacp","rtpf","lfpf","skpf","tppf","ctype","status","stage")

#--- Detailed Internodes
detint_lines = readLines(paste0("DetIntePr_",swap_prj,".OUT"))       #Read files lines
detint_numlines = detint_lines[substr(detint_lines,1,1)=="2"]    #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detint = read.table(text = detint_numlines)                      #Read numeric lines as data.frame
colnames(detint) = c("year","doy","das","dap","diac","itn",paste(rep("itlen",35),1:35),paste(rep("av_itlen",35),1:35),paste(rep("itsuc",35),1:35),paste(rep("av_itsuc",35),1:35),paste(rep("ittdw",35),1:35),paste(rep("av_ittdw",35),1:35))

#--- Detailed Leaves
detleaf_lines = readLines(paste0("DetLeafPr_",swap_prj,".OUT"))   #Read files lines
detleaf_numlines = detleaf_lines[substr(detleaf_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detleaf = read.table(text = detleaf_numlines)                     #Read numeric lines as data.frame
colnames(detleaf) = c("year","doy","das","dap","diac","ngl","ndevgl",paste(rep("lfarea",11),1:11),paste(rep("av_lfarea",11),1:11),paste(rep("lfweight",11),1:11),paste(rep("av_lfweight",11),1:11))

#--- Detailed Stress Factors
detsfac_lines = readLines(paste0("DetPGFac_",swap_prj,".OUT"))    #Read files lines
detsfac_numlines = detsfac_lines[substr(detsfac_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detsfac = read.table(text = detsfac_numlines)                     #Read numeric lines as data.frame
colnames(detsfac) = c("year","doy","das","dap","diac","par","extcoef","lai","li","co2","rue","co2_fac","tstress","agefactor","swfacp","RGP_fac","pg","dRGP_pg","dw","RGP_pg","IPAR_acc","w","wa","w+wdead","wa+wdead","arue_dw","arue_w","arue_dwa","arue_wa","carbcheck")

#--- Detailed RootSystem
detroot_lines = readLines(paste0("DetRootSy_",swap_prj,".OUT"))   #Read files lines
detroot_numlines = detroot_lines[substr(detroot_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detroot = read.table(text = detroot_numlines)                     #Read numeric lines as data.frame
colnames(detroot) = c("year","doy","das","dap","diac","wr","rd","rootsene",paste(rep("rld",45),1:45),"tqropot","ptra")

#--- Soil Water
swba = read.csv(file = "result.vap", skip = 11)

#--- Soil Water Reduction
wstr = read.csv(file = "result.str", skip = 6)

#--- Atmospheric
atm  = read.csv(file = "result.inc", skip = 6)

#--- Indexer: use year_doy as indexer for das from plant
indexc = data.frame(plant$das,plant$year,plant$doy)
colnames(indexc) = c("das","year","doy")

#--- Include das in all db

#--- Measured data
fdr = inx(fdr)    #FDR
et  = inx(et)     #ET
bio = inx(bio)    #Biometrics

#--- Simulated data
#--- Derive "year" and "doy" for atm, swba, wstr
atm$year = as.factor(format(as.Date(atm$Date, format="%d-%b-%Y"),"%Y"))
atm$doy  = as.factor(yday(as.Date(atm$Date, format="%d-%b-%Y")))

swba$year = as.factor(format(as.Date(swba$date, format="%d-%b-%Y"),"%Y"))
swba$doy  = as.factor(yday(as.Date(swba$date, format="%d-%b-%Y")))

wstr$year = as.factor(format(as.Date(wstr$Date, format="%d-%b-%Y"),"%Y"))
wstr$doy  = as.factor(yday(as.Date(wstr$Date, format="%d-%b-%Y")))

#--- Include das in all simulated
atm       = inx(atm)    #Atmosphere
swba      = inx(swba)   #Soil Water Balance
wstr      = inx(wstr)   #Water stresses

#==============================================

#-------------------------------
#------- Data Analysis ---------
#-------- PERFORMANCE ----------
#-------------------------------

#-------------------------------
#----- Soil Water Content ------
#-------------------------------

#--- seting the simulated depths as equal to FDR depths measurements (10, 20, 30, 60)
dsim = data.frame(fdr = colnames(fdr)[4:7], depth = soil_depth)
l = merge(swba,dsim,by = "depth")
l = l[order(l$das),]              #--- sort by das

sim_swc10  = l[l$fdr==colnames(fdr)[4],c("das","wcontent")]
sim_swc20  = l[l$fdr==colnames(fdr)[5],c("das","wcontent")]
sim_swc30  = l[l$fdr==colnames(fdr)[6],c("das","wcontent")]
sim_swc60  = l[l$fdr==colnames(fdr)[7],c("das","wcontent")]
sim_swc    = data.frame(das = sim_swc10$das,
                        sim_swc10 = sim_swc10$wcontent, 
                        sim_swc20 = sim_swc20$wcontent, 
                        sim_swc30 = sim_swc30$wcontent, 
                        sim_swc60 = sim_swc60$wcontent)

so_fdr = merge(fdr,sim_swc, by = "das")

s_swc10  = so_fdr$sim_swc10
s_swc20  = so_fdr$sim_swc20
s_swc30  = so_fdr$sim_swc30
s_swc60  = so_fdr$sim_swc60

o_swc10  = so_fdr$fdr10cm
o_swc20  = so_fdr$fdr20cm
o_swc30  = so_fdr$fdr30cm
o_swc60  = so_fdr$fdr60cm

s_swc = c(s_swc10,s_swc20,s_swc30,s_swc60)
o_swc = c(o_swc10,o_swc20,o_swc30,o_swc60)

png("p_swc_d.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_swc10  = mperf(s_swc10,o_swc10,"SWC at 10cm (cm3 cm-3)")
p_swc20  = mperf(s_swc20,o_swc20,"SWC at 20cm (cm3 cm-3)")
p_swc30  = mperf(s_swc30,o_swc30,"SWC at 30cm (cm3 cm-3)")
p_swc60  = mperf(s_swc60,o_swc60,"SWC at 60cm (cm3 cm-3)")

dev.off() # end of chart exportation

png("p_swc.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_swc    = mperf(s_swc,o_swc,"SWC (cm3 cm-3)")
dev.off() # end of chart exportation

#-------------------
#--- Atmosphere ----
#-------------------

et_obs = data.frame(das = et$das[et$type=="ET" & et$treat=="WithoutStraw"], et = et$et[et$type=="ET" & et$treat=="WithoutStraw"])
so_atm = merge(et_obs,atm,by = "das")
s_et = (so_atm$Tact + so_atm$Eact) * 10
o_et = so_atm$et

png("p_atm.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_atm  = mperf(s_et,o_et,"ET (mm d-1)")
dev.off() # end of chart exportation

#------------------
#--- Biometrics ---
#------------------

o_sfm  = bio[!is.na(bio$SFM) & bio$type == "AVG",c("das","SFM")]          # Stalk fresh mass
o_sdm  = bio[!is.na(bio$SDM) & bio$type == "AVG",c("das","SDM")]          # Stalk dry mass
o_lai  = bio[!is.na(bio$LAIGD) & bio$type == "AVG",c("das","LAIGD")]      # Green Leaf Area Index
o_til  = bio[!is.na(bio$T.AD) & bio$type == "AVG",c("das","T.AD")]        # Tillering
o_pol  = bio[!is.na(bio$SU.FMD) & bio$type == "AVG",c("das","SU.FMD")]    # POL %
o_dgl  = bio[!is.na(bio$N.GL) & bio$type == "AVG",c("das","N.GL")]        # Number of green leaves
o_sth  = bio[!is.na(bio$SHTD) & bio$type == "AVG",c("das","SHTD")]        # Stalks height


so_sfm  = merge(o_sfm,plant[,c("das","tch")]  , by = "das")
so_sdm  = merge(o_sdm,plant[,c("das","sw")]   , by = "das")
so_lai  = merge(o_lai,plant[,c("das","lai")]  , by = "das")
so_til  = merge(o_til,plant[,c("das","till")] , by = "das")
so_pol  = merge(o_pol,plant[,c("das","pol")]  , by = "das")
so_dgl  = merge(o_dgl,plant[,c("das","devgl")], by = "das")
so_sth  = merge(o_sth,plant[,c("das","h")]    , by = "das")

png("p_sfm.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_sfm = mperf(so_sfm$tch,o_sfm$SFM, "SFM (t ha-1)")
dev.off() # end of chart exportation

png("p_sdm.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_sdm = mperf(so_sdm$sw,o_sdm$SDM, "SDM (t ha-1)")
dev.off() # end of chart exportation

png("p_lai.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_lai = mperf(so_lai$lai,o_lai$LAIGD, "LAI (m2 m-2)")
dev.off() # end of chart exportation

png("p_till.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_til = mperf(so_til$till,o_til$T.AD, "Tiller (till m-2)")
dev.off() # end of chart exportation

png("p_pol.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_pol = mperf(so_pol$pol,o_pol$SU.FMD, "POL (%)")
dev.off() # end of chart exportation

png("p_dgl.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_dgl = mperf(so_dgl$devgl,o_dgl$N.GL, "N° dev GL per stalk")
dev.off() # end of chart exportation

png("p_sth.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_sth = mperf(so_sth$h,o_sth$SHTD, "Height (m)")
dev.off() # end of chart exportation

#--- Pooling all results in p_all
p_all = rbind(p_swc10,
              p_swc20,
              p_swc30,
              p_swc60,
              p_swc,
              p_atm,
              p_sfm,
              p_sdm,
              p_lai,
              p_til,
              p_pol,
              p_dgl,
              p_sth)

p_all$model = c("p_swc10",
                "p_swc20",
                "p_swc30",
                "p_swc60",
                "p_swc",
                "p_atm",
                "p_sfm",
                "p_sdm",
                "p_lai",
                "p_til",
                "p_pol",
                "p_dgl",
                "p_sth")

#--- Write performance
write.csv(p_all, file = "Model_performance.csv")

#==============================================

#==============================================
#--------------------------#
#-------- Charts ----------#
#--- Time course charts ---#
#--------------------------#

#--- seting the simulated depths as equal to FDR depths measurements (10, 20, 30, 60)
dsim = data.frame(fdr = colnames(fdr)[4:7], depth = c(-10,-19.5,-28.5,-58.5))

#--- Separating data for lines
l = merge(swba,dsim,by = "depth")
l = l[order(l$das),]#--- sort by das

png("so_swc.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(4,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
s = sapply(colnames(fdr)[4:7], fdrpl)
dev.off() # end of chart exportation

#--- Atmosphere
atm$etact = (atm$Tact + atm$Eact) * 10
atm$etpot = (atm$Tpot + atm$Epot) * 10

#--- temporal distribution
dt = 30 #time pack 

#--- classify das by dt
br = seq(min(atm$das),max(atm$das), by = dt)
lb = seq(min(atm$das) + 0.5*dt ,max(atm$das), by = dt)
if(length(br)==length(lb)){lb = lb[1:length(br)-1]}

atm$das_c = cut(atm$das,breaks = br, labels = lb, right = F)

png("so_atm.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- compute 1st and 3 quartiles
z = boxplot(atm$etact~atm$das_c)
bot = data.frame(das = lb, et = z$stats[2,])
top = data.frame(das = lb, et = z$stats[4,])

#--- plot obs et
plot(et$et[et$type == "ET" & et$treat == "WithoutStraw"]~et$das[et$type == "ET" & et$treat == "WithoutStraw"],
     ylab = expression("ET (mm " ~ d^{-1} ~ ")"),
     xlab = "DAS",
     ylim = c(0,10),
     xlim = c(min(atm$das), max(atm$das)) )

#--- plot sim et
lines(atm$etact~atm$das, col=alpha(rgb(1,0,0), 0.7))

#--- plot temporal distribution
lines(bot$et~bot$das,
      lty = 2,
      lwd = 1.5)
lines(top$et~top$das,
      lty = 2,
      lwd = 1.5)
points(et$et[et$type == "ET" & et$treat == "WithoutStraw"]~et$das[et$type == "ET" & et$treat == "WithoutStraw"])

#--- Add days of planting
pdays = plant$das[plant$dap==1]

#plant cane
lines(c(-1,11)~c(pdays[1],pdays[1]),
      col=alpha(rgb(0,0,0), 0.5),
      lty= 2)

#1st ratoon
lines(c(-1,11)~c(pdays[2],pdays[2]),
      col=alpha(rgb(0,0,0), 0.5),
      lty= 2)
#2nd ratoon
lines(c(-1,11)~c(pdays[3],pdays[3]),
      col=alpha(rgb(0,0,0), 0.5),
      lty= 2)
#3rd ratoon
lines(c(-1,11)~c(pdays[4],pdays[4]),
      col=alpha(rgb(0,0,0), 0.5),
      lty= 2)

dev.off()
#--- Biometrics

#--- Observed data (only average = "AVG") - Include boxplots in a nearfuture
o_sfm  = bio[!is.na(bio$SFM)    & bio$type == "AVG",c("das","SFM")]
o_sdm  = bio[!is.na(bio$SDM)    & bio$type == "AVG",c("das","SDM")]
o_lai  = bio[!is.na(bio$LAIGD)  & bio$type == "AVG",c("das","LAIGD")]
o_til  = bio[!is.na(bio$T.AD)   & bio$type == "AVG",c("das","T.AD")]
o_pol  = bio[!is.na(bio$SU.FMD) & bio$type == "AVG",c("das","SU.FMD")]
o_dgl  = bio[!is.na(bio$N.GL)   & bio$type == "AVG",c("das","N.GL")]
o_sth  = bio[!is.na(bio$SHTD)   & bio$type == "AVG",c("das","SHTD")]

o_bio  = c("SFM","SDM","LAIGD","T.AD","SU.FMD","N.GL" ,"SHTD")
s_bio  = c("tch","sw" ,"lai"  ,"till","pol"   ,"devgl","h")


png("so_bio.png",units="in",width=12,height=12,pointsize=15,res=300)

par(mfrow=c(7,1), 
    mar = c(0., 0.5, 0., 0.5), 
    oma = c(3, 3, 0.5, 0),
    mgp = c(2, 1, 0),
    xpd = NA)

#--- ploting SFM
pl_bio(data.frame(das = o_sfm[,"das"],dat = o_sfm[,o_bio[1]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[1]] , ctype = plant[,"ctype"]),
       "SFM (t ha-1)",FALSE)

#--- ploting SDM
pl_bio(data.frame(das = o_sdm[,"das"],dat = o_sdm[,o_bio[2]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[2]] , ctype = plant[,"ctype"]),
       "SDM (t ha-1)",FALSE)

#--- ploting LAI
pl_bio(data.frame(das = o_lai[,"das"],dat = o_lai[,o_bio[3]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[3]] , ctype = plant[,"ctype"]),
       "LAI (m2 m-2)",FALSE)

#--- ploting Tillering
pl_bio(data.frame(das = o_til[,"das"],dat = o_til[,o_bio[4]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[4]] , ctype = plant[,"ctype"]),
       "Tiller (n° m-2)",FALSE)

#--- ploting POL
pl_bio(data.frame(das = o_pol[,"das"],dat = o_pol[,o_bio[5]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[5]] , ctype = plant[,"ctype"]),
       "POL (%)",FALSE)

#--- ploting dgl
pl_bio(data.frame(das = o_dgl[,"das"],dat = o_dgl[,o_bio[6]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[6]] , ctype = plant[,"ctype"]),
       "n° DGL",FALSE)

#--- ploting heigth
pl_bio(data.frame(das = o_sth[,"das"],dat = o_sth[,o_bio[7]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[7]] , ctype = plant[,"ctype"]),
       "Height (m)",TRUE)
dev.off()


#--- retrive cana type from default outputs
ctype = plant[,c("das","ctype")]

#--- link ctype with das 
detint_m = merge(detint,ctype, by = "das")


plot(detint_m$`itlen 1`~detint_m$das,
     ylim = c(1,25),
     xlim = c(1,max(detint_m$das)),
     type = "n",
     xlab = "DAS",
     ylab = "Internodes length (cm)")
n = 35
for(i in 1:n){
  
  pl_detdata(data.frame(das = 1, dat = -99),
             data.frame(das = detint_m[,"das"],dat = detint_m[,paste("itlen",i)],ctype = detint_m[,"ctype"]),
             "Internodes length (cm)", T,i,n)
}

#----------------------------------------------------------------------------------
#--- It length primary...

dt = 10
c = 2

detint_dt = data.frame(rank = seq(1,35))

plot(seq(2,20)~seq(2,20),
     #ylim = c(1,25),
     xlim = c(1,35),
     type = "n",
     xlab = "Rank",
     ylab = "Internodes length (cm)")

for(i in seq(min(detint_m$das), max(detint_m$das), dt)){
  
  v = sapply(seq(1:35),function(x) detint_m[detint_m$das==i,paste("itlen",x)])
  d = sapply(seq(1:35),function(x) detint_m[detint_m$das==i,"dap"])
  
  detint_dt = cbind(detint_dt, v)
  
  colnames(detint_dt)[c] = paste("len_das_",i,sep="")
  
  lines(v~seq(1:35),
        ylab = "Internodes length (cm)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0,0.5), 0.8* unique(d)/max(detint_m$dap)))
  
  c = c + 1
}

leg = "Primary Stalk"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.0, box.lty = 1)

#----------------------------------------------------------------------------------


#--- It length average

dt = 10
c = 2

detint_dt = data.frame(rank = seq(1,35))

plot(seq(2,20)~seq(2,20),
     #ylim = c(1,25),
     xlim = c(1,35),
     type = "n",
     xlab = "Rank",
     ylab = "Internodes length (cm)")

for(i in seq(min(detint$das), max(detint$das), dt)){
  
  v = sapply(seq(1:35),function(x) detint[detint$das==i,paste("av_itlen",x)])
  d = sapply(seq(1:35),function(x) detint[detint$das==i,"dap"])
  
  detint_dt = cbind(detint_dt, v)
  
  colnames(detint_dt)[c] = paste("len_das_",i,sep="")
  
  lines(v~seq(1:35),
        ylab = "Internodes length (cm)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0,0.5), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}

leg = "Average among stalks"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.0, box.lty = 1)


#----------------------------------------------------------------------------------

#--- It sucrose primary

dt = 10
c = 2

detint_dt = data.frame(rank = seq(1,35))

plot(seq(2,20)~seq(2,20),
     ylim = c(1,15),
     xlim = c(1,35),
     type = "n",
     xlab = "Rank",
     ylab = "Internodes Sucrose mass (g)")

for(i in seq(min(detint$das), max(detint$das), dt)){
  
  v = sapply(seq(1:35),function(x) detint[detint$das==i,paste("itsuc",x)])
  d = sapply(seq(1:35),function(x) detint[detint$das==i,"dap"])
  
  detint_dt = cbind(detint_dt, v)
  
  colnames(detint_dt)[c] = paste("len_das_",i,sep="")
  
  lines(v~seq(1:35),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0,0.5), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}

leg = "Primary stalk"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.0, box.lty = 1)



#----------------------------------------------------------------------------------

#--- It sucrose average

dt = 10
c = 2

detint_dt = data.frame(rank = seq(1,35))

plot(seq(2,20)~seq(2,20),
     ylim = c(1,15),
     xlim = c(1,35),
     type = "n",
     xlab = "Rank",
     ylab = "Internodes Sucrose mass (g)")

for(i in seq(min(detint$das), max(detint$das), dt)){
  
  v = sapply(seq(1:35),function(x) detint[detint$das==i,paste("av_itsuc",x)])
  d = sapply(seq(1:35),function(x) detint[detint$das==i,"dap"])
  
  detint_dt = cbind(detint_dt, v)
  
  colnames(detint_dt)[c] = paste("len_das_",i,sep="")
  
  lines(v~seq(1:35),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0,0.5), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}

leg = "Average among stalks"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)

#----------------------------------------------------------------------------------

#--- It total dry mass primary

dt = 10
c = 2

detint_dt = data.frame(rank = seq(1,35))

plot(seq(2,20)~seq(2,20),
     ylim = c(1,20),
     xlim = c(1,35),
     type = "n",
     xlab = "Rank",
     ylab = "Internodes Total Dry Mass (g)")

for(i in seq(min(detint$das), max(detint$das), dt)){
  
  v = sapply(seq(1:35),function(x) detint[detint$das==i,paste("ittdw",x)])
  d = sapply(seq(1:35),function(x) detint[detint$das==i,"dap"])
  
  detint_dt = cbind(detint_dt, v)
  
  colnames(detint_dt)[c] = paste("len_das_",i,sep="")
  
  lines(v~seq(1:35),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0,0.5), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}

leg = "Primary stalk"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)

#----------------------------------------------------------------------------------

#--- It total dry mass average

dt = 10
c = 2

detint_dt = data.frame(rank = seq(1,35))

plot(seq(2,20)~seq(2,20),
     ylim = c(1,20),
     xlim = c(1,35),
     type = "n",
     xlab = "Rank",
     ylab = "Internodes Total Dry Mass (g)")

for(i in seq(min(detint$das), max(detint$das), dt)){
  
  v = sapply(seq(1:35),function(x) detint[detint$das==i,paste("av_ittdw",x)])
  d = sapply(seq(1:35),function(x) detint[detint$das==i,"dap"])
  
  detint_dt = cbind(detint_dt, v)
  
  colnames(detint_dt)[c] = paste("len_das_",i,sep="")
  
  lines(v~seq(1:35),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0,0.5), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}

leg = "Average among stalks"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)


#----------------------------------------------------------------------------------

#--- It fraction of sucrose primary

dt = 10
c = 2

detint_dt = data.frame(rank = seq(1,35))

plot(seq(2,20)~seq(2,20),
     ylim = c(0,100),
     xlim = c(1,35),
     type = "n",
     xlab = "Rank",
     ylab = "Internodes Sucrose Fraction (%)")

for(i in seq(min(detint$das), max(detint$das), dt)){
  
  w = sapply(seq(1:35),function(x) detint[detint$das==i,paste("ittdw",x)])
  v = sapply(seq(1:35),function(x) detint[detint$das==i,paste("itsuc",x)])
  d = sapply(seq(1:35),function(x) detint[detint$das==i,"dap"])
  
  detint_dt = cbind(detint_dt, v)
  
  colnames(detint_dt)[c] = paste("len_das_",i,sep="")
  
  lines((v/w*100)~seq(1:35),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0,0.5), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}


leg = "Primary stalk"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)


#----------------------------------------------------------------------------------

#--- It fraction of sucrose average

dt = 10
c = 2

detint_dt = data.frame(rank = seq(1,35))

plot(seq(2,20)~seq(2,20),
     ylim = c(0,100),
     xlim = c(1,35),
     type = "n",
     xlab = "Rank",
     ylab = "Internodes Sucrose Fraction (%)")

for(i in seq(min(detint$das), max(detint$das), dt)){
  
  w = sapply(seq(1:35),function(x) detint[detint$das==i,paste("av_ittdw",x)])
  v = sapply(seq(1:35),function(x) detint[detint$das==i,paste("av_itsuc",x)])
  d = sapply(seq(1:35),function(x) detint[detint$das==i,"dap"])
  
  detint_dt = cbind(detint_dt, v)
  
  colnames(detint_dt)[c] = paste("len_das_",i,sep="")
  
  lines((v/w*100)~seq(1:35),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0,0.5), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}


leg = "Average among stalks"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)

#-------------------------------------------------------------------------------












v = sapply(seq(1:35),function(x) detint[detint$das==400,c(paste("itlen",x),"dap")])




#--- detailed data
pl_bio(data.frame(das = o_sth[,"das"],dat = o_sth[,o_bio[7]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[7]] , ctype = plant[,"ctype"]),
       "Height (m)",TRUE)





#-------------------------------------------------------------------------------


#--- Detailed Leaves
detleaf_lines = readLines("DetLeafPr_SWAP-SAMUCA_PIRA.OUT")       #Read files lines
detleaf_numlines = detleaf_lines[substr(detleaf_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detleaf = read.table(text = detleaf_numlines)                     #Read numeric lines as data.frame
colnames(detleaf) = c("year","doy","das","dap","diac","ngl","ndevgl",paste(rep("lfarea",11),1:11),paste(rep("av_lfarea",11),1:11),paste(rep("lfweight",11),1:11),paste(rep("av_lfweight",11),1:11))

#-------------------------------------------------------------------------------

#--- Leaf area (cm2)

dt = 10
c = 2
nleaves_rank = 10

detleaf_dt = data.frame(rank = seq(1,nleaves_rank))

plot(seq(2,20)~seq(2,20),
     ylim = c(0,600),
     xlim = c(1,nleaves_rank),
     type = "n",
     xlab = "Rank",
     ylab = "Leaf area (cm2)")

for(i in seq(min(detleaf$das), max(detleaf$das), dt)){
  
  w = sapply(seq(1:nleaves_rank),function(x) detleaf[detleaf$das==i,paste("lfarea",x)])
  d = sapply(seq(1:nleaves_rank),function(x) detleaf[detleaf$das==i,"dap"])
  
  detleaf_dt = cbind(detleaf_dt, w)
  
  colnames(detleaf_dt)[c] = paste("len_das_",i,sep="")
  
  lines(w~seq(1:nleaves_rank),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0.5,0), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}


leg = "Average among stalks"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)

#-------------------------------------------------------------------------------

#--- Leaf area (cm2)

dt = 10
c = 2
nleaves_rank = 10

detleaf_dt = data.frame(rank = seq(1,nleaves_rank))

plot(seq(2,20)~seq(2,20),
     ylim = c(0,600),
     xlim = c(1,nleaves_rank),
     type = "n",
     xlab = "Rank",
     ylab = "Leaf area (cm2)")

for(i in seq(min(detleaf$das), max(detleaf$das), dt)){
  
  w = sapply(seq(1:nleaves_rank),function(x) detleaf[detleaf$das==i,paste("av_lfarea",x)])
  d = sapply(seq(1:nleaves_rank),function(x) detleaf[detleaf$das==i,"dap"])
  
  detleaf_dt = cbind(detleaf_dt, w)
  
  colnames(detleaf_dt)[c] = paste("len_das_",i,sep="")
  
  lines(w~seq(1:nleaves_rank),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0.5,0), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}


leg = "Average among stalks"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)

#-------------------------------------------------------------------------------

#--- Leaf weight (g)

dt = 10
c = 2
nleaves_rank = 10

detleaf_dt = data.frame(rank = seq(1,nleaves_rank))

plot(seq(2,20)~seq(2,20),
     ylim = c(0,10),
     xlim = c(1,nleaves_rank),
     type = "n",
     xlab = "Rank",
     ylab = "Leaf dry weight (g)")

for(i in seq(min(detleaf$das), max(detleaf$das), dt)){
  
  w = sapply(seq(1:nleaves_rank),function(x) detleaf[detleaf$das==i,paste("lfweight",x)])
  d = sapply(seq(1:nleaves_rank),function(x) detleaf[detleaf$das==i,"dap"])
  
  detleaf_dt = cbind(detleaf_dt, w)
  
  colnames(detleaf_dt)[c] = paste("len_das_",i,sep="")
  
  lines(w~seq(1:nleaves_rank),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0.5,0), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}


leg = "Average among stalks"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)


#-------------------------------------------------------------------------------

#--- Leaf weight (g)

dt = 10
c = 2
nleaves_rank = 10

detleaf_dt = data.frame(rank = seq(1,nleaves_rank))

plot(seq(2,20)~seq(2,20),
     ylim = c(0,10),
     xlim = c(1,nleaves_rank),
     type = "n",
     xlab = "Rank",
     ylab = "Leaf area (cm2)")

for(i in seq(min(detleaf$das), max(detleaf$das), dt)){
  
  w = sapply(seq(1:nleaves_rank),function(x) detleaf[detleaf$das==i,paste("av_lfweight",x)])
  d = sapply(seq(1:nleaves_rank),function(x) detleaf[detleaf$das==i,"dap"])
  
  detleaf_dt = cbind(detleaf_dt, w)
  
  colnames(detleaf_dt)[c] = paste("len_das_",i,sep="")
  
  lines(w~seq(1:nleaves_rank),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0,0.5,0), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}


leg = "Average among stalks"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)


#---------------------------------------------------------------------------------------------


#--- Detailed RootSystem
detroot_lines = readLines("DetRootSy_SWAP-SAMUCA_PIRA.OUT")       #Read files lines
detroot_numlines = detroot_lines[substr(detroot_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detroot = read.table(text = detroot_numlines)                     #Read numeric lines as data.frame
colnames(detroot) = c("year","doy","das","dap","diac","wr","rd","rootsene",paste(rep("rld",45),1:45),"tqropot","ptra")


unique(swba$depth)

sl_df = data.frame(sl = seq(1:length(unique(swba$depth))),
                   dp = sort(unique(swba$depth), decreasing = T)*-1)



#--- RLD (cm cm-3)

dt = 10
c = 2
n_sl = 45

detroot_dt = data.frame(rank = seq(1,n_sl))

plot(seq(2,20)~seq(2,20),
     ylim = c(0,1),
     xlim = c(1,sl_df[n_sl,"dp"]),
     type = "n",
     xlab = "Soil layers",
     ylab = "RLD (cm cm-3)")

for(i in seq(min(detroot$das), max(detroot$das), dt)){
  
  w = sapply(seq(1:n_sl),function(x) detroot[detroot$das==i,paste("rld",x)])
  d = sapply(seq(1:n_sl),function(x) detroot[detroot$das==i,"dap"])
  
  detroot_dt = cbind(detroot_dt, w)
  
  colnames(detroot_dt)[c] = paste("len_das_",i,sep="")
  
  lines(w~(sl_df[1:n_sl,"dp"]),
        ylab = "Internodes Sucrose mass (g)",
        xlab = "Rank",
        ylim = c(1,25),
        xlim = c(0,35),
        yaxs="i",
        type = "l",
        col = alpha(rgb(0.5,0,0), 0.8* unique(d)/max(detint$dap)))
  
  c = c + 1
}


leg = "Average among stalks"
legend("topleft",inset = 0.01, legend =  leg, bg = "grey",cex = 1.2, box.lty = 1)


#--------------------------------------------------------
#--- detailed stresses

das_end = (detsfac$das[detsfac$dap==1]-1)[2:4]
das_end = c(das_end, max(detsfac$das))

das_df = data.frame(s = c(1,das_end[1:3]+1), e = das_end)

plot(c(-20,-20)~c(0,400),ylim=c(0,4))
for(i in das_end) lines(detsfac$lai~detsfac$das)

plot(detsfac$lai~detsfac$das,type = "l")
lines(detsfac$li~detsfac$das)


plot(detsfac$arue_dwa~detsfac$IPAR_acc)

points(detroot$rd/100~detroot$dap)

