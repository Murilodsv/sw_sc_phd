#--- Diviner calib

library(ggplot2)

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

#--- Read data
grav.df = read.csv(paste0(wd.repo,"/doc/excel/grav_fdr.csv"),as.is = T)
fdr.df  = read.csv(paste0(wd.repo,"/doc/excel/min_max_fdr.csv"),as.is = T)

a = 0.14
b = 0.498
c = 0.009

#--- aggregate by trench and depth
grav.df.m = data.frame(Depth  = aggregate(theta ~ Depth + Trench, grav.df, mean)[,1],
                       Trench = aggregate(theta ~ Depth + Trench, grav.df, mean)[,2],
                       theta  = aggregate(theta ~ Depth + Trench, grav.df, mean)[,3],
                       SF     = aggregate(SF ~ Depth + Trench, grav.df, mean)[,3],
                       theta_sd = aggregate(theta ~ Depth + Trench, grav.df, sd)[,3],
                       SF_sd    = aggregate(SF ~ Depth + Trench, grav.df, sd)[,3])

grav.df.m$a = a
grav.df.m$b = b
grav.df.m$c = c

fdr.df.m = data.frame(Depth = aggregate(theta_MVG ~ prof + type,fdr.df,mean )[,1],
                      Type  = aggregate(theta_MVG ~ prof + type,fdr.df,mean )[,2],
                      theta_mvg  = aggregate(theta_MVG ~ prof + type,fdr.df,mean )[,3],
                      theta_mvg_sd  = aggregate(theta_MVG ~ prof + type,fdr.df,sd )[,3],
                      SF = aggregate(SF ~ prof + type,fdr.df,mean )[,3],
                      SF_sd  = aggregate(SF ~ prof + type,fdr.df,sd )[,3])

gg.fdr.calib = 
ggplot(grav.df.m, aes(x = theta, y = SF)) + 
  geom_errorbar(aes(ymin=SF-SF_sd, ymax=SF+SF_sd), 
                width=0.2,
                position=position_dodge(0.1),
                colour = "black",
                size = 0.3) +
  geom_errorbarh(aes(xmin=theta-theta_sd, xmax=theta+theta_sd, height = .01),
                 colour = "black",
                 size = 0.3) +
  geom_errorbar(data = fdr.df.m, 
                aes(x = theta_mvg, y = SF, ymin=SF-SF_sd, ymax=SF+SF_sd, colour = as.factor(Type)), 
                width=0.2, position=position_dodge(0.1),
                size = 0.3) +
  geom_errorbarh(data = fdr.df.m, 
                 aes(x = theta_mvg, y = SF, xmin=theta_mvg-theta_mvg_sd, xmax=theta_mvg+theta_mvg_sd, height = .01,
                     colour = as.factor(Type)),
                 size = 0.3) +
  geom_point(size = 2.5, fill = "grey", colour = "black", shape = 21) + 
  geom_point(data = fdr.df.m, aes(x = theta_mvg, y = SF, fill = as.factor(Type)),
             size = 2.5, colour = "black", shape = 21) + 
  stat_function(fun=function(x) a*x^b+c, geom="line", colour = "black", linetype = 2,size = 0.3) +
  scale_fill_manual(values= c("#3498DB","#CB4335")) + 
  scale_colour_manual(values= c("#3498DB","#CB4335")) + 
  scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,0.25)) + 
  scale_x_continuous(limits = c(0,60), expand = c(0.01,0.01)) + 
  ylab("SF (0-1)") + xlab(bquote('SWC ('*cm^3~cm^-3*')')) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = NULL,
        legend.position="none",
        axis.line = element_line(colour = "black", size = 0.2),
        axis.text=element_text(size=14, colour = "black"),
        axis.title=element_text(size=14),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 5, b = 0, l = 0)),
        axis.ticks = element_blank(),
        legend.key=element_blank(),
        plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"),
        legend.box.background = element_rect(colour = "black"))

gg.fdr.calib

ggsave(paste0(wd.repo,"/doc/excel/fdr_calib.png"), 
       plot = gg.fdr.calib, width = 5, height = 4,
       dpi = 700)





