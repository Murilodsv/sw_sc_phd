#--------------------#
#--- read cumdens ---#
#--------------------#

library(ggplot2)

cumdens = 
read.table('C:/Murilo/swap_samuca/swap_samuca/sw_sc/cases/SAMUCA/Root_cumdens_SEQ_F1_S2.OUT',
           sep = ',',
           col.names = c('seqnow','year','doy','das','dap','rd','cumdens_1','cumdens_2'))

#--- get node count
node = rep(seq(1:nrow(cumdens[cumdens$das == 11,])), length(unique(cumdens$das)))
cumdens$node = node

cumdens$node_depth = cumdens$cumdens_1 * cumdens$rd

#--- base theme for gg
base_theme = theme(text=element_text(family="Cambria", colour ='black'),
                   plot.title = element_text(hjust = 0.5),
                   axis.text = element_text(colour = 'black'),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



ggplot(cumdens[cumdens$dap > 20,], aes(x=das,y=-cumdens_1,fill=cumdens_2)) + 
  #geom_tile(color = "black") + 
  geom_tile() + 
  #scale_fill_gradient(low="red", high="black") +
  #scale_fill_distiller(palette = "Spectral",limits = c(-1,1)*max(abs(df_r_sens_jb$delta_r)),
  #                     name = expression(Delta*"r")) +
  ggtitle('Cumdens', ) +
  xlab(NULL) + ylab(NULL) +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  #facet_wrap(.~scenario_sens_lab,
  #           ncol = 1)+
  theme_bw() + base_theme + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  plot.title = element_text(hjust = 0.5))



