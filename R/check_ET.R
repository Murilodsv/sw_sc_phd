#--------------------#
#--- read stresss ---#
#--------------------#

library(ggplot2)
library(reshape2)

stress = 
  read.table('C:/Murilo/swap_samuca/swap_samuca/sw_sc/cases/SAMUCA/Stress_factors_SEQ_F1_S2.OUT',
             col.names = c('das',
                           'dap',
                           'trasw',
                           'eop',
                           'trwup_v_10',
                           'max_trwup_eop',
                           'swfacp',
                           'swface',
                           'swfacf',
                           'swfact',
                           'tmed',
                           'tempfac_pho',
                           'tempfac_per', 
                           'co2',
                           'pho_fac_co2',
                           'diacem',
                           'agefactor_amax',
                           'agefactor_per',
                           'sug_it_BG',
                           'amaxfbfac',
                           'dtg',
                           'per',
                           'ptra',
                           'peva',
                           'tra',
                           'reva'))


#--- ET check
ET_check = stress

ET_check$ETp = ET_check$ptra + ET_check$peva
ET_check$ETa = ET_check$tra + ET_check$reva

ET_check = melt(ET_check[,c('das','ptra','peva','tra','reva','ETp','ETa')],
                id.vars = 'das')

ggplot(ET_check[ET_check$variable %in% c('ptra','tra'),], aes(x=das,y=value,group=variable, colour=variable)) + 
  geom_line()

ggplot(ET_check[ET_check$variable %in% c('ETp','ptra','peva'),], aes(x=das,y=value,group=variable, colour=variable)) + 
  geom_line()


ggplot(ET_check[ET_check$variable %in% c('ETa','tra','reva'),], aes(x=das,y=value,group=variable, colour=variable)) + 
  geom_line()

ggplot(ET_check[ET_check$variable %in% c('ETa','ETp'),], aes(x=das,y=value,group=variable, colour=variable)) + 
  geom_line()

