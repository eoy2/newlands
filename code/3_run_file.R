
library(ggplot2)
library(rnassqs)
nassqs_auth('C1C3BFB2-6E23-3E72-A6F5-A60236695742')

source("~/Documents/GitHub/AFF/newlands/intensification.R")
source("~/Documents/GitHub/AFF/newlands/code/results_fxns.R")

commodity <- 'cotton'

value_path <- paste0('/Users/eyackulic/workspace/Miss_CDLs/commodities/', commodity, '_raster_vals_v3.rds')
#value_path <- paste0('/Users/eyackulic/workspace/fields_2_forests/', commodity, '_raster_vals_v3.rds') 

out <- getResults(
  value_path = value_path, 
  t_prod = NA, 
  commodity = commodity,
  option = 'remote',
  normalize = T)


ggplot(out) + 
  geom_hline(aes(yintercept = mean(NLi, na.rm = T)), color = 'black', size = 2) + #revised equation for New Lands
  geom_hline(aes(yintercept = mean(NL2, na.rm = T)), color = 'red') +#wills original equation
  geom_hline(aes(yintercept = mean(NL3, na.rm = T)), color = 'blue') +#no intensification
  geom_point(aes(x = Year, y = NLi),size =3)+
  geom_point(aes(x = Year, y = NL2),size = 2, color = 'red') +
  geom_point(aes(x = Year, y = NL3), color = 'blue', size = 2) + 
  tidyquant::theme_tq() +
  ylab('New Lands Value')
