
library(ggplot2)
library(rnassqs)
nassqs_auth('E87292B8-EBF0-3745-A964-479207E3CF4D')

source("C:/Users/NephtaliChavez/American Forest Foundation/FAMILY FOREST CARBON PROGRAM - Documents/Geospatial Library/FieldToForest/New Lands Analysis/Ethans NL Method/newlands-main/code/intensification.R")
source("C:/Users/NephtaliChavez/American Forest Foundation/FAMILY FOREST CARBON PROGRAM - Documents/Geospatial Library/FieldToForest/New Lands Analysis/Ethans NL Method/newlands-main/code/results_fxns.R")

commodity <- 'peaches'
state <- 'GA'
value_path <- paste0('C:/Users/NephtaliChavez/American Forest Foundation/FAMILY FOREST CARBON PROGRAM - Documents/Geospatial Library/FieldToForest/New Lands Analysis/Ethans NL Method/georgia/georgia_commodities/', commodity, '_raster_vals_v3.rds')
csv_path <- paste0('C:/Users/NephtaliChavez/American Forest Foundation/FAMILY FOREST CARBON PROGRAM - Documents/Geospatial Library/FieldToForest/New Lands Analysis/Ethans NL Method/georgia/NL Values/',commodity,'_vals.csv')
#value_path <- '/Users/eyackulic/workspace/fields_2_forests/commodities/other hay_non alfalf_raster_vals_v3.rds'

out <- getResults(
  value_path = value_path, 
  t_prod = NA, 
  commodity = commodity,
  option = 'remote',
  normalize = T,
  state_code = state,
  image_path = NA,
  csv_path = csv_path)



NL_average(file_path = csv_path, n_of_years = 5)

ggplot(out) + 
  geom_hline(aes(yintercept = mean(NLi, na.rm = T)), color = 'black', size = 2) + #revised equation for New Lands
  geom_hline(aes(yintercept = mean(NL2, na.rm = T)), color = 'red') +#wills original equation
  geom_hline(aes(yintercept = mean(NL3, na.rm = T)), color = 'blue') +#no intensification
  geom_point(aes(x = Year, y = NLi),size =3)+
  geom_point(aes(x = Year, y = NL2),size = 2, color = 'red') +
  geom_point(aes(x = Year, y = NL3), color = 'blue', size = 2) + 
  tidyquant::theme_tq() +
  ylab('New Lands Value')
