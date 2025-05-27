cms <- list.files('/Volumes/easystore/cms_biomass_data', full.names = T)
cms_name <- list.files('/Volumes/easystore/cms_biomass_data', full.names = F)

california <- 
  '/Users/eyackulic/Downloads/tl_2024_us_county/tl_2024_us_county.shp' |> 
  sf::read_sf() |>
  dplyr::filter(STATEFP == '06') |>
  dplyr::group_by(STATEFP) |>
  dplyr::summarise() |>
  terra::vect() |>
  terra::project(
    terra::crs(
      terra::rast(cms[1])
    )
  ) 

for(i in 1:length(cms)){
  new_rast <- 
    terra::rast(cms[i]) |>
    terra::crop(california) |>
    terra::mask(california)
  
  terra::writeRaster(new_rast, paste0('/Users/eyackulic/workspace/Cali_Carbon_Layers/',cms_name[i]), overwrite = T)
  print(100 * round(i/length(cms), digits = 2))
}

















source("~/Documents/GitHub/Fire-Adapted-Forests/code/ref_region.R")
cms <- list.files('/Users/eyackulic/workspace/Cali_Carbon_Layers', full.names = T)
cms_name <- list.files('/Users/eyackulic/workspace/Cali_Carbon_Layers', full.names = F)

carbon <- '/Users/eyackulic/workspace/Cali_Carbon_Layers' |> list.files(full.names = T)

projection = get_projection(carbon[1])

tcsi_eco_regions <- prepRR(
  aoi = '/Users/eyackulic/Downloads/phoenix_lake.gpkg',
  projection = projection,
  l4eco =  '/Users/eyackulic/Downloads/us_eco_l4_state_boundaries/us_eco_l4.shp'
) |> terra::vect()

for(i in 1:length(cms)){
  new_rast <- 
    terra::rast(cms[i]) |>
    terra::crop(tcsi_eco_regions) |>
    terra::mask(tcsi_eco_regions)
  
  terra::writeRaster(new_rast, paste0('/Users/eyackulic/workspace/Tuolumne_Layers/',cms_name[i]), overwrite = T)
  print(100 * round(i/length(cms), digits = 2))
}
