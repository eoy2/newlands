
files <-
  list.files(
    '/Users/eyackulic/Downloads/cdls/', 
    pattern = '.tif',
    full.names = T)
filenames <-
  list.files(
    '/Users/eyackulic/Downloads/cdls/', 
    pattern = '.tif',
    full.names = F)

state <- 
  '/Users/eyackulic/Documents/GitHub/AFF/newlands/data/tl_2024_us_county.gpkg' |>
#  '/Users/eyackulic/Downloads/tl_2023_us_state/tl_2023_us_state.shp' |>
  sf::read_sf() |>
 # dplyr::filter(STUSPS %in% 'MS') |>
  dplyr::filter(STATEFP %in% 28) |>
  terra::vect() |>
  terra::project(terra::crs(terra::rast(files[1])))

for(i in 1:length(files)){
  
  file <- 
    files[i] |>
    terra::rast()  
  
  out <- 
    file |> 
    terra::crop(state) |>
    terra::mask(state)
  
  terra::writeRaster(
    x = out,
    filename = 
      paste0(
        '/Users/eyackulic/workspace/Miss_CDLs/',
        filenames[i]
      )
  )
  
}
