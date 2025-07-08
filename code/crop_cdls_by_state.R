require(tidycensus)

cropCDLS <- function(input_cdls_location,states_path, state_of_interest,output_location) {

  files <-
  list.files(
    input_cdls_location, 
    pattern = '.tif',
    full.names = T)

  filenames <-
  list.files(
    input_cdls_location, 
    pattern = '.tif',
    full.names = F)

fips_codes <- 
  tidycensus::fips_codes |>
  dplyr::select(state_code, state_name) |> 
  dplyr::mutate(state_name = toupper(state_name)) |>
  dplyr::distinct() |> 
  dplyr::filter(state_name %in% toupper(state_of_interest))

state <- 
  states_path |>
  sf::read_sf() |>
  dplyr::filter(STATEFP %in% fips_codes$state_code) |> 
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
        output_location,
        filenames[i]
      ),
    overwrite = T,
    datatype = 'INT1U'
  )
  
}
}


input_cdls_location <- '/Users/eyackulic/Downloads/cdls/'
states_path <- '/Users/eyackulic/Documents/GitHub/AFF/newlands/data/tl_2024_us_county.gpkg' 
output_location <- '/Users/eyackulic/workspace/Miss_CDLs/'

cropCDLS(
  input_cdls_location = input_cdls_location,
  states_path = states_path,
  output_location = output_location,
  state_of_interest = 'new jersey')
