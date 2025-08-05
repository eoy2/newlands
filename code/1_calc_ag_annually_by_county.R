
#this first loop will process rasters annually, by county, to determine all pixels that are 
#designated as agricultural at least once during the time of interest. It keeps a record of the location of those 
#pixels, so that the second loop can collect and store the full time series for every single pixel in the state of 
#georgia that is agricultural at one time or another.
get_ag_pixels <- function(counties_path, state_of_interest, cdl_raster_paths, metadata_file, out_dir, mac){

  fips_codes <- 
    tidycensus::fips_codes |>
    dplyr::select(state_code, state_name) |> 
    dplyr::mutate(state_name = toupper(state_name)) |>
    dplyr::distinct() |> 
    dplyr::filter(state_name %in% toupper(state_of_interest))
  
  state_counties <-
    counties_path|>
    sf::read_sf() |>
    dplyr::filter(STATEFP %in% fips_codes$state_code) |>
    dplyr::arrange(COUNTYFP)
  
  meta <-   
    metadata_file |> 
    readr::read_csv() |>
    magrittr::set_colnames(c('Code', 'LandCover','Group')) |>
    dplyr::mutate(Group = dplyr::if_else(Code > 60 & Code < 63, 'Fallow', Group))

for(k in 1:length(state_counties$COUNTYFP)){
  county <- 
    state_counties[k,] |>
    terra::vect() |>
    terra::project(
      terra::crs(terra::rast(cdl_raster_paths[1]))
    )
  j <- 0
  for(i in 1:length(cdl_raster_paths)){
    #working code starts here
    rast <-
      cdl_raster_paths[i] |>
      terra::rast() |>
      terra::crop(county) # can't mask here for focal matching across county boundaries
    
    all_vals <- 
      rast |>
      terra::values() |>
      tibble::tibble() |>
      magrittr::set_colnames('Code')
    
    v2 <- 
      all_vals |>
      dplyr::mutate(
        ag_code = dplyr::if_else(Code %in% meta[meta$Group %in% 'Crops',]$Code, 1, 0)
      )
    
    new_rast <- terra::setValues(rast, v2$ag_code)
    
    #focal windows are applied consecutively to determine whether pixels are part of a 
    #agricultural feature that is at least 15 acres. To do this, the first focal window
    #determines the sum of all pixels(1s and 0s) within the square focal window 
    #(11 x 11 = 121 pixels ~ 27 acres). The second focal window then calculated the maximum 
    #value within the raster of focal sums. This means that boundary pixels are not excluded
    #because the max value reflects the larger feature they are a part of. One weakness here is that 
    # a 15 acre area has to be within a 27 acre square (i.e., we might miss narrow tracts of land)
    foc <- terra::focal(new_rast, w = matrix(1,nrow = 13, ncol = 13), fun = sum) 
    max_foc <- terra::focal(foc, w = matrix(1,nrow = 13, ncol = 13), fun = max)
    
    foc_vals <- 
      max_foc |>
      terra::values() |>
      tibble::tibble() |>
      magrittr::set_colnames('focal_code')  |>
      dplyr::mutate(
        foc_code = dplyr::if_else(focal_code >= 67, 1, 0)
      ) 
    
    values <-
      cdl_raster_paths[i] |>
      terra::rast() |>
      terra::crop(county, mask = T) |>
      terra::values() |>
      tibble::tibble() |>
      magrittr::set_colnames('Code')
    
    values <- data.frame(values$Code * foc_vals$foc_code)
    colnames(values) <- 'Code'
    #former code starts here
    locations <- seq(1, dim(values)[1], by = 1) 
    #crop_locations <- which(values$Code %in% com_vals$Code) |> data.frame() 
    crop_locations <- which(values$Code %in% meta[meta$Group %in% 'Crops',]$Code) |> data.frame() 
    colnames(crop_locations) <- 'Code'
    if(nrow(crop_locations) > 0){
      crop_locations$Value <- 1
    }else{
      out <- data.frame(matrix(nrow = 0,ncol = 2))
      colnames(out) <- c('Code','Value')
      
      print(
        paste0(
          'no ag pixels in ',
          cdl_raster_paths[i],
          ' for ' ,
          state_counties$NAME[k],
          ' county'
        )
        )
      next
    }
    if(j < 1){
      j <- j + 1
      out <- crop_locations
    }else{
      out <- dplyr::bind_rows(crop_locations, out) |> dplyr::group_by(Code) |> dplyr::reframe(Value = sum(Value))  
    }
  }
  if(nrow(out) == 0){
    next
  }
  
  all_values <-
    cdl_raster_paths |>
    terra::rast() |>
    terra::crop(county, mask = T) |>
    terra::values() |>
    #tibble::tibble() |>
    data.frame() |>
    dplyr::slice(out$Code)  
  
  if(mac == T){
  locs <- stringr::str_locate_all(cdl_raster_paths,'/')[[1]]
  }else{
  locs <- stringr::str_locate_all(cdl_raster_paths,'\\\\')[[1]]
  }
  
  last_loc <- locs[nrow(locs),1]
  
  colnames(all_values) <- 
    paste0('n',stringr::str_sub(cdl_raster_paths, last_loc + 1,last_loc + 4))
  
  all_values$locs <- out$Code
  fallow <- meta |> dplyr::filter(Group %in% 'Fallow')

  go_long <-
    all_values |>
    tidyr::pivot_longer(cols = -'locs', values_to = 'id', names_to = 'year') |>
    dplyr::group_by(locs) |>
    dplyr::arrange(locs,year) |>
    dplyr::mutate(
      group = dplyr::if_else(id %in% meta[meta$Group == 'Crops',]$Code, 1, 0),#all crops
      #   group = dplyr::if_else(id %in% com_vals$Code, 2, group),#crops of interest
      group = dplyr::if_else(id %in% fallow$Code, -1, group),#fallow crops
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group == -1, dplyr::lag(group), group),
      group = dplyr::if_else(group >= 1 & dplyr::lag(group) == 0 & dplyr::lead(group) == 0, 0, group)
    )
  
  #if value = 1 and lag(value) + value + lead(value) = 1, value = 0
  
  go_long$county <- county$COUNTYFP
  write.csv(
    go_long,
    paste0(out_dir,
           county$COUNTYFP, '.csv'))
  #if(k == 1){out_log <- lagged}else{out_log <- dplyr::bind_rows(out_log, lagged)}
  print(paste0(round(100 * k/length(state_counties$COUNTYFP), digits = 2), '% of counties finished!'))
}

 }



metadata_file <- 
  '/Users/eyackulic/Documents/GitHub/AFF/newlands/data/cropscape_metadata.csv' 

raster_paths <-
  '/Users/eyackulic/Downloads/2008_2024_CDLs' |>
#  '/Users/eyackulic/workspace/Miss_CDLs' |> #change path here
  list.files(pattern = '.tif$', full.names = T) 

raster_years <- 
  unlist(
    stringr::str_extract_all(
      raster_paths, "(?<!\\d)\\d{4}" #find 4 digits in a row, extract as raster years
      )
    ) |>
  as.numeric()



state_of_interest = 'Mississippi'

counties_path <- 
  '/Users/eyackulic/Documents/GitHub/AFF/newlands/data/tl_2024_us_county.gpkg'  

output_directory = '/Users/eyackulic/workspace/Miss_CDLs/retry/'

get_ag_pixels(
  counties_path = counties_path,
  state_of_interest = 'georgia',
  cdl_raster_paths = raster_paths,
  metadata_file = metadata_file,
  out_dir = output_directory,
  mac = TRUE
    )

#started @ 3:36-9:15
#for troubleshooting
counties_path = counties_path
state_of_interest = 'georgia'
cdl_raster_paths = raster_paths
metadata_file = metadata_file
out_dir = output_directory
mac = TRUE
