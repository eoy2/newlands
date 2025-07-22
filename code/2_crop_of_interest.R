

#rm(list = ls())
#gc()
#file.remove(list.files(tempdir(), full.names = T))

# the second for loop aggregates all of the county data collected in the first
# and is commodity specific. The result is a single file for the state of GA 
#that records all hierarchicical transitions for a commodity of interest

#  function(){
#list file locations for all counties
getCOMvals <- function(meta, commodity){

  com_vals <-
    meta |> 
    dplyr::mutate(
      commodity_name = LandCover,
      commodity_name =
        dplyr::case_when(
          Code %in% c(2,232,238,239) ~ 'Cotton',
          Code %in% c(1,12,13,225,226,237,241) ~ 'Corn',
          Code %in% c(4,235,236) ~ 'Sorghum',
          Code %in% c(5,26,239,240,241,254) ~ 'Soybeans'
    ),
    commodity_name = dplyr::if_else(is.na(commodity_name),LandCover,commodity_name)
    ) |>
    dplyr::filter(toupper(commodity_name) %in% toupper(commodity)) |>
    dplyr::select(Code)
  
  colnames(com_vals) <- 'Code'
  #grassland is wrong -- need to rerun original script to include this
  com_vals
}
getCOItransitions <- function(metadata_file, commodity_of_interest, county_files_location, out_dir){
  
  meta <- 
    metadata_file |> 
    readr::read_csv() |>
    magrittr::set_colnames(c('Code', 'LandCover','Group'))
  
    files <- list.files(county_files_location, full.names = T)
  
  com_vals <- getCOMvals(meta, commodity = commodity_of_interest)

  for(i in 1:length(files)){
    
    outputs <- 
      files[i] |>
      read.csv() |>
      dplyr::group_by(locs) |>
      dplyr::arrange(locs,year) |>
      dplyr::mutate(
        group = dplyr::if_else(id %in% com_vals$Code, 2, group),#crops of interest
        max = max(group,na.rm =T)
      ) |> 
      dplyr::filter(max > 1) |>
      dplyr::mutate(
        assignment = paste0(group, '-', dplyr::lead(group)),
        lag = group - dplyr::lag(group)
      )
    
    xs <- 
      outputs |> 
      dplyr::group_by(locs) |>
      dplyr::mutate(
        x_match = dplyr::if_else(assignment %in% '0-1', 1, NA),
        y_match = dplyr::if_else(assignment %in% '1-2', 1, NA)
      )
    x <- xs |> dplyr::filter(!is.na(x_match)) |> dplyr::select(locs) |> unique()
    y <- xs |> dplyr::filter(!is.na(x_match)) |> dplyr::select(locs) |> unique()
    outputs[which(outputs$locs %in% x$locs & outputs$locs %in% y$locs & outputs$assignment %in%'1-2'),]$assignment <- '0-2'
    
    outputs <- 
      outputs |>
      dplyr::filter(!is.na(lag)) |>
      dplyr::filter(grepl('2', assignment)) |>
      dplyr::mutate(
        year = as.numeric(stringr::str_sub(year,2,5)),
        written_code = dplyr::case_when(
          assignment %in% '2-2' ~ 'stable crop',
          assignment %in% '2-1' ~ 'crop rotation',
          assignment %in% '2-0' ~ 'intermittent cropland',
          assignment %in% '1-2' ~ 'crop change',
          assignment %in% '0-2' ~ 'land conversion'
        ),
        written_code = factor(written_code,levels = c('land conversion','intermittent cropland','crop change', 'crop rotation', 'stable crop' ))
      )
    print(round(100*i/length(files), digits = 2))
    if(i == 1){lagged = outputs}else(lagged = dplyr::bind_rows(outputs,lagged))
  }
  
  lagged |> 
    saveRDS(
      paste0(
        out_dir,
        commodity_of_interest,
        '_raster_vals_v3.rds'
      )
    )    
  
  
  ggplot(
    data = lagged |> dplyr::filter(written_code %in% c(
      'stable crop', 'crop rotation','land conversion'
    ))
  ) +
    geom_bar(
      aes(x = as.factor(year),
          fill = written_code),
      stat = 'count'
    ) +
    scale_fill_viridis_d() +
    theme_classic()
  
}

library(tidyverse)


meta <- 
  '/Users/eyackulic/Documents/GitHub/AFF/newlands/data/cropscape_metadata.csv' 

file_locations <- '/Users/eyackulic/workspace/Miss_CDLs/retry/'

out_dir <- '/Users/eyackulic/workspace/Miss_CDLs/commodities/'
dir.create(path = out_dir)

commodity <- 'cotton'

getCOItransitions(
  metadata_file = meta, 
  commodity_of_interest = commodity, 
  county_files_location = file_locations,
  out_dir = out_dir
)
