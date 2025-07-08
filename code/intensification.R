# commodity <- 'sorghum'
# commodity <- 'soybeans'
# years <- c(1990:2025)
# yield_file <- '/Users/eyackulic/Downloads/GA_yield_data.csv'


get_crop_yields <- function(yield_data, commodity, years){

  yields <- 
    yield_data |> 
    dplyr::filter(
      COMMODITY_DESC %in% toupper(commodity)
      ) |>
    dplyr::mutate(
      Year = as.numeric(YEAR),
      Value = stringr::str_remove_all(string = VALUE, pattern = ','),
      Value = as.numeric(Value),
      Commodity = COMMODITY_DESC
    ) |> 
    dplyr::arrange(Year) |>
  dplyr::filter(Year >= min(as.numeric(years)), Year <= max(as.numeric(years)))#%in% as.numeric(years))
  
  yields$Value2 = (yields$Value/yields[which.min(yields$Year),]$Value) #* 100
 
   yields |>
    dplyr::select(Commodity, Year, Value, Value2)
}



get_crop_yields_old <- function(yield_data, commodity, years){
  yields <- 
    yield_data |> 
    dplyr::filter(Year %in% years)
  
  if(toupper(commodity) == 'CORN'){
    data.item = 'CORN, GRAIN - YIELD, MEASURED IN BU / ACRE'
  }else if(toupper(commodity) == 'COTTON'){
    data.item =  'COTTON - YIELD, MEASURED IN LB / ACRE'
  }else if(toupper(commodity) == 'SORGHUM'){
    data.item = 'SORGHUM, GRAIN - YIELD, MEASURED IN BU / ACRE'
  }else if(toupper(commodity) == 'HAY'){
    data.item = 'HAY, (EXCL ALFALFA) - YIELD, MEASURED IN TONS / ACRE'
  }else if(toupper(commodity) == 'SOYBEANS'){
    data.item = 'SOYBEANS - YIELD, MEASURED IN BU / ACRE'
  }else if(toupper(commodity) == 'PEANUTS'){
    data.item = 'PEANUTS - YIELD, MEASURED IN LB / ACRE'
  }else if(toupper(commodity) == 'RYE'){
    data.item = 'RYE - YIELD, MEASURED IN BU / ACRE'
  }else if(toupper(commodity) == 'WHEAT'){
    data.item = 'WHEAT - YIELD, MEASURED IN BU / ACRE'
  }else if(toupper(commodity) == 'OATS'){
    data.item = 'OATS - YIELD, MEASURED IN BU / ACRE'
  }else if(toupper(commodity) == 'MILLET'){
    print('commodity is not in database')
    break
  }else{
    print('This commodity has not been hard-coded into the function -- check with Ethan')
    break
  }
  
  #data.item = 'COTTON, UPLAND, IRRIGATED, ENTIRE CROP - YIELD, MEASURED IN BALES / ACRE'
  
  #need to generalize data.item selection
  yield_data <- 
    yields |> 
    dplyr::filter(
      Commodity %in% toupper(commodity),
      Data.Item %in% data.item,
      Period %in% 'YEAR') |>
    dplyr::mutate(
      Year = as.numeric(Year),
      Value = stringr::str_remove_all(string = Value, pattern = ','),
      Value = as.numeric(Value)
    ) 
  
  yield_data$Value2 = (yield_data$Value/yield_data[which.min(yield_data$Year),]$Value) * 100
  yield_data
}







calculate_deltaIN <- function(yield_data, variable){
  mod_df <-
    yield_data |>
    dplyr::rename(variable_choice = variable) 
  
  mod <- lm( variable_choice ~ Year, mod_df)
  
  mod$coefficients[2] |> as.numeric()
  
  #  summary(mod)
  #  yield_data$predictions <- stats::predict(mod, newdata = yield_data)
  # ggplot2::ggplot( 
  #   data = yield_data
  # ) +
  #   ggplot2::geom_point(ggplot2::aes(x = Year, y = Value), shape = 21, size = 3, fill = 'gold') +
  #   ggplot2::geom_path(ggplot2::aes(x = Year, y = predictions), alpha = 0.3) +
  #   tidyquant::theme_tq() + 
  #   ggplot2::geom_text(
  #     aes(
  #       x = 2015, 
  #       y = min(Value) + .1 *(mean(Value)), 
  #       label = paste0(
  #         'Slope = ',round(mod$coefficients[2], digits = 1))
  #       )
  #     )+
  #   ylab('Soybeans (Bushels / Acre)')
  # ylab('Soybeans (% Relative to Year 1990)')
  #   ylab(data.item)

}
  
  
    firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  

