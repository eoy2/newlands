
getResults <- function(value_path, t_prod = NA, commodity, option = 'remote', normalize = T){
  lagged <- value_path |> readRDS()
#calculate area in acres for each hierarchical score, by year
acres <-  
  lagged |> 
  dplyr::group_by(written_code, year) |>
  dplyr::reframe(
    n = dplyr::n() * 0.2223945
  )

years <- 
  acres |> 
  dplyr::select(year) |>
  dplyr::distinct() |>
  dplyr::arrange() |>
  dplyr::mutate(
    year = as.numeric(year)
  ) |>
  unlist()

ESarea <- acres[acres$written_code == 'stable crop',]$n
CSarea <- acres[acres$written_code == 'crop change',]$n
LCarea <- acres[acres$written_code == 'land conversion',]$n

areas <- 
  dplyr::bind_cols(ESarea, CSarea, LCarea, years) |>
  magrittr::set_colnames(c('ESarea','CSarea','LCarea', 'Year'))

years <- c(dplyr::first(years) - 1, years)

if(option == 'local'){
yields <- 
  t_prod|> 
  dplyr::filter(
    STATISTICCAT_DESC %in% 'YIELD',
    REFERENCE_PERIOD_DESC %in%  'YEAR',
    COMMODITY_DESC %in% toupper(commodity)
    )
}else if(option == 'remote'){
yields <- 
  nassqs(
    list(
      commodity_desc = toupper(commodity),
      agg_level_desc = "STATE",
      state_alpha = "GA",
      statisticcat_desc = "YIELD",
      reference_period_desc = 'YEAR'
        )
  )
}else(print('need an option choice for data retrieval [remote or local]'))


colnames(yields) <- toupper(colnames(yields))

yields_short_desc <- names(which(table(yields$SHORT_DESC) == max(table(yields$SHORT_DESC))))

yields <- 
  yields |> dplyr::filter(SHORT_DESC %in% yields_short_desc)

Yi <- get_crop_yields(yield_data = yields, years = years, commodity) |> dplyr::arrange(Year)

if(normalize == TRUE){
  Yi$Value <- Yi$Value2
}

deltaIN <- calculate_deltaIN(Yi, variable = 'Value')

TP <- 
  dplyr::bind_cols(Yi$Year, Yi$Value, deltaIN) |>
  magrittr::set_colnames(c('Year', 'Yi','deltaIN')) |>
  dplyr::left_join(areas, by = 'Year') |>
  dplyr::mutate(
    LCi = LCarea * dplyr::lag(Yi), #amount of land change (area) in year i, adjusted for intensification in previous year
    CSi = CSarea * dplyr::lag(Yi), #amount of crop switching (area) in year i, adjusted for intensification in previous year
    INi = (ESarea) * (1 + deltaIN),#predicted intensification of pre-existing lands
    TPi = LCi + CSi + INi,
    NLi = LCi/TPi,
    INi2 = (ESarea + CSarea) * (1 + deltaIN),# wills original equation
    NL2 = LCi/(LCi + CSi + INi2),
    area = (CSarea + LCarea + ESarea),
    NL3 = LCarea / area
  )

if(option == 'local'){
  harv <- 
    t_prod |> 
    dplyr::filter(STATISTICCAT_DESC %in% 'AREA HARVESTED',
                  COMMODITY_DESC %in% toupper(commodity), 
                  grepl(SHORT_DESC, pattern = '- ACRES HARVESTED'), 
                  DOMAIN_DESC %in% 'TOTAL',
                  SOURCE_DESC %in% 'SURVEY', REFERENCE_PERIOD_DESC %in%  'YEAR') 
    
}else if(option == 'remote'){
  harv <- 
    nassqs(
      list(
        commodity_desc = toupper(commodity),
        agg_level_desc = "STATE",
        state_alpha = "GA",
        statisticcat_desc = "AREA HARVESTED",
        reference_period_desc = 'YEAR',
        domain_desc = 'TOTAL',
        source_desc = 'SURVEY'
      )
    )
  harv <-
    harv |> 
    dplyr::filter(grepl(short_desc, pattern = '- ACRES HARVESTED'))
}else(print('need an option choice for data retrieval [remote or local]'))

colnames(harv) <- toupper(colnames(harv))

short_desc <- names(which(table(harv$SHORT_DESC) == max(table(harv$SHORT_DESC))))

harv <-
  harv |>
  dplyr::filter(SHORT_DESC %in% short_desc) |> 
  dplyr::rename(
    Year = YEAR,
    Value = VALUE) |>
  dplyr::mutate(
    Year = as.numeric(Year),
    Value = stringr::str_remove_all(string = Value, pattern = ','),
    Value = as.numeric(Value)
  ) |> 
  dplyr::filter(Year >= 2014 & Year < 2024) |>
  dplyr::arrange(Year) |>
  dplyr::select(Year, Value) |> 
  dplyr::left_join(TP, by = 'Year') |>
  dplyr::filter(!is.na(area)) |>
  dplyr::mutate(
    error = abs(Value - area)/Value,
    n = dplyr::n(),
    mape = sum(error) * (1/n)
  )


zeros <- floor(log10(max(harv$area)))
max <- vector()
for(i in 1:10){
  max[i] = round(max(harv$area), digits = -i)
}
z <- max(max)
lim<- round(max(harv$Value/z,harv$area/z),digits = 1)
lab <- paste0('Mean Percent Error : ', round(unique(harv$mape),digits = 2))

p2 <-
  ggplot() + 
  geom_point(
    data = harv, 
    aes(y = area/z, x = Value / z)
  ) +
  ylab('Predicted Area of Production (Millions of Acres)') +
  xlab('Observed Area of Production (Millions of Acres)') +
  theme_bw()+ geom_abline(aes(slope = 1, intercept = 0)) + xlim(0,lim) + 
  ylim(0,lim)  +
  geom_text(aes(y = (5/6 * lim), x = (1/4 * lim), label = lab))


p1 <-
  ggplot() +
  geom_bar(
    data = TP,
    aes(x = Year, y = 100 * NLi),color = 'black', fill = 'coral', stat = 'identity'
  ) + geom_hline(yintercept = 40) + theme_classic() +
  geom_hline(
    yintercept = 100 * mean(TP$NLi, na.rm = T), 
    linetype = 'dashed')+
  geom_text(
    aes(
      label = paste0('10 year NL : ',round(100 * mean(TP$NLi, na.rm = T), digits = 2),'%'),
      x = 2016, y = 30)
  )  + ylab('NL Estimate (%)')

gridExtra::grid.arrange(p1,p2, ncol = 2)

}




