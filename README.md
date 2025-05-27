# Calculating New Lands
This R codebase provides functions to develop New Land calculations across the United States.
* 1_calc_ag_annually_by_county.R contains a function (get_ag_pixels) that finds all pixels that are agricultural at one time or another during a desired period of investigation and collects each pixel's full time series of agricultural / non-agricultural occupancy.
        * To run this function you will need to input a counties file destination and a metadata source for cropscape -- these are both contained in the /data/ folder. Additionally, the user will have to download cropscape CDL rasters for their area of interest from here :   https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php
