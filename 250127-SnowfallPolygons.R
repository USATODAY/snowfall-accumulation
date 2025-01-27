# Load required libraries
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(tesseract)
library(lubridate)
library(tidyr)
library(geojsonio)

#SOURCE: https://www.nohrsc.noaa.gov/snowfall/

#Set time zone to pull files using Eastern time so GitHub Actions doesn't use UTC.
#Sys.setenv(TZ="America/New_York")
#Sys.setenv(TZ="UTC")

# Form path to URL: First, check the time and output 12 or 00 -- use 12 after 1 p.m. UTC. Otherwise, use 00.
hour <- if (as.numeric(format(Sys.time(), "%H")) >= 13 && as.numeric(format(Sys.time(), "%H")) < 24) {
  "12"
} else {
  "00"
}

timeframes <- c("24h_", "48h_", "72h_")

# we'll make a function to get and process the latest raster 
raster2vector <- function(timeframe){
  
  # make a url
  path_to_raster <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", format(Sys.Date(), "%Y%m"), "/sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour, ".tif")
  
  # load the raster
  r <- rast(path_to_raster)
  
  # round raster values to 1 decimal place to capture those between 0-0.1, or else they'll all be rounded to 0 since as.polygons() outputs the nearest integer
  r_rounded <- round(r, 1)*10
  
  # convert to polygons with rounded raster values
  r_poly <- as.polygons(r_rounded)
  
  # convert raster polygons to sf
  r_poly_sf <- st_as_sf(r_poly)
  
  # define the breaks and colors
  breaks <- c(-0.01, 0, 0.1, 1, 2, 
              3, 4, 6, 8, 12, 
              18, 24, 30, 36, 48, 
              60, 72, 96, 120, 500)  # added -0.01 and 500 to handle zero + >120 explicitly
  
  # multiply by 10 to match the rounding done above.
  breaks = breaks*10
  
  # used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
  colors <- c("#ffffff", "#e4eef4", "#bdd7e7", "#6bafd6", "#2d83be", 
              "#02509d", "#022195", "#fefe96", "#ffc500", "#ff8800", 
              "#dc0c00", "#9f0000", "#690000", "#330000", "#cdcdff", 
              "#a08dd9", "#7d51a6", "#551573", "#290030")  
  
  # define the corresponding labels (numeric values)
  labels <- c("0", "0.1", "1", "2", "3", 
              "4", "6", "8", "12", "18", 
              "24", "30", "36", "48", "60", 
              "72", "96", "120", ">120")
  
  # form what the column name should be 
  column_name <- paste0("sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour)
  
  # assign color categories based on the breaks
  r_poly_sf$color_factor <- cut(r_poly_sf[[column_name]],
                                breaks = breaks, 
                                labels = labels, 
                                include.lowest = FALSE, 
                                right = TRUE)  # ensure right endpoint is included
  
  # now assign colors to the color_factor
  r_poly_sf$colors <- colors[as.numeric(r_poly_sf$color_factor)]
  
  # spatial join all the 0.1 values between breaks together. This will reduce from hundreds of polygons to <=18 categories.
  r_poly_sf2 <- r_poly_sf %>%
    group_by (color_factor, colors) %>%
    summarize (geometry = st_union (geometry)) %>%
    drop_na() %>% # the 48h and 72h file had some NA outline showing as black, removing those
    mutate(accumulation = timeframe)
  
  return(r_poly_sf2)
  
}

# iterate the function for 24hr, 48hr and 72hr accumulations
snow_list <- lapply(timeframes, raster2vector)

# now get some additional info for the chatter
ocr_text <- function(timeframe){
  #timeframe="24h_"
  
  # make a url
  path_to_image <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", format(Sys.Date(), "%Y%m"), "/sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour, ".png")
  
  # extract text
  # make it all caps, since sometimes it reads "issued" and sometimes "Issued" - this helps subsetting later
  text <- tesseract::ocr(path_to_image) %>% toupper()
  
  # get the hour data was updated
  data_updated <- ymd_hms(str_extract(text, "(?<=ISSUED\\s).*?UTC"))
  
  return(data_updated)
  
}

# iterate the function for 24hr, 48hr and 72hr accumulations to get last updated data from .png
ocr_list <- lapply(timeframes, ocr_text)

#Save as GeoJSON using updated time.
save_files <- function(x){
  st_write (snow_list[[x]], paste0("outputs/", substr(timeframes[x], 1,3), "/", format(Sys.Date(), "%Y%m%d"), hour, "_", format(ocr_list[[x]], "%H%M%S"), "_", timeframes[x], "snow_accumulation.geojson"), append=FALSE)
}
lapply (1:length(timeframes), save_files)


save_files2 <- function(x){
  geojsonio::topojson_write(snow_list[[x]], 
                            file = paste0("outputs/", substr(timeframes[x], 1,3), "/", format(Sys.Date(), "%Y%m%d"), hour, "_", format(ocr_list[[x]], "%H%M%S"), "_", timeframes[x], "snow_accumulation.json"),
                            object_name = "snowfall",
                            overwrite = TRUE)
}
lapply (1:length(timeframes), save_files2)
