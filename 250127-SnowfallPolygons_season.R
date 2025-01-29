# Load required libraries
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(tesseract)
library(lubridate)
library(tidyr)
library(jsonlite)

#SOURCE: https://www.nohrsc.noaa.gov/snowfall/

#Set time zone to pull files using Eastern time so GitHub Actions doesn't use UTC.
#Sys.setenv(TZ="America/New_York")
Sys.setenv(TZ="UTC")

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
  #r <- rast(path_to_raster)
  r <- try(rast(path_to_raster), silent = TRUE)
  
  # Check if path_to_raster hits 404 error because it hasn't been created yet.
  if (inherits(r, "try-error")) {
    #Create path to one version behind (if tried 12 UTC for 1/29/25, create path for 00 UTC 1/29/25, etc.)
    new_path_to_raster <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", 
                                 if_else (hour==12, format(Sys.Date(), "%Y%m"), format(Sys.Date()-days(1), "%Y%m")), 
                                 "/sfav2_CONUS_", timeframe, 
                                 if_else (hour==12, format(Sys.Date(), "%Y%m%d"), format(Sys.Date()-days(1), "%Y%m%d")), 
                                 if_else (hour==12, "00", "12"), ".tif")
    print(paste(substr(path_to_raster, nchar(path_to_raster) - 29, nchar(path_to_raster)), "is unavailable."))
    print(paste("Now trying", substr(new_path_to_raster, nchar(new_path_to_raster) - 29, nchar(new_path_to_raster))))
    r <- rast(new_path_to_raster)
    print(paste("Pulled", substr(new_path_to_raster, nchar(new_path_to_raster) - 29, nchar(new_path_to_raster))))
  } else {
    print(paste("Pulled", substr(path_to_raster, nchar(path_to_raster) - 29, nchar(path_to_raster))))
  }
  
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
    drop_na() %>%
    filter (color_factor != 0) %>% 
    ungroup() %>%
    #rename (hue = colors) %>%
    select (color_factor)
  
  return(r_poly_sf2)
}

# iterate the function for 24hr, 48hr and 72hr accumulations
snow_list <- lapply(timeframes, raster2vector)

# also get season accumulation
# it's a slightly different format so getting that separately
raster2vector_season <- function() {
  
  ## construct the URL dynamically
  # first get current year
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # determine the start of the season (oct 1 of the current or previous year)
  season_start_year <- if (format(Sys.Date(), "%m") < "10") {
    current_year - 1  # ff it's before oct, use the previous year
  } else {
    current_year  # otherwise, use the current year
  }
  
  season_start <- paste0(season_start_year, "093012")  # october 1, 12 UTC
  
  # define the end of the season as the current date
  season_end <- paste0(format(Sys.Date(), "%Y%m%d"), "12") # hour fixed, no "00" files
  
  # connect all this for seasonal URL
  path_to_raster <- paste0(
    "https://www.nohrsc.noaa.gov/snowfall/data/",
    format(Sys.Date(), "%Y%m"), 
    "/sfav2_CONUS_", season_start, "_to_", season_end, ".tif")
  
  # load the raster
  #r <- rast(path_to_raster)
  r <- try(rast(path_to_raster), silent = TRUE)
  
  # Check if path_to_raster hits 404 error because it hasn't been created yet.
  if (inherits(r, "try-error")) {
    #Create path to one day behind the version just tried (1/28/25 instead of 1/29/25, etc.)
    new_path_to_raster <- paste0(
      "https://www.nohrsc.noaa.gov/snowfall/data/",
      format(Sys.Date()-days(1), "%Y%m"), 
      "/sfav2_CONUS_", season_start, "_to_", 
      format(ymd(substr(season_end, 1, 8))-days(1), "%Y%m%d"), "12", ".tif")
    print(paste(substr(path_to_raster, nchar(path_to_raster) - 39, nchar(path_to_raster)), "is unavailable."))
    print(paste("Now trying", substr(new_path_to_raster, nchar(new_path_to_raster) - 39, nchar(new_path_to_raster))))
    r <- rast(new_path_to_raster)
    print(paste("Pulled", substr(new_path_to_raster, nchar(new_path_to_raster) - 39, nchar(new_path_to_raster))))
  } else {
    print(paste("Pulled", substr(path_to_raster, nchar(path_to_raster) - 39, nchar(path_to_raster))))
  }
  
  # round raster values to 1 decimal place to capture those between 0-0.1, or else they'll all be rounded to 0 since as.polygons() outputs the nearest integer
  r_rounded <- round(r, 1)*10
  
  # convert to polygons with rounded raster values
  r_poly <- as.polygons(r_rounded)
  
  # convert raster polygons to sf
  r_poly_sf <- st_as_sf(r_poly)

  # define the breaks and colors, different from 24/48/72h scale
  breaks <- c(-0.01, 0, 0.1, 1, 2, 6, 
              # NOAA's scale has them in ft, multiplying by 12:
              1*12, 2*12, 3*12, 4*12, 6*12, 8*12, 10*12, 15*12, 20*12, 30*12, 40*12, 50*12,
              1000)  # added -0.01 and 1000 to handle zero + >50ft explicitly
  
  # multiply by 10 to match the rounding done above.
  breaks = breaks*10
  
  # used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
  colors <- c("#FFFFFF", "#BDD7E7", "#6AAFD6", "#2E83BF", "#024F9B",
              "#022195", "#FEFE96", "#FEC501", "#FD8900", "#DB0C00",
              "#9E0101", "#690100", "#CCCCFE", "#9F8CD8", "#7D51A7",
              "#541471", "#2A0030", "#3DE0FF")
  
  # define the corresponding labels (numeric values)
  labels <- c("0 in", "0.1 in", "1 in", "2 in", "6 in", 
              "1 ft", "2 ft", "3 ft", "4 ft", "6 ft",
              "8 ft", "10 ft", "15 ft", "20 ft", "30 ft",
              "40 ft", "50 ft", "> 50 ft")
  
  # form what the column name should be 
  column_name <- paste0("sfav2_CONUS_", season_start, "_to_", season_end)
  
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
    filter (color_factor != "0 in") %>%
    ungroup() %>%
    #rename (hue = colors) %>%
    select (color_factor)
  
  #Validate geometry and then simplify slightly to reduce file size
  r_poly_sf3 <- r_poly_sf2 %>%
    st_make_valid() %>%
    st_simplify(dTolerance = 10)
  
  return(r_poly_sf3)
}
snow_list[[4]] <- raster2vector_season()

#snow_list <- raster2vector_season()

# adding "season" now
timeframes <- c(timeframes, "season_")

# get some additional info for the chatter
ocr_text <- function(timeframe){
  #timeframe = "season_"
  # make a url dynamically
  if (timeframe == "season_") {
    # first get current year
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # determine the start of the season (oct 1 of the current or previous year)
    season_start_year <- if (format(Sys.Date(), "%m") < "10") {
      current_year - 1  # ff it's before oct, use the previous year
    } else {
      current_year  # otherwise, use the current year
    }
    
    season_start <- paste0(season_start_year, "093012")  # october 1, 12 UTC
    
    # define the end of the season as the current date
    season_end <- paste0(format(Sys.Date(), "%Y%m%d"), "12") # hour fixed, no "00" files
    #season_end <- "2025013012"
    
    # connect all this for seasonal URL
    path_to_image <- paste0(
      "https://www.nohrsc.noaa.gov/snowfall/data/",
      format(Sys.Date(), "%Y%m"), 
      "/sfav2_CONUS_", season_start, "_to_", season_end, ".png")
  } else {
    path_to_image <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", format(Sys.Date(), "%Y%m"), "/sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour, ".png")
  }
  
  #print (paste("Trying", path_to_image))
  # extract text
  # make it all caps, since sometimes it reads "issued" and sometimes "Issued" - this helps subsetting later
  #text <- tesseract::ocr(path_to_image) %>% toupper()
  text <- try(tesseract::ocr(path_to_image) %>% toupper())
  
  # Check if path_to_image for season hits 404 error because it hasn't been created yet.
  if (inherits(text, "try-error") && timeframe == "season_") {
    #Create path to one day behind the version just tried (1/28/25 instead of 1/29/25, etc.)
    new_path_to_image <- paste0(
      "https://www.nohrsc.noaa.gov/snowfall/data/",
      format(Sys.Date()-days(1), "%Y%m"), 
      "/sfav2_CONUS_", season_start, "_to_", 
      format(ymd(substr(season_end, 1, 8))-days(1), "%Y%m%d"), "12", ".png")
    print(paste(substr(path_to_image, nchar(path_to_image) - 39, nchar(path_to_image)), "is unavailable."))
    print(paste("Now trying", substr(new_path_to_image, nchar(new_path_to_image) - 39, nchar(new_path_to_image))))
    
    text <- tesseract::ocr(new_path_to_image) %>% toupper()
    print(paste("Pulled", substr(new_path_to_image, nchar(new_path_to_image) - 39, nchar(new_path_to_image))))
  } 
  
  # Check if path_to_image for other three timeframse hits 404 error because it hasn't been created yet.
  if (inherits(text, "try-error") && timeframe != "season_") {
    new_path_to_image <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", 
                                if_else (hour==12, format(Sys.Date(), "%Y%m"), format(Sys.Date()-days(1), "%Y%m")), 
                                "/sfav2_CONUS_", timeframe, 
                                if_else (hour==12, format(Sys.Date(), "%Y%m%d"), format(Sys.Date()-days(1), "%Y%m%d")), 
                                if_else (hour==12, "00", "12"), ".png")
    
    print(paste(substr(path_to_image, nchar(path_to_image) - 29, nchar(path_to_image)), "is unavailable."))
    print(paste("Now trying", substr(new_path_to_image, nchar(new_path_to_image) - 29, nchar(new_path_to_image))))
    
    text <- tesseract::ocr(new_path_to_image) %>% toupper()
    print(paste("Pulled", substr(new_path_to_image, nchar(new_path_to_image) - 29, nchar(new_path_to_image))))
  } 
  
  # Print output if no error encountered.
  if (!inherits(text, "try-error") && timeframe == "season_") {
    print(paste("Pulled", substr(path_to_image, nchar(path_to_image) - 39, nchar(path_to_image))))
  } 
  if (!inherits(text, "try-error") && timeframe != "season_") {
    print(paste("Pulled", substr(path_to_image, nchar(path_to_image) - 29, nchar(path_to_image))))
  }

  # get the hour data was updated
  #data_updated <- ymd_hms(str_extract(text, "(?<=ISSUED\\s).*?UTC"))
  data_updated <-str_extract(text, "(?<=ISSUED\\s).*?UTC")
  
  return(data_updated)
}

# iterate the function for 24hr/48hr/72hr/season accumulations to get last updated data from .png
ocr_list <- lapply(timeframes, ocr_text)

#Save as GeoJSON using updated time.
#save_files <- function(x){
#  st_write (snow_list[[x]], paste0("outputs/", substr(timeframes[x], 1,3), "/", format(Sys.Date(), "%Y%m%d"), hour, "_", format(ocr_list[[x]], "%H%M%S"), "_", timeframes[x], "snow_accumulation.geojson"), append=FALSE)
#}
#lapply (1:length(timeframes), save_files)

#Save as TopoJSON, overwriting "latest" file.
save_files1 <- function(x){
  #geojsonio::topojson_write(snow_list, 
  geojsonio::topojson_write(snow_list[[x]], 
                            file = paste0("outputs/", str_remove(timeframes[x], "_"), "/", timeframes[x], "snow_accumulation_latest.json"),
                            object_name = "snowfall",
                            overwrite = TRUE)
}
lapply (1:length(timeframes), save_files1)

#Save as TopoJSON using updated time to maintain record.
save_files2 <- function(x){
  #geojsonio::topojson_write(snow_list, 
  geojsonio::topojson_write(snow_list[[x]], 
                            file = paste0("outputs/", str_remove(timeframes[x], "_"), "/", format(Sys.Date(), "%Y%m%d"), hour, "_", format(ocr_list[[x]], "%H%M%S"), "_", timeframes[x], "snow_accumulation.json"),
                            object_name = "snowfall",
                            overwrite = TRUE)
}
lapply (1:length(timeframes), save_files2)

#Save last updated times to JSON
ocr_times <- ocr_list %>% unlist()
last_updated <- tibble(timeframe = str_remove_all(timeframes, "_"), last_updated=ocr_times) 
write_json(last_updated, "outputs/last_updated.json")