# Load required libraries
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(tesseract)
library(lubridate)
library(jsonlite)
library(tibble)
library(stars)

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
  #Set which library to use? 
  #stars::read_stars() or terra::rast() -- read_stars() takes longer but is truer to map on NOAA site.
  #stars is also slightly smaller file size: 17.7MB for full season compared to 20.7MB for terra
  
  version = "stars"
  
  #timeframe <- "24h_"
  # make a url
  path_to_raster <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", format(Sys.Date(), "%Y%m"), "/sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour, ".tif")
  
  if (version=="terra"){
    # load the raster
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
      #Set column name to previous version
      column_name <- paste0("sfav2_CONUS_", timeframe, if_else (hour==12, format(Sys.Date(), "%Y%m%d"), format(Sys.Date()-days(1), "%Y%m%d")), 
                            if_else (hour==12, "00", "12"))
    } else {
      print(paste("Pulled", substr(path_to_raster, nchar(path_to_raster) - 29, nchar(path_to_raster))))
      #Use current version of column name
      column_name <- paste0("sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour)
    }
    
    print (paste("Using column:", column_name))
    
    # convert to polygons with rounded raster values
    r_poly <- as.polygons(r,
                          round=TRUE,
                          digits=1, #round to nearest 1/10"
                          aggregate=TRUE) #false produces separate features for all with same accumulation value.
    
    # convert raster polygons to sf
    r_poly_sf2 <- st_as_sf(r_poly) %>%
      filter (get(column_name) > 0) %>%
      rename (accumulation = column_name)
  }
  
  if (version=="stars"){
    # load the raster
    r <- try(read_stars(path_to_raster), silent = TRUE)
    
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
      r <- read_stars(new_path_to_raster)
      print(paste("Pulled", substr(new_path_to_raster, nchar(new_path_to_raster) - 29, nchar(new_path_to_raster))))
      #Set column name to previous version
      column_name <- paste0("sfav2_CONUS_", timeframe, if_else (hour==12, format(Sys.Date(), "%Y%m%d"), format(Sys.Date()-days(1), "%Y%m%d")), 
                            if_else (hour==12, "00", "12"), ".tif")
    } else {
      print(paste("Pulled", substr(path_to_raster, nchar(path_to_raster) - 29, nchar(path_to_raster))))
      #Use current version of column name
      column_name <- paste0("sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour, ".tif")
    }
    
    print (paste("Using column:", column_name))
    
  # Convert raster to polygons
  #r_poly2 <- st_as_sf(r, merge = TRUE)  # Merge adjacent areas of same value
  
  # Round to the nearest tenth of an inch.
  # r_poly3 <- r_poly2 %>%
  #   mutate (accumulation = case_when(
  #   get(column_name) < 0 ~ -1,
  #   get(column_name) == 0 ~ 0,
  #   between(get(column_name), 0, 0.1) ~ 0.1, #anything below 0.1" rounds up to 0.1"
  #   TRUE ~ round(get(column_name), 1))) #round everything else to nearest 1/10"
  # 
# Round to the nearest half inch.
  # r_poly3 <- r_poly2 %>%
  #   mutate (accumulation = case_when(
  #     get(column_name) < 0 ~ -1,
  #     get(column_name) == 0 ~ 0,
  #     between(get(column_name), 0, 0.5) ~ 0.5, #anything below 0.25" rounds up to 0.5"
  #     TRUE ~ ceiling(get(column_name) * 2) / 2))  # Round to nearest 0.5"
  # 
  # Convert raster to polygons
  r_poly2 <- st_as_sf(r) %>%
    filter (get(column_name) > 0) %>% #remove NAs and 0
    rename (accumulation = column_name)
  
  # Round to the nearest inch.
  r_poly3 <- r_poly2 %>%
    mutate (accumulation = case_when(
      #accumulation< 0 ~ -1,
      #accumulation == 0 ~ 0,
      between(accumulation, 0, 0.1) ~ 0.1, #trace amounts round to 0.1
      between(accumulation, 0.1, 0.99) ~ 1, #round 0.1-0.4 up to 1
      TRUE ~ round(accumulation)))
  
  #check <- r_poly3 %>% st_drop_geometry() 
  #check2 <- check %>% count (accumulation)
  
  r_poly_sf2 <- r_poly3  %>%
    #filter (accumulation > 0) %>%
    group_by (accumulation) %>%
    summarize (geometry=st_union(geometry)) %>%
    st_make_valid()
  #check3 <- r_poly_sf2 %>% st_drop_geometry()
  }
  
  return(r_poly_sf2)
}

# iterate the function for 24hr, 48hr and 72hr accumulations
snow_list <- lapply(timeframes, raster2vector)

# also get season accumulation
# it's a slightly different format so getting that separately
raster2vector_season <- function() {
  #which library to use? stars::read_stars() or terra::rast() -- read_stars() takes longer but is truer to map on NOAA site.
  version = "stars"
  
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
  
  if (version=="terra"){
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
    column_name <- paste0("sfav2_CONUS_", season_start, "_to_", format(ymd(substr(season_end, 1, 8))-days(1), "%Y%m%d"), "12")
  } else {
    print(paste("Pulled", substr(path_to_raster, nchar(path_to_raster) - 39, nchar(path_to_raster))))
    column_name <- paste0("sfav2_CONUS_", season_start, "_to_", season_end)
  }
  
  print (paste("Using column:", column_name))
  
  # convert to polygons with rounded raster values
  r_poly <- as.polygons(r,
                        round=TRUE,
                        digits=1, #round to nearest 1" (will lose 0.01-0.49" to 0)
                        aggregate=TRUE) #false produces separate features for all with same accumulation value.
  
  # # convert raster polygons to sf
  # r_poly2 <- st_as_sf(r_poly) %>%
  #   filter (get(column_name) > 0) %>% #remove NAs and 0
  #   rename (accumulation = column_name)
  # 
  # r_poly3 <- r_poly2 %>%
  #   mutate (accumulation = case_when(
  #     #accumulation< 0 ~ -1,
  #     #accumulation == 0 ~ 0,
  #     between(accumulation, 0, 1) ~ 1, #round 0.1-0.4 up to 1
  #     TRUE ~ round(accumulation)))
  # 
  # #check <- r_poly2 %>% st_drop_geometry()
  # 
  # r_poly4 <- r_poly3 %>%
  #   group_by (accumulation) %>%
  #   summarize (geometry=st_union(geometry))
  }
  
  
  # round raster values to 1 decimal place to capture those between 0-0.1, or else they'll all be rounded to 0 since as.polygons() outputs the nearest integer
  #r_rounded <- round(r, 1)*10
  
  # convert to polygons with rounded raster values
  #r_poly <- as.polygons(r_rounded)
  
  # convert raster polygons to sf
  #r_poly_sf <- st_as_sf(r_poly)

  # define the breaks and colors, different from 24/48/72h scale
  # NOAA's scale
  #breaks <- c(-0.01, 0, 0.1, 1, 2, 6, 
              # NOAA's scale has them in ft, multiplying by 12:
  #            1*12, 2*12, 3*12, 4*12, 6*12, 8*12, 10*12, 15*12, 20*12, 30*12, 40*12, 50*12,
  #            1000)  # added -0.01 and 1000 to handle zero + >50ft explicitly
  
  # multiply by 10 to match the rounding done above.
  #breaks = breaks*10
  
  # used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
  #colors <- c("#FFFFFF", "#BDD7E7", "#6AAFD6", "#2E83BF", "#024F9B",
  #            "#022195", "#FEFE96", "#FEC501", "#FD8900", "#DB0C00",
  #            "#9E0101", "#690100", "#CCCCFE", "#9F8CD8", "#7D51A7",
  #            "#541471", "#2A0030", "#3DE0FF")
  
  # define the corresponding labels (numeric values)
  #labels <- c("0 in", "0.1 in", "1 in", "2 in", "6 in", 
  #            "1 ft", "2 ft", "3 ft", "4 ft", "6 ft",
  #            "8 ft", "10 ft", "15 ft", "20 ft", "30 ft",
  #            "40 ft", "50 ft", "> 50 ft")
  
  # USAT scale
  # breaks2 <- c(-0.01, 0, 0.1, 1, 2, 3, 6,
  #              1*12, 2*12, 3*12, 10*12, 20*12,
  #              1000)
  # breaks2 = breaks2*10
  # # used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
  # colors2 <- c("#FFFFFF", "#BDD7E7", "#6AAFD6", "#2E83BF", "#024F9B",
  #              "#022195", "#FEFE96", "#FEC501", "#FD8900", "#DB0C00",
  #              "#9E0101", "#690100")
  # labels2 <- c("0 in", "0.1 in", "1 in", "2 in", "3 in", "6 in", 
  #              "1 ft", "2 ft", "3 ft", "10 ft", "20 ft", 
  #              "> 30 ft")
  # 
  # form what the column name should be 
  #column_name <- paste0("sfav2_CONUS_", season_start, "_to_", season_end)
  
  # assign color categories based on the breaks we've defined for USAT
  # r_poly_sf$color_factor <- cut(r_poly_sf[[column_name]],
  #                               breaks = breaks2, 
  #                               labels = labels2, 
  #                               include.lowest = FALSE, 
  #                               right = TRUE)  # ensure right endpoint is included
  # 
  # now assign colors to the color_factor
  # r_poly_sf$colors <- colors[as.numeric(r_poly_sf$color_factor)]
  # 
  # # spatial join all the 0.1 values between breaks together. This will reduce from hundreds of polygons to <=18 categories.
  # r_poly_sf2 <- r_poly_sf %>%
  #   group_by (color_factor, colors) %>%
  #   summarize (geometry = st_union (geometry)) %>%
  #   #drop_na() %>% # the 48h and 72h file had some NA outline showing as black, removing those
  #   filter (color_factor != "0 in") %>%
  #   ungroup() %>%
  #   select (color_factor)
  
  # spatial join all the 0.1 values between breaks together. This will reduce from hundreds of polygons to <=18 categories.
  #r_poly_sf2 <- r_poly_sf %>%
    #mutate (accumulation = r_poly_sf[[column_name]]/10) %>%
    #select (accumulation, color_factor) %>%
    #filter (color_factor != "0 in") 
  
  # #Validate geometry and then simplify slightly to reduce file size
  # r_poly_sf3 <- r_poly_sf2 %>%
  #   st_make_valid() %>%
  #   st_simplify(dTolerance = 10)
  
  if (version=="stars"){
    r <- try(read_stars(path_to_raster), silent = TRUE)
    
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
      r <- read_stars(new_path_to_raster)
      print(paste("Pulled", substr(new_path_to_raster, nchar(new_path_to_raster) - 39, nchar(new_path_to_raster))))
      column_name <- paste0("sfav2_CONUS_", season_start, "_to_", format(ymd(substr(season_end, 1, 8))-days(1), "%Y%m%d"), "12", ".tif")
    } else {
      print(paste("Pulled", substr(path_to_raster, nchar(path_to_raster) - 39, nchar(path_to_raster))))
      column_name <- paste0("sfav2_CONUS_", season_start, "_to_", season_end, ".tif")
    }
  
    r_poly <- st_as_sf(r, merge = TRUE)  # Merge adjacent areas of same value
  }
  
  # convert raster polygons to sf
  r_poly2 <- st_as_sf(r_poly) %>%
    filter (get(column_name) > 0) %>% #remove NAs and 0
    rename (accumulation = column_name)
  
  r_poly3 <- r_poly2 %>%
    mutate (accumulation = case_when(
      #accumulation< 0 ~ -1,
      #accumulation == 0 ~ 0,
      between(accumulation, 0, 1) ~ 1, #round 0.1-0.4 up to 1
      TRUE ~ round(accumulation)))
  
  #check <- r_poly2 %>% st_drop_geometry()
  
  r_poly4 <- r_poly3 %>%
    group_by (accumulation) %>%
    summarize (geometry=st_union(geometry)) %>%
    st_make_valid()
  
  return(r_poly4)
}
system.time( #202s for stars
  snow_list[[4]] <- raster2vector_season()
)
#snow_list <- raster2vector_season()

# adding "season" now
timeframes <- c(timeframes, "season_")

# get some additional info for the chatter
ocr_text <- function(timeframe){
  #timeframe = "24h_"
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
  if (timeframe == "season_"){
    if (inherits(text, "try-error")) {
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
    } else {
      print(paste("Pulled", substr(path_to_image, nchar(path_to_image) - 39, nchar(path_to_image))))
    }
  }
  
  # Check if path_to_image for other three timeframes hits 404 error because it hasn't been created yet.
  if (timeframe != "season_"){
    if (inherits(text, "try-error")) {
      new_path_to_image <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", 
                                  if_else (hour==12, format(Sys.Date(), "%Y%m"), format(Sys.Date()-days(1), "%Y%m")), 
                                  "/sfav2_CONUS_", timeframe, 
                                  if_else (hour==12, format(Sys.Date(), "%Y%m%d"), format(Sys.Date()-days(1), "%Y%m%d")), 
                                  if_else (hour==12, "00", "12"), ".png")
      
      print(paste(substr(path_to_image, nchar(path_to_image) - 29, nchar(path_to_image)), "is unavailable."))
      print(paste("Now trying", substr(new_path_to_image, nchar(new_path_to_image) - 29, nchar(new_path_to_image))))
      
      text <- tesseract::ocr(new_path_to_image) %>% toupper()
      print(paste("Pulled", substr(new_path_to_image, nchar(new_path_to_image) - 29, nchar(new_path_to_image))))
    } else {
      print(paste("Pulled", substr(path_to_image, nchar(path_to_image) - 29, nchar(path_to_image))))
    }
  }

  # get the hour data was updated
  #data_updated <- ymd_hms(str_extract(text, "(?<=ISSUED\\s).*?UTC"))
  data_updated <-str_extract(text, "(?<=ISSUED\\s).*?UTC")
  
  return(data_updated)
}

# iterate the function for 24hr/48hr/72hr/season accumulations to get last updated data from .png
ocr_list <- lapply(timeframes, ocr_text)

#Save last updated times to JSON
ocr_times <- ocr_list %>% unlist()
last_updated <- tibble(timeframe = str_remove_all(timeframes, "_"), last_updated=ocr_times) 
write_json(last_updated, "outputs/last_updated.json")

#Convert list to dates for saving properly
ocr_list <- lapply(ocr_list, ymd_hms)

#Save as TopoJSON, overwriting "latest" file.
save_files1 <- function(x){
  #geojsonio::topojson_write(snow_list, 
  geojsonio::topojson_write(snow_list[[x]], 
                            file = paste0("outputs/", str_remove(timeframes[x], "_"), "/", timeframes[x], "inches_snow_accumulation_latest_full.json"),
                            #file = paste0("outputs/test/", timeframes[x], "inches_snow_accumulation_latest_full.json"),
                            object_name = "snowfall",
                            overwrite = TRUE)
}
lapply (1:4, save_files1)
#save season separately
# geojsonio::topojson_write(snow_list[[4]], 
#                           file = paste0("outputs/season/season_inches_snow_accumulation_latest_full.json"),
#                           object_name = "snowfall",
#                           overwrite = TRUE)

#Save as TopoJSON using updated time to maintain record.
save_files2 <- function(x){
  geojsonio::topojson_write(snow_list[[x]], 
                            file = paste0("outputs/", str_remove(timeframes[x], "_"), "/", format(Sys.Date(), "%Y%m%d"), hour, "_", format(ocr_list[[x]], "%H%M%S"), "_", timeframes[x], "snow_accumulation.json"),
                            object_name = "snowfall",
                            overwrite = TRUE)
}
lapply (1:length(timeframes), save_files2)