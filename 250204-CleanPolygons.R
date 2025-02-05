library (sf)
library (geojsonio)
library(smoothr)
library(dplyr)
library(stringr)
library(jsonlite)
library(tesseract)
library(lubridate)

timeframes <- c("24h", "48h", "72h", "season")

lapply(1:3, function (x){
#lapply(4, function (x){
  #x=3
  timeframe <- timeframes[[x]]
  
  # this_shapefile <- st_read (paste0("python/", timeframe, "_snowfall.shp")) %>%
  #   st_make_valid () %>%
  #   rename (accumulation = DN) %>%
  #   filter (accumulation > 0) %>%
  #   mutate (accumulation = case_when (
  #     accumulation == 1 ~ 0.1,
  #     TRUE ~ accumulation - 1),
  #     area = st_area(geometry))

  #redone to separate trace, <1 and all others.
  this_shapefile <- st_read (paste0("python/", timeframe, "_snowfall.shp")) %>%
    st_make_valid () %>%
    rename (accumulation = DN) %>%
    filter (accumulation > 0) %>%
    mutate (accumulation = case_when (
      accumulation == 1 ~ 0.1,
      accumulation == 2 ~ 0.75,
      TRUE ~ accumulation - 2),
      area = st_area(geometry))
  
  print ("starting smoothed_polygons")
  smoothed_polygons <- smooth(this_shapefile, method = "ksmooth")
  print ("starting smoothed_polygons2")
  #smoothed_polygons2 <- smoothed_polygons #%>%
    #st_make_valid() 
  
  smoothed_polygons3 <- smoothed_polygons %>%
    st_transform(5070) %>%   # Convert to a projected CRS to avoid Error: Snap function moved vertex (nan, nan, nan) by 3.14159265358979, which is more than the specified snap radius of 1.23413730872177e-09
    st_make_valid() 

  #print ("starting smoothed_polygons3")
  #smoothed_polygons3 <- st_transform(smoothed_polygons2, crs = 5070) combined above
  
  print ("starting singlepixels")
  #buffer single pixels at 1300
  singlepixels <- smoothed_polygons3 %>% 
    filter (as.numeric(area) < 20000000) %>%
    st_buffer (1200) 
  
  print ("starting multipixels")
  #buffer multipixel polygons s at 1500 to fill in gaps
  multipixels <- smoothed_polygons3 %>% 
    filter (as.numeric(area) >= 20000000) %>%
    st_buffer (1800)
  
  print ("starting smoothed_polygons4")
  #rejoin two types
  smoothed_polygons4 <- rbind (multipixels, singlepixels) %>%
    arrange (desc(area)) %>%
    st_make_valid()
  
  print ("starting smoothed_polygons5")
  #smoothed_polygons4 <- st_buffer (smoothed_polygons3, 1300)
  smoothed_polygons5 <- st_transform(smoothed_polygons4, crs = 4326) %>%
    arrange(desc(area)) %>%
    st_make_valid()
  
  print ("starting smoothed_polygons6")
  smoothed_polygons6 <- smoothed_polygons5 %>%
    st_simplify(dTolerance = 500) %>% 
    st_make_valid() %>%
    arrange(desc(area), desc(accumulation)) #%>%
  
  file3 <- paste0("outputs/", timeframe, "/", timeframe, "_inches_snow_accumulation_latest.json")
  
  geojsonio::topojson_write(smoothed_polygons6, 
                            file = file3,
                            object_name = "snowfall", 
                            overwrite = TRUE)
})

# Fix mapshaper-compressed version of seasonal map
# lapply(4, function (x){
#   timeframe <- timeframes[[x]]
#   file <- paste0("outputs/", timeframe, "/", timeframe, "_inches_snow_accumulation_latest.json")
#   
#   squeezed <- topojson_read(file)
#   
#   #remove slivers on edges
#   squeezed2 <- squeezed %>% 
#     #st_make_valid() %>%
#     st_simplify(dTolerance = 0.01)
#   
#   #save fixed file
#   geojsonio::topojson_write(squeezed2, 
#                             file = file,
#                             object_name = "snowfall", 
#                             overwrite = TRUE)
# })

# set timeframes
timeframes <- c("24h_", "48h_", "72h_", "season_")

# Form path to URL: First, check the time and output 12 or 00 -- use 12 after 1 p.m. UTC. Otherwise, use 00.
hour <- if (as.numeric(format(Sys.time(), "%H")) >= 13 && as.numeric(format(Sys.time(), "%H")) < 24) {
  "12"
} else {
  "00"
}

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
